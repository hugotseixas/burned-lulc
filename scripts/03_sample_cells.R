# HEADER ----------------------------------------------------------------------
#
# Title:
# Description:
#
# Author:         Hugo Tameirao Seixas
# Contact:        hugo.seixas@alumni.usp.br / tameirao.hugo@gmail.com
# Date:           2021-02-05
#
# Notes:
#
# LIBRARIES -------------------------------------------------------------------
#
library(sf)
library(arrow)
library(lubridate)
library(fs)
library(glue)
library(tidyverse)
#
# OPTIONS ---------------------------------------------------------------------
#
source("conf/config.R")
sf_use_s2(TRUE) # Use spherical operators for sf
#
# OPEN DATA SETS --------------------------------------------------------------

# Masks
ds_mask <- open_dataset("data/tables/mask/")

# Monthly variables
ds_mvar <- open_dataset("data/tables/monthly_var/")

# LULC
ds_lulc <- open_dataset("data/tables/lulc/")

# EXTRACT DATA ----------------------------------------------------------------

## Get yearly burned pixels ----

# Create list of tiles
tile_list <-
  dir_ls("data/tables/monthly_var/") %>%
  str_extract("(?<=tile=).*")

# Get yearly burned pixels table
yearly_burn <-
  map_df(
    .x = tile_list,
    .f =
      ~ {

        # Summarize burning count for each year
        mvar <-
          ds_mvar %>%
          select(id, date, burn, tile) %>%
          filter(tile == .x) %>%
          collect() %>%
          # Transform burning day of the month into binary
          # (burned = 1, not burned = 0)
          mutate(
            burn = if_else(
              condition = burn >= 1,
              true = 1,
              false =  0
            )
          ) %>%
          group_by(id, year = year(date)) %>%
          summarise(burn = sum(burn, na.rm = TRUE), .groups = "drop")

        # Join yearly burn with lulc table
        return(
          ds_lulc %>%
            filter(tile == .x) %>%
            collect() %>%
            mutate(year = year(date)) %>%
            full_join(mvar, by = c("id", "year")) %>%
            drop_na()
        )

      }
  )

## Collect mask values ----
mask <-
  ds_mask %>%
  select(id, x, y, mask) %>%
  collect()

# SAMPLE CELLS ----------------------------------------------------------------

## Query burned cells ----
# Get cells which only burned once
burned_cells <-
  yearly_burn %>%
  group_by(id) %>%
  summarise(burn = sum(burn, na.rm = TRUE)) %>%
  filter(burn == 1) %>%
  select(id) %>%
  inner_join(yearly_burn, by = "id")

# Calculate relative years, and filter cells of forest only
burned_cells <-
  burned_cells %>%
  filter(burn == 1) %>% # Get the year of the burn
  rename(burn_year = year) %>%
  select(id, burn_year) %>%
  full_join(burned_cells, by = "id") %>% # Regenerate table
  mutate(relative_year = year - burn_year) %>%
  group_by(id) %>%
  filter(all(lulc == 3)) %>% # Forest only
  select(id, date, relative_year, burn) %>%
  ungroup()

# Get cells which belongs to the initial mask (mask == 2)
# Initial mask is the one that the whole time series is occupied by forest
# (class 3), and that presented at least one burning event
burned_cells <-
  burned_cells %>%
  inner_join(mask, by = "id") %>%
  filter(mask == 2) %>%
  select(-mask)

## Query control cells ----
# Get cells which contains only forest, no burn,
# and are inside the expanded mask
control_cells <-
  yearly_burn %>%
  group_by(id) %>%
  filter(all(lulc == 3, burn == 0)) %>%
  select(id) %>%
  inner_join(mask, by = "id") %>%
  filter(mask == 1) %>%
  select(-mask) %>%
  ungroup()

## Perform sampling ----
walk(
  .x = c(1:n_iteration),
  .f =
    ~ {

      sample_id <- .x

      cat("\r", "Performing sampling: ", sample_id)

      ### Extract and match samples ----
      sample <-
        burned_cells %>%
        distinct(id, x, y) %>%
        sample_n(n_experiment) %>% # Number of experiment cells to sample
        rename(burn_id = id) %>%
        st_as_sf(coords = c("x", "y"), crs = "EPSG:4326") %>%
        st_join(
          st_as_sf(
            control_cells %>%
              sample_n(n_control) %>% # Number of control cells to sample
              rename(control_id = id),
            coords = c("x", "y"),
            crs = "EPSG:4326"
          ),
          join = st_is_within_distance, # Join cells inside a distance buffer
          dist = match_dist
        ) %>%
        select(burn_id, control_id) %>%
        drop_na() # Drops cells with no match

      ## Add info to samples ----
      # Add date information and create a unique id for each sample match
      sample <-
        sample %>%
        mutate(pair_id = row_number()) %>%
        left_join(
          y = burned_cells %>% select(burn_id = id, date, relative_year),
          by = "burn_id"
        ) %>%
        pivot_longer(
          cols = burn_id:control_id, names_to = "group_name", values_to = "id"
        )

      ## Extract monthly values for the sample ----
      sample_mvar <-
        map_df(
          .x = tile_list,
          .f =
            ~ {

              # Get monthly values
              mvar <-
                ds_mvar %>%
                filter(tile == .x) %>%
                select(id, date, burn:precip) %>%
                collect() %>%
                rename(mon_burn = burn) %>%
                mutate(year = year(date))

              # Join with cells sample
              return(
                sample %>%
                  mutate(year = year(date)) %>%
                  select(-date) %>%
                  inner_join(mvar, by = c("id", "year")) %>%
                  select(-c(year, geometry))
              )

            }
        )

      ## Export sample to parquet data set ----
      # Create folder
      dir_create(glue::glue("data/tables/sample/sample_id={sample_id}/"))

      # Export as a parquet file
      write_parquet(
        x = sample_mvar,
        sink = glue(
          "data/tables/sample/sample_id={sample_id}/sample_{sample_id}.parquet"
        ),
        version = "2.0"
      )

    }
)
