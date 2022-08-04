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
# Get forest -> agriculture transitions
burned_cells <-
  yearly_burn %>%
  group_by(id) %>%
  filter(any(lulc == 3) & any(lulc %in% c(15, 39, 20, 41, 36))) %>%
  select(id, date, year, lulc, burn) %>%
  ungroup()

change_cells <- burned_cells %>%
  mutate(
    forest_year = if_else(lulc == 3, year, NA_real_),
    anth_year = if_else(lulc != 3, year, NA_real_)
  ) %>%
  group_by(id) %>%
  summarise(
    max_forest = max(forest_year, na.rm = TRUE),
    min_anth = min(anth_year, na.rm = TRUE)
  ) %>%
  mutate(condition = if_else(max_forest > min_anth, FALSE, TRUE)) %>%
  filter(condition == TRUE) %>%
  select(id, transition_year = min_anth)

annual_cells <- burned_cells %>%
  inner_join(change_cells, by = "id")

annual_cells <- annual_cells %>%
  mutate(burn_year = if_else(burn != 0, year, 0)) %>%
  group_by(id) %>%
  summarise(
    total_burns = sum(burn),
    last_burn = max(burn_year)
  ) %>%
  full_join(annual_cells, by = "id") %>%
  select(-c(burn, year))

rm(burned_cells, yearly_burn)

ds_mvar %>%
  filter(id %in% pull(change_cells, id)) %>%
  select(id, date, burn:precip) %>%
  collect() %>%
  left_join(annual_cells, by = c("date", "id")) %>%
  write_parquet("data/lulc_change.parquet", version = "2.0")

read_parquet("data/lulc_change.parquet")
