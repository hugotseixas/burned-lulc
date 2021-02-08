# HEADER ----------------------------------------------------------------------
#
# Title:          Transform raster into tables
# Description:    This routine loads chunks of raster (of different variables)
#                 and transform them into a table containing its values, its
#                 occurrence year and coordinates information.
#                 The tables are then exported to a PostgreSQL database.
#
# Author:         Hugo Tameirao Seixas
# Contact:        hugo.seixas@alumni.usp.br / tameirao.hugo@gmail.com
# Date:           2020-10-07
#
# Notes:         Check the "00_config.R" file to change options for this
#                routine
#
# LIBRARIES -------------------------------------------------------------------
#
#
library(raster)
library(tabularaster)
library(glue)
library(fs)
library(furrr)
library(magrittr)
library(lubridate)
library(arrow)
library(tidyverse)
#
# OPTIONS ---------------------------------------------------------------------
#
source("conf/config.R")
#
# EXPORT MASK TABLES ----------------------------------------------------------

# Create tables dir
dir_create("data/tables")

## Get paths for mask raster ----
# Burned pixels mask
initial_mask_path <-
  dir_ls('data/original_raster/', regexp = "initial_mask.+tif$")

# Not burned pixels mask
expanded_mask_path <-
  dir_ls('data/original_raster/', regexp = "expanded_mask.+tif$")

## Convert mask raster to table ----
mask_list <-
  map2(
    .x = list(initial_mask_path, expanded_mask_path),
    .y = list("initial_mask", "expanded_mask"),
    function(path_list, mask_name) {

      mask_paths <- path_list

      # Convert raster to table
      mask <-
        map_df(
          .x = mask_paths,
          .id = "tile",
          function(raster_path) {

            return(
              raster(raster_path) %>%
                tabularaster::as_tibble(xy = TRUE, cell = TRUE) %>%
                filter(!(cellvalue == 0)) %>%
                rename(!!sym(mask_name) := cellvalue)
            )

          }
        )

      # Add tile number
      mask %<>%
        mutate(tile = str_extract(tile, "(?<=mask-).*(?=.tif)"))

    }
  )

## Join masks ----
mask <-
  reduce(mask_list, full_join, by = c("tile", "cellindex", "x", "y")) %>%
  replace_na(replace = list(initial_mask = 0)) %>%
  arrange(x, y) %>%
  mutate(
    id = row_number(),
    mask = initial_mask + expanded_mask
  ) %>%
  dplyr::select(tile, cellindex, id, x, y, mask)

## Save masks table ----
mask %>%
  group_by(tile) %>%
  group_walk(
    ~ {

      # Create folder to store files
      dir_create(glue("data/tables/mask/tile={.y}/"))

      # Write parquet file
      write_parquet(
        x = .x,
        sink = glue("data/tables/mask/tile={.y}/mask-{.y}.parquet"),
        version = "2.0"
      )

    }
  )

# EXPORT ANNUAL LULC ----------------------------------------------------------

## Get lulc raster paths ----
path_list <- dir_ls(path = "data/original_raster/", regexp = "lulc")

## Plan multi-threading ----
plan(strategy = "multisession", workers = workers_num)

## Convert raster to table ----
future_walk(
  path_list,
  .options = furrr_options(seed = NULL),
  # Future was raising a warning about the generation of random numbers,
  # to suppress this I set the option "seed = NULL", since the function
  # is not generating random numbers
  function(raster_path) {

    # Load raster stack
    raster_stack <- stack(raster_path)

    # Get band names
    raster_bands <- names(raster_stack)

    # Get tile number
    tile <- str_extract(raster_path, "(?<=lulc-).*(?=.tif)")

    # Convert raster to table
    raster_table <-
      map2_df(
        .x = raster_bands,
        .y = seq_along(raster_bands),
        function(band_name, band_num) {

          # Load raster
          raster <- raster::raster(raster_path, band = band_num)

          # Transform to table and join with mask table
          return(
            raster %>%
              tabularaster::as_tibble(xy = FALSE, cell = TRUE) %>%
              mutate(tile = tile) %>%
              inner_join(mask, by = c('tile', 'cellindex')) %>%
              mutate(date = str_remove(band_name, "X")) %>%
              mutate(date = ymd(date, truncated = 2)) %>%
              dplyr::select(id, date, cellvalue) %>%
              rename(lulc = cellvalue)
          )

        }
      )

    # Create folder to store files
    dir_create(glue("data/tables/lulc/tile={tile}/"))

    # Write parquet file
    write_parquet(
      x = raster_table,
      sink = glue("data/tables/lulc/tile={tile}/lulc-{tile}.parquet"),
      version = "2.0"
    )

  }
)

# EXPORT MONTHLY VARIABLES ----------------------------------------------------

## Get name of variables ----
tile_list <-
  unique(
    str_extract(
      string = dir_ls('data/original_raster/', glob = '*.tif'),
      pattern = "(?<=-)[^.tif]+"
    )
  )

## Transform raster to table ----
walk(
  tile_list,
  function(tile) {

    cat('\n')
    cat('Creating tables for: ', tile, '\n', sep = ' ')

    # Get all raster files for one raster chunk
    path_list <-
      dir_ls(path = "data/original_raster/", regexp = glue("{tile}.+tif$")) %>%
      as_tibble() %>%
      rename(path = value) %>%
      mutate(var = str_extract(path, "(?<=raster/)[^-|.]+")) %>%
      filter(!var %in% c("initial_mask", "expanded_mask", "lulc"))

    plan(strategy = "multisession", workers = workers_num)

    # Convert raster to table
    raster_table <-
      future_map2(
        .x = path_list$path,
        .y = path_list$var,
        .options = furrr_options(seed = NULL),
        # Future was raising a warning about the generation of random numbers,
        # to suppress this I set the option "seed = NULL", since the function
        # is not generating random numbers
        function(raster_path, raster_var) {

          # Load raster stack
          raster_stack <- stack(raster_path)

          # Get band names
          raster_bands <- names(raster_stack)

          raster_table <-
            map2_df(
              .x = raster_bands,
              .y = seq_along(raster_bands),
              function(band_name, band_num) {

                # Load raster
                raster <- raster::raster(raster_path, band = band_num)

                # Transform to table and join with mask table
                return(
                  raster %>%
                    tabularaster::as_tibble(xy = FALSE, cell = TRUE) %>%
                    mutate(tile = tile) %>%
                    inner_join(mask, by = c('tile', 'cellindex')) %>%
                    mutate(
                      date = str_remove(band_name, "X"),
                      cellvalue = ifelse(
                        is.nan(cellvalue),
                        NA_real_,
                        cellvalue
                      )
                    ) %>%
                    dplyr::select(id, cellvalue, date) %>%
                    rename(!!sym(raster_var) := cellvalue)
                )

              }
            )

        }
      )

    # Join list of tables
    raster_table <- reduce(raster_table, full_join, by = c('id', 'date'))

    # Make some mutations and organize table
    raster_table %<>%
      mutate(
        burn_day = mday(
          as_date(
            burn,
            origin = ceiling_date(ymd(date, truncated = 1), unit = "year")
          )
        ),
        date = lubridate::ymd(date, truncated = 1)
      ) %>%
      mutate(burn = if_else(burn == 0, 0L, burn_day)) %>%
      dplyr::select(-burn_day) %>%
      arrange(id, date) %>%
      relocate(id, date)

    # Create folder to store files
    dir_create(glue("data/tables/monthly_var/tile={tile}/"))

    # Write parquet file
    write_parquet(
      x = raster_table,
      sink = glue(
        "data/tables/monthly_var/tile={tile}/",
        "monthly_var-{tile}.parquet"
        ),
      version = "2.0"
    )

  }
)
