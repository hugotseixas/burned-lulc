## ------------------------------------------------------------------------- ##
####* ------------------------------- HEADER ---------------------------- *####
## ------------------------------------------------------------------------- ##
##
#### Description ####
##
## Script name:   Transform raster into tables
##
## Description:   This routine loads chunks of raster (of different variables)
##                and transform them into a table containing its values, its
##                occurrence year and coordinates information.
##                The tables are then exported to a PostgreSQL database.
##
## Author:        Hugo Tameirao Seixas
## Contact:       hugo.seixas@alumni.usp.br / tameirao.hugo@gmail.com
##
## Date created:  2020-10-07
##
## Copyright (c) Hugo Tameirao Seixas, 2020
##
## ------------------------------------------------------------------------- ##
##
## Notes:
##
##
##
## ------------------------------------------------------------------------- ##
##
#### Libraries ####
##
library(DBI)
library(RPostgres)
library(raster)
library(tabularaster)
library(glue)
library(fs)
library(furrr)
library(magrittr)
library(lubridate)
library(tidyverse)
library(tictoc)
##
## ------------------------------------------------------------------------- ##
##
#### Options ####
##
source("00_config.R")
source("R/run_sql.R")
##
## ------------------------------------------------------------------------- ##
####* ------------------------------- CODE ------------------------------ *####
## ------------------------------------------------------------------------- ##

tic()

####' ----- Create connection with database ####
con <-
  dbConnect(
    Postgres(),
    host = 'localhost',
    dbname = 'burned_lulc',
    user = 'postgres',
    password = 'postgres'
  )

#### -------------------------------------------------- Export mask tables ####

####' ----- Create tables dir ####
dir_create("data/tables")

####' ----- Get paths for initial mask raster ####
initial_mask_path <- dir_ls('data/original_raster/', regexp = "initial_mask")

####' ----- Get paths for expanded mask raster ####
expanded_mask_path <- dir_ls('data/original_raster/', regexp = "expanded_mask")

####' ----- Convert mask raster to table ####
mask_list <-
  map2(
    .x = list(initial_mask_path, expanded_mask_path),
    .y = list("initial_mask", "expanded_mask"),
    function(path_list, mask_name) {

      mask_paths <- path_list

      mask <-
        map_df(
          mask_paths,
          function(raster_path) {

            return(
              raster(raster_path) %>%
                tabularaster::as_tibble(xy = TRUE, cell = FALSE) %>%
                filter(!(cellvalue == 0)) %>%
                rename(!!sym(mask_name) := cellvalue)
            )

          }
        )

    }
  )

####' ----- Join masks ####
mask <-
  reduce(mask_list, full_join, by = c('x', 'y')) %>%
  replace_na(replace = list(initial_mask = 0)) %>%
  arrange(x, y) %>%
  mutate(
    id = row_number(),
    mask = initial_mask + expanded_mask
  ) %>%
  select(id, x, y, mask)

####' ----- Save masks table ####
write_delim(
  mask,
  'data/tables/mask.txt',
  delim = ','
)

####' ----- Get the path to the mask table ####
path <- here::here('data/tables/mask.txt')

####' ----- Run SQL query to create mask table and indexes ####
run_sql(connection = con, query_path = "SQL/create_cells_table.sql")

#### -------------------------------------------------- Export annual lulc ####

####' ----- Create annual lulc table on db ####
run_sql(connection = con, query_path = "SQL/create_lulc_table.sql")

####' ----- Get lulc raster paths ####
path_list <- dir_ls(path = "data/original_raster/", regexp = "lulc")

####' ----- Plan multi-threading ####
plan(strategy = "multisession", workers = workers_num)

####' ----- Convert raster to table ####
annual_lulc <-
  future_map(
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
                tabularaster::as_tibble(xy = TRUE, cell = FALSE) %>%
                inner_join(mask, by = c('x', 'y')) %>%
                mutate(date = str_remove(band_name, "X")) %>%
                mutate(date = ymd(date, truncated = 2)) %>%
                select(id, date, cellvalue) %>%
                rename(lulc = cellvalue)
            )

          }
        )

      return(raster_table)

    }
  )

####' ----- Get the path to the lulc table ####
path <- here::here('data/tables/lulc.txt')

####' ----- Expor tables to db ####
walk(
  annual_lulc[18:19],
  function(lulc_table) {

    write_delim(
      lulc_table,
      'data/tables/lulc.txt',
      delim = ','
    )

    run_sql(connection = con, query_path = "SQL/insert_annual_lulc_values.sql")

  }
)

####' ----- Create constraints for db lulc table ####
run_sql(
  connection = con,
  query_path = "SQL/create_yearly_lulc_constraints.sql"
)

####' ----- Delete lulc table ####
file_delete('data/tables/lulc.txt')

rm(annual_lulc, mask_list)

#### -------------------------------------------- Export monthly variables ####

####' ----- Create db table ####
run_sql(connection = con, query_path = "SQL/create_monthly_var_table.sql")

####' ----- Get name of variables ####
chunk_list <-
  unique(
    str_extract(
      string = dir_ls('data/original_raster/', glob = '*.tif'),
      pattern = "(?<=-)[^.tif]+"
    )
  )

####' ----- Get the path to the monthly var table ####
path <- here::here('data/tables/monthly_var.txt')

####' ----- Transform raster to table and export to db ####
map_df(
  chunk_list,
  function(raster_chunk) {

    cat('\n')
    cat('Creating tables for: ', raster_chunk, '\n', sep = ' ')

    # Get all raster files for one raster chunk
    path_list <-
      dir_ls(path = "data/original_raster/", regexp = raster_chunk) %>%
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
                    tabularaster::as_tibble(xy = TRUE, cell = FALSE) %>%
                    inner_join(mask, by = c('x', 'y')) %>%
                    mutate(
                      date = str_remove(band_name, "X"),
                      cellvalue = ifelse(
                        is.nan(cellvalue),
                        NA_real_,
                        cellvalue
                      )
                    ) %>%
                    select(id, cellvalue, date) %>%
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
      select(-burn_day) %>%
      arrange(id, date) %>%
      relocate(id, date)

    # Save file
    write_delim(
      x = raster_table,
      file = "data/tables/monthly_var.txt",
      delim = ","
    )

    run_sql(connection = con, query_path = "SQL/insert_monthly_var_values.sql")

    cat('\n')

  }
)

file_delete("data/tables/monthly_var.txt")

run_sql(
  connection = con,
  query_path = "SQL/create_monthly_var_constraints.sql"
)

toc()

## ------------------------------------------------------------------------- ##
####* ------------------------------- END ------------------------------- *####
## ------------------------------------------------------------------------- ##
