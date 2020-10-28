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
##                The table of each variable is saved in .txt format for later
##                use.
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
library(raster)
library(tabularaster)
library(fs)
library(glue)
library(furrr)
library(tidyverse)
library(tictoc)
##
## ------------------------------------------------------------------------- ##
##
#### Options ####
##
source("00_config.R")
##
## ------------------------------------------------------------------------- ##
####* ------------------------------- CODE ------------------------------ *####
## ------------------------------------------------------------------------- ##

tic()

####' ----- Convert mask raster to table ####
mask <-
  raster('data/original_raster/mask.tif') %>%
  tabularaster::as_tibble(xy = TRUE, cell = TRUE) %>%
  filter(!(cellvalue == 0)) %>%
  select(-cellvalue)

write_delim(
  mask,
  'data/tables/mask.txt',
  delim = ','
)

####' ----- Function to transform raster data to table ####
raster_to_table <- function(raster_var) {

  cat('\n')
  cat('Creating tables for: ', raster_var, '\n', sep = ' ')

  # Save an empty table
  write_delim(
    tibble(
      !!sym(raster_var) := double(),
      cellindex = integer(),
      x = double(),
      y = double(),
      date = character()
    ),
    glue('data/tables/{raster_var}.txt'),
    delim = ','
  )

  # Get all raster files for one variable
  raster_path <- dir_ls('data/original_raster/', regexp = raster_var)

  # Loop trough all the files
  for (path in raster_path) {

    cat('\n')
    cat('Extracting data from: ', path, '\n', sep = ' ')

    # Load as a stack and get its bands number
    raster_stack <- stack(path)
    raster_bands <- names(raster_stack)

    # Loop trough bands
    for (band_num in seq_along(raster_bands)) {

      cat('\r', 'Band: ', band_num, '  ')

      # Load raster
      raster_chunk <- raster(path, band = band_num)

      # Transform to table and join with mask table
      raster_table <-
        raster_chunk %>%
        tabularaster::as_tibble(xy = TRUE, cell = TRUE) %>%
        right_join(mask, by = c('x', 'y', 'cellindex')) %>%
        mutate(
          date = str_remove(raster_bands[band_num], "X"),
          cellvalue = if_else(is.nan(cellvalue), NA_real_, cellvalue)
        ) %>%
        rename(!!sym(raster_var) := cellvalue)

      # Append to table created before
      write_delim(
        raster_table,
        glue('data/tables/{raster_var}.txt'),
        delim = ',',
        append = TRUE
      )

    }

  }

  cat('\n')

}

####' ----- Get name of variables ####
variables <-
  dir_ls('data/original_raster/') %>%
  str_extract("(?<=raster/)[^-|.]+") %>%
  as_tibble() %>%
  filter(value != 'mask') %>%
  distinct(value) %>%
  pull(value)

####' ----- Apply function to all variables ####
if (multi_thread == TRUE) {

  plan(strategy = "multisession", workers = workers_num)

  future_walk(
    variables, raster_to_table,
    .options = furrr_options(seed = NULL)
  )

} else if (multi_thread == FALSE) {

  walk(variables, raster_to_table)

}

toc()

## ------------------------------------------------------------------------- ##
####* ------------------------------- END ------------------------------- *####
## ------------------------------------------------------------------------- ##
