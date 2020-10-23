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
##                occurance year and coordinates information.
##                The table of each variable is saved in .txt format for later
##                use.
##
## Author:        Hugo Tameirao Seixas
## Contact:       hugo.seixas@alumni.usp.br / tameirao.hugo@gmail.com
##
## Date created:  07-oct-2020
## Last update:
## Last tested:   07-oct-2020
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
library(tidyverse)
library(tictoc)
##
## ------------------------------------------------------------------------- ##
##
#### Options ####
##

##
## ------------------------------------------------------------------------- ##
####* ------------------------------- CODE ------------------------------ *####
## ------------------------------------------------------------------------- ##

tic()

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
      year = integer()
    ),
    glue('data/tables/var_{raster_var}.txt'),
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
    raster_bands <- seq_along(names(raster_stack))

    # Loop trough bands
    for (band_num in raster_bands) {

      cat('\r', 'Band: ', band_num, '  ')

      # Load raster
      raster_chunk <- raster(path, band = band_num)

      # Transform to table
      raster_table <- raster_chunk %>%
        tabularaster::as_tibble(xy = TRUE, cell = TRUE) %>%
        drop_na() %>% # Remove cells with no values
        filter(!(cellvalue == 0)) %>%
        rename(!!sym(raster_var) := cellvalue) %>%
        mutate(month_num = band_num)

      # Append to table created before
      write_delim(
        raster_table,
        glue('data/tables/var_{raster_var}.txt'),
        delim = ',',
        append = TRUE
      )

    }

  }

  cat('\n')

}

####' ----- Get name of variables ####
variables <- dir_ls('data/original_raster/') %>%
  str_extract("(?<=raster/)[^-]+") %>%
  as_tibble() %>%
  distinct(value) %>%
  pull(value)

####' ----- Apply function to all variables ####
walk(variables, raster_to_table)

toc()

## ------------------------------------------------------------------------- ##
####* ------------------------------- END ------------------------------- *####
## ------------------------------------------------------------------------- ##
