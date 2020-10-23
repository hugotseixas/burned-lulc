## ------------------------------------------------------------------------- ##
####* ------------------------------- HEADER ---------------------------- *####
## ------------------------------------------------------------------------- ##
##
#### Description ####
##
## Script name: Filter and join tables
##
## Description:
##
##
##
##
##
##
##
##
## Author:        Hugo Tameirao Seixas
## Contact:       hugo.seixas@alumni.usp.br / tameirao.hugo@gmail.com
##
## Date created:
## Last update:
## Last tested:
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
library(fs)
library(vroom)
library(tidyverse)
library(magrittr)
##
## ------------------------------------------------------------------------- ##
##
#### Options ####
##
options(scipen = 6, digits = 4) # View outputs in non-scientific notation
##
## ------------------------------------------------------------------------- ##
####* ------------------------------- CODE ------------------------------ *####
## ------------------------------------------------------------------------- ##

table_list <- dir_ls('data/tables/', regex = 'var')

filter_table <- function(path) {

  table <- vroom(file = path, delim = ',')

  table %>%
    select(-cellindex) %>%
    mutate(year = year + 2000) %>%
    filter(year %in% c(2005, 2010, 2015, 2019))

}

filtered_tables <- map(table_list, filter_table)

joined_tables <- filtered_tables %>%
  reduce(full_join, by = c('x', 'y', 'year')) %>%
  group_by(x, y) %>%
  mutate(cellindex = cur_group_id()) %>%
  ungroup() %>%
  drop_na(mb_img)

burned_forests <- joined_tables %>%
  filter(year != 2019) %>%
  mutate(modis_burned = if_else(modis_burned == 1, 1, 0, missing = 0)) %>%
  mutate(modis_burned = if_else(mb_img == 3, 1, 0)) %>%
  group_by(cellindex) %>%
  filter(any(mb_img == 3), any(modis_burned == 1)) %>%
  ungroup()

burn_sequence <- burned_forests %>%
  group_by(cellindex, x, y) %>%
  arrange(cellindex, year) %>%
  summarise(burn_seq = str_c(as.character(modis_burned), collapse = '')) %>%
  ungroup() %>%
  filter(str_length(burn_seq) == 3)

burned_lulc <- joined_tables %>%
  filter(year == 2019) %>%
  select(cellindex, x, y, mb_img, modis_gpp) %>%
  right_join(burn_sequence, by = c('cellindex', 'x', 'y')) %>%
  rename(lulc = mb_img, gpp = modis_gpp) %>%
  mutate(
    lulc_name = case_when(
      lulc == 3 ~ 'natural_forest',
      lulc == 15 ~ 'pasture',
      lulc %in% c(4, 12, 5) ~ 'non_forest_natural',
      lulc == 9 ~ 'forestry',
      lulc %in% c(19, 39, 20, 41, 36) ~ 'agriculture',
      lulc == 33 ~ 'water',
      lulc %in% c(24, 30, 32, 25) ~ 'other'
      )
    )

write_delim(burned_lulc, 'data/tables/burned_lulc.txt', delim = ',')

## ------------------------------------------------------------------------- ##
####* ------------------------------- END ------------------------------- *####
## ------------------------------------------------------------------------- ##
