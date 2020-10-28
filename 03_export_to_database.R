## ------------------------------------------------------------------------- ##
####* ------------------------------- HEADER ---------------------------- *####
## ------------------------------------------------------------------------- ##
##
#### Description ####
##
## Script name:
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
## Date created:  2020-10-23
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
library(sf)
library(magrittr)
library(lubridate)
library(fs)
library(tsibble)
library(glue)
library(tidyverse)
##
## ------------------------------------------------------------------------- ##
##
#### Options ####
##
source('R/run_sql.R')
##
## ------------------------------------------------------------------------- ##
####* ------------------------------- CODE ------------------------------ *####
## ------------------------------------------------------------------------- ##

####' ----- Create connection with database ####
con <-
  dbConnect(
    Postgres(),
    host = 'localhost',
    dbname = 'burned_lulc',
    user = 'postgres',
    password = 'postgres'
  )

#### ------------------------------ Create table of mask cells in database ####

####' ----- Get the path to the mask table ####
path <- here::here('data/tables/mask.txt')

####' ----- Run SQL query to create table and indexes ####
run_sql(connection = con, query_path = "SQL/create_cells_table.sql")

#### ------------------------------------ Create table of burned lulc data ####

####' ----- Get annual year data ####
yearly_burn <- read_delim('data/tables/burn.txt', delim = ',') %>%
  select(-c(x, y)) %>%
  mutate(date = lubridate::ymd(date, truncated = 1)) %>%
  as_tsibble(key = cellindex, index = date) %>%
  group_by_key() %>%
  index_by(year = ~ year(.)) %>%
  summarise(burn = max(burn, na.rm = TRUE)) %>%
  mutate(burn = if_else(burn > 0, 1, 0)) %>%
  as_tibble()

####' ----- Create temporary table to be loaded into db ####
read_delim('data/tables/lulc.txt', delim = ',') %>%
  select(-c(x, y)) %>%
  rename(year = date) %>%
  left_join(yearly_burn, by = c("cellindex", "year")) %>%
  relocate(cellindex, year) %>%
  arrange(cellindex, year) %>%
  write_delim(
    file = 'data/temp.txt',
    delim = ','
  )

####' ----- Create path to temporary table of lulc data ####
path <- here::here('data/temp.txt')

####' ----- Run SQL query to create table and indexes ####
run_sql(connection = con, query_path = "SQL/create_lulc_table.sql")

#### ----------------------------------- Create table of monthly variables ####

####' ----- Organize and process burned area data ####
monthly_var <-
  read_delim('data/tables/burn.txt', delim = ',') %>%
  select(-c(x, y)) %>%
  na_if(0) %>%
  mutate(
    date = format(parse_date_time(date, orders = c("Y_m")), "%Y-%m"),
    burn = mday(
      as_date(
        burn,
        origin = ceiling_date(ymd(date, truncated = 1), unit = "year")
      )
    )
  ) %>%
  mutate(burn = ifelse(is.na(burn), 0, burn))

####' ----- Get name of variables ####
variables <-
  dir_ls('data/tables/') %>%
  str_extract("(?<=tables/)[^-|.]+") %>%
  as_tibble() %>%
  filter(!value %in% c("mask", "lulc", "burn")) %>%
  distinct(value) %>%
  pull(value)

####' ----- Join other variables ####
for(v in variables) {

  monthly_var <-
    read_delim(glue('data/tables/{v}.txt'), delim = ',') %>%
    select(-c(x, y)) %>%
    mutate(
      date = format(parse_date_time(date, orders = c("Y_m")), "%Y-%m")
    ) %>%
    full_join(monthly_var, by = c('cellindex', 'date'))

}

####' ----- Organize monthly variables table ####
monthly_var %<>%
  arrange(cellindex, date) %>%
  relocate(cellindex, date) %>%
  rename(cell_id = cellindex) %>%
  mutate(
    cellindex = as.integer(cell_id),
    burn = as.integer(burn)
  )

####' ----- Create monthly variables table ####
dbWriteTable(
  conn = con,
  name = "monthly_var",
  value = monthly_var,
  overwrite = TRUE
)

####' ----- Create constraints ####
run_sql(connection = con, query_path = "SQL/create_var_constraints.sql")

## ------------------------------------------------------------------------- ##
####* ------------------------------- END ------------------------------- *####
## ------------------------------------------------------------------------- ##
