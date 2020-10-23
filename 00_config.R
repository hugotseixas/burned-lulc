## ------------------------------------------------------------------------- ##
####* ------------------------------ CONFIG ----------------------------- *####
## ------------------------------------------------------------------------- ##

####' ----- Set your GEE email ####
gee_email <- "hugo.seixas@alumni.usp.br"
# You must be registered in Google Earth Engine (GEE) with a Gmail account
# To have acces to the google drive to download the data
# GEE home page:
# https://earthengine.google.com/

####' ----- Set the biome to download data from ####
biome <- 1
# Default:
#   biome <- 1

## The option must be a value from 1 to 6
## Each number represents the following biome:
##
## 1  ------  Amazônia
## 2  ------  Caatinga
## 3  ------  Cerrado
## 4  ------  Mata Atlântica
## 5  ------  Pampa
## 6  ------  Pantanal
##

####' ----- The spatial resolution of the download ####
scale <- 1000
# Default:
#   scale <- 30

####' ----- The dimension of tiles to be downloaded from GEE ####
tile_dim <- 1536L
# Values have to be multiple of 256
# Values ALWAYS have to be followed by the letter L
# You can use tile_dim <- NULL to let GEE to choose the size automatically
# Bigger values will result in bigger tiles, demanding more memory
# Default:
#   tile_dim <- 1280L

####' ----- List of variables to download ####
products <-
  c(
    "precipitation",   # Chirps daily
    "gpp",             # MODIS GPP 8 days
    "evi",             # MODIS EVI 16 days
    "ndvi",            # MODIS NDVI 16 days
    "lai",             # MODIS LAI 8 days
    "fpar",            # MODIS FPAR 8 days
    "et",              # MODIS ET 16 days
    "lst",            # MODIS LST 8 days

    "" # Do NOT remove or comment this line!
  )
# Comment a variable line to remove it from the routine

####' ----- Set quality filter for each product ####
qa_info <-
  tibble::tribble( # Tribble formatted using datapasta package!
       ~filter, ~bit_num, ~bit_value,
    "filter_1",       0L,         0L,
    "filter_1",       1L,         0L,
    "filter_1",       2L,         NA,
    "filter_1",       3L,         NA,
    "filter_1",       4L,         NA,
    "filter_1",       5L,         NA,
    "filter_1",       6L,         NA,
    "filter_1",       7L,         NA,
    "filter_1",       8L,         NA,
    "filter_1",       9L,         NA,
    "filter_1",      10L,         NA,
    "filter_1",      11L,         NA,
    "filter_1",      12L,         NA,
    "filter_1",      13L,         NA,
    "filter_1",      14L,         NA,
    "filter_1",      15L,         NA,
    "filter_2",       0L,         0L,
    "filter_2",       1L,         NA,
    "filter_2",       2L,         0L,
    "filter_2",       3L,         0L,
    "filter_2",       4L,         0L,
    "filter_2",       5L,         0L,
    "filter_2",       6L,         0L,
    "filter_2",       7L,         0L,
    "filter_3",       0L,         0L,
    "filter_3",       1L,         0L,
    "filter_3",       2L,         NA,
    "filter_3",       3L,         NA,
    "filter_3",       4L,         NA,
    "filter_3",       5L,         NA,
    "filter_3",       6L,         NA,
    "filter_3",       7L,         NA
)
# There are two filters (since most variables share the same QA flags)
# "filter_1" is used to filter EVI and NDVI products
# "filter_2" is used to filter GPP, LAI, FPAR and ET products
# "filter_3" is used to filter LST products
# The value of bits can range from 0 to 1, you may set to NA to ignore


## ------------------------------------------------------------------------- ##
####* ------------------------------- END ------------------------------- *####
## ------------------------------------------------------------------------- ##
