# CONFIG ----------------------------------------------------------------------
#
# This is a configuration file that sets options and variables that will be
# used in the "01_download.R" and "02_db_export_mask.R".
# The options "gee_email" and "clear_driver_folder" are safe
# to be changed ("gee_email" is actually essential to run the download script).
# However, it is not guaranteed that changes in the other options will generate
# valid results, changing them may even break the codes. They are available
# here to facilitate testing and future developments.

# Set your GEE email ----------------------------------------------------------
gee_email <- "hugo.seixas@alumni.usp.br"
# You must be registered in Google Earth Engine (GEE) with a Gmail account
# To have acces to the google drive to download the data
# GEE home page:
# https://earthengine.google.com/

# Clear Google Drive target download folder? ----------------------------------
clear_driver_folder <- FALSE
# Default:
#   clear_driver_folder <- FALSE
# This will delete the folder named "burned_lulc" from your google drive folder
# if set TRUE. When downloading data, make sure that this folder is empty
# since Google Drive create duplicates of files with same name
# DUPLICATE RASTER FILES MAY GENERATE ERRORS IN THE FOLLOWING CODES

# Set the biome to download data from -----------------------------------------
biome <- 1L
# Default:
#   biome <- 1
# Values ALWAYS have to be followed by the letter L

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
# Bear in mind that all the codes were developed aiming to perform the
# analysis over the Amazon biome, and may not be appropriate to be applied
# in other regions

# Set the time span to download -----------------------------------------------
time_span <- tribble(
   ~scale,   ~start,    ~end,
   "year",    2000L,   2020L,
  "month",        1,     12L
)
# Change values of start and end year/month
# Values ALWAYS have to be followed by the letter L


# The spatial resolution of the download --------------------------------------
scale <- 500L
# Default:
#   scale <- 500L
# Values ALWAYS have to be followed by the letter L

# The dimension of tiles to be downloaded from GEE ----------------------------
tile_dim <- 512L
# Values have to be multiple of 256
# Values ALWAYS have to be followed by the letter L
# You can use tile_dim <- NULL to let GEE to choose the size automatically
# Bigger values will result in bigger tiles, demanding more memory
# Default:
#   tile_dim <- 512L

# List of variables to download -----------------------------------------------
products <-
  c(
    "precip",          # Chirps daily precipitation (mm)
    "gpp",             # MODIS GPP 8 days
    "evi",             # MODIS EVI 16 days
    "ndvi",            # MODIS NDVI 16 days
    "lai",             # MODIS LAI 8 days
    "fpar",            # MODIS FPAR 8 days
    "et",              # MODIS ET 16 days
    "lst",             # MODIS LST 8 days
    "burn",            # MODIS monthly burned area

    "" # Do NOT remove or comment this line!
  )
# Comment a variable line to remove it from the routine
# It's advised to always keep the "burn" variable

# Set quality filter for each product -----------------------------------------
qa_info <-
  tibble::tribble( # Tribble formatted using datapasta package!
       ~filter, ~bit_num, ~bit_value,
    "f_1",       0L,         0L,
    "f_1",       1L,         0L,
    "f_1",       2L,         NA,
    "f_1",       3L,         NA,
    "f_1",       4L,         NA,
    "f_1",       5L,         NA,
    "f_1",       6L,         NA,
    "f_1",       7L,         NA,
    "f_1",       8L,         NA,
    "f_1",       9L,         NA,
    "f_1",      10L,         NA,
    "f_1",      11L,         NA,
    "f_1",      12L,         NA,
    "f_1",      13L,         NA,
    "f_1",      14L,         NA,
    "f_1",      15L,         NA,
    "f_2",       0L,         0L,
    "f_2",       1L,         NA,
    "f_2",       2L,         0L,
    "f_2",       3L,         0L,
    "f_2",       4L,         0L,
    "f_2",       5L,         0L,
    "f_2",       6L,         0L,
    "f_2",       7L,         0L,
    "f_3",       0L,         0L,
    "f_3",       1L,         0L,
    "f_3",       2L,         NA,
    "f_3",       3L,         NA,
    "f_3",       4L,         NA,
    "f_3",       5L,         NA,
    "f_3",       6L,         NA,
    "f_3",       7L,         NA,
    "f_4",       0L,         1L,
    "f_4",       1L,         1L,
    "f_4",       2L,         NA,
    "f_4",       3L,         NA,
    "f_4",       4L,         NA,
    "f_4",       5L,         NA,
    "f_4",       6L,         NA,
    "f_4",       7L,         NA
)
# There are four filters (since most variables share the same QA flags)
# "f_1" is used to filter EVI and NDVI products
# "f_2" is used to filter GPP, LAI, FPAR and ET products
# "f_3" is used to filter LST products
# "f_4" is used to filter the burned area product
# The value of bits can range from 0 to 1, you may set to NA to ignore
# Values ALWAYS have to be followed by the letter L

# Set multi-thread to transform raster to table -------------------------------
multi_thread <- TRUE
# Default:
#   multi_thread <- TRUE
workers_num <- 9L
# Default:
#   workers_num <- 10L
# This have the potential to greatly increase the speed of this process
# However it will demand more memory, use it with caution

# Sampling options ------------------------------------------------------------
n_iteration <- 100
n_experiment <- 1000
n_control <- 50000
match_dist <- 5000
