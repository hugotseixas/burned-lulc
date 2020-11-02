## ------------------------------------------------------------------------- ##
####* ------------------------------- HEADER ---------------------------- *####
## ------------------------------------------------------------------------- ##
##
#### Description ####
##
## Script name:   Download images
##
## Description:   This routine is responsible for downloading data from
##                MapBiomas, CHIRPS, and MODIS products using GEE.
##                Before downloading the data is filtered based on quality
##                flags. After filtering we calculate monthly composites
##                for each variable. Downloaded files will be stored in the
##                working dir.
##
##
##
## Author:        Hugo Tameirao Seixas
## Contact:       hugo.seixas@alumni.usp.br / tameirao.hugo@gmail.com
##
## Date created:  2020-10-22
##
## Copyright (c) Hugo Tameirao Seixas, 2020
##
## ------------------------------------------------------------------------- ##
##
## Notes:         Check the "00_config.R" file to change options for this
##                routine.
##
## ------------------------------------------------------------------------- ##
##
#### Libraries ####
##
library(rgee)
library(googledrive)
library(sf)
library(geobr)
library(glue)
library(magrittr)
library(fs)
library(tidyverse)
##
## ------------------------------------------------------------------------- ##
##
#### Options ####
##
source('00_config.R')
##
## ------------------------------------------------------------------------- ##
####* ------------------------------- CODE ------------------------------ *####
## ------------------------------------------------------------------------- ##

#### ---------------------------- Start GEE api and set region of interest ####

####' ----- Initialize GEE ####
ee_Initialize(email = gee_email, drive = TRUE)

####' ----- Load region of interest ####
biomes <-
  read_biomes(simplified = TRUE) %>%
  filter(code_biome == biome)

####' ----- Set region of interest ####
aoi <- sf_as_ee(biomes) # Can take some minutes to import the polygon

####' ----- Set dates to be extracted ####
time_span %<>%
  rowwise() %>%
  mutate(time_range = list(c(start:end))) %>%
  pull(time_range)

years <- time_span[[1]]
months <- time_span[[2]]

#### ------------------------------------------------------- Load products ####

####' ----- Create list of products ####
ee_products <-
  tribble(
    ~var, ~prod, ~qa_band, ~filter, ~band, ~scale,
    "precip", "UCSB-CHG/CHIRPS/DAILY", NA, NA, "precipitation", 1,
    "gpp", "MODIS/006/MOD17A2H", "Psn_QC", "f_2", "Gpp", 0.0001,
    "evi", "MODIS/006/MOD13A1", "DetailedQA", "f_1", "EVI", 0.0001,
    "ndvi", "MODIS/006/MOD13A1", "DetailedQA", "f_1", "NDVI", 0.0001,
    "lai", "MODIS/006/MOD15A2H", "FparLai_QC", "f_2", "Lai_500m", 0.1,
    "fpar", "MODIS/006/MOD15A2H", "FparLai_QC", "f_2", "Fpar_500m", 0.01,
    "et", "MODIS/006/MOD16A2", "ET_QC",   "f_2", "ET", 0.1,
    "lst", "MODIS/006/MOD11A2", "QC_Day",   "f_3", "LST_Day_1km", 0.02,
    "burn", "MODIS/006/MCD64A1", "QA",   "f_4", "BurnDate", 1
  )

####' ----- Filter products list ####
ee_products %<>%
  filter(var %in% products)

####' ----- Load products ####
ee_product_list <-
  map(
    ee_products$prod,
    function(prod) {

      ee$ImageCollection(prod)

    }
  )

#### ------------------------------------------- Perform quality filtering ####

####' ----- Create bit list ####
bit_list <-
  tibble(
    bit = map(c(0:15), function(bit) { ee$Number(2)$pow(bit)$int() }),
    bit_num = c(0:15)
  )

####' ----- Join QA information ####
bit_list <-
  qa_info %>%
  left_join(ee_products, by = 'filter') %>%
  left_join(bit_list, by = "bit_num") %>%
  drop_na() %>%
  arrange(var)

####' ----- Function to filter data by quality ####
apply_quality_filter <-
  function(image, product, qa_band) {

    if(product != "precip") {

      qa_bits <-
        bit_list %>%
        filter(var == product)

      qa_band <-
        qa_bits %>%
        distinct(qa_band) %>%
        pull(qa_band)

      active_image <- ee$ImageCollection(image)

      return(
        active_image$map(
          function(band) {

            qa_mask <-
              ee$ImageCollection$fromImages(
                pmap(
                  .l = list(
                    qa_bits$bit,
                    qa_bits$bit_value
                  ),
                  function(bit, bit_value) {

                    return(
                      band$
                        select(qa_band)$
                        bitwiseAnd(bit)$
                        eq(bit_value)
                    )

                  }
                )
              )

            return(band$updateMask(qa_mask$max()))

          }
        )
      )

    } else {

      return(image)

    }

  }

####' ----- Apply quality mask ####
filtered_products <-
  map2(
    .x = ee_product_list,
    .y = ee_products$var,
    apply_quality_filter
  )

#### ---------------------------------------- Calculate monthly composites ####

####' ----- Select bands ####
filtered_products <-
  map2(
    .x = filtered_products,
    .y = ee_products$band,
    function(image, band) { return(image$select(band)) }
  )

####' ----- Function to extract monthly values ####
calc_monthly_composite <-
  function(product, var_name) {

    return(
      ee$Image(
        map(
          years,
          function(y) {

            return(
              map(
                months,
                function(m) {

                  if (var_name == "precip") {

                    return(
                      product$
                        filter(ee$Filter$calendarRange(y, y, 'year'))$
                        filter(ee$Filter$calendarRange(m, m, 'month'))$
                        sum()$
                        rename(glue('{y}_{m}'))
                    )

                  } else if (var_name == 'burn') {

                    return(
                      product$
                        filter(ee$Filter$calendarRange(y, y, 'year'))$
                        filter(ee$Filter$calendarRange(m, m, 'month'))$
                        max()$
                        rename(glue('{y}_{m}'))
                    )

                  } else {

                    return(
                      product$
                        filter(ee$Filter$calendarRange(y, y, 'year'))$
                        filter(ee$Filter$calendarRange(m, m, 'month'))$
                        mean()$
                        rename(glue('{y}_{m}'))
                    )

                  }

                }
              )
            )

          }
        )
      )
    )

  }

####' ----- Calculate monthly composites ####
monthly_products <-
  map2(
    .x = filtered_products,
    .y = ee_products$var,
    calc_monthly_composite
  )

####' ----- Apply scale factor ####
monthly_products <-
  map2(
    .x = monthly_products,
    .y = ee_products$scale,
    function(product, scale) {product$multiply(scale)}
  )

#### ----------------------------------- Create masks of burns and forests ####

####' ----- Load MODIS burned area ####
modis_burned <- ee$ImageCollection("MODIS/006/MCD64A1")

####' ----- Get projection from modis ####
modis_proj <- modis_burned$first()$projection()

####' ----- Load MapBiomas collection 5 ####
mb_img <-
  ee$Image(
    paste(
      'projects',
      'mapbiomas-workspace',
      'public',
      'collection5',
      'mapbiomas_collection50_integration_v1',
      sep = '/'
    )
  )

####' ----- Get only the available years from MapBiomas ####
mb_bands <-
  (years - 1985) %>%
  as_tibble() %>%
  inner_join(
    as_tibble(seq_along(mb_img$bandNames()$getInfo()) - 1),
    by = "value"
  ) %>%
  pull(value)

####' ----- Select MapBiomas bands ####
mb_img <-
  mb_img$
  select(mb_bands, as.character(mb_bands + 1985))

####' ----- Create forest mask ####
f_mask <-
  ee$Image(
    map(
      mb_img$bandNames()$getInfo(),
      function(band_name) {
        return(
          mb_img$
            select(band_name)$
            remap(from = list(3L), to = list(1L), defaultValue = 0)$
            rename(as.character(band_name))
        )
      }
    )
  )

####' ----- Reproject and reduce resolution ####
f_mask <-
  f_mask$
  reproject(crs = modis_proj)$
  reduceResolution(reducer = ee$Reducer$mode(), bestEffort = TRUE)

####' ----- Create annual burn mask ####
b_mask <-
  ee$Image(
    map(
      (mb_bands + 1985),
      function(y) {
        return(
          modis_burned$
            filter(ee$Filter$calendarRange(y, y, 'year'))$
            select('BurnDate')$
            sum()$
            set('year', y)$
            remap(from = list(0), to = list(0), defaultValue = 1)$
            byte()$
            rename(as.character(y))
        )
      }
    )
  )

####' ----- Create mask for both variables ####
mask <-
  b_mask$
  updateMask(f_mask)$
  reduce(ee$Reducer$max())

####' ----- Create expanded mask ####
expanded_mask <- mask$focal_max(radius = 3)

#### --------------------------------------------------------- Apply masks ####

####' ----- Add MapBiomas image to image list ####
monthly_products <- c(monthly_products, mb_img)

####' ----- Apply mask ####
masked_images <-
  map(
    monthly_products,
    function(image) { return(image$updateMask(expanded_mask)) }
  )

####' ----- Add masks to image list ####
all_products <- c(masked_images, mask, expanded_mask)

#### ----------------------------------------------------- Download images ####

####' ----- List products ####
products_list <- c(ee_products$var, "lulc", "initial_mask", "expanded_mask")

####' ----- Delete googledrive download folder ####
if (clear_driver_folder == TRUE) { drive_rm('burned_lulc') }

####' ----- Download images one by one ####
for(i in seq_along(all_products)) {

  image <- ee$Image(all_products[[i]])

  product <- products_list[i]

  cat('Downloading', product, 'data:', '\n', sep = ' ')

  # Set download task
  download_mb <-
    ee_image_to_drive(
      image = image$clip(aoi),
      description = glue("{product}"),
      folder = "burned_lulc",
      timePrefix = FALSE,
      region = aoi$geometry()$bounds(),
      scale = scale,
      maxPixels = 1e13,
      fileDimensions = tile_dim,
      skipEmptyTiles = TRUE,
      fileFormat = "GEO_TIFF",
      crs = "EPSG:4326"
    )

  # Start download
  download_mb$start()

  # Monitor the download
  ee_monitoring(download_mb, task_time = 10)

}

####' ----- List files in drive ####
drive_files <- drive_ls('/burned_lulc')

####' ----- Create local download dir ####
dir_create("data/original_raster")

####' ----- Download files from drive to project folder ####
walk2(
  .x = drive_files$id,
  .y = drive_files$name,
  function(id, name) {

    drive_download(
      file = as_id(id),
      path = glue::glue('data/original_raster/{name}'),
      overwrite = TRUE
    )

  }
)

## ------------------------------------------------------------------------- ##
####* ------------------------------- END ------------------------------- *####
## ------------------------------------------------------------------------- ##
