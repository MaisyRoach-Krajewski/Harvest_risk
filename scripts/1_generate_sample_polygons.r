library(dplyr)
library(sf)
library(readxl)
library(stringr)

setwd("C:/harvest_risk")

# Load helper functions ==================================================================================
source("scripts/helper_functions.R") 

# Get the codes that correspond with each FMU ============================================================
fmu_codes <- st_read('data/raw/ecoforestry_tile_coverage/Pee_maj.shp')
fmu_codes <- paste(fmu_codes$feuillet, collapse = "|")


# Create sample set using custom function 'sample_polygons' ==============================================
sample.set <- sample_polygons(10, # number of harvested polygons you want
                              10, #number of non-harvested polygons you want
                              fmu_codes,
                              shapefile_path = 'data/raw/ecoforestry_4e_shapefiles/')

#Save dataset into processed data folder =================================================================
st_write(sample.set, 'data/processed/sample_polygons.shp', overwrite = T)

rm(list = ls())
