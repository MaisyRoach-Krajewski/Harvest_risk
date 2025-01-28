library(dplyr)
library(sf)
library(readxl)
library(stringr)

setwd("C:/harvest_risk")

# Load helper functions ==================================================================================
source("scripts/helper_functions.R") 

# Create sample set using custom function 'sample_polygons' ==============================================
sample.set <- sample_by_AU(10, # number of years back tha you want to consider as 'recent' harvest 
                           50, # number of harvested polygons you want
                           50, #number of non-harvested polygons you want
                           shapefile_path = 'data/processed/data_by_UA/')

#Save dataset into processed data folder =================================================================
st_write(sample.set, 'data/processed/sample_polygons.shp', overwrite = T, append = FALSE)

#Done!
rm(list = ls())