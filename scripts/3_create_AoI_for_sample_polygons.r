library(dplyr)
library(sf)
library(readxl)
library(stringr)
library(readr)
library(foreign)
library(raster)
library(terra)
library(exactextractr)

setwd("C:/harvest_risk")

# Load helper functions ===========================================================================
source("scripts/helper_functions.R") 

## Load in a list of all shapefiles with their path ===============================================
file_list <- list.files(path = 'data/raw/ecoforestry_4e_shapefiles/', 
                       pattern = '\\.shp$', 
                       full.names = TRUE)
#to only include the 'PEE' layers that we are interested in
file_list <- Filter(function(x) grepl("PEE", x), file_list) 
#where there are duplicates, remove the "NC" versions...
file_list <- Filter(function(x) !any(grepl("_NC.", x)), file_list) 
# file_list <- file_list[8:12] #use to run samples on smaller number of tiles

## Bind all the shapefles together into one large dataframe =======================================
all_polygons <- do.call(rbind, sapply(file_list, st_read, simplify = FALSE))
gc() #clear out unused memory 

##Load in sample polygons from Step 2 =============================================================
sample_polys <- st_read('data/processed/sample_polygons_with_vars.shp')

## Create directory to store the AoI files for each sample polygon ================================
dir.create(file.path('data/processed/AoI_by_sample_poly'))

## Loop through sample polygons, extract and save AoI subsets =====================================
unique_tile_ID <- unique(sample_polys$ID)
for (i in 1:length(unique_tile_ID)){
  
  current_tile_code <- unique_tile_ID[i]
  
  #Filter sample polygons by this data tile code 
  sample_polys_filtered <- sample_polys %>% filter(ID %in% current_tile_code)
  rm(sample_polys_filtered)
  
  #Upload current tile
  path = Filter(function(x) grepl(current_tile_code, x), file_list)
  current_tile_data <- st_read(path)
  
  #Get area of interest around current tile using custom function 
  #(I've set the buffer here to 7.5km because we'll be applying a 7km buffer to each polygon later)
  AoI_data <- get_tile_AoI(current_tile_data, 
                          buffer_dist = 7500, 
                          all_polygons = all_polygons)
  rm(current_tile_data)
  
  #Save AoI data to new directory folder 
  st_write(AoI_data, 
           print(paste0("data/processed/AoI_by_sample_poly/AoI_", current_tile_code,".shp")))
  
  rm(AoI_data)
  gc()
} 

rm(list = ls())
gc()
