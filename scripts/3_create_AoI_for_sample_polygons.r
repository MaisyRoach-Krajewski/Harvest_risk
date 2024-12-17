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

## Load in a list of all FMU shapefiles with their path ===========================================
fmu_list <- list.files(path = 'data/raw/ecoforestry_4e_shapefiles/', 
                       pattern = '\\.shp$', 
                       full.names = TRUE)
#to only include the 'PEE' layers that we are interested in
fmu_list <- Filter(function(x) grepl("PEE", x), fmu_list) 
#where there are duplicates, remove the "NC" versions...
fmu_list <- Filter(function(x) !any(grepl("_NC.", x)), fmu_list) 
# fmu_list <- fmu_list[8:12] #use to run samples on smaller number of FMUs

## Bind all the FMU shapefles together into one large dataframe ===================================
all_polygons <- do.call(rbind, sapply(fmu_list, st_read, simplify = FALSE))
gc() #clear out unused memory 

##Load in sample polygons from Step 2 =============================================================
sample_polys <- st_read('data/processed/sample_polygons_with_vars.shp')

## Create directory to store the AoI files for each sample polygon ================================
dir.create(file.path('data/processed/AoI_by_sample_poly'))

## Loop through sample polygons, extract and save AoI subsets =====================================
unique_FMUs <- unique(sample_polys$ID)
for (i in 1:length(unique_FMUs)){
  
  current_fmu_code <- unique_FMUs[i]
  
  #Filter sample polygons by this FMU code 
  sample_polys_filtered <- sample_polys %>% filter(ID %in% current_fmu_code)
  rm(sample_polys_filtered)
  
  #Upload current FMU
  path = Filter(function(x) grepl(current_fmu_code, x), fmu_list)
  current_fmu_data <- st_read(path)
  
  #Get area of interest around current FMU using custom function 
  #(I've set the buffer here to 7.5km because we'll be applying a 7km buffer to each polygon later)
  AoI_data <- get_FMU_AoI(current_fmu_data, 
                          buffer_dist = 7500, 
                          all_polygons = all_polygons)
  rm(current_fmu_data)
  
  #Save AoI data to new directory folder 
  st_write(AoI_data, 
           print(paste0("data/processed/AoI_by_sample_poly/AoI_", current_fmu_code,".shp")))
  
  rm(AoI_data)
  gc()
} 

rm(list = ls())
gc()
