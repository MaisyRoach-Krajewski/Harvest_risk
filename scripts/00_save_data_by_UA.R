# EXPLANATION:
## Previously, steps 1-4 used the ecoforestry 'grid tiles' to sample and loops through data.
## Now, I want to introduce this preliminary step that reorganizes ecoforestry data based on UA 
## Moving forward, any ecoforestry data that does not fall in a UA is disregarded for steps 1-4 (less data = less computing time)

# This script will: 
##      - Upload and merge all polygons into one dataframe 
##      - Use custom function to loop through all polygons and extract a subset for those overlapping each UA boundary
##      - Save new subset data in 'processed data' folder 

# Then, I will edit steps 1-4 to sample and process based on UA, instead of by grid tile... 

library(dplyr)
library(sf)
library(readxl)
library(stringr)
library(future.apply)

setwd("C:/harvest_risk")

# Load helper functions
source("scripts/helper_functions.R") 

# Upload UA and RGA polygon data 
UA <- st_read('data/raw/UA_SHP/STF_UA.shp')
# RGA <- st_read('data/raw/RGA_SHP/STF_RGA.shp')

# Dissolve both by their ID unit number 
UA <- UA %>% group_by(NO_UG_RESP) %>% summarize() #should result in 31 UAs
# RGA <- RGA %>% group_by(NO_RGA) %>% summarize() #should result in 13 RGAs 
gc() 

# Now, we need to load in and combine all the ecoforestry polygon data, by tile (how it's downloaded from the website)

## Load in a list of all shapefiles with their path
file_list <- list.files(path = 'data/raw/ecoforestry_4e_shapefiles/', 
                        pattern = '\\.shp$', 
                        full.names = TRUE)
## to only include the 'PEE' layers that we are interested in...
file_list <- Filter(function(x) grepl("PEE", x), file_list) 
## where there are duplicates, remove the "NC" versions...
file_list <- Filter(function(x) !any(grepl("_NC.", x)), file_list) 

## Bind all the shapefles together into one large dataframe
all_polygons <- do.call(rbind, sapply(file_list, st_read, simplify = FALSE))
gc() #clear out unused memory 

# Create the output directory if it doesn't exist
output_dir <- "data/processed/data_by_UA"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}



# # Iterate through each UA polygon and process it
# for (i in 1:nrow(UA)) {
#   process_UA(UA[i, ], all_polygons, output_dir)
#   
#   current_UA <- UA[i,1]
#   
#   print(paste0("Shapefile saved for ", current_UA, ". ", i, " of ", nrow(UA), " complete."))  
#   
#   gc()
# }

parallel_process_UA(UA, all_polygons, output_dir)

