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

## Load auxiliary data for queries ================================================================
ORI <- read_excel('data/raw/ecoforestry_auxiliary_data/uniqueORI_categories.xlsx')
exclude <- read_excel('data/raw/ecoforestry_auxiliary_data/eliminated_polygons.xlsx')
age <- read_excel('data/raw/ecoforestry_auxiliary_data/age.xlsx')
species <- read_excel('data/raw/ecoforestry_auxiliary_data/Species_codes.xlsx')

# Load GMV raster layer 
mean_gmv <- rast(raster("data/raw/rasters/NRCan_250_combined.tif", band = 3))

cut_codes <- ORI %>% filter(Cut == 'X') %>% pull(`Origine code`)
exclude_codes <- exclude %>% filter(Eliminate == 'X') %>% pull(Code)

SUP_data <- read.csv("data/processed/SUP_data_combined.csv")
gc()

##Load in sample polygons from Step 2 =============================================================
sample_polys <- st_read('data/processed/sample_polygons_with_vars.shp')

## Load a list of the AoI files saved in step 3 ===================================================
AoI_list <- list.files(path = 'data/processed/AoI_by_sample_poly/', pattern = '\\.shp$', full.names = TRUE)
#Create list of unique FMU tile codes to loop through 
unique_FMUs <- unique(sample_polys$ID)

## Create directory to store extracted variables for each tile code ===============================
dir.create(file.path('data/processed/extracted_variables'))

## Loop through tile codes to extract spatial variables and save results ==========================
for (j in 1:length(unique_FMUs)){
  
  current_fmu_code <- unique_FMUs[j]
  
  #Filter sample polygons by this FMU code 
  sample_polys_filtered <- sample_polys %>% filter(ID %in% current_fmu_code)
  
  #upload AoI data for this FMU
  path = Filter(function(x) grepl(current_fmu_code, x), AoI_list)
  AoI_data <- st_read(path)
  
  #Attach supplementary data to AoI dataset 
  AoI_data <- AoI_data %>%
    left_join(SUP_data, by = "GEOCODE")
  
  #Use custom functions to identify harvest polygons in AoI, and assign age, density, and height values 
  
  ##Exclude unnecessary polygons 
  AoI_data <- AoI_data %>%
    filter(!CO_TER %in% exclude_codes | !TYPE_TE %in% exclude_codes)
  
  ##Assign harvested vs. non-harvested
  AoI_data <- get_harvested_polygons(AoI_data, ORI)
  
  ##Assign age, height, and density values 
  AoI_data <- AoI_data %>% assign_age(age) %>% assign_height() %>% assign_density()
  
  ## Get combined density and height values to reduce NAs in as many polygons as possible
  # IF DENSITE == NA, pull value from density_der - repeat with height
  AoI_data <- AoI_data %>%
    mutate(dens_combined = ifelse(!is.na(DENSITE), DENSITE, density_der)) %>%
    mutate(haut_combined = ifelse(!is.na(HAUTEUR), HAUTEUR, height_der))
  
  print(paste0("Internal queries complete for AoI polygons for ", current_fmu_code, 
               ". Starting spatial queries."))  
  gc()
  
  # Extract water polygons from AoI dataset
  AoI_water <- AoI_data %>% 
    filter(CO_TER == 'EAU'| TYPE_TE == 'EAU')
  
  #Use custom filter to calculate proportion of water in each buffered sample polygon 
  sample_polys_filtered$water <- parallel_calculate_in_buffer(sample_polys_filtered, 
                                                              AoI_water, 
                                                              buffer_dist = 7000)
  
  print(paste0("Water proportion calculated for ", current_fmu_code, ". Starting cluster selection."))
  rm(AoI_water)
  
  #For the next steps, we want water to be removed from AoI_data
  AoI_cluster_data <- AoI_data %>% 
    filter(!CO_TER == 'EAU'| !TYPE_TE == 'EAU')
  rm(AoI_data)
  gc()
  
  #Use custom function to calculate proportion of various target layers in each cluster 
  # cluster = polygons 'completely within' a 7km buffer of the polygon in question. 
  #           Why?  1. Creates irregular 'clusters' of area that better represent how industry considers ecoforestry poplygons 
  #                 2. Faster to run when avoiding st_intersection() - no longer clipping any polygons
  
  # Calculate proportion of harvested polygons in cluster 
  AoI_harvest <- AoI_cluster_data %>% 
    filter(harvest == 1)              # This is our target layer. 
  
  sample_polys_filtered$harv_cluster <- para_calculate_in_cluster(sample_polys_filtered, 
                                                                  AoI_cluster_data, 
                                                                  AoI_harvest, 
                                                                  buffer_dist = 7000)  
  
  print(paste0("Harvest proportion calculated for ", current_fmu_code, ". Continuing cluster selection."))
  rm(AoI_harvest)
  gc()
  
  # Calculate proportion of forest type in buffer
  
  AoI_coniferous <- AoI_cluster_data %>% 
    filter(TYPE_CO == 'R')
  AoI_mixed <- AoI_cluster_data %>% 
    filter(TYPE_CO == 'M')
  AoI_deciduous <- AoI_cluster_data %>% 
    filter(TYPE_CO == 'F')
  
  sample_polys_filtered$sw_cluster <- para_calculate_in_cluster(sample_polys_filtered, 
                                                                AoI_cluster_data, 
                                                                AoI_coniferous, 
                                                                buffer_dist = 7000)
  sample_polys_filtered$mix_cluster <- para_calculate_in_cluster(sample_polys_filtered, 
                                                                 AoI_cluster_data, 
                                                                 AoI_mixed, 
                                                                 buffer_dist = 7000)
  sample_polys_filtered$hw_cluster <- para_calculate_in_cluster(sample_polys_filtered, 
                                                                AoI_cluster_data, 
                                                                AoI_deciduous, 
                                                                buffer_dist = 7000)
  
  print(paste0("Forest type proportion calculated for ", current_fmu_code, ". Starting average calculations."))
  rm(AoI_coniferous, AoI_deciduous, AoI_mixed)
  gc()
  
  # Extract average GMV, forest age, height and density in cluster around each sample polygon
                
  #Extract mean GMV values from raster
  AoI_cluster_data$mean_gmv <- exact_extract(mean_gmv, AoI_cluster_data, 'mean')
  
  #Apply custom function
  sample_polys_filtered <- para_ave_in_cluster(sample_polys_filtered, 
                                               AoI_cluster_data, 
                                               buffer_dist = 7000)
  
  print(paste0("All sample polygon calculations for ", current_fmu_code, " are complete. ", j, " of ",length(unique_FMUs), " complete." ))
  rm(AoI_cluster_data)
  gc()
  
  #Save file for FMU 
  
  sample_dataset <- st_drop_geometry(sample_polys_filtered)
  
  #Subset only useful columns
  final_sample <- dplyr::select(sample_dataset, c("GEOCODE",
                                                  "ID",
                                                  "TYPE_CO", 
                                                  "harvest", 
                                                  "dns_cmb", 
                                                  "ht_cmbn",
                                                  "m1_age", 
                                                  "m2_age", 
                                                  "tiers", 
                                                  "tburn_5", 
                                                  "tbrn_10", 
                                                  "pburn_5", 
                                                  "pbrn_10", 
                                                  "majOB_5", 
                                                  "mjOB_10",
                                                  "minOB_5",
                                                  "mnOB_10", 
                                                  "dom_spp",
                                                  "n_spp",
                                                  "min_dst",
                                                  "men_dst",
                                                  "min_mlldst",
                                                  "men_mlldst", 
                                                  "men_gmv",
                                                  "men_slp",
                                                  "mean_lv",
                                                  "max_slp",
                                                  "water",
                                                  "harv_cluster",
                                                  "sw_cluster",
                                                  "mix_cluster",
                                                  "hw_cluster",
                                                  "ave.age",
                                                  "ave.dens",
                                                  "ave.haut",
                                                  "ave.gmv"))
  
  write.csv(final_sample,print(paste0("data/processed/extracted_variables/extracted_vars_", current_fmu_code,".csv")), row.names=FALSE)
  gc()
}
