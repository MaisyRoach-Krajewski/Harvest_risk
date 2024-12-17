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

# Load helper functions ==================================================================================
source("scripts/helper_functions.R") 

## Load auxiliary data for queries =======================================================================
ORI <- read_excel('data/raw/ecoforestry_auxiliary_data/uniqueORI_categories.xlsx')
exclude <- read_excel('data/raw/ecoforestry_auxiliary_data/eliminated_polygons.xlsx')
age <- read_excel('data/raw/ecoforestry_auxiliary_data/age.xlsx')
species <- read_excel('data/raw/ecoforestry_auxiliary_data/Species_codes.xlsx')

#Prepare codes
cut_codes <- ORI %>% filter(Cut == 'X') %>% pull(`Origine code`)
exclude_codes <- exclude %>% filter(Eliminate == 'X') %>% pull(Code)

#Read in raster layers (all should have matching CRS, res, and alignment)
# **see 'C:/harvest_risk/scripts/harvest_raster_prep.R' for details**
dist <- rast(raster("data/raw/rasters/NRCan_250_combined.tif", band = 1))
mill <- rast(raster("data/raw/rasters/NRCan_250_combined.tif", band = 2))
mean_gmv <- rast(raster("data/raw/rasters/NRCan_250_combined.tif", band = 3))
elevation<-rast("data/raw/rasters/elevation_redefined.tif")
slope<-rast("data/raw/rasters/slope_redefined.tif")

## Manage standalone data ================================================================================
# NOTE: only do this once to create the 'SUP_data_combined.csv', then you can skip these steps
data_list <- list.files(path = 'data/raw/ecoforestry_4e_shapefiles/', pattern = '\\.dbf$', full.names = TRUE)
data_list <- Filter(function(x) grepl("ETAGE_ORI", x), data_list) #to only include the 'ETAGE' layers
all_extra_data <- do.call(rbind, sapply(data_list, read.dbf, simplify = FALSE))

#Filter to only include density and hieight values for dominant stands
SUP_data <- all_extra_data %>% filter(ETAGE == 'SUP')
SUP_data <- dplyr::select(SUP_data, c("GEOCODE", "DENSITE", "HAUTEUR"))

### ISSUE WITH DUPLICATE ENTRIES IN SUP DATA ###
# Check for duplicate GEOCODEs in SUP_data
# duplicates <- SUP_data %>%
#   group_by(GEOCODE) %>%
#   filter(n() > 1)
# there are a lot of duplicates, and the data in the height/age/density columns are the same (the whole row is duplicated),
#so I'm removing the duplicates below: (keeping the first occurrence)
SUP_data <- SUP_data %>%
  distinct(GEOCODE, .keep_all = TRUE)

SUP_data <- as.data.frame(SUP_data)
write.csv(SUP_data, "data/processed/SUP_data_combined.csv")

# NOTE: once SUP_data file is saved once, access it here: 
SUP_data <- read.csv("data/processed/SUP_data_combined.csv")

##Load in sample polygons from Step 1 ===========================================================
sample_polys <- st_read('data/processed/sample_polygons.shp')

##Attach additional data to sample polygons =====================================================
sample_polys <- sample_polys %>%
  left_join(SUP_data, by = "GEOCODE")

## Assign density value based of density class ==================================================
#   Function creates new column called 'density_der' to store these derived values 
sample_polys <- assign_density(sample_polys)

## Assign height value based of density class ===================================================
#   Function creates new column called 'height_der' to store these derived values 
sample_polys <- assign_height(sample_polys)

## Get combined density and height values =======================================================
#   Reasoning: to reduce NAs in as many polygons as possible
#   IF DENSITE == NA, pull value from density_der - repeat with height
sample_polys <- sample_polys %>%
  mutate(dens_combined = ifelse(!is.na(DENSITE), DENSITE, density_der)) %>%
  mutate(haut_combined = ifelse(!is.na(HAUTEUR), HAUTEUR, height_der))

## Identify age from age class ==================================================================
#     Classify the ECO_CARTES age class categories into actual estimated ages 
#     via two methods (m1 and m2), using the assign_age custom function 
sample_polys <- assign_age(sample_polys, age)

## Identify even / un-even tiered forest polygons from age class ================================
#     Classify each forested polygon as even or un-even tiered based on age class 
sample_polys <- classify_tiers(sample_polys, age)

## Identify polygons with recent burns or pest outbreaks ========================================
#     Classifying based on the following categories: 
#             a. stand-replacing (total) burn / severe outbreak in the last 5 years: yes/no
#             b. stand-replacing (total) burn / severe outbreak in the last 10 years: yes/no 
#             c. partial burn / minor outbreak in the last 5 years: yes/no
#             d. partial burn / minor outbreak in the last 10 years: yes/no
#     'recent_disturbance()' function creates column for each category and assigns 0 or 1)
sample_polys <- recent_disturbance(sample_polys, 
                                   version = 4) #version = inventory number

## Identify dominant species type + number of species types in each polygon =====================

#First, need to classify species into type groups
species_groups <- list(
  spruce = species %>% filter(Class == 'Spruce') %>% pull(Code),
  fir = species %>% filter(Class == 'Fir') %>% pull(Code),
  pine = species %>% filter(Class == 'Pine') %>% pull(Code),
  larch = species %>% filter(Class == 'Larch') %>% pull(Code),
  birch = species %>% filter(Class == 'Birch') %>% pull(Code),
  oak = species %>% filter(Class == 'Oak') %>% pull(Code), 
  hickory = species %>% filter(Class == 'Hickory') %>% pull(Code),
  cherry = species %>% filter(Class == 'Cherry') %>% pull(Code),
  maple = species %>% filter(Class == 'Maple') %>% pull(Code),
  ash = species %>% filter(Class == 'Ash') %>% pull(Code),
  unknwn_hw = species %>% filter(Class == 'Hardwood') %>% pull(Code),
  beech = species %>% filter(Class == 'Beech') %>% pull(Code),
  walnut = species %>% filter(Class == 'Walnut') %>% pull(Code),
  elm = species %>% filter(Class == 'Elm') %>% pull(Code),
  ostryer = species %>% filter(Class == 'Ostryer') %>% pull(Code),
  poplar = species %>% filter(Class == 'Poplar') %>% pull(Code),
  aspen = species %>% filter(Class == 'Aspen') %>% pull(Code),
  hemlock = species %>% filter(Class == 'Hemlock') %>% pull(Code), 
  unknwn_sw = species %>% filter(Class == 'Softwood') %>% pull(Code), 
  basswood = species %>% filter(Class == 'Basswood') %>% pull(Code),
  cedar = species %>% filter(Class == 'Cedar') %>% pull(Code)
)

# Extract the first two characters of the species string 
sample_polys <- sample_polys %>%
  mutate(
    dom_spp = substr(sample_polys$GR_ESS, 1, 2))

# Replace the two-letter species code with the tree type
sample_polys$dom_spp <- dplyr::case_when(
  sample_polys$dom_spp %in% species_groups$fir ~ "fir",
  sample_polys$dom_spp %in% species_groups$larch ~ "larch",
  sample_polys$dom_spp %in% species_groups$pine ~ "pine",
  sample_polys$dom_spp %in% species_groups$spruce ~ "spruce",
  sample_polys$dom_spp %in% species_groups$cedar ~ "cedar",
  sample_polys$dom_spp %in% species_groups$hemlock ~ "hemlock",
  sample_polys$dom_spp %in% species_groups$unknwn_sw ~ "softwood (unknown)", 
  sample_polys$dom_spp %in% species_groups$unknwn_hw ~ "hardwood (unknown)", 
  sample_polys$dom_spp %in% species_groups$poplar ~ "poplar",
  sample_polys$dom_spp %in% species_groups$walnut ~ "walnut",
  sample_polys$dom_spp %in% species_groups$ostryer ~ "ostryer",
  sample_polys$dom_spp %in% species_groups$oak ~ "oak",
  sample_polys$dom_spp %in% species_groups$maple ~ "maple",
  sample_polys$dom_spp %in% species_groups$hickory ~ "hickory",
  sample_polys$dom_spp %in% species_groups$elm ~ "elm",
  sample_polys$dom_spp %in% species_groups$cherry ~ "cherry",
  sample_polys$dom_spp %in% species_groups$birch ~ "birch",
  sample_polys$dom_spp %in% species_groups$beech ~ "beech",
  sample_polys$dom_spp %in% species_groups$basswood ~ "basswood",
  sample_polys$dom_spp %in% species_groups$aspen ~ "aspen",
  sample_polys$dom_spp %in% species_groups$ah ~ "ash",
  TRUE ~ NA_character_
)

# Apply custome function to give species count 
sample_polys <- species_count(sample_polys, 
                              species_groups)

## Extract values from raster layers ==============================================================
sample_polys$min_dist <- exact_extract(dist, sample_polys, 'min')
sample_polys$mean_dist <- exact_extract(dist, sample_polys, 'mean')
sample_polys$min_milldist <- exact_extract(mill, sample_polys, 'min')
sample_polys$mean_milldist <- exact_extract(mill, sample_polys, 'mean')
sample_polys$mean_gmv <- exact_extract(mean_gmv, sample_polys, 'mean')
sample_polys$mean_slope <- exact_extract(slope, sample_polys, 'mean')
sample_polys$mean_elev <- exact_extract(elevation, sample_polys, 'mean')
sample_polys$max_slope <- exact_extract(slope, sample_polys, 'max')

## Save dataset in processed data folder ==========================================================
st_write(sample_polys, 'data/processed/sample_polygons_with_vars.shp', overwrite = T)

rm(list = ls())
