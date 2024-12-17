# CUSTOM FUNCTIONS FOR HARVEST RISK VARIABLE EXTRACTION ========================
# Finalized 12-12-2024 

## PART 1: GENERATE SAMPLE POLYGONS  -------------------------------------------

sample_polygons <- function(n_harvested, 
                            n_nonharvested, 
                            fmu_codes, 
                            shapefile_path) {
  
  # Load auxiliary data needed for sampling 
  ORI <- read_excel('data/raw/ecoforestry_auxiliary_data/uniqueORI_categories.xlsx')
  exclude <- read_excel('data/raw/ecoforestry_auxiliary_data/eliminated_polygons.xlsx')
  
  cut_codes <- ORI %>% filter(Cut == 'X') %>% pull(`Origine code`)
  exclude_codes <- exclude %>% filter(Eliminate == 'X') %>% pull(Code)
  
  # Get list of relevant shapefiles
  fmu_list <- list.files(path = shapefile_path, pattern = '\\.shp$', full.names = TRUE)
  fmu_list <- fmu_list[grepl("PEE", fmu_list) & grepl(fmu_codes, fmu_list) & !grepl("_C", fmu_list)]
  
  # Initialize final sample set
  sample.set <- as.data.frame(NULL)
  
  # Loop through each shapefile
  for (i in seq_along(fmu_list)) {
    
    # Read shapefile
    data <- st_read(fmu_list[i])
    code <- str_sub(fmu_list[i], -7, -5)
    data$ID <- rep(code, nrow(data))
    
    # Filter polygons based on exclude codes
    incl <- data %>% filter(!CO_TER %in% exclude_codes | !TYPE_TE %in% exclude_codes)
    
    # Assign harvested or non-harvested based on cut codes
    incl <- incl %>% mutate(harvest = if_else(ORIGINE %in% cut_codes, 1, 0))
    
    # Generate random sample of harvested polygons 
    harv_poly <- incl[incl$harvest == 1, ] #subset harvested polygons 
    n_harvest <- min(n_harvested, nrow(harv_poly)) #Set number of sampled rows
    harv_sample <- harv_poly[sample(nrow(harv_poly), n_harvest), ]
    
    # Generate random sample of non-harvested polygons 
    non_harv <- incl[incl$harvest == 0, ]
    n_nonharv <- min(n_nonharvested, nrow(non_harv))
    nonharv_sample <- non_harv[sample(nrow(non_harv), n_nonharv), ]
    
    # Combine samples for this FMU
    fmu_sample <- rbind(harv_sample, nonharv_sample)
    
    # Add to final sample set
    sample.set <- rbind(sample.set, fmu_sample)
    
    message(paste0("FMU ", code, " added to sample set (", i, "/", length(fmu_list), ")"))
  }
  
  return(sample.set)
}


## PART 2: EXTRACT VARIABLES FROM SAMPLE POLYGONS ------------------------------

assign_density <- function(data) {
  # Define the mapping of old values to new values
  density_map <- c(
    "+" = "0.5",
    "A" = "90",
    "B" = "70",
    "C" = "50",
    "D" = "32.5",
    "E" = "15",
    "F" = "3",
    "H" = "80",
    "I" = "30",
    "O" = "0"
  )
  
  # Replace values in the CL_DENS column based on the map
  data$density_der <- ifelse(data$CL_DENS == "", "", density_map[data$CL_DENS])
  
  return(data)
}


assign_height <- function(data) {
  # Define the mapping of old values to new values
  height_map <- c(
    "-" = "0.75",
    "1" = "28.25", #Check at some point what the max height is across FMUs (for now its 35 so 21.5-35)
    "2" = "19",
    "3" = "14",
    "4" = "9",
    "5" = "5",
    "6" = "2.5",
    "7" = "1"
  )
  
  # Replace values in the CL_DENS column based on the map
  data$height_der <- ifelse(data$CL_HAUT == "", "", height_map[data$CL_HAUT])
  
  return(data)
}


get_harvested_polygons <- function(data, 
                                   ORI) {
  cut_codes <- ORI %>% filter(Cut == 'X') %>% pull(`Origine code`)
  data %>%
    mutate(harvest = ifelse(ORIGINE %in% cut_codes, 1, 0))
}

assign_age <- function(data, 
                       age) {
  age_groups <- list(
    m1_10 = age %>% filter(method_1 == 10) %>% pull(Code),
    m2_10 = age %>% filter(method_2 == 10) %>% pull(Code),
    m1_120 = age %>% filter(method_1 == 120) %>% pull(Code),
    m2_120 = age %>% filter(method_2 == 120) %>% pull(Code),
    m1_30 = age %>% filter(method_1 == 30) %>% pull(Code),
    m2_30 = age %>% filter(method_2 == 30) %>% pull(Code),
    m1_50 = age %>% filter(method_1 == 50) %>% pull(Code),
    m2_50 = age %>% filter(method_2 == 50) %>% pull(Code),
    m1_70 = age %>% filter(method_1 == 70) %>% pull(Code),
    m2_70 = age %>% filter(method_2 == 70) %>% pull(Code),
    m1_90 = age %>% filter(method_1 == 90) %>% pull(Code),
    m2_90 = age %>% filter(method_2 == 90) %>% pull(Code),
    m1_40 = age %>% filter(method_1 == 40) %>% pull(Code),
    m2_40 = age %>% filter(method_2 == 40) %>% pull(Code),
    m1_100 = age %>% filter(method_1 == 100) %>% pull(Code),
    m2_100 = age %>% filter(method_2 == 100) %>% pull(Code),
    m1_110 = age %>% filter(method_1 == 110) %>% pull(Code),
    m2_110 = age %>% filter(method_2 == 110) %>% pull(Code),
    m1_130 = age %>% filter(method_1 == 130) %>% pull(Code),
    m2_130 = age %>% filter(method_2 == 130) %>% pull(Code)
  )
  data %>%
    mutate(
      m1_age = case_when(
        CL_AGE %in% age_groups$m1_10 ~ 10,
        CL_AGE %in% age_groups$m1_100 ~ 100,
        CL_AGE %in% age_groups$m1_110 ~ 110,
        CL_AGE %in% age_groups$m1_120 ~ 120,
        CL_AGE %in% age_groups$m1_130 ~ 130,
        CL_AGE %in% age_groups$m1_30 ~ 30,
        CL_AGE %in% age_groups$m1_40 ~ 40,
        CL_AGE %in% age_groups$m1_50 ~ 50,
        CL_AGE %in% age_groups$m1_70 ~ 70,
        CL_AGE %in% age_groups$m1_90 ~ 90,
        TRUE ~ NA_real_
      ),
      m2_age = case_when(
        CL_AGE %in% age_groups$m2_10 ~ 10,
        CL_AGE %in% age_groups$m2_100 ~ 100,
        CL_AGE %in% age_groups$m2_110 ~ 110,
        CL_AGE %in% age_groups$m2_120 ~ 120,
        CL_AGE %in% age_groups$m2_130 ~ 130,
        CL_AGE %in% age_groups$m2_30 ~ 30,
        CL_AGE %in% age_groups$m2_40 ~ 40,
        CL_AGE %in% age_groups$m2_50 ~ 50,
        CL_AGE %in% age_groups$m2_70 ~ 70,
        CL_AGE %in% age_groups$m2_90 ~ 90,
        TRUE ~ NA_real_
      )
    )
}

classify_tiers <- function(data, 
                           age) {
  even_codes <- age %>% filter(Even == 'X') %>% pull(Code)
  
  data %>%
    mutate(tiers = ifelse(is.na(CL_AGE), NA, ifelse(CL_AGE %in% even_codes, 'E', 'U')))
  
}

recent_disturbance <- function(data, 
                               version) {
  
  if (version == 2) {
    end_year = 1994
  } else if (version == 3) {
    end_year = 2003
  } else if (version == 4) {
    end_year = 2015
  } else if (version == 5) {
    end_year = 2023
  } else {
    print("Invalid version.")
  }
  
  tburn_codes <- c('BR', 'BRD')
  pburn_codes <- c('BRP')
  maj_OB <- c('ES')
  min_OB <- c('EL')
  yrs5 <- c(seq((end_year - 4), end_year))
  yrs10 <- c(seq((end_year - 9), end_year))
  
  data %>%
    mutate(
      tburn_5 = ifelse(ORIGINE %in% tburn_codes & AN_ORIG %in% yrs5, 1, 0),
      tburn_10 = ifelse(ORIGINE %in% tburn_codes & AN_ORIG %in% yrs10, 1, 0),
      pburn_5 = ifelse(PERTURB %in% pburn_codes & AN_PERT %in% yrs5, 1, 0),
      pburn_10 = ifelse(PERTURB %in% pburn_codes & AN_PERT %in% yrs10, 1, 0),
      majOB_5 = ifelse(ORIGINE %in% maj_OB & AN_ORIG %in% yrs5, 1, 0),
      majOB_10 = ifelse(ORIGINE %in% maj_OB & AN_ORIG %in% yrs10, 1, 0),
      minOB_5 = ifelse(PERTURB %in% min_OB & AN_PERT %in% yrs5, 1, 0),
      minOB_10 = ifelse(PERTURB %in% min_OB & AN_PERT %in% yrs10, 1, 0))
}

species_count <- function(data, 
                          species_groups) {
  data$n_spp <- sapply(data$GR_ESS, function(x) {
    if (is.na(x)) return(NA_integer_)
    
    # Split the string into two-letter species codes
    species_codes <- unique(substring(x, seq(1, nchar(x), by = 2), seq(2, nchar(x), by = 2)))
    
    # Classify the species based on the type
    species_type <- sapply(species_codes, function(code) {
      dplyr::case_when(
        code %in% species_groups$fir ~ "fir",
        code %in% species_groups$larch ~ "larch",
        code %in% species_groups$pine ~ "pine",
        code %in% species_groups$spruce ~ "spruce",
        code %in% species_groups$cedar ~ "cedar",
        code %in% species_groups$hemlock ~ "hemlock",
        code %in% species_groups$unknwn_sw ~ "softwood (unknown)",
        code %in% species_groups$unknwn_hw ~ "hardwood (unknown)",
        code %in% species_groups$poplar ~ "poplar",
        code %in% species_groups$walnut ~ "walnut",
        code %in% species_groups$ostryer ~ "ostryer",
        code %in% species_groups$oak ~ "oak",
        code %in% species_groups$maple ~ "maple",
        code %in% species_groups$hickory ~ "hickory",
        code %in% species_groups$elm ~ "elm",
        code %in% species_groups$cherry ~ "cherry",
        code %in% species_groups$birch ~ "birch",
        code %in% species_groups$beech ~ "beech",
        code %in% species_groups$basswood ~ "basswood",
        code %in% species_groups$aspen ~ "aspen",
        code %in% species_groups$ah ~ "ash",
        TRUE ~ NA_character_
      )
    })
    
    # Count the number of unique non-NA species types
    length(unique(na.omit(species_type)))
  })
  
  return(data)  # Return the modified dataframe
}

# PART 3: CREATE AOI POLYGON SUBSETS -------------------------------------------

get_FMU_AoI <- function(data, 
                        buffer_dist, 
                        all_polygons){
  
  data <- data %>% #Dissolve polygons
    st_union %>%
    st_as_sf
  
  buff_coverage <- st_buffer(data, dist = buffer_dist)
  
  rm(data)
  
  AOI <- st_intersection(all_polygons, buff_coverage)
  
  return(AOI)
}

# PART 4: EXTRACT SPATIAL VARIABLES --------------------------------------------

parallel_calculate_in_buffer <- function(data, 
                                         target_layer, 
                                         buffer_dist) {
  
  library(future)         # For parallel processing
  library(future.apply)   # For applying functions in parallel
  
  # Set up a parallel plan (e.g., using all available cores)
  options(future.globals.maxSize = 4 * 1024^3)
  plan(multisession, workers = parallel::detectCores())
  
  # Ensure that 'data' and 'target_layer' are 'sf' objects
  if (!inherits(data, "sf")) {
    stop("'data' must be an 'sf' object.")
  }
  if (!inherits(target_layer, "sf")) {
    stop("'target_layer' must be an 'sf' object.")
  }
  
  # Ensure that 'data' and 'target_layer' are in the same CRS
  if (st_crs(data) != st_crs(target_layer)) {
    warning("'data' and 'target_layer' have different CRS. Transforming 'target_layer' to match 'data'.")
    target_layer <- st_transform(target_layer, st_crs(data))
  }
  
  # Ensure that CRS is projected in meters (not longitude/latitude)
  if (sf::st_is_longlat(data)) {
    warning("'data' is in longitude/latitude. Reprojecting to EPSG:3857 for metric units.")
    data <- sf::st_transform(data, 3857)
    target_layer <- sf::st_transform(target_layer, 3857)
  }
  
  #Dissolve target layer polygons
  target_layer <- target_layer %>%  ##NOTE: this was added in to correct the issue I was having with 
    st_union %>%                    ##      water area being greater that the buffer area, thus giving a >1 proportion. 
    st_as_sf()                      ##      Could have been caused by overlapping water polygons, but I'm not sure. 
                                    ##      All I know is this fixed it.
  
  # Ensure geometries are valid
  data <- st_make_valid(data)
  target_layer <- st_make_valid(target_layer)
  
  # Function to process each polygon
  process_by_polygon <- function(i) {
    # Get the i-th polygon from 'data'
    polygon <- data[i, ]
    
    # Get the centroid of the polygon
    centroid <- st_centroid(polygon)
    
    # Create a circular buffer around the centroid
    buffer_geom <- st_buffer(centroid, dist = buffer_dist)
    
    # Calculate the total area of the buffer
    buffer_area <- st_area(buffer_geom)
    
    # Clip 'target_layer' to the buffer
    clipped_target <- st_intersection(buffer_geom, target_layer)
    
    # Check if there is any geometry left after clipping
    if (nrow(clipped_target) == 0 || is.null(clipped_target$geometry)) {
      target_area <- 0
    } else {
      # Calculate the area of the clipped features
      target_area <- sum(st_area(clipped_target))
    }
    
    # Calculate the proportion of the target area within the buffer
    proportion <- as.numeric(target_area) / as.numeric(buffer_area)
    
    # Ensure the proportion does not exceed 1 due to rounding errors
    proportion <- min(proportion, 1)
    
    return(proportion)
  }
  
  # Apply the 'process_polygon' function in parallel over all polygons in 'data'
  indices <- seq_len(nrow(data))
  proportions <- future_sapply(indices, process_by_polygon)
  
  return(proportions)
  
}

para_calculate_in_cluster <- function(data, 
                                      AoI_data, 
                                      target_layer, 
                                      buffer_dist) {
  
  library(future)
  library(future.apply)
  
  # Set up a parallel plan
  options(future.globals.maxSize = 4 * 1024^3)
  plan(multisession, workers = parallel::detectCores())
  
  # Ensure 'data' and 'target_layer' are 'sf' objects
  if (!inherits(data, "sf")) stop("'data' must be an 'sf' object.")
  if (!inherits(target_layer, "sf")) stop("'target_layer' must be an 'sf' object.")
  
  # Ensure 'data' and 'target_layer' are in the same CRS
  if (st_crs(data) != st_crs(target_layer)) {
    warning("'data' and 'target_layer' have different CRS. Transforming 'target_layer' to match 'data'.")
    target_layer <- st_transform(target_layer, st_crs(data))
  }
  
  # Ensure CRS is projected in meters
  if (sf::st_is_longlat(data)) {
    warning("'data' is in longitude/latitude. Reprojecting to EPSG:3857 for metric units.")
    data <- sf::st_transform(data, 3857)
    target_layer <- sf::st_transform(target_layer, 3857)
  }
  
  # Ensure geometries are valid
  data <- st_make_valid(data)
  target_layer <- st_make_valid(target_layer)
  
  # Function to process each polygon
  process_by_polygon <- function(i) {
    polygon <- data[i, ]
    centroid <- st_centroid(polygon)
    buffer_geom <- st_buffer(centroid, dist = buffer_dist)
    
    # Select clusters within the buffer area
    cluster_target <- target_layer[st_within(target_layer, buffer_geom, sparse = FALSE), ]
    cluster_total <- AoI_data[st_within(AoI_data, buffer_geom, sparse = FALSE), ]
    
    # Calculate total area
    total_area <- sum(st_area(cluster_total))
    
    # Calculate target area
    if (nrow(cluster_target) == 0) {
      target_area <- 0
    } else {
      target_area <- sum(st_area(cluster_target))
    }
    
    
    # Calculate proportion
    proportion <- as.numeric(target_area) / as.numeric(total_area)
    return(proportion)
  }
  
  # Apply the process in parallel and return results
  proportions <- future_sapply(1:nrow(data), process_by_polygon)
  return(proportions)
}

para_ave_in_cluster <- function(data, 
                                target_layer, 
                                buffer_dist) {
  
  library(future)
  library(future.apply)
  library(sf)
  
  # Set up a parallel plan
  options(future.globals.maxSize = 4 * 1024^3)
  plan(multisession, workers = parallel::detectCores())
  
  # Ensure 'data' and 'target_layer' are 'sf' objects
  if (!inherits(data, "sf")) stop("'data' must be an 'sf' object.")
  if (!inherits(target_layer, "sf")) stop("'target_layer' must be an 'sf' object.")
  
  # Ensure 'data' and 'target_layer' are in the same CRS
  if (st_crs(data) != st_crs(target_layer)) {
    warning("'data' and 'target_layer' have different CRS. Transforming 'target_layer' to match 'data'.")
    target_layer <- st_transform(target_layer, st_crs(data))
  }
  
  # Ensure CRS is projected in meters
  if (sf::st_is_longlat(data)) {
    warning("'data' is in longitude/latitude. Reprojecting to EPSG:3857 for metric units.")
    data <- sf::st_transform(data, 3857)
    target_layer <- sf::st_transform(target_layer, 3857)
  }
  
  # Ensure geometries are valid
  data <- st_make_valid(data)
  target_layer <- st_make_valid(target_layer)
  
  # Function to process each polygon
  process_by_polygon <- function(i) {
    polygon <- data[i, ]
    centroid <- st_centroid(polygon)
    buffer_geom <- st_buffer(centroid, dist = buffer_dist)
    
    # Select clusters within the buffer area
    cluster_target <- target_layer[st_within(target_layer, buffer_geom, sparse = FALSE), ]
    
    # Calculate average values if any clusters are found
    if (nrow(cluster_target) == 0) {
      ave_age <- NA
      ave_dens <- NA
      ave_haut <- NA
      ave_gmv <- NA
    } else {
      ave_age <- mean(as.numeric(cluster_target$m1_age), na.rm = TRUE)
      ave_dens <- mean(as.numeric(cluster_target$dens_combined), na.rm = TRUE)
      ave_haut <- mean(as.numeric(cluster_target$haut_combined), na.rm = TRUE)
      ave_gmv <- mean(as.numeric(cluster_target$mean_gmv), na.rm = TRUE)
    }
    
    # Return results as a list
    return(list(ave_age = ave_age, ave_dens = ave_dens, ave_haut = ave_haut, ave_gmv = ave_gmv))
  }
  
  # Apply the process in parallel
  results <- future_sapply(1:nrow(data), process_by_polygon, simplify = FALSE)
  
  # Extract results into columns
  data$ave.age <- sapply(results, function(x) x$ave_age)
  data$ave.dens <- sapply(results, function(x) x$ave_dens)
  data$ave.haut <- sapply(results, function(x) x$ave_haut)
  data$ave.gmv <- sapply(results, function(x) x$ave_gmv)
  
  return(data)
  
}
