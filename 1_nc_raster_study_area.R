library(raster)
library(sf)
library(tidyverse)
library(terra)

shp <- st_read("A:/TEST/AREA_ESTUDIO.shp")
shp_vp <- terra::vect("A:/TEST/AREA_ESTUDIO.shp")

shp <- sf::st_read("B:/A_DAVID/EUROPE/STUDY_MAP/STUDY_MAP.shp")
shp_vp <- terra::vect("B:/A_DAVID/EUROPE/STUDY_MAP/STUDY_MAP.shp")


shp <- st_transform(shp, "+init=epsg:4326")

forest <- c("PFT1", "PFT2", "PFT3", "PFT4", "PFT5", "PFT6", "PFT7", "PFT8")
non_forest <- c("PFT9", "PFT10", "PFT11", "PFT12", "PFT13", "PFT14")
irrigated_crop <- c("PFT16", "PFT18", "PFT20", "PFT22", "PFT24", "PFT26", "PFT28", "PFT30")
non_irrigated_crop <- c("PFT15", "PFT17", "PFT19", "PFT21", "PFT23", "PFT25", "PFT27", "PFT29")

years <- c("2015", "2020", "2025", "2030", "2035", "2040", "2045", "2050")


## Forest ----
result_forest <- shp
raster_forest <- raster::stack()

for (i in forest){
  for (j in years){
    raster <- raster::raster(paste0("B:/A_DATA/LAND_USE/PFT/PFT/GCAM-Demeter-LU/GCAM_Demeter_LU_ssp1_rcp26_modelmean_", j, ".nc"),
                     varname = paste0(i))
    raster <- t(flip(raster, direction = 'y'))
    raster <- mask(crop(raster, shp), shp)
    names(raster) <- paste0("forest_", j, "_", i)
    raster_forest <- stack(raster_forest, raster)
    #writeRaster(raster, paste0("A:/PFT/forest_", j,"_", i, ".tif "))
  }
}

crs(raster_forest) <- "+init=epsg:4326"

for (p in years) {
  kk <- subset(raster_forest, grep(p, names(raster_forest), value = T))
  a <- calc(kk, max)
  a <- as(a, "SpatRaster")
  data <- terra::extract(a, shp_vp,  ID = TRUE)
  
  m <- data %>%
    group_by(ID) %>%
    summarise(a = mean(layer))
  m <- as.numeric(m$a)
  result_forest <- cbind(result_forest, m)
}

colnames(result_forest) <- c("HYBAS_ID", "m_2015", "m_2020", "m_2025", "m_2030", "m_2035", "m_2040", "m_2045", "m_2050", "geometry")

## non_forest ----
result_non_forest <- shp
raster_non_forest <- raster::stack()

for (i in non_forest){
  for (j in years){
    raster <- raster::raster(paste0("B:/A_DATA/LAND_USE/PFT/PFT/GCAM-Demeter-LU/GCAM_Demeter_LU_ssp1_rcp26_modelmean_", j, ".nc"),
                             varname = paste0(i))
    raster <- t(flip(raster, direction = 'y'))
    raster <- mask(crop(raster, shp), shp)
    names(raster) <- paste0("non_forest_", j, "_", i)
    raster_non_forest <- stack(raster_non_forest, raster)
    #writeRaster(raster, paste0("A:/PFT/non_forest_", j,"_", i, ".tif "))
  }
}

crs(raster_non_forest) <- "+init=epsg:4326"

for (p in years) {
  kk <- subset(raster_non_forest, grep(p, names(raster_non_forest), value = T))
  a <- calc(kk, max)
  a <- as(a, "SpatRaster")
  data <- terra::extract(a, shp_vp,  ID = TRUE)
  
  m <- data %>%
    group_by(ID) %>%
    summarise(a = mean(layer))
  m <- as.numeric(m$a)
  result_non_forest <- cbind(result_non_forest, m)
}

colnames(result_non_forest) <- c("HYBAS_ID", "m_2015", "m_2020", "m_2025", "m_2030", "m_2035", "m_2040", "m_2045", "m_2050", "geometry")

## irrigated_crop ----
result_irrigated_crop <- shp
raster_irrigated_crop <- raster::stack()

for (i in irrigated_crop){
  for (j in years){
    raster <- raster::raster(paste0("B:/A_DATA/LAND_USE/PFT/PFT/GCAM-Demeter-LU/GCAM_Demeter_LU_ssp1_rcp26_modelmean_", j, ".nc"),
                             varname = paste0(i))
    raster <- t(flip(raster, direction = 'y'))
    raster <- mask(crop(raster, shp), shp)
    names(raster) <- paste0("irrigated_crop_", j, "_", i)
    raster_irrigated_crop <- stack(raster_irrigated_crop, raster)
    #writeRaster(raster, paste0("A:/PFT/irrigated_crop_", j,"_", i, ".tif "))
  }
}

crs(raster_irrigated_crop) <- "+init=epsg:4326"

for (p in years) {
  kk <- subset(raster_irrigated_crop, grep(p, names(raster_irrigated_crop), value = T))
  a <- calc(kk, max)
  a <- as(a, "SpatRaster")
  data <- terra::extract(a, shp_vp,  ID = TRUE)
  
  m <- data %>%
    group_by(ID) %>%
    summarise(a = mean(layer))
  m <- as.numeric(m$a)
  result_irrigated_crop <- cbind(result_irrigated_crop, m)
}

colnames(result_irrigated_crop) <- c("HYBAS_ID", "m_2015", "m_2020", "m_2025", "m_2030", "m_2035", "m_2040", "m_2045", "m_2050", "geometry")


## non_irrigated_crop ----
result_non_irrigated_crop <- shp

raster_non_irrigated_crop <- raster::stack()

for (i in non_irrigated_crop){
  for (j in years){
    raster <- raster::raster(paste0("B:/A_DATA/LAND_USE/PFT/PFT/GCAM-Demeter-LU/GCAM_Demeter_LU_ssp1_rcp26_modelmean_", j, ".nc"),
                             varname = paste0(i))
    raster <- t(flip(raster, direction = 'y'))
    raster <- mask(crop(raster, shp), shp)
    names(raster) <- paste0("non_irrigated_crop_", j, "_", i)
    raster_non_irrigated_crop <- stack(raster_non_irrigated_crop, raster)
    #writeRaster(raster, paste0("A:/PFT/non_irrigated_crop_", j,"_", i, ".tif "))
  }
}

crs(raster_non_irrigated_crop) <- "+init=epsg:4326"

for (p in years) {
  kk <- subset(raster_non_irrigated_crop, grep(p, names(raster_non_irrigated_crop), value = T))
  a <- calc(kk, max)
  a <- as(a, "SpatRaster")
  data <- terra::extract(a, shp_vp,  ID = TRUE)
  
  m <- data %>%
    group_by(ID) %>%
    summarise(a = mean(layer))
  m <- as.numeric(m$a)
  result_non_irrigated_crop <- cbind(result_non_irrigated_crop, m)
}

colnames(result_non_irrigated_crop) <- c("HYBAS_ID", "m_2015", "m_2020", "m_2025", "m_2030", "m_2035", "m_2040", "m_2045", "m_2050", "geometry")

