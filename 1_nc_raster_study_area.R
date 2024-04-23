library(raster)
library(sf)
library(tidyverse)
library(terra)


shp <- terra::vect("B:/A_DAVID/EUROPE/basins_WGS84.shp")

#shp <- shp[,c(1, 298, 299)] # Select ID, AREA, PRIORITY

shp <- project(shp, "+init=epsg:4326")

#forest <- c("PFT1", "PFT2", "PFT3", "PFT4", "PFT5", "PFT6", "PFT7", "PFT8")
#non_forest <- c("PFT9", "PFT10", "PFT11", "PFT12", "PFT13", "PFT14")


natural <- c("PFT1", "PFT2", "PFT3", "PFT4", "PFT5", "PFT6", "PFT7", "PFT8", "PFT9", "PFT10",
             "PFT11", "PFT12", "PFT13", "PFT14")
irrigated_crop <- c("PFT16", "PFT18", "PFT20", "PFT22", "PFT24", "PFT26", "PFT28", "PFT30")
rainfed_crop <- c("PFT15", "PFT17", "PFT19", "PFT21", "PFT23", "PFT25", "PFT27", "PFT29")

years <- c("2015", "2020", "2025", "2030", "2035", "2040", "2045", "2050")

#forest <- c("PFT1", "PFT2")
#years <- c("2015", "2020")

# Natural ----
raster_natural <- rast()

for (i in natural){
  for (j in years){
    raster <- terra::rast(paste0("B:/A_DATA/LAND_USE/PFT/PFT/GCAM-Demeter-LU/GCAM_Demeter_LU_ssp5_rcp85_modelmean_", j, ".nc"),
                          subds = paste0(i))
    raster <- t(terra::flip(raster, direction = 'vertical'))
    raster <- mask(crop(raster, shp), shp)
    names(raster) <- paste0("natural_", j, "_", i)
    raster_natural <- c(raster_natural, raster)
    }
}

crs(raster_natural) <- "+init=epsg:4326"

result_natural <- data.frame(matrix(NA, nrow = length(shp), ncol = 1))

for (p in years) {
  result_raster_natural <- subset(raster_natural, grep(p, names(raster_natural), value = T))
  result_raster_natural <- app(result_raster_natural, max, cores=6)
  data <- terra::extract(result_raster_natural, shp,  ID = TRUE)
  
  m <- data %>%
    group_by(ID) %>%
    summarise(a = mean(na.omit(max))) #MEAN
  
  m <- as.numeric(m$a)
  result_natural <- cbind(result_natural, round(m,2))
}

result_natural[,1] <- shp$HYBAS_ID
colnames(result_natural) <- c("HYBAS_ID", "y_2015", "y_2020","y_2025", "y_2030", "y_2035", "y_2040", "y_2045", "y_2050")

result_natural <- cbind(prior = shp$PRIOR, area = shp$Area, lat = shp$Latitude, long = shp$Longitude, result_natural)

rm(data, raster, raster_natural, result_raster_natural)
writexl::write_xlsx(result_natural, "A:/PFT/result_natural.xlsx")

# Irrigated ----
raster_irrigated <- rast()

for (i in irrigated){
  for (j in years){
    raster <- terra::rast(paste0("B:/A_DATA/LAND_USE/PFT/PFT/GCAM-Demeter-LU/GCAM_Demeter_LU_ssp5_rcp85_modelmean_", j, ".nc"),
                          subds = paste0(i))
    raster <- t(terra::flip(raster, direction = 'vertical'))
    raster <- mask(crop(raster, shp), shp)
    names(raster) <- paste0("irrigated_", j, "_", i)
    raster_irrigated <- c(raster_irrigated, raster)
  }
}

crs(raster_irrigated) <- "+init=epsg:4326"

result_irrigated <- data.frame(matrix(NA, nrow = length(shp), ncol = 1))

for (p in years) {
  result_raster_irrigated <- subset(raster_irrigated, grep(p, names(raster_irrigated), value = T))
  result_raster_irrigated <- app(result_raster_irrigated, max, cores=6)
  data <- terra::extract(result_raster_irrigated, shp,  ID = TRUE)
  
  m <- data %>%
    group_by(ID) %>%
    summarise(a = mean(na.omit(max))) #MEAN
  
  m <- as.numeric(m$a)
  result_irrigated <- cbind(result_irrigated, round(m,2))
}

result_irrigated[,1] <- shp$HYBAS_ID
colnames(result_irrigated) <- c("HYBAS_ID", "y_2015", "y_2020","y_2025", "y_2030", "y_2035", "y_2040", "y_2045", "y_2050")

result_irrigated <- cbind(prior = shp$PRIOR, area = shp$Area, lat = shp$Latitude, long = shp$Longitude, result_irrigated)

rm(data, raster, raster_irrigated, result_raster_irrigated)
writexl::write_xlsx(result_irrigated, "A:/PFT/result_irrigated.xlsx")

# Rainfed ----
raster_rainfed <- rast()

for (i in rainfed){
  for (j in years){
    raster <- terra::rast(paste0("B:/A_DATA/LAND_USE/PFT/PFT/GCAM-Demeter-LU/GCAM_Demeter_LU_ssp5_rcp85_modelmean_", j, ".nc"),
                          subds = paste0(i))
    raster <- t(terra::flip(raster, direction = 'vertical'))
    raster <- mask(crop(raster, shp), shp)
    names(raster) <- paste0("rainfed_", j, "_", i)
    raster_rainfed <- c(raster_rainfed, raster)
  }
}

crs(raster_rainfed) <- "+init=epsg:4326"

result_rainfed <- data.frame(matrix(NA, nrow = length(shp), ncol = 1))

for (p in years) {
  result_raster_rainfed <- subset(raster_rainfed, grep(p, names(raster_rainfed), value = T))
  result_raster_rainfed <- app(result_raster_rainfed, max, cores=6)
  data <- terra::extract(result_raster_rainfed, shp,  ID = TRUE)
  
  m <- data %>%
    group_by(ID) %>%
    summarise(a = mean(na.omit(max))) #MEAN
  
  m <- as.numeric(m$a)
  result_rainfed <- cbind(result_rainfed, round(m,2))
}

result_rainfed[,1] <- shp$HYBAS_ID
colnames(result_rainfed) <- c("HYBAS_ID", "y_2015", "y_2020","y_2025", "y_2030", "y_2035", "y_2040", "y_2045", "y_2050")

result_rainfed <- cbind(prior = shp$PRIOR, area = shp$Area, lat = shp$Latitude, long = shp$Longitude, result_rainfed)

rm(data, raster, raster_rainfed, result_raster_rainfed)
writexl::write_xlsx(result_rainfed, "A:/PFT/result_rainfed.xlsx")

