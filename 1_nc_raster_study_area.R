library(raster)
library(sf)
library(tidyverse)


shp <- st_read("B:/A_DAVID/EUROPE/STUDY_MAP/STUDY_MAP.shp")

natural <- c("PFT1", "PFT2", "PFT3", "PFT4", "PFT5", "PFT6", "PFT7", "PFT8", "PFT9", "PFT10", "PFT11", "PFT12", "PFT13", "PFT14")
irrigated_crop <- c("PFT16", "PFT18", "PFT20", "PFT22", "PFT24", "PFT26", "PFT28", "PFT30")
non_irrigated_crop <- c("PFT15", "PFT17", "PFT19", "PFT21", "PFT23", "PFT25", "PFT27", "PFT29")


years <- c("2015", "2020", "2025", "2030", "2035", "2040")



## Natural ----
raster_natural <- raster::stack()

for (i in natural){
  for (j in years){
    raster <- raster::raster(paste0("B:/A_DATA/LAND_USE/PFT/PFT/GCAM-Demeter-LU/GCAM_Demeter_LU_ssp1_rcp26_modelmean_", j, ".nc"),
                     varname = paste0(i))
    raster <- t(flip(raster, direction = 'y'))
    raster <- mask(crop(raster, shp), shp)
    raster_natural <- stack(raster_natural, raster)
    assign(paste0("raster_natural_", j), calc(raster_natural, max))
  }
}

## Irrigated crop ------
raster_irrigated <- raster::stack()

for (i in irrigated_crop){
  for (j in years){
    raster <- raster(paste0("B:/DATA/land_use/Project ID 68344/GCAM-Demeter-LU/GCAM_Demeter_LU_ssp1_rcp26_modelmean_", j, ".nc"),
                     varname = paste0(i))
    raster <- t(flip(raster, direction = 'y'))
    raster <- mask(crop(raster, shp), shp)
    raster_irrigated <- stack(raster_irrigated, raster)
    assign(paste0("raster_irrigated_", j), calc(raster_irrigated, max))
  }
}

## Non irrigated crop ----
raster_non_irrigated <- raster::stack()

for (i in non_irrigated_crop){
  for (j in years){
    raster <- raster(paste0("B:/DATA/land_use/Project ID 68344/GCAM-Demeter-LU/GCAM_Demeter_LU_ssp1_rcp26_modelmean_", j, ".nc"),
                     varname = paste0(i))
    raster <- t(flip(raster, direction = 'y'))
    raster <- mask(crop(raster, shp), shp)
    raster_non_irrigated <- stack(raster_non_irrigated, raster)
    assign(paste0("raster_non_irrigated_", j), calc(raster_non_irrigated, max))
  }
}


writeRaster(raster_natural_2015, "A:/MARRUECOS/LAND_USE/raster_natural_2015.tif")
writeRaster(raster_natural_2020, "A:/MARRUECOS/LAND_USE/raster_natural_2020.tif")
writeRaster(raster_natural_2025, "A:/MARRUECOS/LAND_USE/raster_natural_2025.tif")
writeRaster(raster_natural_2030, "A:/MARRUECOS/LAND_USE/raster_natural_2030.tif")
writeRaster(raster_natural_2035, "A:/MARRUECOS/LAND_USE/raster_natural_2035.tif")
writeRaster(raster_natural_2040, "A:/MARRUECOS/LAND_USE/raster_natural_2040.tif")
writeRaster(raster_irrigated_2015, "A:/MARRUECOS/LAND_USE/raster_irrigated_2015.tif")
writeRaster(raster_irrigated_2020, "A:/MARRUECOS/LAND_USE/raster_irrigated_2020.tif")
writeRaster(raster_irrigated_2025, "A:/MARRUECOS/LAND_USE/raster_irrigated_2025.tif")
writeRaster(raster_irrigated_2030, "A:/MARRUECOS/LAND_USE/raster_irrigated_2030.tif")
writeRaster(raster_irrigated_2035, "A:/MARRUECOS/LAND_USE/raster_irrigated_2035.tif")
writeRaster(raster_irrigated_2040, "A:/MARRUECOS/LAND_USE/raster_irrigated_2040.tif")
writeRaster(raster_non_irrigated_2015, "A:/MARRUECOS/LAND_USE/raster_non_irrigated_2015.tif")
writeRaster(raster_non_irrigated_2020, "A:/MARRUECOS/LAND_USE/raster_non_irrigated_2020.tif")
writeRaster(raster_non_irrigated_2025, "A:/MARRUECOS/LAND_USE/raster_non_irrigated_2025.tif")
writeRaster(raster_non_irrigated_2030, "A:/MARRUECOS/LAND_USE/raster_non_irrigated_2030.tif")
writeRaster(raster_non_irrigated_2035, "A:/MARRUECOS/LAND_USE/raster_non_irrigated_2035.tif")
writeRaster(raster_non_irrigated_2040, "A:/MARRUECOS/LAND_USE/raster_non_irrigated_2040.tif")