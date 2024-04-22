library(raster)
library(sf)
library(tidyverse)
library(terra)


shp <- terra::vect("B:/A_DAVID/EUROPE/basins.shp")

shp <- shp[,c(1, 298, 299)] # Select ID, AREA, PRIORITY

shp <- project(shp, "+init=epsg:4326")

forest <- c("PFT1", "PFT2", "PFT3", "PFT4", "PFT5", "PFT6", "PFT7", "PFT8")
non_forest <- c("PFT9", "PFT10", "PFT11", "PFT12", "PFT13", "PFT14")
irrigated_crop <- c("PFT16", "PFT18", "PFT20", "PFT22", "PFT24", "PFT26", "PFT28", "PFT30")
non_irrigated_crop <- c("PFT15", "PFT17", "PFT19", "PFT21", "PFT23", "PFT25", "PFT27", "PFT29")

years <- c("2015", "2020", "2025", "2030", "2035", "2040", "2045", "2050")

#forest <- c("PFT1", "PFT2")
#years <- c("2015")

## Forest ----
raster_forest <- rast()

for (i in forest){
  for (j in years){
    raster <- terra::rast(paste0("B:/A_DATA/LAND_USE/PFT/PFT/GCAM-Demeter-LU/GCAM_Demeter_LU_ssp5_rcp85_modelmean_", j, ".nc"),
                          subds = paste0(i))
    raster <- t(terra::flip(raster, direction = 'vertical'))
    raster <- mask(crop(raster, shp), shp)
    names(raster) <- paste0("forest_", j, "_", i)
    raster_forest <- c(raster_forest, raster)
    #writeRaster(raster, paste0("A:/PFT/forest_", j,"_", i, ".tif "))
  }
}

crs(raster_forest) <- "+init=epsg:4326"

result_forest <- data.frame(matrix(NA, nrow = length(shp), ncol = 1))

for (p in years) {
  result_raster_forest <- subset(raster_forest, grep(p, names(raster_forest), value = T))
  result_raster_forest <- app(result_raster_forest, max)
  data <- terra::extract(result_raster_forest, shp,  ID = TRUE)
  
  m <- data %>%
    group_by(ID) %>%
    summarise(a = median(max)) #MEDIAN
  
  m <- as.numeric(m$a)
  result_forest <- cbind(result_forest, round(m,2))
}

result_forest[,1] <- shp$HYBAS_ID
colnames(result_forest) <- c("HYBAS_ID", "y_2015", "y_2020","y_2025", "y_2030", "y_2035", "y_2040", "y_2045", "y_2050")


writeRaster(result_forest, paste0("B:/PFT/irrigated_crop_", j,"_", i, ".tif "))



#####################
Data <- read_excel("./All_basins_results_GIS.xlsx", sheet = "Individual_trends")

x <- "Year" # Variable dependiente
y <- "value" # Variables independiente

# PRESENTE ----
## Natural ----

### Top 5
top_5_p_n <- filter(Data, Data$`12prior_rf_max_pres_5` == 3)

top_5_p_n <- top_5_p_n[,c(8:13)]
names <- colnames(top_5_p_n)
top_5_p_n <- as.data.frame(t(colMedians(as.matrix(na.omit(top_5_p_n)))))
colnames(top_5_p_n) <- names

top_5_p_n <- pivot_longer(top_5_p_n, 
                          cols = c("2015_Natural", "2020_Natural", "2025_Natural", "2030_Natural", "2035_Natural","2040_Natural"),
                          names_to = "year" ,
                          values_to = 'value')


top_5_p_n <- top_5_p_n %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Top 5")

top_5_p_n$Year <- as.numeric(top_5_p_n$Year)

### Rest_p_n
Rest_p_n <- filter(Data, Data$`14prior_rf_max_pres_20` == 0|Data$`13prior_rf_max_pres_10` == 3|Data$`14prior_rf_max_pres_20` == 3)

Rest_p_n <- Rest_p_n[,c(8:13)]
names <- colnames(Rest_p_n)
Rest_p_n <- as.data.frame(t(colMedians(as.matrix(na.omit(Rest_p_n)))))
colnames(Rest_p_n) <- names


Rest_p_n <- pivot_longer(Rest_p_n, 
                         cols = c("2015_Natural", "2020_Natural", "2025_Natural", "2030_Natural", "2035_Natural","2040_Natural"),
                         names_to = "year" ,
                         values_to = 'value')


Rest_p_n <- Rest_p_n %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Rest")

Rest_p_n$Year <- as.numeric(Rest_p_n$Year)

All <- rbind(top_5_p_n, Rest_p_n)

# Tabla vacia para guardar los resultados 
tabla_pre <- data.frame(
  "prior" = character(),
  "Trend" = numeric(),
  "t" = numeric(),
  "p" = numeric(),
  "Dif_pvalue" = numeric(),
  "prior_dif" = character(),
  "land_use" = character(), 
  "period" = character()
)


# Bucle para calcular las tendencias de cada una de las cuencas

prior <- unique(All$priority)

for (n in 1:length(prior)) {           
  ind <- All %>% 
    filter(All$priority == prior[n])
  
  
  tabla <- data.frame(
    "prior" = NA,
    "Trend" = NA,
    "t" = NA,
    "p" = NA,
    "Dif_pvalue" = NA,
    "prior_dif" = NA,
    "land_use" = NA,
    "period" = NA
  )
  gen <- All %>% 
    filter(All$priority == prior[-n])
  
  model_g <- lm(ind$value ~ ind$Year, data = gen )
  
  tabla$prior <- unique(ind[[4]])
  
  model_i <-  lm(ind$value ~ ind$Year, data = ind)                
  summary(model_i)
  
  tabla$Trend <- model_i$coefficients[[2]]
  tabla$t <- summary(model_i)$coefficients[2, 3]
  tabla$p <- summary(model_i)$coefficients[2, 4]
  
  dat <- rbind(gen,ind)
  
  model_int = lm(dat$value ~ dat$Year+dat$priority, data = dat)
  
  tabla$Dif_pvalue <- summary(model_int)$coefficients[3,4]
  tabla$prior_dif <- prior[-n]
  tabla$land_use <- c("Natural")
  tabla$period <- c("Present")
  
  tabla_pre <- rbind(tabla_pre, tabla)  # Unimos tablas
}
##########################

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

