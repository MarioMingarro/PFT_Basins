Natural <- readxl::read_excel("A:/PFT/natural_basins.xlsx")
hhh

kk <- Natural$HYBAS_ID
tabla_ind <- data.frame(
  "HYBAS_ID" = character(),
  "Trend" = numeric(),
  "t" = numeric(),
  "p" = numeric())
#nrow(Natural)
for(i in 1:nrow(Natural)){
  tryCatch({
  
  tabla <- data.frame(
    "HYBAS_ID"= NA,
    "Trend" = NA,
    "t" = NA,
    "p" = NA
  )
  p <- Natural[i,6:13]
  p <- pivot_longer(
    p,
    cols = c(
      "2015_Natural",
      "2020_Natural",
      "2025_Natural",
      "2030_Natural",
      "2035_Natural",
      "2040_Natural",
      "2045_Natural",
      "2050_Natural"),
    names_to = "year" ,
    values_to = "value")
  p <- p %>%
    separate(year, c("Year", "Type"), "_")
  p$Year <- as.numeric(p$Year)
  
  a <- lm(value ~ Year, data = p) 
  tabla[1,1] <- Natural[i, 5]
  tabla[1,2] <- round(a$coefficients[[2]], 4)
  tabla[1,3] <- summary(a)$coefficients[2, 3]
  tabla[1,4] <- summary(a)$coefficients[2, 4]
  tabla_ind <- rbind(tabla_ind, tabla)
  }, error = function(e){
    # En caso de error, agregar NA
    tabla_ind <- rbind(tabla_ind, data.frame(HYBAS_ID = Natural[i, 5],  Trend = NA, t = NA, p = NA))
  })
}



irrigated <- readxl::read_excel("A:/PFT/irrigated_basins.xlsx")

kk <- irrigated$HYBAS_ID
tabla_ind <- data.frame(
  "HYBAS_ID" = character(),
  "Trend" = numeric(),
  "t" = numeric(),
  "p" = numeric())
#nrow(irrigated)
for(i in 1:nrow(irrigated)){
  tryCatch({
    
    tabla <- data.frame(
      "HYBAS_ID"= NA,
      "Trend" = NA,
      "t" = NA,
      "p" = NA
    )
    p <- irrigated[i,6:13]
    p <- pivot_longer(
      p,
      cols = c(
        "2015_Irrigated",
        "2020_Irrigated",
        "2025_Irrigated",
        "2030_Irrigated",
        "2035_Irrigated",
        "2040_Irrigated",
        "2045_Irrigated",
        "2050_Irrigated"),
      names_to = "year" ,
      values_to = "value")
    p <- p %>%
      separate(year, c("Year", "Type"), "_")
    p$Year <- as.numeric(p$Year)
    
    a <- lm(value ~ Year, data = p) 
    tabla[1,1] <- irrigated[i, 5]
    tabla[1,2] <- round(a$coefficients[[2]], 4)
    tabla[1,3] <- summary(a)$coefficients[2, 3]
    tabla[1,4] <- summary(a)$coefficients[2, 4]
    tabla_ind <- rbind(tabla_ind, tabla)
  }, error = function(e){
    # En caso de error, agregar NA
    tabla_ind <- rbind(tabla_ind, data.frame(HYBAS_ID = irrigated[i, 5],  Trend = NA, t = NA, p = NA))
  })
}



library(sf)
kk <- st_read("B:/A_DAVID/EUROPE/basins_WGS84.shp")
kk <- kk %>% 
  left_join(tabla_ind, by = "HYBAS_ID")
plot(kk['Trend'])
