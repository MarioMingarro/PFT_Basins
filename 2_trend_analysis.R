# Natural ----
Natural <- readxl::read_excel("A:/PFT/Natural_basins.xlsx")
colnames(Natural) <- c("prior" ,   "area"  ,   "lat"      ,"long"    , "HYBAS_ID", "2015_Natural", "2020_Natural", "2025_Natural", "2030_Natural", "2035_Natural","2040_Natural","2045_Natural","2050_Natural" )



## Priority ----
prior <- filter(Natural, Natural$prior == 1)

prior <- prior[,c(6:13)]
names <- colnames(prior)
prior <- as.data.frame(t(robustbase::colMedians(as.matrix(na.omit(prior))))) # MEDIAN
colnames(prior) <- names

prior <- pivot_longer(
  prior,
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


prior <- prior %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Priority")

prior$Year <- as.numeric(prior$Year)

## Rest ----
rest <- filter(Natural, Natural$prior == 0)

rest <- rest[,c(6:13)]
names <- colnames(rest)
rest <- as.data.frame(t(robustbase::colMedians(as.matrix(na.omit(rest))))) # MEDIAN
colnames(rest) <- names

rest <- pivot_longer(
  rest,
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


rest <- rest %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Rest")

rest$Year <- as.numeric(rest$Year)

## Interaction ----
All <- rbind(prior, rest)

tabla <- data.frame(
  "prior" = NA,
  "Trend" = NA,
  "t" = NA,
  "p" = NA,
  "Dif_pvalue" = NA,
  "land_use" = NA
)

model_prior <- lm(prior$value ~ prior$Year, data = prior)
model_rest <-  lm(rest$value ~ rest$Year, data = rest) 

tabla[1,1] <- unique(prior$priority)
tabla[1,2] <- round(model_prior$coefficients[[2]], 4)
tabla[1,3] <- summary(model_prior)$coefficients[2, 3]
tabla[1,4] <- summary(model_prior)$coefficients[2, 4]
tabla[2,1] <- unique(rest$priority)
tabla[2,2] <- round(model_rest$coefficients[[2]], 4)
tabla[2,3] <- summary(model_rest)$coefficients[2, 3]
tabla[2,4] <- summary(model_rest)$coefficients[2, 4]


model_int = lm(All$value ~ All$Year * All$priority, data = All)

tabla$Dif_pvalue <- summary(model_int)$coefficients[4,4]
tabla$land_use <- c("Natural")

writexl::write_xlsx(tabla, "A:/PFT/Natural_results.xlsx")

# Irrigated ----
Irrigated <- readxl::read_excel("A:/PFT/Irrigated_basins.xlsx")
colnames(Irrigated) <- c("prior" ,   "area"  ,   "lat"      ,"long"    , "HYBAS_ID", "2015_Irrigated", "2020_Irrigated", "2025_Irrigated", "2030_Irrigated", "2035_Irrigated","2040_Irrigated","2045_Irrigated","2050_Irrigated" )



## Priority ----
prior <- filter(Irrigated, Irrigated$prior == 1)

prior <- prior[,c(6:13)]
names <- colnames(prior)
prior <- as.data.frame(t(robustbase::colMedians(as.matrix(na.omit(prior))))) # MEDIAN
colnames(prior) <- names

prior <- pivot_longer(
  prior,
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


prior <- prior %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Priority")

prior$Year <- as.numeric(prior$Year)

## Rest ----
rest <- filter(Irrigated, Irrigated$prior == 0)

rest <- rest[,c(6:13)]
names <- colnames(rest)
rest <- as.data.frame(t(robustbase::colMedians(as.matrix(na.omit(rest))))) # MEDIAN
colnames(rest) <- names

rest <- pivot_longer(
  rest,
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


rest <- rest %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Rest")

rest$Year <- as.numeric(rest$Year)

## Interaction ----
All <- rbind(prior, rest)

tabla <- data.frame(
  "prior" = NA,
  "Trend" = NA,
  "t" = NA,
  "p" = NA,
  "Dif_pvalue" = NA,
  "land_use" = NA
)

model_prior <- lm(prior$value ~ prior$Year, data = prior)
model_rest <-  lm(rest$value ~ rest$Year, data = rest) 

tabla[1,1] <- unique(prior$priority)
tabla[1,2] <- round(model_prior$coefficients[[2]], 4)
tabla[1,3] <- summary(model_prior)$coefficients[2, 3]
tabla[1,4] <- summary(model_prior)$coefficients[2, 4]
tabla[2,1] <- unique(rest$priority)
tabla[2,2] <- round(model_rest$coefficients[[2]], 4)
tabla[2,3] <- summary(model_rest)$coefficients[2, 3]
tabla[2,4] <- summary(model_rest)$coefficients[2, 4]


model_int = lm(All$value ~ All$Year * All$priority, data = All)

tabla$Dif_pvalue <- summary(model_int)$coefficients[4,4]
tabla$land_use <- c("Irrigated")

writexl::write_xlsx(tabla, "A:/PFT/Irrigated_results.xlsx")


# Rainfed ----
Rainfed <- readxl::read_excel("A:/PFT/Rainfed_basins.xlsx")
colnames(Rainfed) <- c("prior" ,   "area"  ,   "lat"      ,"long"    , "HYBAS_ID", "2015_Rainfed", "2020_Rainfed", "2025_Rainfed", "2030_Rainfed", "2035_Rainfed","2040_Rainfed","2045_Rainfed","2050_Rainfed" )



## Priority ----
prior <- filter(Rainfed, Rainfed$prior == 1)

prior <- prior[,c(6:13)]
names <- colnames(prior)
prior <- as.data.frame(t(robustbase::colMedians(as.matrix(na.omit(prior))))) # MEDIAN
colnames(prior) <- names

prior <- pivot_longer(
  prior,
  cols = c(
    "2015_Rainfed",
    "2020_Rainfed",
    "2025_Rainfed",
    "2030_Rainfed",
    "2035_Rainfed",
    "2040_Rainfed",
    "2045_Rainfed",
    "2050_Rainfed"),
  names_to = "year" ,
  values_to = "value")


prior <- prior %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Priority")

prior$Year <- as.numeric(prior$Year)

## Rest ----
rest <- filter(Rainfed, Rainfed$prior == 0)

rest <- rest[,c(6:13)]
names <- colnames(rest)
rest <- as.data.frame(t(robustbase::colMedians(as.matrix(na.omit(rest))))) # MEDIAN
colnames(rest) <- names

rest <- pivot_longer(
  rest,
  cols = c(
    "2015_Rainfed",
    "2020_Rainfed",
    "2025_Rainfed",
    "2030_Rainfed",
    "2035_Rainfed",
    "2040_Rainfed",
    "2045_Rainfed",
    "2050_Rainfed"),
  names_to = "year" ,
  values_to = "value")


rest <- rest %>%
  separate(year, c("Year", "Type"), "_") %>% 
  mutate(priority = "Rest")

rest$Year <- as.numeric(rest$Year)

## Interaction ----
All <- rbind(prior, rest)

tabla <- data.frame(
  "prior" = NA,
  "Trend" = NA,
  "t" = NA,
  "p" = NA,
  "Dif_pvalue" = NA,
  "land_use" = NA
)

model_prior <- lm(prior$value ~ prior$Year, data = prior)
model_rest <-  lm(rest$value ~ rest$Year, data = rest) 

tabla[1,1] <- unique(prior$priority)
tabla[1,2] <- round(model_prior$coefficients[[2]], 4)
tabla[1,3] <- summary(model_prior)$coefficients[2, 3]
tabla[1,4] <- summary(model_prior)$coefficients[2, 4]
tabla[2,1] <- unique(rest$priority)
tabla[2,2] <- round(model_rest$coefficients[[2]], 4)
tabla[2,3] <- summary(model_rest)$coefficients[2, 3]
tabla[2,4] <- summary(model_rest)$coefficients[2, 4]


model_int = lm(All$value ~ All$Year * All$priority, data = All)

tabla$Dif_pvalue <- summary(model_int)$coefficients[4,4]
tabla$land_use <- c("Rainfed")

writexl::write_xlsx(tabla, "A:/PFT/Rainfed_results.xlsx")
