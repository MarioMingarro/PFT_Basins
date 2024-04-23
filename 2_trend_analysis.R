# Natural ----
natural <- readxl::read_excel("A:/PFT/natural_basins.xlsx")
colnames(natural) <- c("prior" ,   "area"  ,   "lat"      ,"long"    , "HYBAS_ID", "2015_Natural", "2020_Natural", "2025_Natural", "2030_Natural", "2035_Natural","2040_Natural","2045_Natural","2050_Natural" )



## Priority ----
prior <- filter(natural, natural$prior == 1)

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
rest <- filter(natural, natural$prior == 0)

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

writexl::write_xlsx(tabla, "A:/PFT/natural_results.xlsx")


