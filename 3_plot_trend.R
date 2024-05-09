
pr <- 10


Natural <- readxl::read_excel("B:/A_DAVID/EUROPE/PFT/RESULT/SSP2/natural_basins.xlsx")

## Priority ----
prior <- filter(Natural, Natural$prior <= pr)

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
rest <- filter(Natural, Natural$prior == 999)

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
model_prior <- lm(prior$value ~ prior$Year, data = prior)
model_prior <- round(model_prior$coefficients[[2]], 4)

model_rest <-  lm(rest$value ~ rest$Year, data = rest) 
model_rest <- round(model_rest$coefficients[[2]], 4)

prior <- prior %>% mutate(trend =model_prior)
rest <- rest %>% mutate(trend =model_rest)

## Plot ----
All <- rbind(prior, rest)

P1 <- ggplot(All, aes(Year, value, col = priority)) +
  geom_point(shape = "diamond", size = 2) +
  geom_labelsmooth(aes(label = trend), fill = "white",
                   method = "lm", formula = y ~ x,
                   size = 3, linewidth = .5, boxlinewidth = .5)+
  ylab("% Natural") +
  xlab("Year") + scale_color_manual(
    name = NULL,
    guide = "legend",
    values = c("Priority" = "darkorange2",
               "Rest" = "gray")
  ) +
  theme_bw()


# Irrigated ----
Irrigated <- readxl::read_excel("B:/A_DAVID/EUROPE/PFT/RESULT/SSP2/irrigated_basins.xlsx")

## Priority ----
prior <- filter(Irrigated, Irrigated$prior <= pr)

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
rest <- filter(Irrigated, Irrigated$prior == 999)

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

model_prior <- lm(prior$value ~ prior$Year, data = prior)
model_prior <- round(model_prior$coefficients[[2]], 4)
model_rest <-  lm(rest$value ~ rest$Year, data = rest) 
model_rest <- round(model_rest$coefficients[[2]], 4)

prior <- prior %>% mutate(trend = model_prior)
rest <- rest %>% mutate(trend = model_rest)

## Plot ----
All <- rbind(prior, rest)

P2 <- ggplot(All, aes(Year, value, col = priority)) +
  geom_point(shape = "diamond", size = 2) +
  geom_labelsmooth(aes(label = trend), fill = "white",
                   method = "lm", formula = y ~ x,
                   size = 3, linewidth = .5, boxlinewidth = .5)+
  ylab("% Irrigated") +
  xlab("Year") + scale_color_manual(
    name = NULL,
    guide = "legend",
    values = c("Priority" = "darkorange2",
               "Rest" = "gray")
  ) +
  theme_bw()
# Rainfed ----
Rainfed <- readxl::read_excel("B:/A_DAVID/EUROPE/PFT/RESULT/SSP2/rainfed_basins.xlsx")

## Priority ----
prior <- filter(Rainfed, Rainfed$prior <= pr)

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
rest <- filter(Rainfed, Rainfed$prior == 999)

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

model_prior <- lm(prior$value ~ prior$Year, data = prior)
model_prior <- round(model_prior$coefficients[[2]], 4)
model_rest <-  lm(rest$value ~ rest$Year, data = rest) 
model_rest <- round(model_rest$coefficients[[2]], 4)

prior <- prior %>% mutate(trend =model_prior)
rest <- rest %>% mutate(trend =model_rest)
## Plot ----
All <- rbind(prior, rest)

P3 <- ggplot(All, aes(Year, value, col = priority)) +
  geom_point(shape = "diamond", size = 2) +
  geom_labelsmooth(aes(label = trend), fill = "white",
                   method = "lm", formula = y ~ x,
                   size = 3, linewidth = .5, boxlinewidth = .5)+
  ylab("% Rainfed") +
  xlab("Year") + scale_color_manual(
    name = NULL,
    guide = "legend",
    values = c("Priority" = "darkorange2",
               "Rest" = "gray")
  ) +
  theme_bw()


library(ggpubr)
library(geomtextpath)
a <- ggarrange(P1, P2, P3,ncol = 1, nrow = 3, common.legend = T)
annotate_figure(a, top = text_grob(paste0(pr, "%"), 
                                      color = "Black", face = "bold", size = 14))
