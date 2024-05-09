
pr <- 1


Natural <- readxl::read_excel("B:/A_DAVID/EUROPE/PFT/natural_basins.xlsx")

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

## Plot ----
All <- rbind(prior, rest)

P1 <- ggplot(All, aes(Year, value, col = priority)) +
  geom_point(shape = "diamond", size = 2) +
  geom_smooth(method = lm, se = FALSE, size = .5) +
  ylab("% Natural") +
  xlab("Year") + scale_color_manual(
    name = NULL,
    guide = "legend",
    values = c("Priority" = "darkorange2",
               "Rest" = "gray")
  ) +
  theme_bw()


# Irrigated ----
Irrigated <- readxl::read_excel("A:/PFT/irrigated_basins.xlsx")

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

## Plot ----
All <- rbind(prior, rest)

P2 <- ggplot(All, aes(Year, value, col = priority)) +
  geom_point(shape = "diamond", size = 2) +
  geom_smooth(method = lm, se = FALSE, size = .5) +
  ylab("% Irrigated") +
  xlab("Year") + scale_color_manual(
    name = NULL,
    guide = "legend",
    values = c("Priority" = "darkorange2",
               "Rest" = "gray")
  ) +
  theme_bw()
# Rainfed ----
Rainfed <- readxl::read_excel("A:/PFT/rainfed_basins.xlsx")

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

## Plot ----
All <- rbind(prior, rest)

P3 <- ggplot(All, aes(Year, value, col = priority)) +
  geom_point(shape = "diamond", size = 2) +
  geom_smooth(method = lm, se = FALSE, size = .5) +
  ylab("% Rainfed") +
  xlab("Year") + scale_color_manual(
    name = NULL,
    guide = "legend",
    values = c("Priority" = "darkorange2",
               "Rest" = "gray")
  ) +
  theme_bw()

ggscatter(All, x = "Year", y = "value",
          add = "reg.line",               # Add regression line
          conf.int = TRUE,                # Add confidence interval
          color = "priority", palette = "jco", # Color by groups "cyl"
          shape = "priority"                   # Change point shape by groups "cyl"
)+stat_cor(aes(color = priority), label.x = 2030) 

library(ggpubr)
ggarrange(P1, P2, P3,ncol = 1, nrow = 3, common.legend = T)

library(geomtextpath)
library(ggplot2)
ggplot(All, aes(Year, value, col = priority)) +
  geom_point(shape = "diamond", size = 2) +
  geom_labelsmooth(method = lm, se = FALSE, size = .5) +
  ylab("% Rainfed") +
  xlab("Year") + scale_color_manual(
    name = NULL,
    guide = "legend",
    values = c("Priority" = "darkorange2",
               "Rest" = "gray")
  ) +
  theme_bw()
