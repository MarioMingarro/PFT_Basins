library(terra)
library(sf)
library(tidyverse)
library(mblm) # Theil-Sen
library(Kendall) # Mann-Kendall
library(scales)
library(patchwork)
library(writexl)
library(exactextractr)

# 1. PARAMETROS ----
SCENARIO <- "ssp5_rcp85" # "ssp2_rcp45" o "ssp5_rcp85"
PRIORITY_LEVELS <- c("A", "B", "C") # A = maxima prioridad
ALPHA <- 0.05
MIN_OBS <- 4 # minimo de años validos para estimar tendencia
CRS_MAP <- 3035 # LAEA89 Europa (equiarea)

# Rutas
DIR_NC <- "D:/A_DATA/LAND_USE/PFT/PFT/GCAM-Demeter-LU/"
SHP_PATH <- "C:/A_TRABAJO/CARLOTA/basins_bio_class.shp"
DIR_OUT <- file.path("C:/A_TRABAJO/CARLOTA/RESULTADOS_TENDENCIAS", toupper(SCENARIO))
DIR_RAST <- file.path(DIR_OUT, "RASTERS")
DIR_FIG <- file.path(DIR_OUT, "FIGURES")
for (d in c(DIR_OUT, DIR_RAST, DIR_FIG))
 dir.create(d, recursive = TRUE, showWarnings = FALSE)

# Columnas del shapefile
COL_HYBAS <- "HYBAS_ID"
COL_PRIOR <- "PRIOR"
COL_PA <- "PA_PERC"
COL_LAT <- "Latitude"
COL_LON <- "Longitude"


YEARS <- seq(2015, 2050, by = 5) # Años
CLASSES <- list(                 # Definicion de clases (PFT, Chen 2020)
 Natural = paste0("PFT", 1:14),
 Irrigated = paste0("PFT", c(16, 18, 20, 22, 24, 26, 28, 30)),
 Rainfed = paste0("PFT", c(15, 17, 19, 21, 23, 25, 27, 29))
)

grp_cols <- c(A = "#E41A1C", B = "#4DAF4A", C = "#377EB8") # Paleta de colores grupos

# 2. EXTRACCIÓN: fraccion de cada clase por cuenca y año ----
shp <- terra::vect(SHP_PATH)
shp <- terra::project(shp, "EPSG:4326")

extract_class_year <- function(class_pfts, year, scenario, shp, class_name, dir_rast) {
 nc_file <- file.path(
 DIR_NC, sprintf("GCAM_Demeter_LU_%s_modelmean_%d.nc", scenario, year))
 layers <- terra::rast(lapply(class_pfts, function(pft) {
 r <- terra::rast(nc_file, subds = pft)
 terra::trans(r)
 }))
 terra::crs(layers) <- "EPSG:4326"
 class_frac <- sum(layers, na.rm = TRUE)
 class_frac <- terra::mask(terra::crop(class_frac, shp), shp)
 
 names(class_frac) <- sprintf("%s_%d", class_name, year)
 out_tif <- file.path(dir_rast, sprintf("%s_%d_%s.tif", class_name, year, scenario))
 terra::writeRaster(class_frac, out_tif, overwrite = TRUE,
 gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2"))
 
 area_r <- terra::cellSize(class_frac, unit = "km")
 exactextractr::exact_extract(
 class_frac, sf::st_as_sf(shp),
 fun = "weighted_mean", weights = area_r, progress = TRUE)
}

meta_df <- as.data.frame(shp)

basin_meta <- tibble(
  HYBAS_ID = as.character(meta_df[[COL_HYBAS]]),
  PRIOR    = meta_df[[COL_PRIOR]],
  PA_PERC  = meta_df[[COL_PA]],
  lat      = meta_df[[COL_LAT]],
  long     = meta_df[[COL_LON]]
)

long_list <- list()
for (cls in names(CLASSES)) {
 message(" clase: ", cls)
 mat <- sapply(YEARS, function(y)
 extract_class_year(CLASSES[[cls]], y, SCENARIO, shp, cls, DIR_RAST))
 stopifnot(is.matrix(mat))
 df <- as.data.frame(mat)
 names(df) <- as.character(YEARS)
 long_list[[cls]] <- cbind(basin_meta, df) %>%
 pivot_longer(cols = all_of(as.character(YEARS)),
 names_to = "year", values_to = "fraction") %>%
 mutate(year = as.numeric(year), class = cls)
}

 basins_long <- bind_rows(long_list) %>%
 mutate(group = factor(PRIOR, levels = PRIORITY_LEVELS)) %>%
 filter(!is.na(group))

write_xlsx(basins_long, file.path(DIR_OUT, sprintf("basins_long_%s.xlsx", SCENARIO)))
#basins_long <- readxl::read_xlsx(file.path(DIR_OUT, sprintf("basins_long_%s.xlsx", SCENARIO)))

# 3. TENDENCIA POR CUENCA: Theil-Sen (magnitud) + Mann-Kendall (significancia)----
basin_trend <- function(df) {
 df <- df[is.finite(df$year) & is.finite(df$fraction), , drop = FALSE]
 df <- df[order(df$year), , drop = FALSE]
 n <- nrow(df)
 out <- tibble(slope = NA_real_, slope_decade = NA_real_, net_change = NA_real_,
 mean_frac = if (n > 0) mean(df$fraction) else NA_real_,
 mk_p = NA_real_, sig_trend = FALSE)
 if (n < MIN_OBS || stats::sd(df$fraction) == 0) return(out) # serie constante o insuficiente -> NA (no se estima tendencia)
 sl <- tryCatch(
 coef(mblm::mblm(fraction ~ year, dataframe = df, repeated = FALSE))[[2]],
 error = function(e) NA_real_)
 mk <- tryCatch(Kendall::MannKendall(df$fraction), error = function(e) NULL)
 out$slope <- sl
 out$slope_decade <- sl * 10
 out$net_change <- mean(tail(df$fraction, 2)) - mean(head(df$fraction, 2))
 if (!is.null(mk)) out$mk_p <- as.numeric(mk$sl)
 out$sig_trend <- is.finite(out$mk_p) & out$mk_p < ALPHA
 out
}

slopes <- basins_long %>%
 group_by(HYBAS_ID, group, class, PA_PERC, lat, long) %>%
 group_modify(~ basin_trend(.x)) %>%
 ungroup() %>%
 mutate(scenario = SCENARIO)

write_xlsx(slopes, file.path(DIR_OUT, sprintf("slopes_per_basin_%s.xlsx", SCENARIO)))
#slopes <- readxl::read_xlsx(file.path(DIR_OUT, sprintf("slopes_per_basin_%s.xlsx", SCENARIO)))
CLASSES_PRESENT <- sort(unique(slopes$class))

slopes <- slopes %>%
  group_by(class) %>%
  mutate(mk_p_adj = p.adjust(mk_p, method = "BH"), #Corrección Benjamini-Hochberg (falsos positivos-muchas cuencas)
         sig_trend = is.finite(mk_p_adj) & mk_p_adj < ALPHA) %>%
  ungroup()

# 4. RESUMEN ----
resumen <- slopes %>%
 group_by(class, group) %>%
 summarise(
 n = n(),
 # tendencia (slope_decade, %/decada)
 mean_decade = mean(slope_decade, na.rm = TRUE),
 median_decade = median(slope_decade, na.rm = TRUE),
 p05_decade = quantile(slope_decade, 0.05, na.rm = TRUE),
 q25_decade = quantile(slope_decade, 0.25, na.rm = TRUE),
 q75_decade = quantile(slope_decade, 0.75, na.rm = TRUE),
 p95_decade = quantile(slope_decade, 0.95, na.rm = TRUE),
 iqr_decade = IQR(slope_decade, na.rm = TRUE),
  # cambio neto (%)
 mean_net = mean(net_change, na.rm = TRUE),
 median_net = median(net_change, na.rm = TRUE),
 # proporciones interpretables
 pct_increasing = mean(slope > 0, na.rm = TRUE) * 100,
 pct_sig_trend = mean(sig_trend, na.rm = TRUE) * 100,
 .groups = "drop"
 ) %>%
 mutate(scenario = SCENARIO)

write_xlsx(list(resumen = resumen),
 file.path(DIR_OUT, sprintf("resumen_%s.xlsx", SCENARIO))
)

# 5. TRAYECTORIAS MEDIANAS POR CLASE Y GRUPO ----
traj <- basins_long %>%
 filter(!is.na(group), !is.na(fraction)) %>%
 group_by(class, group, year) %>%
 summarise(median_frac = median(fraction),
 p25 = quantile(fraction, 0.25),
 p75 = quantile(fraction, 0.75),
 .groups = "drop")

p_traj <- ggplot(traj, aes(x = year, colour = group, fill = group)) +
 geom_ribbon(aes(ymin = p25, ymax = p75), colour = NA, alpha = 0.12) + # banda IQR
 geom_line(aes(y = median_frac), linewidth = 0.9) + # mediana
 geom_point(aes(y = median_frac), size = 1.8) + # mediana
 facet_wrap(~ class, scales = "free_y") +
 scale_colour_manual(values = grp_cols) +
 scale_fill_manual(values = grp_cols) +
 labs(x = "Year", y = "Median %", colour = NULL, fill = NULL,
 title = sprintf("Theil-Sen median trends - %s", toupper(SCENARIO))) +
 theme_bw()
ggsave(file.path(DIR_FIG, sprintf("trayectorias_%s.png", SCENARIO)),
 p_traj, width = 10, height = 4, dpi = 300)

# 6. MAPAS DE TENDENCIA POR CLASE----

basins <- st_read(SHP_PATH, quiet = TRUE) %>%
 st_make_valid() %>%
 rename(HYBAS_ID = !!COL_HYBAS) %>%
 mutate(HYBAS_ID = as.character(HYBAS_ID)) %>%
 select(HYBAS_ID)

basins_sl <- basins %>%
 inner_join(slopes, by = "HYBAS_ID") %>%
 st_transform(CRS_MAP)

study_area <- basins %>% 
 st_union() %>% 
 st_transform(CRS_MAP)

map_class <- function(cls) {
 d <- basins_sl %>%
 filter(class == cls, is.finite(slope_decade))
 cap <- as.numeric(quantile(abs(d$slope_decade), 0.95, na.rm = TRUE))
 if (!is.finite(cap) || cap == 0) cap <- max(abs(d$slope_decade), na.rm = TRUE)
 a <- d %>% filter(group == "A")
 
 ggplot(d) +
 geom_sf(aes(fill = slope_decade), colour = NA) +
 geom_sf(data = study_area, fill = NA, colour = "grey50", linewidth = 0.6)+
 {if (nrow(a) > 0)
 geom_sf(data = a, fill = NA, colour = "black", linewidth = 0.3) } +
 scale_fill_gradient2(
 low = "#2166AC", mid = "grey92", high = "#B2182B", midpoint = 0,
 limits = c(-cap, cap), oob = scales::squish) +
 labs(title = cls) +
 theme_void(base_size = 11) +
 theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
}

p_maps <- wrap_plots(lapply(CLASSES_PRESENT, map_class),
 ncol = length(CLASSES_PRESENT))
ggsave(file.path(DIR_FIG, sprintf("mapa_tendencias_%s.png", SCENARIO)),
 p_maps, width = 5 * length(CLASSES_PRESENT), height = 5, dpi = 300)

