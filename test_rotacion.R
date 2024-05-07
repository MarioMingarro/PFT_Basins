library(sf)
library(dplyr)

# Definir CRS y crear un triángulo como área de estudio
crs_defined <- 4326  # Ejemplo con WGS 84
study_area_coords <- matrix(c(0, 0, 1, 0, 0.5, 1, 0, 0), ncol = 2, byrow = TRUE)
study_area <- st_sfc(st_polygon(list(study_area_coords)), crs = crs_defined)

# Crear dos polígonos rectangulares más pequeños dentro del triángulo
rect1_coords <- matrix(c(0.1, 0.1, 0.2, 0.1, 0.2, 0.2, 0.1, 0.2, 0.1, 0.1), ncol = 2, byrow = TRUE)
rect2_coords <- matrix(c(0.3, 0.3, 0.4, 0.3, 0.4, 0.4, 0.3, 0.4, 0.3, 0.3), ncol = 2, byrow = TRUE)
rectangles <- st_sfc(st_polygon(list(rect1_coords)), st_polygon(list(rect2_coords)), crs = crs_defined)

# Función para rotar y trasladar polígonos
rotate <- function(poly) {
  ang <- runif(1, 0, 2 * pi)  # Ángulo de rotación aleatorio en radianes
  
  while (TRUE) {
    rnd_x <- runif(1, st_bbox(study_area)$xmin, st_bbox(study_area)$xmax)  # Coordenada x aleatoria para el nuevo centroide
    rnd_y <- runif(1, st_bbox(study_area)$ymin, st_bbox(study_area)$ymax)  # Coordenada y aleatoria para el nuevo centroide
    rnd_centroid <- c(rnd_x, rnd_y)  # Nuevo punto centroide
    
    # Obtener coordenadas, centrar en origen, rotar y trasladar al nuevo centroide
    coords <- st_coordinates(poly)[,1:2]  # Asegurar que sólo se toman las primeras dos columnas
    centered_coords <- coords - matrix(colMeans(coords), nrow = nrow(coords), ncol = ncol(coords), byrow = TRUE)
    rotation_matrix <- matrix(c(cos(ang), -sin(ang), sin(ang), cos(ang)), nrow = 2)
    rotated_coords <- centered_coords %*% rotation_matrix
    new_coords <- rotated_coords + matrix(rnd_centroid, nrow = nrow(rotated_coords), ncol = ncol(rotated_coords), byrow = TRUE)
    
    # Crear el nuevo polígono y verificar si está completamente dentro del triángulo
    candidate_poly <- st_sfc(st_polygon(list(new_coords)), crs = crs_defined)
    if (st_within(candidate_poly, study_area, sparse = FALSE)[1, 1]) {
      return(candidate_poly)
    }
  }
}

# Repetir el proceso 100 veces
all_new_polys <- vector("list", 3)
for (j in 1:3) {
  new_polys <- vector("list", length(rectangles))
  
  for (i in seq_along(rectangles)) {
    poly <- rectangles[[i]]
    new_poly <- rotate(poly)
    new_polys[[i]] <- new_poly
  }
  
  all_new_polys[[j]] <- do.call(c, new_polys)
}

# Combinar todos en un objeto sf y escribir en disco
final_new_polys_sf <- do.call(c, all_new_polys)
st_write(final_new_polys_sf, "path/to/output/all_new_polys.shp")
plot(final_new_polys_sf)
