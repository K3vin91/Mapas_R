library(tmap)
library(sf)

# Leer y transformar el shapefile si aún no lo has hecho
shp <- st_read("Departamentos_Honduras/Departamentos_Honduras.shp")
shp <- st_transform(shp, 32616)

# Mapa temático con mejoras estéticas
tm_shape(shp) +
  tm_fill(
    col = "Km2",
    palette = "Greens",
    style = "quantile",
    title = "Área (km²)"
  ) +
  tm_borders(lwd = 0.5, col = "black") +
  
  # Escala gráfica y flecha norte
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(type = "8star", position = c("left", "top"), size = 2) +
  
  # Grilla con coordenadas UTM
  tm_grid(
    lines = TRUE,
    labels.inside.frame = FALSE,
    x = seq(250000, 700000, by = 50000),
    y = seq(1400000, 1900000, by = 50000),
    col = "gray60",
    lwd = 0.2,
    labels.rot=c(0,90)
  ) +
  
  # Layout con título más alto y centrado
  tm_layout(
    title = "Mapa de Superficie por Departamento",
    title.position = c("center", "top"),
    title.size = 1.4,
    title.fontface = "bold",
    legend.outside = FALSE,
    frame = TRUE,
    legend.frame = TRUE
  )
