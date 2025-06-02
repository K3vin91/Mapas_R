library(ggplot2)
library(sf)
library(dplyr)

# Capas de cada regional
regs <- "Capas_individuales/"

# Cargar los shapefiles de la carpeta de regionales
reg_shp <- list.files(path = regs, pattern = "\\.shp$", full.names = TRUE)

# Bucle para generar mapas individuales
for (shp in reg_shp){
  capa <- st_read(shp)
  nombre_archivo <- tools::file_path_sans_ext(basename(shp))
  mapa <- ggplot() +
    geom_sf(data = capa) +
    coord_sf(datum = sf::st_crs(32616)) +
    theme_minimal() +
    ggtitle(paste("Mapa de ", nombre_archivo))
  
  ggsave(paste0("mapa_", nombre_archivo, ".jpg"), plot = mapa, width = 10, height = 8, units = "in")
} 
 







