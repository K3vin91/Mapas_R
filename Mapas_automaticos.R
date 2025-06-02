library(ggplot2)
library(sf)
library(dplyr)
library(ggspatial)
library(RColorBrewer)
library(cowplot)

# Carpeta donde se encuentran los shapefiles
regs <- "Capas_individuales/"

# Carpeta de salida donde se guardarán los mapas generados
output_folder <- "Mapas_Generados/"

# Cargar los shapefiles de la carpeta de regionales
reg_shp <- list.files(path = regs, pattern = "\\.shp$", full.names = TRUE)

# Bounding box común (para todos los mapas)
bbox_fijo <- c(xmin = 420000, xmax = 580000, ymin = 1450000, ymax = 1600000)

# Paleta de colores
colores <- brewer.pal(8, "Set2")

# Función para generar mapas con detalles
generar_mapa_layout <- function(shp_path) {
  # Cargar el shapefile desde la ruta proporcionada
  capa <- st_read(shp_path, quiet = TRUE)
  
  # Obtener el nombre del archivo sin la extensión para usarlo en el título
  nombre_archivo <- tools::file_path_sans_ext(basename(shp_path))
  
  # Mapa principal (izquierda)
  p_mapa <- ggplot() +
    geom_sf(data = capa, fill = sample(colores, 1), color = "black", size = 0.3) +
    coord_sf(
      datum = st_crs(32616),
      xlim = c(bbox_fijo["xmin"], bbox_fijo["xmax"]),
      ylim = c(bbox_fijo["ymin"], bbox_fijo["ymax"])
    ) +
    annotation_scale(location = "bl", width_hint = 0.25) +  # Escala gráfica
    annotation_north_arrow(location = "tl", which_north = "true", 
                           style = north_arrow_fancy_orienteering) +  # Flecha de norte
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = "black"),
      plot.margin = margin(5, 5, 5, 5),  # Márgenes del mapa
      axis.text = element_text(color = "black"),
      axis.title = element_blank(),
      axis.text.x = element_text(angle = 90)  # Coordenadas verticales a la izquierda
    ) +
    theme(plot.background = element_rect(color = "black", size = 1))  # Marco simple
  
  # Panel derecho con información
  p_info <- ggplot() + 
    theme_void() +
    annotate("text", x = 0.5, y = 0.95, label = paste("Mapa de", nombre_archivo),
             size = 6, fontface = "bold", hjust = 0.5) +  # Título
    annotate("rect", xmin = 0, xmax = 1, ymin = 0.80, ymax = 0.90, fill = "gray90", color = "black") +  # Espacio para logo
    annotate("text", x = 0.5, y = 0.85, label = "Logo institucional", size = 3, hjust = 0.5) +  # Logo (texto placeholder)
    annotate("rect", xmin = 0, xmax = 1, ymin = 0.60, ymax = 0.78, fill = "gray95", color = "black") +  # Espacio para simbología
    annotate("text", x = 0.5, y = 0.69, label = "Simbología (placeholder)", size = 3, hjust = 0.5) +  # Simbología (texto placeholder)
    annotate("rect", xmin = 0, xmax = 1, ymin = 0.50, ymax = 0.58, fill = "white", color = "black") +  # Espacio para escala numérica
    annotate("text", x = 0.5, y = 0.54, label = "Escala numérica: 1:50,000", size = 3, hjust = 0.5)  # Escala numérica
  
  # Combinar ambos paneles (mapa a la izquierda y panel a la derecha)
  final_layout <- plot_grid(
    p_mapa, p_info,
    rel_widths = c(2, 1),  # Relación de tamaños: mapa 2/3, panel 1/3
    nrow = 1
  )
  
  # Guardar el mapa generado en la carpeta especificada
  output_path <- paste0(output_folder, "mapa_", nombre_archivo, ".jpg")
  ggsave(output_path, plot = final_layout, width = 11.7, height = 8.3, units = "in")  # Guardar en formato A4
}

# Crear la carpeta de salida si no existe
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# Bucle para generar los mapas de todos los shapefiles
for (shp in reg_shp) {
  generar_mapa_layout(shp)
}
