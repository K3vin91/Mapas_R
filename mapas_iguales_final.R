library(ggplot2)
library(sf)
library(dplyr)
library(ggspatial)
library(janitor)  # para limpiar nombres
library(stringr)
library(RColorBrewer)
library(cowplot)
library(png)
library(grid)


############# Directorios de trabajo #########################

# Carpeta con shapefile. 
shp <- st_read("C:/Users/kevin/Documents/R_map_UTM/Departamentos_Honduras/Departamentos_Honduras.shp") ### Se esta usando una direccion absoluta con el fin de separar los scrips 
# (Opcional) transformar a UTM si quieres luego trabajar con áreas                                     ### de las capas, aunque no es necesario, normalmente se usa direccion relativas
shp <- st_transform(shp, 32616)
# Calcular los límites fijos (toda la capa) para mapa secundario
limites_fijos <- st_bbox(shp)
# Carpeta de salida
salida <- "C:/Users/kevin/Documents/R_map_UTM/Mapas_iguales/"
# Crear carpeta de salida si no existe
if (!dir.exists(salida)) dir.create(salida)


############# Imagenes de logos institucionales ##############
# El numero de imagenes puede cambiar 
img1 <- readPNG("logo-unah.png")
img2 <- readPNG("logo-ctig.png")
img3 <- readPNG("faces_logo.png")

grob1 <- rasterGrob(img1, interpolate = TRUE)
grob2 <- rasterGrob(img2, interpolate = TRUE)
grob3 <- rasterGrob(img3, interpolate = TRUE)

logo1 <- ggdraw() + draw_grob(grob1, width = 1, height = 1)
logo2 <- ggdraw() + draw_grob(grob2, width = 1, height = 1)
logo3 <- ggdraw() + draw_grob(grob3, width = 1, height = 1)

logos_plot <- plot_grid(
  logo1, logo2, logo3,
  ncol = 3,
  rel_widths = c(1, 1, 1),
  align = "h"
)

############## Texto descriptivo del mapa ####################
texto <- "Fuente: INE Honduras\nDatos procesados por SIG\nNotas: Escala aproximada. Proyección UTM zona 16N. 
          Año: 2025, Creado por: Kevin Irias"

# Crear un grob de texto centrado
text_box <- ggdraw() +
  draw_label(
    label = texto,
    size = 7,  # tamaño base del texto
    fontface = "plain",
    x = 0.5, y = 0.5,
    hjust = 0.5, vjust = 0.2,
    lineheight = 1.2
  ) +
  theme(
    plot.background = element_rect(fill = "gray98", color = "black")
  )

############# Funcion para extraer leyenda ###################
extraer_leyenda <- function(plot) {
  ggdraw(get_legend(plot))
}

########################################################################################
####### Función para crear límites cuadrados con margen para el mapa principal #########
ajustar_cuadro <- function(sf_obj, margen = 0.1, proporcion = 1.0) {
  bbox <- st_bbox(sf_obj)
  x_range <- bbox$xmax - bbox$xmin
  y_range <- bbox$ymax - bbox$ymin
  
  # Calculamos la base según la proporción deseada
  base_range <- max(x_range / proporcion, y_range)
  
  x_mid <- (bbox$xmax + bbox$xmin) / 2
  y_mid <- (bbox$ymax + bbox$ymin) / 2
  
  x_half <- (1 + margen) * base_range * proporcion / 2
  y_half <- (1 + margen) * base_range / 2
  
  xlim <- c(x_mid - x_half, x_mid + x_half)
  ylim <- c(y_mid - y_half, y_mid + y_half)
  
  list(xlim = xlim, ylim = ylim)
}

######################### Separacion de las entidades ##################################
# Asumimos que la columna que identifica al municipio se llama "NOMBRE". Si tiene otro nombre, cámbialo aquí
shp_split <- split(shp, shp$DEPTO)

############ Bucle de creacion de mapas ##############
for (i in seq_along(shp_split)) {
  capa <- shp_split[[i]]
  nombre_mapa <- names(shp_split)[i]
  nombre_archivo <- names(shp_split)[i]
  nombre_archivo <- janitor::make_clean_names(nombre_archivo)  # Limpia para el nombre del archivo
  
  limites <- ajustar_cuadro(capa, margen = 0.1, proporcion = 1.2)
  
  
  ########## Mapa principal ##########
  mapa_con_leyenda <- ggplot() +
    geom_sf(data = shp, fill = "gray90", color = "darkgray", linewidth = 0.5) +
    geom_sf(data = capa, aes(fill = DEPTO), color = "black", linewidth = 0.3) +
    coord_sf(xlim = limites$xlim, ylim = limites$ylim, expand = FALSE, datum = sf::st_crs(32616)) +
    theme_minimal()+
    scale_fill_brewer(palette = "Set3", name = "Departamento") +
    theme_minimal() +
    theme(
      legend.title = element_text(size = 9, face = "bold", hjust = 0),  # Alineación izquierda
      legend.text = element_text(size = 8, hjust = 0),                  # Alineación izquierda
      legend.key.size = unit(0.5, "cm"),                                # Tamaño de cajas
      legend.box.margin = margin(0, 0, 0, 0),
      legend.margin = margin(2, 2, 2, 2)
    )
  
  
  ### Variable para guardar simbologia ###
  leyenda <- extraer_leyenda(mapa_con_leyenda)
  leyenda_plot <- ggdraw() +
    draw_plot(leyenda, x = 0, y = 0, width = 1, height = 1, hjust = 0, vjust = 0) +
    theme(
      plot.background = element_rect(color = "black", fill = NA, linewidth = 0.5)
    )
  
  
  # Mapa sin leyenda (para mostrar en el layout final)
  mapa <- mapa_con_leyenda +
    annotation_scale(location = "bl", width_hint = 0.4, line_width = 0.8) +
    annotation_north_arrow(
      location = "tl", which_north = "true",
      style = north_arrow_fancy_orienteering(),
      height = unit(1.2, "cm"), width = unit(1.2, "cm")
    ) +
    theme(
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
      axis.text.y = element_text(angle = 90, hjust = 0.5),
      plot.title = element_text(hjust = 0.5),
      legend.position = "none"  # 👈 Solo en esta versión
    )
  
  
  ###### Crear el mapa secundario ######
  mapa_secundario <- ggplot() +
    geom_sf(data = shp, fill = "gray90", color = "darkgray", linewidth = 0.5) +  # Fondo
    geom_sf(data = capa, fill = NA, color = "black", linewidth = 1) +  # Contorno grueso
    geom_sf(data = capa, fill = "orange", color = "black", linewidth = 0.5) +  # Relleno naranja
    coord_sf(
      xlim = c(limites_fijos$xmin, limites_fijos$xmax),
      ylim = c(limites_fijos$ymin, limites_fijos$ymax),
      expand = FALSE
    ) +
    theme_void()
  mapa_secundario_plot <- ggdraw() +
    draw_plot(
      plot = mapa_secundario,
      x = 0.05, y = 0.05,   # Mueve el plot adentro del contenedor (padding izquierdo e inferior)
      width = 0.9, height = 0.9  # Reduce el tamaño del plot (padding derecho y superior)
    ) +
    theme(
      plot.background = element_rect(color = "black", fill = NA, linewidth = 0.5)
    )
  
  ### Crear el título del mapa ###
  titulo_mapa <- ggdraw() +
    draw_label(
      label = paste("Mapa de prueba de\n", nombre_mapa),
      fontface = "bold",
      size = 20,        # Puedes ajustar esto según el diseño general
      x = 0.5, y = 0.5,  # Centrado
      hjust = 0.5, vjust = 0.5,
      lineheight = 1.2
    ) +
    theme(
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  ######## Panel derecho con información (el uso de "annotate" es provisional, no sirve para los elementos del mapa)
  # Combinar elementos del panel derecho en una sola columna
  panel_derecho <- plot_grid(
    titulo_mapa,
    ggdraw(),
    logos_plot,
    ggdraw(),
    leyenda_plot,
    ggdraw(),
    text_box,
    ggdraw(),
    mapa_secundario_plot,
    ggdraw(),
    ncol = 1,
    rel_heights = c(0.10, 0.01, 0.06, 0.02, 0.30, 0.02, 0.2, 0.02, 0.25, 0.02)
  )
  
  # Combinar ambos paneles (mapa a la izquierda y panel a la derecha)
  final_layout <- plot_grid(
    mapa,
    ggdraw(),
    panel_derecho,
    ggdraw(),
    rel_widths = c(3, 0.05, 0.90, 0.05),  # Relación de tamaños: mapa 3/4, panel 1/4
    nrow = 1
  )
  ######## Segmento para salvar los mapas ########
  ggsave(
    filename = paste0(salida, "mapa_", nombre_archivo, ".jpg"),
    plot = final_layout, width = 17, height = 11, units = "in", dpi = 300
  )
  cat("Mapa de", nombre_archivo, "fue creado\n")
} 
