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
shp <- st_read("~/R_map_UTM/Departamentos_Honduras/Departamentos_Honduras.shp") ### Se esta usando una direccion absoluta con el fin de separar los scrips 
# (Opcional) transformar a UTM si quieres luego trabajar con áreas                                     ### de las capas, aunque no es necesario, normalmente se usa direccion relativas
shp <- st_transform(shp, 32616)
# Calcular los límites fijos (toda la capa) para mapa secundario
limites_fijos <- st_bbox(shp)
# Carpeta de salida
salida <- "~/R_map_UTM/Mapas_iguales/"
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
    plot.background = element_rect(fill = "white", color = "black")
  )

############# Funcion para extraer leyenda ###################
extraer_leyenda <- function(plot) {
  ggdraw(get_legend(plot))
}

########################################################################################
####### Función para crear límites cuadrados con margen para el mapa principal #########
ajustar_cuadro <- function(sf_obj, proporcion = 1.5, margen = 0.05) {
  
  bbox <- st_bbox(sf_obj)
  x_mid <- (bbox$xmax + bbox$xmin) / 2
  y_mid <- (bbox$ymax + bbox$ymin) / 2
  
  # Calcular rangos reales
  x_range <- bbox$xmax - bbox$xmin
  y_range <- bbox$ymax - bbox$ymin
  
  # Ajustar al rectángulo manteniendo el mayor de los rangos y la proporción
  base_height <- max(y_range * (1 + margen), x_range * (1 + margen) / proporcion)
  base_width  <- base_height * proporcion
  
  x_half <- base_width / 2
  y_half <- base_height / 2
  
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
  
  limites <- ajustar_cuadro(capa, proporcion = 2, margen = 0.05)
  
  # Número de categorías únicas en el campo DEPTO
  num_categorias <- length(unique(capa$DEPTO))
  
  # Definir número de columnas dinámicamente
  num_columnas <- ifelse(num_categorias > 12, 2, 1)   # si hay más de 12, divide en 2 columnas
  
  ########## Mapa principal ##########
  mapa <- ggplot() +
    geom_sf(data = shp, fill = "gray90", color = "darkgray", linewidth = 0.5) +
    geom_sf(data = capa, aes(fill = DEPTO), color = "black", linewidth = 0.3) +
    coord_sf(xlim = limites$xlim, ylim = limites$ylim, expand = FALSE, datum = sf::st_crs(32616)) +
    theme_minimal()+
    scale_fill_brewer(palette = "Set3", name = "Simbologia",
                      guide = guide_legend(
                        keywidth = unit(0.6, "cm"),
                        keyheight = unit(0.6, "cm"),
                        title.position = "top",
                        label.position = "right",
                        nrow = 5,
                        ncol = num_columnas
                      )
    ) +
    annotation_scale(location = "bl", width_hint = 0.4, line_width = 0.8) +
    annotation_north_arrow(
      location = "tl", which_north = "true",
      style = north_arrow_fancy_orienteering(),
      height = unit(1.2, "cm"), width = unit(1.2, "cm")
    ) +
    theme_minimal()+
    theme(
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
      axis.text.y = element_text(angle = 90, hjust = 0.5),
      plot.title = element_text(hjust = 0.5),
      legend.position = c(0.90, 0.12),   # 👈 
      legend.title = element_text(size = 9, face = "bold"),
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.5, "cm"),
      legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5), # fondo y borde
      legend.box.background = element_rect(fill = "white", color = "black", linewidth = 0.5) # marco general
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
      label = paste("Mapa de prueba de", nombre_mapa),
      fontface = "bold",
      size = 20,        # Puedes ajustar esto según el diseño general
      x = 0.5, y = 0.5,  # Centrado
      hjust = 0.5, vjust = 0.5,
      lineheight = 1.2
    ) +
    theme(
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  ######## Panel inferior con información (el uso de "annotate" es provisional, no sirve para los elementos del mapa)
  # Bloque vertical: título encima de logos
  bloque_titulo_logos <- plot_grid(
    titulo_mapa,
    logos_plot,
    ncol = 1,         # vertical
    rel_heights = c(0.5, 0.5)
  )
  # Crear un "marco" usando un plot vacío con borde
  bloque_con_marco <- ggdraw() +
    draw_plot(bloque_titulo_logos, x = 0.05, y = 0.09, width = 0.9, height = 0.9) +
    theme(
      panel.background = element_rect(fill = "white", color = "black", size = 0.4)
    )
  
  # Combinar elementos del panel inferior en una sola fila (horizontal)
  panel_inferior <- plot_grid(
    bloque_con_marco,
    text_box,
    mapa_secundario_plot,
    ncol = 3,
    rel_widths = c(0.6, 0.2, 0.2)  # Ajusta proporciones según convenga
  )
  
  panel_inferior_nuevo <- ggdraw() +
    draw_plot(panel_inferior, x = 0.02, y = 0.05, width = 0.97, height = 0.97)
  
  
  ######## Combinar ambos paneles (mapa a la izquierda y panel a la derecha)
  final_layout <- plot_grid(
    mapa,
    panel_inferior_nuevo,
    ncol = 1,           # Una columna: mapa arriba, panel abajo
    rel_heights = c(5, 1)  # Ajusta altura relativa según diseño
  )
  
  ######## Segmento para salvar los mapas ########
  ggsave(
    filename = paste0(salida, "mapa_", nombre_archivo, ".jpg"),
    plot = final_layout, width = 17, height = 11, units = "in", dpi = 300
  )
  cat("Mapa de", nombre_archivo, "fue creado\n")
} 
