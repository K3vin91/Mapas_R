library(raster)
library(rayshader)
library(dplyr)
library(osmdata)
library(sf)
library(ggplot2)

# Cargar dem en forma de archivo raster 
dem_Raster <- raster("C:/Users/kevin/Documents/R_map_UTM/DEM/lago.tif")
#plot(dem_Raster)
# Coordenadas del raster inf_izq y sup_der
ext <- extent(dem_Raster)


################################################################################
## Las siguientes lineas son necesarias para la extaccion en openstreetmap

#utm_coords <- data.frame(x = c(ext[1], ext[2]), y = c(ext[3], ext[4])) # Ordena las coordenadas en una tabla (dataframe)
#utm_sf <- st_as_sf(utm_coords, coords = c("x", "y"), crs = 32616)   # Convierte las coordenadas a un objeto sf
#geo_sf <- st_transform(utm_sf, crs = 4326)   # convierte las coordenadas utm a geograficas
#df <- st_as_sf(geo_sf)  # Convierte el objeto sf a dataframe
#df_coords <- st_coordinates(df)  # Extraer las coordenadas (latitud y longitud) en columnas separadas
#geo_df <- as.data.frame(df_coords)  # Combierte al objeto matriz a dataframe

## Zona de trabajo en openstreetmap
#osm_bbox <- c(geo_df[1,2], geo_df[1,1], geo_df[2,2], geo_df[2,1])   # Extrae las coordenadas de ambos puntos

## Extraccion de carreteras
#zone_highway <- opq(osm_bbox) %>%
#  add_osm_feature(key = "highway", value = "track") %>%
#  osmdata_sf()

#lines = st_transform(zone_highway$osm_points, crs=crs(dem_Raster))

#ggplot(lines,aes(color=osm_id)) + 
#  geom_sf() +
#  theme(legend.position = "none") +
#  labs(title = "Open Street Map `highway`")
############################################################################


### En caso de no encontrarse datos en openstreetmap se usan capas locales
# Leer todos los shapefiles en la carpeta
shapefiles_dir <- "C:/Users/kevin/Documents/R_map_UTM/shapes_mapa/"  
shapefiles <- list.files(shapefiles_dir, pattern = "\\.shp$", full.names = TRUE)

# Extrae las coordenadas del extent() del raster
xmin <- ext[1]
ymin <- ext[3]
xmax <- ext[2]
ymax <- ext[4]

# Definir las otras dos esquinas para crear el poligono con la funcion matrix()
# (xmin, ymax) y (xmax, ymin)
coords <- matrix(c(xmin, ymin,     # Esquina inferior izquierda
                   xmin, ymax,     # Esquina superior izquierda
                   xmax, ymax,     # Esquina superior derecha
                   xmax, ymin,     # Esquina inferior derecha
                   xmin, ymin),    # Volver a la esquina inicial (inferior izquierda)
                 ncol = 2, byrow = TRUE)

# Convertir las coordenadas a un objeto tipo sf (polígono)
poligono_sf <- st_sfc(st_polygon(list(coords))) %>%
  st_set_crs(32616)
#plot(polygon_sf)

# Iterar sobre los shapefiles y extraer los datos dentro del polígono
for (shape in shapefiles) {
  # Leer el shapefile
  capa <- st_read(shape)
  
  # Filtrar los datos dentro del polígono
  datos_corte <- st_intersection(capa, poligono_sf)
  
  # Guardar el objeto con el nombre de la capa
  # Extraer el nombre de la capa sin la extensión .shp
  nombre_capa <- gsub(".shp$", "", basename(shape))
  
  # Asignar el objeto filtrado con el nombre de la capa
  assign(nombre_capa, datos_corte)
  
  # Imprimir el nombre de la capa y la cantidad de registros extraídos
  cat(paste("Datos extraídos de:", nombre_capa, "con", nrow(datos_corte), "registros\n"))
}


###   Aqui empieza lo bueno  ###
# Convertir de raster a matrix
dem_Mtx <- raster_to_matrix(dem_Raster)

# Redimencionar la matriz
dem_Mtx_small <- resize_matrix(dem_Mtx, 0.25)

# Mapa base para pruebas
mapa_base <- dem_Mtx_small %>% 
  sphere_shade(texture = 'imhof1') %>%
  add_shadow(lamb_shade(dem_Mtx_small, zscale=5), 0.5) %>%     # Agregar sombra lambertiana
  add_shadow(ambient_shade(dem_Mtx_small), 0.5) %>%              
  add_shadow(texture_shade(dem_Mtx_small, detail=8/10, contrast=100, brightness = 10), 0.5) %>%   # Textura de sombra
  add_overlay(generate_line_overlay(red_vial,extent = ext,
                                    heightmap = dem_Mtx_small)) %>% 
  plot_map()


# Visualizacion en 3D
dem_Mtx_small %>% 
  sphere_shade(texture = 'imhof1') %>%
  add_shadow(lamb_shade(dem_Mtx_small, zscale=5), 0.5) %>%     # Agregar sombra lambertiana
  add_shadow(ambient_shade(dem_Mtx_small), 0.5) %>%              
  add_shadow(texture_shade(dem_Mtx_small, detail=8/10, contrast=100, brightness = 10), 0.5) %>%   # Textura de sombra
  plot_3d(dem_Mtx_small, 
          zscale = 40,
          windowsize = 900)
render_clouds(dem_Mtx_small, zscale = 10, start_altitude = 500, end_altitude = 700, 
              sun_altitude = 45, attenuation_coef = 2, offset_y = 300,
              cloud_cover = 0.5, frequency = 0.05, scale_y=3, fractal_levels = 32, clear_clouds = T)





