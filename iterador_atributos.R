library(sf)
library(dplyr)

# Leer el shapefile de entrada
shp_cuencas<- st_read("shp_cuencas/cuencas.shp")
 
# Obtener los valores únicos de la columna CUENCA
cuencas <- unique(shp_cuencas$CUENCA)

# Bucle for para filtrar y guardar shapefiles por cada valor único
for (cuenca in cuencas) {
  # Filtrar los datos según el valor 
  entidad_filtrada <- shp_cuencas %>%
    filter(CUENCA == cuenca)
  
  # Definir el nombre del archivo de salida
  archivo_salida <- paste0("shp_cuencas/cuencas_resultado/", cuenca, "_cuenca.shp")
  
  # Guardar el shapefile filtrado
  st_write(entidad_filtrada, archivo_salida)
  
  # Imprimir mensaje de progreso
  cat("Shapefile creado para la entidad:", cuenca, "\n")
}
