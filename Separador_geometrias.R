library(sf)
library(dplyr)
library(janitor)  # para limpiar nombres

# Leer capa completa
munis <- st_read("Departamentos_Honduras/Departamentos_Honduras.shp")

# (Opcional) transformar a UTM si quieres luego trabajar con áreas
munis <- st_transform(munis, 32616)

# Asumimos que la columna que identifica al municipio se llama "NOMBRE"
# Si tiene otro nombre, cámbialo aquí
munis_split <- split(munis, munis$DEPTO)

# Guardar cada municipio como un objeto separado
for (nombre in names(munis_split)) {
  nombre_limpio <- make_clean_names(nombre)  # limpia espacios, tildes, símbolos
  assign(nombre_limpio, munis_split[[nombre]])
}

# Filtrar objetos sf en el entorno
nombres_sf <- Filter(function(x) inherits(get(x), "sf"), ls())

# Crear lista con esos objetos
deptos <- mget(nombres_sf)
names(deptos) <- nombres_sf

# Calcular áreas
areas <- sapply(deptos, function(x) as.numeric(st_area(x)[1]))

# Encontrar el de mayor área
nombre_max <- names(areas)[which.max(areas)]
area_max <- max(areas)

# Mostrar resultado
cat("Departamento más grande:", nombre_max, "\n")
cat("Área (m²):", area_max, "\n")


