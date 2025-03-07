# Script para procesar datos históricos de temperatura
# Cargar librerías necesarias
library(dplyr)
library(readr)

# Definir la ruta base
base_path <- paste0(getwd(), "/BBDD/historico/temp")

# Listar archivos CSV en la carpeta
archivos <- list.files(path = base_path, pattern = "*.csv", full.names = TRUE)

# Verificar si hay archivos
if (length(archivos) == 0) {
  stop("No se encontraron archivos CSV en la carpeta de temperatura.")
}

# Leer archivo de metadata
metadata_path <- paste0(getwd(), "/BBDD/historico/estaciones_DGA_DMC.csv")
if (!file.exists(metadata_path)) {
  stop("No se encontró el archivo de metadata de estaciones.")
}

metadata <- read_csv(metadata_path, col_types = cols(
  Codigo_nacional = col_character()  # Asegurar consistencia en el tipo de dato
))

# Leer y concatenar todos los archivos CSV
datos_combinados <- data.frame()

for (archivo in archivos) {
  print(paste("Procesando archivo:", basename(archivo)))
  
  # Leer el archivo CSV con tipos de columnas específicos
  datos <- read_csv(archivo, col_types = cols(
    Codigo_nacional = col_character(),  # Forzar como character
    Year = col_double(),
    Month = col_double(),
    temp_max = col_double(),
    temp_min = col_double()
  ))
  
  # Asegurarse de que las columnas necesarias existen
  if (!all(c("temp_max", "temp_min", "Year", "Month", "Codigo_nacional") %in% names(datos))) {
    warning(paste("El archivo", basename(archivo), "no contiene las columnas requeridas"))
    next
  }
  
  # Calcular la temperatura media
  datos <- datos %>%
    mutate(temp_mean = (temp_max + temp_min) / 2)
  
  # Concatenar con los datos existentes
  datos_combinados <- bind_rows(datos_combinados, datos)
}

# Agrupar por año y código nacional, y calcular promedios anuales
datos_anuales <- datos_combinados %>%
  group_by(Codigo_nacional, Year) %>%
  summarise(
    temp_max_anual = max(temp_max, na.rm = TRUE),
    temp_min_anual = min(temp_min, na.rm = TRUE),
    temp_mean_anual = mean(temp_mean, na.rm = TRUE)
  ) %>%
  arrange(Codigo_nacional, Year)

# Unir con metadata
datos_anuales_con_metadata <- datos_anuales %>%
  left_join(metadata, by = "Codigo_nacional")

# Calcular promedios por comuna y año
datos_comunales <- datos_anuales_con_metadata %>%
  group_by(Year, COD_COM, COD_REG, COD_PROV, NOM_REG, NOM_PROV, NOM_COM) %>%
  summarise(
    temp_max_anual = mean(temp_max_anual, na.rm = TRUE),
    temp_min_anual = mean(temp_min_anual, na.rm = TRUE),
    temp_mean_anual = mean(temp_mean_anual, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(COD_COM, Year)

# Guardar los resultados
output_path <- paste0(getwd(), "/BBDD/historico/temp/temperaturas_comunales.csv")
write_csv(datos_comunales, output_path)

print(paste("Datos procesados y guardados en:", output_path))
print("Resumen de los datos comunales:")
print(summary(datos_comunales))

# Mostrar información sobre la agregación
print("\nEstadísticas de la agregación:")
print(paste("Número de comunas:", n_distinct(datos_comunales$COD_COM)))
print(paste("Rango de años:", min(datos_comunales$Year), "-", max(datos_comunales$Year))) 