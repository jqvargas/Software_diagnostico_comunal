# Script para procesar datos históricos de caudal
# Cargar librerías necesarias
library(dplyr)
library(readr)

# Definir la ruta del archivo
archivo_q <- paste0(getwd(), "/BBDD/historico/q/q_DGA_mensual_1989_2025_2.csv")

# Verificar si el archivo existe
if (!file.exists(archivo_q)) {
  stop("No se encontró el archivo de caudal.")
}

# Leer archivo de metadata específico para caudal
metadata_path <- paste0(getwd(), "/BBDD/historico/q/estaciones_DGA_caudal.csv")
if (!file.exists(metadata_path)) {
  stop("No se encontró el archivo de metadata de estaciones de caudal.")
}

# Leer los datos
print("Leyendo archivo de caudal...")
datos_q <- read_csv(archivo_q, col_types = cols(
  Year = col_double(),
  Month = col_double(),
  Codigo_nacional = col_character(),
  q_month = col_double()
))

# Calcular promedios anuales y estacionales por estación
print("Calculando promedios anuales y estacionales...")
datos_anuales <- datos_q %>%
  # Crear columna de estación
  mutate(
    season = case_when(
      Month %in% c(12, 1, 2) ~ "verano",
      Month %in% c(3, 4, 5) ~ "otoño",
      Month %in% c(6, 7, 8) ~ "invierno",
      Month %in% c(9, 10, 11) ~ "primavera"
    ),
    # Ajustar el año para diciembre (pertenece al verano del año siguiente)
    Year = if_else(Month == 12, Year + 1, Year)
  ) %>%
  # Agrupar por código, año y calcular medias
  group_by(Codigo_nacional, Year) %>%
  summarise(
    q_anual = mean(q_month, na.rm = TRUE),
    q_verano = mean(q_month[season == "verano"], na.rm = TRUE),
    q_otoño = mean(q_month[season == "otoño"], na.rm = TRUE),
    q_invierno = mean(q_month[season == "invierno"], na.rm = TRUE),
    q_primavera = mean(q_month[season == "primavera"], na.rm = TRUE)
  ) %>%
  arrange(Codigo_nacional, Year)

# Leer metadata específica de caudal
print("Leyendo metadata de estaciones de caudal...")
metadata <- read_csv(metadata_path, col_types = cols(
  Codigo_nacional = col_character()
))

# Unir con metadata
print("Uniendo datos con metadata...")
datos_con_metadata <- datos_anuales %>%
  left_join(metadata, by = "Codigo_nacional")


# Mostrar resumen de los datos
print("\nResumen de los datos:")
print(summary(datos_con_metadata))

# Guardar resultados intermedios para análisis
output_path <- paste0(getwd(), "/BBDD/historico/q/caudal_estaciones.csv")
write_csv(datos_con_metadata, output_path)
print(paste("\nDatos guardados en:", output_path)) 