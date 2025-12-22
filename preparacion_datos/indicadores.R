# Script para procesar datos históricos de indicadores
# Definir la ruta del archivo
archivo_ipe <- paste0(getwd(), "/BBDD/historico/indicadores/IPE_consolidado_1979_2025_2.csv")

# Verificar si el archivo existe
if (!file.exists(archivo_ipe)) {
  stop("No se encontró el archivo de IPE.")
}

# Leer los datos
print("Leyendo archivo de IPE...")
datos_ipe <- read_csv(archivo_ipe)

# Calcular promedios anuales de los índices SPI
print("Calculando promedios anuales de índices SPI...")
datos_anuales <- datos_ipe %>%
  group_by(Codigo_nacional, Year) %>%
  summarise(
    spi_1_anual = mean(spi_1, na.rm = TRUE),
    spi_3_anual = mean(spi_3, na.rm = TRUE),
    spi_6_anual = mean(spi_6, na.rm = TRUE),
    spi_12_anual = mean(spi_12, na.rm = TRUE),
    spi_24_anual = mean(spi_24, na.rm = TRUE)
  ) %>%
  arrange(Codigo_nacional, Year)

# Leer archivo de metadata
print("Leyendo archivo de metadata de estaciones...")
metadata_path <- paste0(getwd(), "/BBDD/historico/estaciones_DGA_DMC.csv")
if (!file.exists(metadata_path)) {
  stop("No se encontró el archivo de metadata de estaciones.")
}

metadata <- read_csv(metadata_path, col_types = cols(
  Codigo_nacional = col_character()  # Asegurar consistencia en el tipo de dato
))

# Unir con metadata
print("Uniendo datos con metadata...")
datos_con_metadata <- datos_anuales %>%
  left_join(metadata, by = "Codigo_nacional")

# Calcular promedios por comuna y año
print("Calculando promedios comunales...")
datos_comunales <- datos_con_metadata %>%
  group_by(Year, COD_COM, COD_REG, COD_PROV, NOM_REG, NOM_PROV, NOM_COM) %>%
  summarise(
    spi_1_comunal = mean(spi_1_anual, na.rm = TRUE),
    spi_3_comunal = mean(spi_3_anual, na.rm = TRUE),
    spi_6_comunal = mean(spi_6_anual, na.rm = TRUE),
    spi_12_comunal = mean(spi_12_anual, na.rm = TRUE),
    spi_24_comunal = mean(spi_24_anual, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(COD_COM, Year)

# Guardar los resultados comunales
output_path_comunal <- paste0(getwd(), "/BBDD/historico/indicadores/spi_comunal.csv")
write_csv(datos_comunales, output_path_comunal)
print(paste("\nDatos comunales guardados en:", output_path_comunal))

# Mostrar resumen de los datos comunales
print("\nResumen de los datos comunales:")
print(summary(datos_comunales))

print("\nEstadísticas de la agregación comunal:")
print(paste("Número de comunas:", n_distinct(datos_comunales$COD_COM)))
print(paste("Rango de años:", min(datos_comunales$Year), "-", max(datos_comunales$Year)))

# Mostrar información sobre el join
print("\nEstadísticas del join:")
print(paste("Número de estaciones en datos SPI:", n_distinct(datos_anuales$Codigo_nacional)))
print(paste("Número de estaciones en metadata:", n_distinct(metadata$Codigo_nacional)))
print(paste("Número de estaciones después del join:", n_distinct(datos_con_metadata$Codigo_nacional)))

# Verificar estaciones sin coincidencia
estaciones_sin_metadata <- datos_anuales %>%
  anti_join(metadata, by = "Codigo_nacional") %>%
  pull(Codigo_nacional) %>%
  unique()

print("\nEstaciones sin metadata:")
print(paste("Número de estaciones sin metadata:", length(estaciones_sin_metadata)))
if (length(estaciones_sin_metadata) > 0) {
  print("Códigos de estaciones sin metadata:")
  print(estaciones_sin_metadata)
}

# Mostrar resumen de los datos procesados
print("\nResumen de los datos finales:")
print(summary(datos_con_metadata))

# Guardar los resultados
output_path <- paste0(getwd(), "/BBDD/historico/indicadores/spi_anual_con_metadata.csv")
write_csv(datos_con_metadata, output_path)
print(paste("\nDatos guardados en:", output_path))

# Mostrar información inicial sobre los datos
print("\nEstructura de los datos:")
str(datos_ipe)

print("\nResumen de los datos:")
print(summary(datos_ipe))

print("\nPrimeras filas del dataset:")
print(head(datos_ipe))

# Mostrar información sobre la cobertura temporal
print("\nCobertura temporal:")
print(paste("Rango de años:", min(datos_ipe$Year), "-", max(datos_ipe$Year)))

# Mostrar número de estaciones/códigos únicos
print("\nNúmero de estaciones:")
print(n_distinct(datos_ipe$Codigo_nacional))

# Guardar esta exploración inicial en un archivo de texto
log_path <- paste0(getwd(), "/BBDD/historico/indicadores/exploracion_inicial_ipe.txt")
sink(log_path)
cat("Exploración inicial de datos IPE\n\n")
cat("Estructura de los datos:\n")
str(datos_ipe)
cat("\nResumen estadístico:\n")
print(summary(datos_ipe))
cat("\nPrimeras filas:\n")
print(head(datos_ipe))
cat("\nCobertura temporal:", min(datos_ipe$Year), "-", max(datos_ipe$Year))
cat("\nNúmero de estaciones:", n_distinct(datos_ipe$Codigo_nacional))
sink() 