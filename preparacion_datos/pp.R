# Script para procesar datos históricos de precipitación

# Definir la ruta base
base_path <- paste0(getwd(), "/BBDD/historico/pp")

# Listar archivos CSV específicos
archivos <- c(
  paste0(base_path, "/pp_DMC_mensual_1979_2025_2.csv"),
  paste0(base_path, "/pp_DGA_mensual_1979_2025_2.csv")
)

# Verificar si los archivos existen
archivos_existentes <- file.exists(archivos)
if (!all(archivos_existentes)) {
  stop("No se encontraron todos los archivos de precipitación necesarios.")
}

# Leer archivo de metadata
metadata_path <- paste0(getwd(), "/BBDD/historico/estaciones_DGA_DMC.csv")
if (!file.exists(metadata_path)) {
  stop("No se encontró el archivo de metadata de estaciones.")
}

metadata <- read_csv(metadata_path, col_types = cols(
  Codigo_nacional = col_character()  # Asegurar consistencia en el tipo de dato
))

# Leer y concatenar los archivos CSV
datos_combinados <- data.frame()

for (archivo in archivos) {
  print(paste("Procesando archivo:", basename(archivo)))
  
  # Leer el archivo CSV con tipos de columnas específicos
  datos <- read_csv(archivo, col_types = cols(
    Codigo_nacional = col_character(),
    pp_month = col_double(),
    Year = col_double(),
    Month = col_double()
  ))
  
  # Asegurarse de que las columnas necesarias existen
  if (!all(c("pp_month", "Year", "Month", "Codigo_nacional") %in% names(datos))) {
    warning(paste("El archivo", basename(archivo), "no contiene las columnas requeridas"))
    next
  }
  
  # Concatenar con los datos existentes
  datos_combinados <- bind_rows(datos_combinados, datos)
}

# Agrupar por año y código nacional, y calcular suma anual de precipitación
#datos_anuales <- datos_combinados %>%
#  group_by(Codigo_nacional, Year) %>%
 # summarise(
#    pp_anual = sum(pp_month, na.rm = TRUE)  # Suma de precipitación mensual
#  ) %>%
#  arrange(Codigo_nacional, Year)

# Unir con metadata
datos_anuales_con_metadata <- datos_combinados %>%
  left_join(metadata, by = "Codigo_nacional")

# Calcular máximos por comuna y año
# Ocupo el máximo porque en el caso de la precipitación hay estaciones que tienen muy mala medición, 
#y miden siempre 0,01 milimetros, esto baja el promedio y sesgaría los datos. Exploando la data nos 
#damos cuenta que generalmente las estaciones que miden la mayor cantida de precipitaciones son las 
#que están funcionando correctamente 

datos_comunales_mensuales <- datos_anuales_con_metadata %>%
  group_by(Year, Month, COD_COM, COD_REG, COD_PROV, NOM_REG, NOM_PROV, NOM_COM) %>%
  summarise(
    pp_month = median(pp_month, na.rm = TRUE)  
  ) %>%
  ungroup() %>%
  arrange(COD_COM, Year)

datos_comunales_anuales <- datos_comunales_mensuales  %>%
  group_by(Year, COD_COM, COD_REG, COD_PROV, NOM_REG, NOM_PROV, NOM_COM) %>%
  summarise(
    pp_acumulado_anual_comunal = sum(pp_month, na.rm = TRUE)  
  ) %>%
  ungroup() %>%
  arrange(COD_COM, Year)


# Guardar los resultados
output_path <- paste0(getwd(), "/BBDD/historico/pp/precipitacion_comunal.csv")
write_csv(datos_comunales_anuales, output_path)

print(paste("Datos procesados y guardados en:", output_path))
print("Resumen de los datos comunales:")
print(summary(datos_comunales))

# Mostrar información sobre la agregación
print("\nEstadísticas de la agregación:")
print(paste("Número de comunas:", n_distinct(datos_comunales$COD_COM)))
print(paste("Rango de años:", min(datos_comunales$Year), "-", max(datos_comunales$Year)))

# Mostrar estadísticas adicionales de precipitación
print("\nEstadísticas de precipitación:")
print(paste("Precipitación anual promedio:", round(mean(datos_comunales$pp_anual, na.rm = TRUE), 2), "mm"))
print(paste("Precipitación anual máxima:", round(max(datos_comunales$pp_anual, na.rm = TRUE), 2), "mm"))
print(paste("Precipitación anual mínima:", round(min(datos_comunales$pp_anual, na.rm = TRUE), 2), "mm")) 