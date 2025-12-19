# Script para generar reportes HTML de cadenas de impacto desde un archivo Excel usando R
# Basado en la estructura de generador_fichas.R
# Instalar paquetes si es necesario
if (!require("readxl")) install.packages("readxl")
if (!require("stringr")) install.packages("stringr")
if (!require("stringi")) install.packages("stringi")
if (!require("dplyr")) install.packages("dplyr")
if (!require("webshot2")) install.packages("webshot2")
if (!require("fs")) install.packages("fs")

# Cargar paquetes
library(readxl)
library(stringr)
library(stringi)
library(dplyr)
library(webshot2)
library(fs)

# Función para normalizar texto - remover acentos y convertir a minúsculas
normalize_text <- function(text) {
  if (is.na(text)) return("")
  text <- tolower(text)
  text <- stri_trans_general(text, "Latin-ASCII")
  return(text)
}

# Función para determinar el nivel de riesgo y asignar clase CSS
get_riesgo_clase <- function(riesgo_text) {
  if (is.na(riesgo_text) || riesgo_text == "") return("")
  
  # Normalizar texto para comparación
  riesgo_normalizado <- normalize_text(riesgo_text)
  
  # Determinar la clase CSS según el valor
  if (grepl("alto|elevado|critico|crítico", riesgo_normalizado)) {
    return("riesgo-alto")
  } else if (grepl("medio|moderado", riesgo_normalizado)) {
    return("riesgo-medio")
  } else if (grepl("bajo|reducido", riesgo_normalizado)) {
    return("riesgo-bajo")
  } else {
    return("")
  }
}

# Función para procesar la plantilla con datos de una fila
replace_template_tags_cadenas <- function(template, data_row) {
  result <- template
  
  # Lista de campos específicos del Excel para cadenas de impacto
  campos_cadenas <- c(
    "ID", "Cadena de Impacto", "Fuente", "Amenaza", 
    "Exposición", "Vulnerabilidad", "Riesgo", "Zonas de mayor riesgo"
  )
  
  # Reemplazar cada etiqueta con el valor correspondiente
  for (col_name in names(data_row)) {
    tag <- paste0("${", col_name, "}")
    value <- data_row[[col_name]]
    
    # Manejar valores NA
    if (is.na(value)) {
      value <- ""
    }
    
    # Reemplazo directo para la mayoría de los campos
    result <- gsub(tag, value, result, fixed = TRUE)
  }
  
  # Revisar cada campo y asegurar que esté correctamente reemplazado
  for (campo in campos_cadenas) {
    tag <- paste0("${", campo, "}")
    if (grepl(tag, result, fixed = TRUE)) {
      # Si el campo existe en data_row, usar su valor
      if (campo %in% names(data_row)) {
        value <- data_row[[campo]]
        if (is.na(value)) value <- ""
      } else {
        # Si no existe, usar cadena vacía
        value <- ""
      }
      result <- gsub(tag, value, result, fixed = TRUE)
    }
  }
  
  # Verificar si hay tags no reemplazados y eliminarlos
  pattern <- "\\$\\{[^}]+\\}"
  remaining_tags <- stringr::str_extract_all(result, pattern)[[1]]
  
  if (length(remaining_tags) > 0) {
    cat("Advertencia: Tags no reemplazados:", paste(remaining_tags, collapse=", "), "\n")
    # Reemplazar todos los tags restantes con cadena vacía
    for (tag in remaining_tags) {
      result <- gsub(tag, "", result, fixed = TRUE)
    }
  }
  
  return(result)
}

# Función para generar una ficha individual para una cadena de impacto
generate_individual_ficha <- function(template, data_row, index) {
  # Procesar la plantilla con los datos de esta fila
  ficha_html <- replace_template_tags_cadenas(template, data_row)
  
  # Generar nombre de archivo único
  id_cadena <- ifelse(is.na(data_row[["ID"]]) || data_row[["ID"]] == "", 
                      paste0("Cadena_", index), 
                      as.character(data_row[["ID"]]))
  
  # Limpiar el ID para usar como nombre de archivo
  id_limpio <- gsub("[^a-zA-Z0-9_-]", "_", id_cadena)
  
  return(list(
    html = ficha_html,
    filename = paste0("ficha_cadena_", id_limpio, ".html")
  ))
}

# Rutas de los archivos
excel_file <- "BBDD/medidas/Medidas_PACCC_Puerto_Varas.xlsx"
template_fichas_file <- "BBDD/medidas/plantilla_ficha_cadena_impacto.html"

# Verificar existencia del archivo Excel
if (!file.exists(excel_file)) {
  stop(paste("El archivo Excel no existe:", excel_file))
}

# Verificar existencia de la plantilla
if (!file.exists(template_fichas_file)) {
  stop(paste("La plantilla HTML no existe:", template_fichas_file))
}

# Leer el archivo Excel - hoja específica "Cadenas de impacto"
tryCatch({
  # Intentar leer la hoja específica
  data <- read_excel(excel_file, sheet = "Cadenas de impacto")
  cat("Hoja 'Cadenas de impacto' leída exitosamente\n")
}, error = function(e) {
  # Si falla, intentar leer la primera hoja y mostrar advertencia
  cat("Advertencia: No se pudo leer la hoja 'Cadenas de impacto'. Intentando leer la primera hoja...\n")
  data <- read_excel(excel_file, sheet = 1)
  cat("Se leyó la primera hoja del archivo. Verifica que sea la correcta.\n")
})

# Verificar los nombres de columnas disponibles
print("Columnas disponibles en el archivo Excel:")
print(colnames(data))

# Verificar que las columnas requeridas estén presentes
columnas_requeridas <- c("ID", "Cadena de Impacto", "Fuente", "Amenaza", 
                         "Exposición", "Vulnerabilidad", "Riesgo", "Zonas de mayor riesgo")

columnas_faltantes <- setdiff(columnas_requeridas, colnames(data))
if (length(columnas_faltantes) > 0) {
  warning(paste("Columnas faltantes:", paste(columnas_faltantes, collapse = ", ")))
  cat("Se continuará con las columnas disponibles, pero algunas funcionalidades pueden no funcionar correctamente.\n")
}

# Leer la plantilla HTML
template_fichas <- readLines(template_fichas_file, encoding = "UTF-8")
template_fichas <- paste(template_fichas, collapse = "\n")

# Crear directorios para los reportes si no existen
output_dir_html <- "BBDD/medidas/reportes/fichas_cadenas"
output_dir_png <- "BBDD/medidas/reportes/fichas_cadenas_png"

if (!dir.exists(output_dir_html)) {
  dir.create(output_dir_html, recursive = TRUE)
}

if (!dir.exists(output_dir_png)) {
  dir.create(output_dir_png, recursive = TRUE)
}

# Generar fichas individuales para cada cadena de impacto
cat("Generando fichas individuales de cadenas de impacto...\n")

fichas_generadas <- list()
total_fichas <- nrow(data)

for (i in 1:total_fichas) {
  row_data <- as.list(data[i, ])
  
  # Generar ficha individual
  ficha_result <- generate_individual_ficha(template_fichas, row_data, i)
  
  # Guardar archivo HTML
  output_file_html <- file.path(output_dir_html, ficha_result$filename)
  writeLines(ficha_result$html, output_file_html, useBytes = TRUE)
  
  # Convertir a PNG
  output_file_png <- file.path(output_dir_png, gsub("\\.html$", ".png", ficha_result$filename))
  
  tryCatch({
    webshot(
      url = output_file_html,
      file = output_file_png,
      vwidth = 1920,    # Ancho para mejor visualización
      vheight = 1080,   # Alto para mejor visualización
      selector = ".ficha-container", # Capturar solo el contenedor de la ficha
      expand = 20,      # Añadir margen alrededor del elemento
      zoom = 1.0,       # Sin zoom adicional
      delay = 2         # Esperar 2 segundos para que cargue el JavaScript
    )
    cat("Ficha", i, "de", total_fichas, "generada:", ficha_result$filename, "\n")
  }, error = function(e) {
    cat("Error al crear PNG de la ficha", i, ":", e$message, "\n")
    cat("La ficha HTML se generó correctamente, pero falló la conversión a PNG.\n")
  })
  
  fichas_generadas[[i]] <- list(
    html_file = output_file_html,
    png_file = output_file_png,
    id = row_data[["ID"]],
    cadena = row_data[["Cadena de Impacto"]]
  )
}

# Generar archivo índice con enlaces a todas las fichas
cat("Generando archivo índice...\n")

indice_html <- paste0(
  '<!DOCTYPE html>',
  '<html lang="es">',
  '<head>',
  '<meta charset="UTF-8">',
  '<meta name="viewport" content="width=device-width, initial-scale=1.0">',
  '<title>Índice de Fichas de Cadenas de Impacto</title>',
  '<style>',
  'body { font-family: Arial, sans-serif; margin: 20px; background-color: #f0f8ff; }',
  '.container { max-width: 1200px; margin: 0 auto; }',
  '.header { text-align: center; color: #2c3e50; margin-bottom: 30px; }',
  '.fichas-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(300px, 1fr)); gap: 20px; }',
  '.ficha-card { background: white; border-radius: 8px; padding: 20px; box-shadow: 0 4px 8px rgba(0,0,0,0.1); }',
  '.ficha-card h3 { margin-top: 0; color: #2989d8; }',
  '.ficha-card p { color: #666; margin: 5px 0; }',
  '.ficha-links { margin-top: 15px; }',
  '.ficha-links a { display: inline-block; margin-right: 10px; padding: 8px 15px; background: #2989d8; color: white; text-decoration: none; border-radius: 4px; }',
  '.ficha-links a:hover { background: #1e3c72; }',
  '.stats { background: white; border-radius: 8px; padding: 20px; margin-bottom: 30px; box-shadow: 0 4px 8px rgba(0,0,0,0.1); }',
  '.stats h2 { color: #2c3e50; margin-top: 0; }',
  '.stats-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(150px, 1fr)); gap: 15px; margin-top: 15px; }',
  '.stat-item { text-align: center; padding: 15px; background: #f8f9fa; border-radius: 6px; }',
  '.stat-numero { font-size: 24px; font-weight: bold; color: #2989d8; }',
  '.stat-etiqueta { font-size: 14px; color: #666; }',
  '</style>',
  '</head>',
  '<body>',
  '<div class="container">',
  '<div class="header">',
  '<h1>Índice de Fichas de Cadenas de Impacto</h1>',
  '<p>Total de cadenas procesadas: ', total_fichas, '</p>',
  '</div>',
  '<div class="stats">',
  '<h2>Resumen Estadístico</h2>',
  '<div class="stats-grid">',
  '<div class="stat-item">',
  '<div class="stat-numero">', total_fichas, '</div>',
  '<div class="stat-etiqueta">Total de Cadenas</div>',
  '</div>',
  '<div class="stat-item">',
  '<div class="stat-numero" id="riesgo-alto-count">-</div>',
  '<div class="stat-etiqueta">Riesgo Alto</div>',
  '</div>',
  '<div class="stat-item">',
  '<div class="stat-numero" id="riesgo-medio-count">-</div>',
  '<div class="stat-etiqueta">Riesgo Medio</div>',
  '</div>',
  '<div class="stat-item">',
  '<div class="stat-numero" id="riesgo-bajo-count">-</div>',
  '<div class="stat-etiqueta">Riesgo Bajo</div>',
  '</div>',
  '</div>',
  '</div>',
  '<div class="fichas-grid">'
)

# Agregar cada ficha al índice
for (ficha in fichas_generadas) {
  id_display <- ifelse(is.na(ficha$id) || ficha$id == "", "Sin ID", ficha$id)
  cadena_display <- ifelse(is.na(ficha$cadena) || ficha$cadena == "", "Sin descripción", ficha$cadena)
  
  # Truncar descripción si es muy larga
  if (nchar(cadena_display) > 100) {
    cadena_display <- paste0(substr(cadena_display, 1, 97), "...")
  }
  
  indice_html <- paste0(indice_html,
    '<div class="ficha-card">',
    '<h3>Cadena ', id_display, '</h3>',
    '<p><strong>Descripción:</strong> ', cadena_display, '</p>',
    '<div class="ficha-links">',
    '<a href="', basename(ficha$html_file), '" target="_blank">Ver HTML</a>',
    '<a href="', basename(ficha$png_file), '" target="_blank">Ver PNG</a>',
    '</div>',
    '</div>'
  )
}

indice_html <- paste0(indice_html,
  '</div>',
  '</div>',
  '<script>',
  '// Contar niveles de riesgo',
  'let alto = 0, medio = 0, bajo = 0;',
  'const celdasRiesgo = document.querySelectorAll(".ficha-card");',
  'celdasRiesgo.forEach(celda => {',
  '  const texto = celda.textContent.toLowerCase();',
  '  if (texto.includes("alto") || texto.includes("elevado") || texto.includes("crítico")) alto++;',
  '  else if (texto.includes("medio") || texto.includes("moderado")) medio++;',
  '  else bajo++;',
  '});',
  'document.getElementById("riesgo-alto-count").textContent = alto;',
  'document.getElementById("riesgo-medio-count").textContent = medio;',
  'document.getElementById("riesgo-bajo-count").textContent = bajo;',
  '</script>',
  '</body>',
  '</html>'
)

# Guardar archivo índice
indice_file <- file.path(output_dir_html, "indice_fichas_cadenas.html")
writeLines(indice_html, indice_file, useBytes = TRUE)

# Mostrar resumen del proceso
cat("\n=== RESUMEN DEL PROCESO ===\n")
cat("Archivo Excel procesado:", excel_file, "\n")
cat("Total de cadenas de impacto procesadas:", total_fichas, "\n")
cat("Columnas encontradas:", length(colnames(data)), "\n")
cat("Columnas requeridas:", length(columnas_requeridas), "\n")
if (length(columnas_faltantes) > 0) {
  cat("Columnas faltantes:", paste(columnas_faltantes, collapse = ", "), "\n")
}

cat("\nArchivos generados:\n")
cat("- Fichas HTML:", length(fichas_generadas), "archivos en", output_dir_html, "\n")
cat("- Fichas PNG:", length(fichas_generadas), "archivos en", output_dir_png, "\n")
cat("- Archivo índice:", indice_file, "\n")

cat("\nProceso completado exitosamente.\n")
cat("Cada cadena de impacto tiene su propia ficha individual para mejor legibilidad.\n")
cat("Puedes acceder al índice en:", indice_file, "\n") 