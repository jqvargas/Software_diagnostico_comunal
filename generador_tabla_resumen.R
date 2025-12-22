# Script para generar tabla resumen de medidas en formato HTML y PNG
# Función para normalizar texto - remover acentos y convertir a minúsculas
normalize_text <- function(text) {
  if (is.na(text)) return("")
  text <- tolower(text)
  text <- stri_trans_general(text, "Latin-ASCII")
  return(text)
}

# Función para determinar el tipo de línea de acción
get_linea_accion_tipo <- function(linea_accion) {
  if (is.na(linea_accion)) return("default")
  
  # Normalizar texto para comparación
  texto_normalizado <- normalize_text(linea_accion)
  
  # Determinar el tipo basado en palabras clave
  if (grepl("mitigacion|reduccion de emisiones|gei|eficiencia energetica", texto_normalizado)) {
    return("mitigacion")
  } else if (grepl("adaptacion|capacidad adaptativa|biodiversidad", texto_normalizado)) {
    return("adaptacion")
  } else if (grepl("integracion", texto_normalizado)) {
    return("integracion")
  } else if (grepl("medios de implementacion", texto_normalizado)) {
    return("medios")
  } else if (grepl("gobernanza", texto_normalizado)) {
    return("gobernanza")
  } else if (grepl("transicion", texto_normalizado)) {
    return("transicion")
  } else {
    return("default")
  }
}

# Rutas de los archivos
excel_file <- "BBDD/medidas/Medidas_PACCC_Puerto_Varas.xlsx"
template_file <- "BBDD/medidas/plantilla_tabla_resumen.html"

# Crear directorios para las salidas si no existen
output_dir_html <- "BBDD/medidas/tabla_resumen/html"
output_dir_png <- "BBDD/medidas/tabla_resumen/png"

if (!dir.exists(output_dir_html)) {
  dir.create(output_dir_html, recursive = TRUE)
}

if (!dir.exists(output_dir_png)) {
  dir.create(output_dir_png, recursive = TRUE)
}

# Leer el archivo Excel
data <- read_excel(excel_file)

# Verificar si las columnas necesarias existen
required_columns <- c("Línea de acción", "Medida ID", "Descripción de medida")
missing_columns <- required_columns[!required_columns %in% colnames(data)]

if (length(missing_columns) > 0) {
  stop(paste("Faltan columnas requeridas en el Excel:", paste(missing_columns, collapse = ", ")))
}

# Reordenar las filas según las líneas de acción en el orden especificado
# Función para asignar un valor numérico a cada línea de acción para ordenar
assign_order <- function(linea) {
  if (is.na(linea)) return(999) # Poner los NA al final
  
  # Normalizar texto para comparación
  texto_normalizado <- normalize_text(linea)
  
  # Asignar orden según las líneas de acción
  if (grepl("mitigacion", texto_normalizado)) {
    return(1)
  } else if (grepl("adaptacion", texto_normalizado)) {
    return(2)
  } else if (grepl("integracion", texto_normalizado)) {
    return(3)
  } else if (grepl("medios de implementacion|implementacion", texto_normalizado)) {
    return(4)
  } else if (grepl("gobernanza", texto_normalizado)) {
    return(5)
  } else if (grepl("transicion", texto_normalizado)) {
    return(6)
  } else {
    return(7) # Otras líneas de acción al final
  }
}

# Aplicar la función de ordenamiento
data$orden_linea <- sapply(data$`Línea de acción`, assign_order)
data <- data %>% arrange(orden_linea)

# Eliminar la columna de ordenamiento
data$orden_linea <- NULL

# Leer la plantilla HTML
template <- readLines(template_file, encoding = "UTF-8")
template <- paste(template, collapse = "\n")

# Ya no necesitamos el diccionario de nombres de líneas
# puesto que eliminamos esa columna de la tabla

# Procesar los datos y generar filas HTML
lineas_unicas <- unique(data$`Línea de acción`)
filas_html <- ""

for (linea in lineas_unicas) {
  if (is.na(linea)) next
  
  # Filtrar medidas por línea de acción
  medidas_linea <- data %>% filter(`Línea de acción` == linea)
  
  # Extraer la primera fila para información de la línea de acción
  primera_fila <- medidas_linea[1, ]
  
  # Nota: Ya no necesitamos el nombre completo de la línea
  # ya que eliminamos esa columna de la tabla
  
  # Determinar el tipo/categoría de la línea de acción para aplicar estilo CSS
  linea_tipo <- get_linea_accion_tipo(linea)
  linea_clase_css <- paste0("linea-", linea_tipo)  
  
  # Generar filas HTML para esta línea de acción
  num_medidas <- nrow(medidas_linea)
  
  for (i in 1:num_medidas) {
    medida <- medidas_linea[i, ]
    
    # Determinar si es la primera fila para esta línea de acción
    es_primera_fila <- (i == 1)
    
    # Generar HTML para la fila
    fila_html <- paste0(
      '<tr class="fila">',
      if (es_primera_fila) paste0('<td class="celda celda-linea ', linea_clase_css, '" rowspan="', num_medidas, '">', linea, '</td>') else "",
      '<td class="celda">', medida$`Descripción de medida`, ' (ID: ', medida$`Medida ID`, ')</td>',
      '</tr>'
    )
    
    filas_html <- paste0(filas_html, fila_html)
  }
}

# Reemplazar el marcador en la plantilla HTML
tabla_html <- gsub("<!-- Aquí se insertarán dinámicamente las filas de medidas -->", filas_html, template)

# Guardar archivo HTML
output_file_html <- file.path(output_dir_html, "tabla_resumen_medidas.html")
writeLines(tabla_html, output_file_html, useBytes = TRUE)

# Convertir HTML a PNG
tryCatch({
  webshot(
    url = output_file_html,
    file = file.path(output_dir_png, "tabla_resumen_medidas.png"),
    vwidth = 1200,    # Ancho inicial del viewport (se ajustará al contenido)
    vheight = 1600,   # Alto inicial del viewport (se ajustará al contenido)
    selector = ".tabla-container", # Capturar solo el contenedor de la tabla
    expand = 15,      # Añadir 15px de margen alrededor del elemento
    zoom = 1.0,       # Sin zoom adicional
    delay = 2         # Esperar 2 segundos para que cargue el JavaScript y aplique los estilos
  )
  cat("Tabla resumen creada en formato PNG\n")
}, error = function(e) {
  cat("Error al crear PNG para la tabla resumen:", e$message, "\n")
})

cat("\nProceso completado. Tabla resumen generada en los directorios:\n")
cat("- HTML:", output_dir_html, "\n")
cat("- PNG:", output_dir_png, "\n") 