# Script para generar un reporte integral de análisis de la base de datos de medidas
# Instalar paquetes si es necesario
if (!require("readxl")) install.packages("readxl")
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr") 
if (!require("knitr")) install.packages("knitr")
if (!require("stringi")) install.packages("stringi")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("fs")) install.packages("fs")
if (!require("rmarkdown")) install.packages("rmarkdown")

# Cargar paquetes
library(readxl)
library(dplyr)
library(stringr)
library(knitr)
library(stringi)
library(ggplot2)
library(fs)
library(rmarkdown)

# Función para normalizar texto
normalize_text <- function(text) {
  if (is.na(text) || text == "") return("")
  
  # Convertir a minúsculas
  text <- tolower(text)
  
  # Eliminar tildes y caracteres especiales
  text <- stri_trans_general(text, "Latin-ASCII")
  
  # Eliminar puntos y otros signos de puntuación
  text <- gsub("[[:punct:]]", " ", text)
  
  # Eliminar espacios múltiples y al inicio/final
  text <- gsub("\\s+", " ", text)
  text <- trimws(text)
  
  return(text)
}

# Función para extraer categorías únicas de una columna con valores separados por comas
extract_unique_categories <- function(data, column_name) {
  if (!column_name %in% names(data)) {
    return(character())
  }
  
  # Extraer todos los valores de la columna
  all_values <- c()
  
  for (i in 1:nrow(data)) {
    if (!is.na(data[[column_name]][i]) && data[[column_name]][i] != "") {
      # Dividir por comas o punto y coma y normalizar
      values <- unlist(strsplit(data[[column_name]][i], ",|;"))
      values <- trimws(values)
      values <- sapply(values, normalize_text)
      
      # Añadir a la lista de todos los valores
      all_values <- c(all_values, values)
    }
  }
  
  # Eliminar valores vacíos
  all_values <- all_values[all_values != ""]
  
  # Obtener categorías únicas
  unique_categories <- unique(all_values)
  
  return(unique_categories)
}

# Función para calcular la frecuencia de categorías en una columna
calculate_category_frequencies <- function(data, column_name) {
  # Extraer categorías únicas
  categories <- extract_unique_categories(data, column_name)
  
  if (length(categories) == 0) {
    return(data.frame(Categoría = "No disponible", 
                      Cantidad = 0, 
                      Porcentaje = 0))
  }
  
  # Crear dataframe de resultados
  results <- data.frame(
    Categoría = categories,
    Cantidad = 0,
    Porcentaje = 0,
    stringsAsFactors = FALSE
  )
  
  # Para cada categoría, contar en cuántas medidas aparece
  for (i in 1:length(categories)) {
    count <- 0
    
    # Revisar cada medida
    for (medida_idx in 1:nrow(data)) {
      if (!is.na(data[[column_name]][medida_idx]) && data[[column_name]][medida_idx] != "") {
        # Obtener valores normalizados para esta medida
        medida_values <- unlist(strsplit(data[[column_name]][medida_idx], ",|;"))
        medida_values <- trimws(medida_values)
        medida_values <- sapply(medida_values, normalize_text)
        
        # Verificar si la categoría está presente
        if (categories[i] %in% medida_values) {
          count <- count + 1
        }
      }
    }
    
    results$Cantidad[i] <- count
  }
  
  # Calcular porcentajes basados en la suma total
  total_ocurrencias <- sum(results$Cantidad)
  if (total_ocurrencias > 0) {
    results$Porcentaje <- round(results$Cantidad / total_ocurrencias * 100, 1)
  }
  
  # Ordenar por cantidad descendente
  results <- results[order(results$Cantidad, decreasing = TRUE), ]
  
  return(results)
}

# Función para convertir una lista de ejemplos en HTML
ejemplos_a_html <- function(ejemplos) {
  html <- "<ul>"
  for (ejemplo in ejemplos) {
    html <- paste0(html, '<li class="example-item">', ejemplo, "</li>")
  }
  html <- paste0(html, "</ul>")
  return(html)
}

# Función para convertir un dataframe de distribución a una tabla HTML
distribucion_a_tabla_html <- function(dist, encabezados = c("Categoría", "Cantidad", "Porcentaje (%)")) {
  html <- "<table>"
  
  # Encabezados
  html <- paste0(html, "<tr>")
  for (encabezado in encabezados) {
    html <- paste0(html, "<th>", encabezado, "</th>")
  }
  html <- paste0(html, "</tr>")
  
  # Filas de datos
  for (i in 1:nrow(dist)) {
    html <- paste0(html, "<tr>")
    for (j in 1:ncol(dist)) {
      valor <- dist[i, j]
      # Agregar el símbolo % para la columna de porcentajes
      if (j == 3) {
        valor <- paste0(valor, "%")
      }
      html <- paste0(html, "<td>", valor, "</td>")
    }
    html <- paste0(html, "</tr>")
  }
  
  html <- paste0(html, "</table>")
  return(html)
}

# Función para convertir una lista de componentes en HTML
componentes_a_html <- function(componentes, data, total_medidas) {
  html <- "<ul>"
  
  for (comp_name in names(componentes)) {
    if (comp_name %in% colnames(data)) {
      non_empty <- sum(!is.na(data[[comp_name]]) & data[[comp_name]] != "")
      perc <- round(non_empty / total_medidas * 100, 1)
      html <- paste0(html, 
                     '<li><strong>', comp_name, '</strong>: ', 
                     componentes[comp_name], 
                     ' (Presente en ', non_empty, ' medidas, ', 
                     perc, '%)</li>')
    }
  }
  
  html <- paste0(html, "</ul>")
  return(html)
}

# Función para obtener ejemplos representativos (sin duplicados y no vacíos)
get_examples <- function(data, column_name, n = 3) {
  examples <- data %>% 
    filter(!is.na(!!sym(column_name)) & !!sym(column_name) != "") %>%
    distinct(!!sym(column_name)) %>%
    sample_n(min(n, n())) %>%
    pull()
  
  return(examples)
}

# Ruta del archivo Excel
excel_file <- "BBDD/medidas/Medidas_PACCC_Puerto_Varas.xlsx"
template_file <- "BBDD/medidas/plantilla_reporte.html"

# Crear directorio para el reporte si no existe
output_dir <- "BBDD/medidas/reporte"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Leer el archivo Excel
cat("Leyendo archivo Excel...\n")
data <- read_excel(excel_file)

# Verificar si las columnas necesarias existen
required_columns <- c("Línea de acción", "Medida ID", "Descripción de medida")
missing_columns <- required_columns[!required_columns %in% colnames(data)]

if (length(missing_columns) > 0) {
  stop(paste("Faltan columnas requeridas en el Excel:", paste(missing_columns, collapse = ", ")))
}

# Reordenar las filas según las líneas de acción en el orden especificado
cat("Reordenando datos por línea de acción...\n")
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

# Imprimir columnas disponibles
cat("Columnas disponibles en el archivo Excel:\n")
print(colnames(data))

# Leer la plantilla HTML
cat("Leyendo plantilla HTML...\n")
template <- readLines(template_file, encoding = "UTF-8")
template <- paste(template, collapse = "\n")

# Total de medidas
total_medidas <- nrow(data)

# Distribución por líneas de acción
lineas_dist <- calculate_category_frequencies(data, "Línea de acción")
tabla_lineas_accion <- distribucion_a_tabla_html(lineas_dist)

# Estructura conceptual
componentes <- c(
  "Amenaza Climática" = "Amenaza que busca abordar la medida",
  "Vulnerabilidad" = "Vulnerabilidad identificada",
  "Exposición" = "Elementos expuestos a las amenazas",
  "Riesgo" = "Riesgo climático asociado",
  "Actividad 1" = "Primera actividad principal",
  "Actividad 2" = "Segunda actividad principal",
  "Actividad 3" = "Tercera actividad principal",
  "Actividad 4" = "Cuarta actividad principal",
  "Indicador(es)" = "Indicadores para medir el avance",
  "Meta" = "Metas establecidas para los indicadores"
)
estructura_conceptual <- componentes_a_html(componentes, data, total_medidas)

# Análisis de componentes climáticos
# Riesgos
if ("Riesgo" %in% colnames(data)) {
  riesgos_dist <- calculate_category_frequencies(data, "Riesgo")
  tabla_riesgos <- distribucion_a_tabla_html(riesgos_dist, 
                                        c("Categoría de Riesgo", "Cantidad", "Porcentaje (%)"))
  riesgos_ejemplos <- get_examples(data, "Riesgo")
  ejemplos_riesgos <- ejemplos_a_html(riesgos_ejemplos)
} else {
  tabla_riesgos <- "<p>No hay datos disponibles sobre riesgos.</p>"
  ejemplos_riesgos <- ""
}

# Amenazas climáticas
if ("Amenaza Climática" %in% colnames(data)) {
  amenazas_dist <- calculate_category_frequencies(data, "Amenaza Climática")
  tabla_amenazas <- distribucion_a_tabla_html(amenazas_dist, 
                                         c("Categoría de Amenaza", "Cantidad", "Porcentaje (%)"))
  amenazas_ejemplos <- get_examples(data, "Amenaza Climática")
  ejemplos_amenazas <- ejemplos_a_html(amenazas_ejemplos)
} else {
  tabla_amenazas <- "<p>No hay datos disponibles sobre amenazas climáticas.</p>"
  ejemplos_amenazas <- ""
}

# Información institucional
# Áreas municipales
if ("Área municipal responsable" %in% colnames(data)) {
  areas_dist <- calculate_category_frequencies(data, "Área municipal responsable")
  tabla_areas_municipales <- distribucion_a_tabla_html(areas_dist, 
                                                  c("Área Municipal", "Cantidad", "Porcentaje (%)"))
  areas_ejemplos <- get_examples(data, "Área municipal responsable")
  ejemplos_areas_municipales <- ejemplos_a_html(areas_ejemplos)
} else {
  tabla_areas_municipales <- "<p>No hay datos disponibles sobre áreas municipales responsables.</p>"
  ejemplos_areas_municipales <- ""
}

# Roles del municipio
if ("Rol del municipio" %in% colnames(data)) {
  roles_dist <- calculate_category_frequencies(data, "Rol del municipio")
  tabla_roles <- distribucion_a_tabla_html(roles_dist, 
                                      c("Rol del Municipio", "Cantidad", "Porcentaje (%)"))
} else {
  tabla_roles <- "<p>No hay datos disponibles sobre roles del municipio.</p>"
}

# Fuentes de financiamiento
if ("Fuentes de financiamiento" %in% colnames(data)) {
  financiamiento_dist <- calculate_category_frequencies(data, "Fuentes de financiamiento")
  tabla_financiamiento <- distribucion_a_tabla_html(financiamiento_dist, 
                                               c("Fuente de Financiamiento", "Cantidad", "Porcentaje (%)"))
  financiamiento_ejemplos <- get_examples(data, "Fuentes de financiamiento")
  ejemplos_financiamiento <- ejemplos_a_html(financiamiento_ejemplos)
} else {
  tabla_financiamiento <- "<p>No hay datos disponibles sobre fuentes de financiamiento.</p>"
  ejemplos_financiamiento <- ""
}

# Implementación y seguimiento
# Análisis de actividades
activity_columns <- c("Actividad 1", "Actividad 2", "Actividad 3", "Actividad 4")
available_activities <- activity_columns[activity_columns %in% colnames(data)]

if (length(available_activities) > 0) {
  # Contar medidas por número de actividades definidas
  act_counts <- data.frame(
    Actividades_Definidas = numeric(total_medidas),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:total_medidas) {
    count <- 0
    for (col in available_activities) {
      if (!is.na(data[[col]][i]) && data[[col]][i] != "") {
        count <- count + 1
      }
    }
    act_counts$Actividades_Definidas[i] <- count
  }
  
  act_summary <- act_counts %>%
    group_by(Actividades_Definidas) %>%
    summarize(Cantidad = n()) %>%
    mutate(Porcentaje = round(Cantidad / total_medidas * 100, 1))
  
  tabla_actividades <- distribucion_a_tabla_html(act_summary, 
                                               c("Número de Actividades", "Cantidad de Medidas", "Porcentaje (%)"))
  
  # Ejemplos representativos de actividades
  ejemplos_act_html <- ""
  for (col in available_activities[1:min(2, length(available_activities))]) {
    act_ejemplos <- get_examples(data, col)
    if (length(act_ejemplos) > 0) {
      ejemplos_act_html <- paste0(ejemplos_act_html, 
                                 '<div class="example-group">',
                                 '<div class="example-subheader">', col, ':</div>',
                                 ejemplos_a_html(act_ejemplos),
                                 '</div>')
    }
  }
  ejemplos_actividades <- ejemplos_act_html
} else {
  tabla_actividades <- "<p>No hay datos disponibles sobre actividades.</p>"
  ejemplos_actividades <- ""
}

# Indicadores
if ("Indicador(es)" %in% colnames(data)) {
  indicadores_ejemplos <- get_examples(data, "Indicador(es)", 5)
  ejemplos_indicadores <- ejemplos_a_html(indicadores_ejemplos)
} else {
  ejemplos_indicadores <- "<p>No hay datos disponibles sobre indicadores.</p>"
}

# Metas
if ("Meta" %in% colnames(data)) {
  metas_completadas <- sum(!is.na(data[["Meta"]]) & data[["Meta"]] != "")
  metas_porcentaje <- round(metas_completadas / total_medidas * 100, 1)
  
  distribucion_metas <- paste0('<div class="highlight">De las ', total_medidas, 
                               ' medidas totales, <strong>', metas_completadas, 
                               ' (', metas_porcentaje, '%)</strong> tienen metas definidas para sus indicadores.</div>')
  
  metas_ejemplos <- get_examples(data, "Meta")
  ejemplos_metas <- ejemplos_a_html(metas_ejemplos)
} else {
  distribucion_metas <- "<p>No hay datos disponibles sobre metas.</p>"
  ejemplos_metas <- ""
}

# Análisis de viabilidad
# Barreras
if ("Potenciales barreras y obstáculos" %in% colnames(data)) {
  barreras_ejemplos <- get_examples(data, "Potenciales barreras y obstáculos", 5)
  ejemplos_barreras <- ejemplos_a_html(barreras_ejemplos)
} else {
  ejemplos_barreras <- "<p>No hay datos disponibles sobre barreras y obstáculos.</p>"
}

# Priorización
if ("Priorización" %in% colnames(data)) {
  priorizacion_dist <- calculate_category_frequencies(data, "Priorización")
  tabla_priorizacion <- distribucion_a_tabla_html(priorizacion_dist, 
                                               c("Nivel de Priorización", "Cantidad", "Porcentaje (%)"))
} else {
  tabla_priorizacion <- "<p>No hay datos disponibles sobre priorización.</p>"
}

# Reemplazar placeholders en la plantilla con los datos procesados
cat("Generando reporte HTML personalizado...\n")
reporte_html <- template

# Reemplazar los marcadores en la plantilla
reporte_html <- gsub("\\$\\{FECHA\\}", format(Sys.time(), "%d de %B de %Y"), reporte_html)
reporte_html <- gsub("\\$\\{TOTAL_MEDIDAS\\}", paste0("<strong>", total_medidas, "</strong> medidas en total"), reporte_html)
reporte_html <- gsub("\\$\\{TABLA_LINEAS_ACCION\\}", tabla_lineas_accion, reporte_html)
reporte_html <- gsub("\\$\\{ESTRUCTURA_CONCEPTUAL\\}", estructura_conceptual, reporte_html)
reporte_html <- gsub("\\$\\{TABLA_RIESGOS\\}", tabla_riesgos, reporte_html)
reporte_html <- gsub("\\$\\{EJEMPLOS_RIESGOS\\}", ejemplos_riesgos, reporte_html)
reporte_html <- gsub("\\$\\{TABLA_AMENAZAS\\}", tabla_amenazas, reporte_html)
reporte_html <- gsub("\\$\\{EJEMPLOS_AMENAZAS\\}", ejemplos_amenazas, reporte_html)
reporte_html <- gsub("\\$\\{TABLA_AREAS_MUNICIPALES\\}", tabla_areas_municipales, reporte_html)
reporte_html <- gsub("\\$\\{EJEMPLOS_AREAS_MUNICIPALES\\}", ejemplos_areas_municipales, reporte_html)
reporte_html <- gsub("\\$\\{TABLA_ROLES\\}", tabla_roles, reporte_html)
reporte_html <- gsub("\\$\\{TABLA_FINANCIAMIENTO\\}", tabla_financiamiento, reporte_html)
reporte_html <- gsub("\\$\\{EJEMPLOS_FINANCIAMIENTO\\}", ejemplos_financiamiento, reporte_html)
reporte_html <- gsub("\\$\\{TABLA_ACTIVIDADES\\}", tabla_actividades, reporte_html)
reporte_html <- gsub("\\$\\{EJEMPLOS_ACTIVIDADES\\}", ejemplos_actividades, reporte_html)
reporte_html <- gsub("\\$\\{EJEMPLOS_INDICADORES\\}", ejemplos_indicadores, reporte_html)
reporte_html <- gsub("\\$\\{DISTRIBUCION_METAS\\}", distribucion_metas, reporte_html)
reporte_html <- gsub("\\$\\{EJEMPLOS_METAS\\}", ejemplos_metas, reporte_html)
reporte_html <- gsub("\\$\\{EJEMPLOS_BARRERAS\\}", ejemplos_barreras, reporte_html)
reporte_html <- gsub("\\$\\{TABLA_PRIORIZACION\\}", tabla_priorizacion, reporte_html)

# También generar la versión en Markdown para referencia
report_md_file <- file.path(output_dir, "reporte_medidas_analisis.md")
report_html_file <- file.path(output_dir, "reporte_medidas_analisis.html")
report_styled_html_file <- file.path(output_dir, "reporte_medidas_visual.html")

# Escribir el archivo HTML estilizado
writeLines(reporte_html, report_styled_html_file, useBytes = TRUE)

# También seguimos generando el reporte Markdown y HTML simple para mantener compatibilidad
con <- file(report_md_file, "w", encoding = "UTF-8")

# Escribir encabezado del reporte
writeLines("# Reporte Integral de Análisis de Medidas de Acción Climática\n", con)
writeLines(paste0("*Fecha de generación: ", format(Sys.time(), "%d de %B de %Y"), "*\n"), con)

# 1. Descripción general
writeLines("\n## 1. Descripción General\n", con)

# Total de medidas
writeLines(paste0("### Total de medidas en la base de datos: ", total_medidas, "\n"), con)

# Distribución por líneas de acción
writeLines("### Distribución por líneas de acción\n", con)
writeLines("| Línea de acción | Cantidad | Porcentaje (%) |", con)
writeLines("|----------------|----------|---------------|", con)
for (i in 1:nrow(lineas_dist)) {
  writeLines(paste0(
    "| ", lineas_dist$Categoría[i], " | ", 
    lineas_dist$Cantidad[i], " | ", 
    lineas_dist$Porcentaje[i], " |"
  ), con)
}

# Estructura conceptual
writeLines("\n### Estructura conceptual de cada medida\n", con)
writeLines("Cada medida en la base de datos está estructurada con los siguientes componentes principales:\n", con)

for (comp_name in names(componentes)) {
  if (comp_name %in% colnames(data)) {
    non_empty <- sum(!is.na(data[[comp_name]]) & data[[comp_name]] != "")
    perc <- round(non_empty / total_medidas * 100, 1)
    writeLines(paste0("- **", comp_name, "**: ", componentes[comp_name], 
                     " (Presente en ", non_empty, " medidas, ", perc, "%)"), con)
  }
}

# 2. Análisis de componentes climáticos
writeLines("\n## 2. Análisis de Componentes Climáticos\n", con)

# Riesgos más frecuentes
if ("Riesgo" %in% colnames(data)) {
  writeLines("### Riesgos más frecuentes\n", con)
  writeLines("| Categoría de Riesgo | Cantidad | Porcentaje (%) |", con)
  writeLines("|---------------------|----------|---------------|", con)
  for (i in 1:nrow(riesgos_dist)) {
    writeLines(paste0(
      "| ", riesgos_dist$Categoría[i], " | ", 
      riesgos_dist$Cantidad[i], " | ", 
      riesgos_dist$Porcentaje[i], " |"
    ), con)
  }
  
  # Ejemplos representativos
  writeLines("\n**Ejemplos representativos de riesgos:**\n", con)
  for (ejemplo in riesgos_ejemplos) {
    writeLines(paste0("- ", ejemplo), con)
  }
}

# Amenazas climáticas más comunes
if ("Amenaza Climática" %in% colnames(data)) {
  writeLines("\n### Amenazas climáticas más comunes\n", con)
  writeLines("| Categoría de Amenaza | Cantidad | Porcentaje (%) |", con)
  writeLines("|----------------------|----------|---------------|", con)
  for (i in 1:nrow(amenazas_dist)) {
    writeLines(paste0(
      "| ", amenazas_dist$Categoría[i], " | ", 
      amenazas_dist$Cantidad[i], " | ", 
      amenazas_dist$Porcentaje[i], " |"
    ), con)
  }
  
  # Ejemplos representativos
  writeLines("\n**Ejemplos representativos de amenazas climáticas:**\n", con)
  for (ejemplo in amenazas_ejemplos) {
    writeLines(paste0("- ", ejemplo), con)
  }
}

# 3. Información institucional
writeLines("\n## 3. Información Institucional\n", con)

# Direcciones municipales a cargo
if ("Área municipal responsable" %in% colnames(data)) {
  writeLines("### Direcciones municipales a cargo\n", con)
  writeLines("| Área Municipal | Cantidad | Porcentaje (%) |", con)
  writeLines("|---------------|----------|---------------|", con)
  for (i in 1:nrow(areas_dist)) {
    writeLines(paste0(
      "| ", areas_dist$Categoría[i], " | ", 
      areas_dist$Cantidad[i], " | ", 
      areas_dist$Porcentaje[i], " |"
    ), con)
  }
  
  # Ejemplos representativos
  writeLines("\n**Ejemplos representativos de áreas municipales responsables:**\n", con)
  for (ejemplo in areas_ejemplos) {
    writeLines(paste0("- ", ejemplo), con)
  }
}

# Roles del municipio
if ("Rol del municipio" %in% colnames(data)) {
  writeLines("\n### Roles del municipio por tipo de medida\n", con)
  writeLines("| Rol del Municipio | Cantidad | Porcentaje (%) |", con)
  writeLines("|-------------------|----------|---------------|", con)
  for (i in 1:nrow(roles_dist)) {
    writeLines(paste0(
      "| ", roles_dist$Categoría[i], " | ", 
      roles_dist$Cantidad[i], " | ", 
      roles_dist$Porcentaje[i], " |"
    ), con)
  }
}

# Fuentes de financiamiento
if ("Fuentes de financiamiento" %in% colnames(data)) {
  writeLines("\n### Principales fuentes de financiamiento identificadas\n", con)
  writeLines("| Fuente de Financiamiento | Cantidad | Porcentaje (%) |", con)
  writeLines("|--------------------------|----------|---------------|", con)
  for (i in 1:nrow(financiamiento_dist)) {
    writeLines(paste0(
      "| ", financiamiento_dist$Categoría[i], " | ", 
      financiamiento_dist$Cantidad[i], " | ", 
      financiamiento_dist$Porcentaje[i], " |"
    ), con)
  }
  
  # Ejemplos representativos
  writeLines("\n**Ejemplos representativos de fuentes de financiamiento:**\n", con)
  for (ejemplo in financiamiento_ejemplos) {
    writeLines(paste0("- ", ejemplo), con)
  }
}

# 4. Implementación y seguimiento
writeLines("\n## 4. Implementación y Seguimiento\n", con)

# Tipos de actividades más comunes
writeLines("### Tipos de actividades más comunes\n", con)
writeLines("A continuación se presenta un resumen de las actividades principales:\n", con)

if (length(available_activities) > 0) {
  writeLines("| Número de Actividades | Cantidad de Medidas | Porcentaje (%) |", con)
  writeLines("|----------------------|---------------------|---------------|", con)
  for (i in 1:nrow(act_summary)) {
    writeLines(paste0(
      "| ", act_summary$Actividades_Definidas[i], " | ", 
      act_summary$Cantidad[i], " | ", 
      act_summary$Porcentaje[i], " |"
    ), con)
  }
  
  # Ejemplos representativos de actividades
  writeLines("\n**Ejemplos representativos de actividades:**\n", con)
  for (col in available_activities[1:min(2, length(available_activities))]) { # Mostrar ejemplos de las primeras 2 actividades
    act_ejemplos <- get_examples(data, col)
    if (length(act_ejemplos) > 0) {
      writeLines(paste0("\n*", col, "*:"), con)
      for (ejemplo in act_ejemplos) {
        writeLines(paste0("- ", ejemplo), con)
      }
    }
  }
}

# Categorías de indicadores utilizados
if ("Indicador(es)" %in% colnames(data)) {
  writeLines("\n### Categorías de indicadores utilizados\n", con)
  writeLines("A continuación se presentan ejemplos representativos de indicadores:\n", con)
  
  for (ejemplo in indicadores_ejemplos) {
    writeLines(paste0("- ", ejemplo), con)
  }
}

# Distribución de metas establecidas
if ("Meta" %in% colnames(data)) {
  writeLines("\n### Distribución de metas establecidas\n", con)
  writeLines(paste0("De las ", total_medidas, " medidas totales, ", 
                   metas_completadas, " (", metas_porcentaje, 
                   "%) tienen metas definidas para sus indicadores.\n"), con)
  
  writeLines("**Ejemplos representativos de metas establecidas:**\n", con)
  for (ejemplo in metas_ejemplos) {
    writeLines(paste0("- ", ejemplo), con)
  }
}

# 5. Análisis de viabilidad
writeLines("\n## 5. Análisis de Viabilidad\n", con)

# Barreras más frecuentes
if ("Potenciales barreras y obstáculos" %in% colnames(data)) {
  writeLines("### Principales barreras y obstáculos identificados\n", con)
  writeLines("**Ejemplos representativos de barreras:**\n", con)
  for (ejemplo in barreras_ejemplos) {
    writeLines(paste0("- ", ejemplo), con)
  }
}

# Distribución de priorización
if ("Priorización" %in% colnames(data)) {
  writeLines("\n### Distribución de priorización\n", con)
  writeLines("| Nivel de Priorización | Cantidad | Porcentaje (%) |", con)
  writeLines("|----------------------|----------|---------------|", con)
  for (i in 1:nrow(priorizacion_dist)) {
    writeLines(paste0(
      "| ", priorizacion_dist$Categoría[i], " | ", 
      priorizacion_dist$Cantidad[i], " | ", 
      priorizacion_dist$Porcentaje[i], " |"
    ), con)
  }
}

# Cerrar conexión al archivo
close(con)

# Convertir el archivo Markdown a HTML (versión simple)
cat("Convirtiendo reporte Markdown a HTML...\n")
render(report_md_file, output_format = "html_document", output_file = report_html_file, quiet = TRUE)

cat("\nProceso completado. Reporte generado en los siguientes archivos:\n")
cat("- Markdown:", report_md_file, "\n")
cat("- HTML simple:", report_html_file, "\n")
cat("- HTML estilizado:", report_styled_html_file, "\n") 