# Script para generar fichas HTML y PNG desde un archivo Excel usando R

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
  if (grepl("mitigacion", texto_normalizado)) {
    return("mitigacion")
  } else if (grepl("adaptacion", texto_normalizado)) {
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

# Función para procesar el texto de ODS e insertar los iconos correspondientes
process_ods_text <- function(ods_text, output_dir_html) {
  if (is.na(ods_text) || ods_text == "") {
    return("")
  }
  
  # Crear directorio para las imágenes de ODS si no existe
  icons_output_dir <- file.path(output_dir_html, "ods_icons")
  if (!dir.exists(icons_output_dir)) {
    dir.create(icons_output_dir, recursive = TRUE)
  }
  
  # Separar las líneas para procesar cada ODS por separado
  # Considera varios tipos de separadores
  ods_lines <- unlist(strsplit(ods_text, "\n|\\. |, |,|;"))
  ods_lines <- trimws(ods_lines)
  ods_lines <- ods_lines[ods_lines != ""]
  
  # Iniciar contenedor de ODS
  result_html <- '<div class="ods-container">'
  
  # Patrones comunes para extraer números de ODS
  patterns <- list(
    "ODS\\s*([0-9]{1,2})",          # ODS seguido de números (ej: ODS 1, ODS1)
    "^\\s*([0-9]{1,2})\\s*$",        # Solo un número (ej: 1, 2)
    "^\\s*([0-9]{1,2})[.:]",         # Número seguido de punto o dos puntos (ej: 1., 2:)
    "objetivo\\s*([0-9]{1,2})",     # Palabra "objetivo" seguida de número (ej: Objetivo 1)
    "goal\\s*([0-9]{1,2})"          # Palabra "goal" seguida de número (ej: Goal 1) 
  )
  
  for (line in ods_lines) {
    # Inicializar como NA
    ods_number <- NA
    
    # Probar cada patrón para extraer el número de ODS
    for (pattern in patterns) {
      if (grepl(pattern, tolower(line), perl = TRUE)) {
        ods_number <- as.numeric(gsub(paste0(".*", pattern, ".*"), "\\1", tolower(line), perl = TRUE))
        break  # Si encontramos una coincidencia, salimos del bucle
      }
    }
    
    # Si después de todos los patrones no encontramos un número, buscamos cualquier número en el texto
    if (is.na(ods_number) && grepl("[0-9]+", line)) {
      # Extraer el primer número que encontremos
      ods_number <- as.numeric(gsub(".*?([0-9]+).*", "\\1", line))
    }
    
    # Si tenemos un número de ODS, procesamos
    if (!is.na(ods_number) && ods_number >= 1 && ods_number <= 17) {  # Solo ODS válidos (1-17)
      # Formatear el número de ODS como "01", "02", etc.
      icon_number <- sprintf("%02d", ods_number)
      original_icon_path <- paste0("BBDD/medidas/ODS/", icon_number, ".png")
      
      # Comprobar si el archivo de icono existe
      if (file.exists(original_icon_path)) {
        # Copiar el icono al directorio de salida
        icon_filename <- paste0(icon_number, ".png")
        output_icon_path <- file.path(icons_output_dir, icon_filename)
        file.copy(original_icon_path, output_icon_path, overwrite = TRUE)
        
        # Crear HTML con el icono + texto
        # Usar una ruta relativa al archivo HTML
        icon_html <- paste0(
          '<div class="ods-item">',
          '<img src="ods_icons/', icon_filename, '" class="ods-icon" alt="ODS ', ods_number, '">',
          '<span>', line, '</span>',
          '</div>'
        )
        result_html <- paste0(result_html, icon_html)
        cat("ODS detectado:", ods_number, "- Texto:", line, "\n")
      } else {
        # Si no se encuentra el icono, solo incluir el texto
        result_html <- paste0(result_html, '<div class="ods-item"><span>', line, '</span></div>')
        cat("Advertencia: No se encontró icono para ODS", ods_number, ":", original_icon_path, "\n")
      }
    } else if (!is.na(ods_number)) {
      # Si el número no está en el rango válido (1-17)
      result_html <- paste0(result_html, '<div class="ods-item"><span>', line, ' (Número de ODS inválido: ', ods_number, ')</span></div>')
      cat("Advertencia: Número de ODS inválido:", ods_number, "- Texto:", line, "\n")
    } else {
      # Si no se detecta un número de ODS, solo incluir el texto
      result_html <- paste0(result_html, '<div class="ods-item"><span>', line, ' (No se detectó número de ODS)</span></div>')
      cat("Advertencia: No se detectó número de ODS en:", line, "\n")
    }
  }
  
  # Cerrar contenedor de ODS
  result_html <- paste0(result_html, '</div>')
  
  return(result_html)
}

# Función para procesar la plantilla con datos de una fila
replace_template_tags <- function(template, data_row, output_dir_html) {
  result <- template
  
  # Procesar campo especial de ODS con iconos
  if ("Co-beneficios con ODS" %in% names(data_row)) {
    ods_text <- data_row[["Co-beneficios con ODS"]]
    ods_html <- process_ods_text(ods_text, output_dir_html)
    result <- gsub("${ODS_con_iconos}", ods_html, result, fixed = TRUE)
  } else {
    result <- gsub("${ODS_con_iconos}", "", result, fixed = TRUE)
  }
  
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
  
  # Procesar la línea de acción para las etiquetas
  linea_accion <- data_row[["Línea de acción"]]
  if (!is.na(linea_accion) && grepl("\\+", linea_accion)) {
    # Separar por "+" y generar etiquetas HTML
    acciones <- unlist(strsplit(linea_accion, "\\+"))
    acciones <- trimws(acciones)
    tags_html <- sapply(acciones, function(a) {
      paste0('<span class="tag">', a, '</span>')
    })
    tags_combined <- paste(tags_html, collapse = "")
    
    # Reemplazar el procesamiento especial para la línea de acción
    special_tag <- "${Línea de acción.split('+').map(item => `<span class=\"tag\">${item.trim()}</span>`).join('')}"
    result <- gsub(special_tag, tags_combined, result, fixed = TRUE)
  } else {
    # Si no hay "+", solo usar una etiqueta
    if (!is.na(linea_accion)) {
      tag_html <- paste0('<span class="tag">', linea_accion, '</span>')
      special_tag <- "${Línea de acción.split('+').map(item => `<span class=\"tag\">${item.trim()}</span>`).join('')}"
      result <- gsub(special_tag, tag_html, result, fixed = TRUE)
    } else {
      special_tag <- "${Línea de acción.split('+').map(item => `<span class=\"tag\">${item.trim()}</span>`).join('')}"
      result <- gsub(special_tag, "", result, fixed = TRUE)
    }
  }
  
  # Lista de campos específicos del Excel
  # Asegurar que estos campos existan y sean reemplazados correctamente
  campos_del_excel <- c(
    "ID", "Línea de acción", "Descripción de medida", "Alcance territorial",
    "Objetivo específico", "Amenaza Climática", "Vulnerabilidad", "Exposición", 
    "Riesgo", "Año de implementación 1", "Año de implementación 2", 
    "Año de implementación 3", "Año de implementación 4", "Año de implementación 5", "Indicador(es)", "Meta", "Medio de verificación", 
    "Área municipal responsable", "Rol del municipio", "Colaboradores", 
    "Priorización", 
    "Co-beneficios con ODS", "Potenciales barreras y obstáculos", 
    "Fuentes de financiamiento", "Justificación de la medida"
  )
  
  # Revisar cada campo y asegurar que esté correctamente reemplazado
  for (campo in campos_del_excel) {
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

# Rutas de los archivos
excel_file <- "BBDD/medidas/Medidas_PACCC_Puerto_Varas.xlsx"
template_file <- "BBDD/medidas/plantilla_ficha.html"
template_simplificado_file <- "BBDD/medidas/plantilla_ficha_simplificada.html"

# Verificar existencia de la carpeta de iconos ODS
ods_icons_dir <- "BBDD/medidas/ODS"
if (!dir.exists(ods_icons_dir)) {
  warning(paste("La carpeta de iconos ODS no existe:", ods_icons_dir))
}

# Leer el archivo Excel
data <- read_excel(excel_file)

# Verificar los nombres de columnas disponibles
print("Columnas disponibles en el archivo Excel:")
print(colnames(data))

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

# Leer las plantillas HTML
template <- readLines(template_file, encoding = "UTF-8")
template <- paste(template, collapse = "\n")

template_simplificado <- readLines(template_simplificado_file, encoding = "UTF-8")
template_simplificado <- paste(template_simplificado, collapse = "\n")

# Leer las nuevas plantillas específicas
template_mitigacion_file <- "BBDD/medidas/plantilla_mitigacion.html"
template_otras_medidas_file <- "BBDD/medidas/plantilla_otras_medidas.html"

template_mitigacion <- readLines(template_mitigacion_file, encoding = "UTF-8")
template_mitigacion <- paste(template_mitigacion, collapse = "\n")

template_otras_medidas <- readLines(template_otras_medidas_file, encoding = "UTF-8")
template_otras_medidas <- paste(template_otras_medidas, collapse = "\n")

# Crear directorios para las fichas si no existen
output_dir_html <- "BBDD/medidas/fichas/html"
output_dir_png <- "BBDD/medidas/fichas/png"
output_dir_png_simplificado <- "BBDD/medidas/fichas/png_simplificado"
output_dir_png_mitigacion <- "BBDD/medidas/fichas/png_mitigacion"
output_dir_png_otras_medidas <- "BBDD/medidas/fichas/png_otras_medidas"

if (!dir.exists(output_dir_html)) {
  dir.create(output_dir_html, recursive = TRUE)
}

if (!dir.exists(output_dir_png)) {
  dir.create(output_dir_png, recursive = TRUE)
}

if (!dir.exists(output_dir_png_simplificado)) {
  dir.create(output_dir_png_simplificado, recursive = TRUE)
}

if (!dir.exists(output_dir_png_mitigacion)) {
  dir.create(output_dir_png_mitigacion, recursive = TRUE)
}

if (!dir.exists(output_dir_png_otras_medidas)) {
  dir.create(output_dir_png_otras_medidas, recursive = TRUE)
}

# Generar una ficha HTML y PNG para cada fila
for (i in 1:nrow(data)) {
  row_data <- as.list(data[i, ])
  
  # Obtener tipo de línea de acción para logging
  if ("Línea de acción" %in% names(row_data)) {
    linea_tipo <- get_linea_accion_tipo(row_data[["Línea de acción"]])
    cat("Fila", i, "- Línea de acción tipo:", linea_tipo, "\n")
  }
  
  # Procesar ODS si existen
  if ("Co-beneficios con ODS" %in% names(row_data)) {
    ods_texto <- row_data[["Co-beneficios con ODS"]]
    if (!is.na(ods_texto) && ods_texto != "") {
      cat("Procesando ODS para fila", i, "\n")
    }
  }
  
  # Procesar la plantilla NORMAL con los datos de esta fila
  ficha_html <- replace_template_tags(template, row_data, output_dir_html)
  
  # Procesar la plantilla SIMPLIFICADA con los datos de esta fila
  ficha_html_simplificado <- replace_template_tags(template_simplificado, row_data, output_dir_html)
  
  # Determinar qué plantilla específica usar según la línea de acción
  linea_accion <- row_data[["Línea de acción"]]
  es_mitigacion <- FALSE
  
  if (!is.na(linea_accion)) {
    linea_normalizada <- normalize_text(linea_accion)
    es_mitigacion <- grepl("mitigacion", linea_normalizada)
  }
  
  # Procesar plantilla específica según el tipo
  if (es_mitigacion) {
    ficha_html_especifica <- replace_template_tags(template_mitigacion, row_data, output_dir_html)
    cat("Usando plantilla de mitigación para fila", i, "\n")
  } else {
    ficha_html_especifica <- replace_template_tags(template_otras_medidas, row_data, output_dir_html)
    cat("Usando plantilla de otras medidas para fila", i, "\n")
  }
  
  # Procesar el campo de priorización para añadir representación visual
  if ("Priorización" %in% names(row_data)) {
    prioridad_valor <- row_data[["Priorización"]]
    
    if (!is.na(prioridad_valor) && prioridad_valor != "") {
      # Normalizar el texto para comparación
      prioridad_normalizada <- normalize_text(prioridad_valor)
      
      # Determinar la clase CSS según el valor
      clase_prioridad <- ""
      if (grepl("alta|alto|elevad", prioridad_normalizada)) {
        clase_prioridad <- "prioridad-alta"
      } else if (grepl("media|medio|moderada", prioridad_normalizada)) {
        clase_prioridad <- "prioridad-moderada"
      } else if (grepl("baja|bajo", prioridad_normalizada)) {
        clase_prioridad <- "prioridad-baja"
      }
      
      if (clase_prioridad != "") {
        # Crear el HTML para la representación visual (ficha normal)
        html_barras <- paste0(
          '<div class="prioridad-contenedor ', clase_prioridad, '">',
          '<span class="texto-prioridad">', prioridad_valor, '</span>',
          '<div class="barras-contenedor">',
          '<div class="barra-prioridad"></div>',
          '<div class="barra-prioridad"></div>',
          '<div class="barra-prioridad"></div>',
          '</div>',
          '</div>'
        )
        
        # Crear el HTML para la representación visual (ficha simplificada)
        html_barras_simplificado <- paste0(
          '<div class="prioridad-contenedor ', clase_prioridad, '">',
          '<span class="texto-prioridad">', prioridad_valor, '</span>',
          '<div class="barras-contenedor">',
          '<div class="barra-prioridad"></div>',
          '<div class="barra-prioridad"></div>',
          '<div class="barra-prioridad"></div>',
          '</div>',
          '</div>'
        )
        
        # Crear el HTML para la representación visual (ficha específica)
        html_barras_especifica <- paste0(
          '<div class="prioridad-contenedor ', clase_prioridad, '">',
          '<span class="texto-prioridad">', prioridad_valor, '</span>',
          '<div class="barras-contenedor">',
          '<div class="barra-prioridad"></div>',
          '<div class="barra-prioridad"></div>',
          '<div class="barra-prioridad"></div>',
          '</div>',
          '</div>'
        )
        
        # Reemplazar el valor en el HTML (ficha normal)
        ficha_html <- gsub(
          paste0('<td class="contenido" colspan="3" id="contenedor-priorizacion">', prioridad_valor, '</td>'),
          paste0('<td class="contenido" colspan="3" id="contenedor-priorizacion">', html_barras, '</td>'),
          ficha_html,
          fixed = TRUE
        )
        
        # Reemplazar el valor en el HTML (ficha simplificada)
        ficha_html_simplificado <- gsub(
          '<div class="prioridad-contenedor" id="contenedor-priorizacion-titulo">',
          paste0('<div class="prioridad-contenedor ', clase_prioridad, '" id="contenedor-priorizacion-titulo">',
                '<span class="texto-prioridad">', prioridad_valor, '</span>',
                '<div class="barras-contenedor">',
                '<div class="barra-prioridad"></div>',
                '<div class="barra-prioridad"></div>',
                '<div class="barra-prioridad"></div>',
                '</div>'),
          ficha_html_simplificado,
          fixed = TRUE
        )
        
        # Reemplazar el valor en el HTML (ficha específica)
        ficha_html_especifica <- gsub(
          '<div class="prioridad-contenedor" id="contenedor-priorizacion-titulo">',
          paste0('<div class="prioridad-contenedor ', clase_prioridad, '" id="contenedor-priorizacion-titulo">',
                '<span class="texto-prioridad">', prioridad_valor, '</span>',
                '<div class="barras-contenedor">',
                '<div class="barra-prioridad"></div>',
                '<div class="barra-prioridad"></div>',
                '<div class="barra-prioridad"></div>',
                '</div>'),
          ficha_html_especifica,
          fixed = TRUE
        )
        
        # Cerrar el div del contenedor en las fichas
        ficha_html_simplificado <- gsub(
          '<!-- La priorización se insertará aquí via JavaScript/R -->',
          '',
          ficha_html_simplificado,
          fixed = TRUE
        )
        
        ficha_html_especifica <- gsub(
          '<!-- La priorización se insertará aquí via JavaScript/R -->',
          '',
          ficha_html_especifica,
          fixed = TRUE
        )
      }
    }
  }
  
  # Asegurar que el tema de color se aplique correctamente al contenedor (para todas las fichas)
  if ("Línea de acción" %in% names(row_data)) {
    linea_tipo <- get_linea_accion_tipo(row_data[["Línea de acción"]])
    tema_clase <- paste0("tema-", linea_tipo)
    
    # Asegurar que la clase de tema se aplique al contenedor ficha-container (ficha normal)
    ficha_html <- gsub('<div class="ficha-container">', 
                      paste0('<div class="ficha-container ', tema_clase, '">'), 
                      ficha_html)
    
    # Asegurar que la clase de tema se aplique al contenedor ficha-container (ficha simplificada)
    ficha_html_simplificado <- gsub('<div class="ficha-container">', 
                                   paste0('<div class="ficha-container ', tema_clase, '">'), 
                                   ficha_html_simplificado)
    
    # Asegurar que la clase de tema se aplique al contenedor ficha-container (ficha específica)
    ficha_html_especifica <- gsub('<div class="ficha-container">', 
                                  paste0('<div class="ficha-container ', tema_clase, '">'), 
                                  ficha_html_especifica)
  }
  
  # Determinar nombre de archivo de salida
  medida_id <- if (is.na(row_data[["ID"]])) i else row_data[["ID"]]
  output_file_html <- file.path(output_dir_html, paste0("ficha_medida_", medida_id, ".html"))
  output_file_png <- file.path(output_dir_png, paste0("ficha_medida_", medida_id, ".png"))
  output_file_html_simplificado <- file.path(output_dir_html, paste0("ficha_medida_simplificada_", medida_id, ".html"))
  output_file_png_simplificado <- file.path(output_dir_png_simplificado, paste0("ficha_medida_", medida_id, ".png"))
  
  # Archivos para las plantillas específicas
  if (es_mitigacion) {
    output_file_html_especifica <- file.path(output_dir_html, paste0("ficha_mitigacion_", medida_id, ".html"))
    output_file_png_especifica <- file.path(output_dir_png_mitigacion, paste0("ficha_medida_", medida_id, ".png"))
  } else {
    output_file_html_especifica <- file.path(output_dir_html, paste0("ficha_otras_medidas_", medida_id, ".html"))
    output_file_png_especifica <- file.path(output_dir_png_otras_medidas, paste0("ficha_medida_", medida_id, ".png"))
  }
  
  # Guardar archivo HTML normal
  writeLines(ficha_html, output_file_html, useBytes = TRUE)
  
  # Guardar archivo HTML simplificado
  writeLines(ficha_html_simplificado, output_file_html_simplificado, useBytes = TRUE)
  
  # Guardar archivo HTML específico
  writeLines(ficha_html_especifica, output_file_html_especifica, useBytes = TRUE)
  
  # Convertir HTML normal a PNG
  tryCatch({
    webshot(
      url = output_file_html,
      file = output_file_png,
      vwidth = 1200,
      vheight = 1500,
      selector = ".ficha-container",
      expand = 10,
      zoom = 1.0,
      delay = 2
    )
    cat("Ficha normal creada:", output_file_png, "\n")
  }, error = function(e) {
    cat("Error al crear PNG normal para la medida", medida_id, ":", e$message, "\n")
  })
  
  # Convertir HTML simplificado a PNG
  tryCatch({
    webshot(
      url = output_file_html_simplificado,
      file = output_file_png_simplificado,
      vwidth = 2480,
      vheight = 3508,
      selector = ".ficha-container",
      expand = 20,
      zoom = 2.0,
      delay = 2
    )
    cat("Ficha simplificada creada:", output_file_png_simplificado, "\n")
  }, error = function(e) {
    cat("Error al crear PNG simplificado para la medida", medida_id, ":", e$message, "\n")
  })
  
  # Convertir HTML específico a PNG
  tryCatch({
    webshot(
      url = output_file_html_especifica,
      file = output_file_png_especifica,
      vwidth = 2480,
      vheight = 3508,
      selector = ".ficha-container",
      expand = 20,
      zoom = 2.0,
      delay = 2
    )
    if (es_mitigacion) {
      cat("Ficha de mitigación creada:", output_file_png_especifica, "\n")
    } else {
      cat("Ficha de otras medidas creada:", output_file_png_especifica, "\n")
    }
  }, error = function(e) {
    cat("Error al crear PNG específico para la medida", medida_id, ":", e$message, "\n")
  })
}

cat("\nProceso completado. Todas las fichas han sido generadas en los directorios:\n")
cat("- HTML:", output_dir_html, "\n")
cat("- PNG:", output_dir_png, "\n")
cat("- PNG Simplificado:", output_dir_png_simplificado, "\n")
cat("- PNG Mitigación:", output_dir_png_mitigacion, "\n")
cat("- PNG Otras Medidas:", output_dir_png_otras_medidas, "\n")
cat("\nClasificación de medidas:\n")
cat("- Medidas de Mitigación: Se usó plantilla específica con justificación\n")
cat("- Otras medidas: Se usó plantilla con riesgos climáticos y justificación\n")