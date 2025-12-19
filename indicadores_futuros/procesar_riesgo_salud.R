# Script para procesar indicadores de riesgo de salud por calor extremo

# Cargar librerías necesarias
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

#' Función para cargar y procesar datos de riesgo de salud
#' @param ruta_archivo Ruta al archivo Excel
#' @return Lista con metadatos y datos procesados
cargar_datos_riesgo_salud <- function(ruta_archivo = NULL) {
  print("Cargando datos de riesgo de salud por calor extremo...")
  
  # Si no se proporciona ruta, usar la predeterminada
  if (is.null(ruta_archivo)) {
    ruta_archivo <- "BBDD/ARCLIM/riesgo/Salud_y_bienestar_humano/ARCLIM_aumento_mortalidad_calor_addcbit_comunas.xlsx"
  }
  
  # Cargar las hojas del archivo Excel
  tryCatch({
    metadatos <- read_excel(ruta_archivo, sheet = "METADATOS")
    datos <- read_excel(ruta_archivo, sheet = "DATOS")
    
    # Verificar que las columnas necesarias existen
    required_cols_data <- c("NOM_COMUNA", "NOM_PROVIN", "NOM_REGION")
    
    if (!all(required_cols_data %in% names(datos))) {
      stop("La hoja DATOS no contiene las columnas requeridas para información geográfica")
    }
    
    return(list(
      metadatos = metadatos,
      datos = datos
    ))
  }, error = function(e) {
    stop(paste("Error al cargar los datos:", e$message))
  })
}

#' Función para obtener comunas disponibles
#' @param datos DataFrame con datos de salud
#' @return Vector con nombres de comunas disponibles
obtener_comunas_disponibles_salud <- function(datos) {
  comunas <- unique(datos$NOM_COMUNA)
  return(sort(comunas))
}

#' Función para calcular estadísticas para una columna específica
#' @param datos DataFrame con datos
#' @param columna Nombre de la columna
#' @return Lista con estadísticas básicas
calcular_estadisticas_columna_salud <- function(datos, columna) {
  # Verificar si la columna existe
  if (!columna %in% names(datos)) {
    return(NULL)
  }
  
  # Obtener valores válidos
  valores <- datos[[columna]]
  valores_validos <- valores[!is.na(valores)]
  
  if (length(valores_validos) == 0) {
    return(NULL)
  }
  
  # Calcular estadísticas básicas
  stats <- list(
    minimo = min(valores_validos, na.rm = TRUE),
    maximo = max(valores_validos, na.rm = TRUE),
    promedio = mean(valores_validos, na.rm = TRUE),
    mediana = median(valores_validos, na.rm = TRUE),
    n_comunas = length(valores_validos)
  )
  
  return(stats)
}

#' Función para obtener valores y estadísticas para una comuna
#' @param datos DataFrame con datos
#' @param nombre_comuna Nombre de la comuna
#' @return Lista con valores y estadísticas
obtener_valores_comuna_salud <- function(datos, nombre_comuna) {
  # Filtrar datos para la comuna específica
  fila_comuna <- datos %>% 
    filter(tolower(NOM_COMUNA) == tolower(nombre_comuna))
  
  if (nrow(fila_comuna) == 0) {
    stop(paste("No se encontraron datos para la comuna:", nombre_comuna))
  }
  
  # Definir las columnas de interés
  columnas_interes <- list(
    amenaza_temp_actual = "salud_addcbit_amen",
    amenaza_cambio_temp = "salud_addcbit_amen_cambio_temp",
    exposicion_poblacion = "salud_addcbit_expo",
    sensibilidad_egresos = "salud_addcbit_sens",
    capacidad_adaptativa = "salud_addcbit_ca",
    capacidad_adaptativa_norm = "salud_addcbit_ca_norm",
    riesgo_sin_adaptacion = "salud_addcbit_riesgo_sinca",
    riesgo_con_adaptacion = "salud_addcbit_riesgoca",
    variacion_riesgo = "salud_addcbit_variacion"
  )
  
  # Obtener valores para cada columna con sus estadísticas
  obtener_valor_con_stats <- function(nombre_columna) {
    if (!nombre_columna %in% names(datos)) {
      return(list(valor = NA, stats = NULL))
    }
    
    valor <- fila_comuna[[nombre_columna]][1]
    stats <- calcular_estadisticas_columna_salud(datos, nombre_columna)
    
    return(list(
      valor = valor,
      stats = stats
    ))
  }
  
  # Construir el resultado
  resultado <- list(
    NOM_COMUNA = fila_comuna$NOM_COMUNA[1],
    NOM_PROVIN = fila_comuna$NOM_PROVIN[1],
    NOM_REGION = fila_comuna$NOM_REGION[1]
  )
  
  # Agregar cada valor con sus estadísticas
  for (nombre in names(columnas_interes)) {
    resultado[[nombre]] <- obtener_valor_con_stats(columnas_interes[[nombre]])
  }
  
  return(resultado)
}

#' Función para interpretar nivel de riesgo
#' @param valor Valor numérico del riesgo
#' @return Texto con interpretación
interpretar_nivel_riesgo_salud <- function(valor) {
  if (is.na(valor)) {
    return("No disponible")
  }
  
  if (valor <= 0.2) {
    return("Muy Bajo")
  } else if (valor <= 0.4) {
    return("Bajo")
  } else if (valor <= 0.6) {
    return("Moderado")
  } else if (valor <= 0.8) {
    return("Alto")
  } else {
    return("Muy Alto")
  }
}

#' Función para generar el informe de riesgo de salud por calor extremo
#' @param nombre_comuna Nombre de la comuna
#' @return Lista con el informe HTML y datos para visualización
generar_informe_riesgo_salud <- function(nombre_comuna) {
  print(paste0("Generando informe de riesgo de salud por calor extremo en ", nombre_comuna))
  
  # Cargar datos
  tryCatch({
    datos_cargados <- cargar_datos_riesgo_salud()
    
    # Verificar que la comuna existe en los datos
    comunas_disponibles <- obtener_comunas_disponibles_salud(datos_cargados$datos)
    if (!tolower(nombre_comuna) %in% tolower(comunas_disponibles)) {
      print("Comuna no encontrada en los datos disponibles")
      return(list(
        informe_html = paste0(
          "<h4>Informe de Riesgo de Salud por Calor Extremo - Comuna de ", nombre_comuna, "</h4>",
          "<p style='color: red;'><b>Error:</b> No se encontraron datos para la comuna especificada.</p>",
          "<p>Comunas disponibles: ", paste(comunas_disponibles[1:min(10, length(comunas_disponibles))], collapse = ", "),
          if(length(comunas_disponibles) > 10) "..." else "", "</p>"
        ),
        datos_visualizacion = NULL
      ))
    }
    
    # Obtener valores para la comuna
    print("Obteniendo valores para la comuna")
    valores <- obtener_valores_comuna_salud(datos_cargados$datos, nombre_comuna)
    
    # Función para formatear valores numéricos
    formatear_valor <- function(valor_info, decimales = 2, sufijo = "", percentil = FALSE) {
      if (is.null(valor_info) || is.na(valor_info$valor)) {
        return("No disponible")
      }
      
      valor_formateado <- round(valor_info$valor, decimales)
      
      # Formatear información comparativa si están disponibles las estadísticas
      comparacion_text <- ""
      if (!is.null(valor_info$stats)) {
        diferencia <- valor_info$valor - valor_info$stats$promedio
        comparacion <- if (diferencia > 0.001) {
          "superior"
        } else if (diferencia < -0.001) {
          "inferior"
        } else {
          "similar"
        }
        
        if (percentil) {
          # Calcular percentil aproximado
          n_comunas <- valor_info$stats$n_comunas
          rango <- ifelse(valor_info$valor <= valor_info$stats$minimo, 0,
                 ifelse(valor_info$valor >= valor_info$stats$maximo, 100,
                       round((valor_info$valor - valor_info$stats$minimo) / 
                            (valor_info$stats$maximo - valor_info$stats$minimo) * 100)))
          
          comparacion_text <- paste0(
            " (percentil ", rango, " a nivel nacional, ",
            "valor promedio nacional: ", round(valor_info$stats$promedio, decimales), sufijo, ")"
          )
        } else {
          comparacion_text <- paste0(
            " (", comparacion, " al promedio nacional de ", 
            round(valor_info$stats$promedio, decimales), sufijo, ")"
          )
        }
      }
      
      return(paste0(valor_formateado, sufijo, comparacion_text))
    }
    
    # Interpretar niveles de riesgo
    nivel_riesgo_sin_adapt <- interpretar_nivel_riesgo_salud(valores$riesgo_sin_adaptacion$valor)
    nivel_riesgo_con_adapt <- interpretar_nivel_riesgo_salud(valores$riesgo_con_adaptacion$valor)
    
    # Porcentaje de reducción
    porcentaje_reduccion <- if (!is.na(valores$variacion_riesgo$valor)) {
      abs(valores$variacion_riesgo$valor)
    } else {
      NA
    }
    
    # Sección de amenaza climática
    amenaza_climatica <- paste0(
      "<h5>Amenaza Climática: Temperatura y Cambio Proyectado</h5>",
      "<p>La temperatura máxima actual en la comuna es de <b>", formatear_valor(valores$amenaza_temp_actual, sufijo = "°C"), "</b>. ",
      "Se proyecta un cambio de temperatura para el año 2050 de <b>", formatear_valor(valores$amenaza_cambio_temp, sufijo = "°C", percentil = TRUE), "</b> ",
      "con respecto al período base. Este cambio representa la intensidad de la amenaza climática futura por calor extremo.</p>"
    )
    
    # Sección de exposición
    exposicion <- paste0(
      "<h5>Exposición: Población Proyectada</h5>",
      "<p>La población proyectada para el año 2050 en la comuna es de <b>", formatear_valor(valores$exposicion_poblacion, 0, " habitantes"), "</b>. ",
      "Este valor determina el nivel de exposición, ya que a mayor población, mayor será el número de personas potencialmente afectadas por el calor extremo.</p>"
    )
    
    # Sección de sensibilidad
    sensibilidad <- paste0(
      "<h5>Sensibilidad: Vulnerabilidad Sanitaria</h5>",
      "<p>La tasa de egresos hospitalarios por causas respiratorias y cardiovasculares en la comuna es de <b>", 
      formatear_valor(valores$sensibilidad_egresos, sufijo = " por 1000 habitantes"), "</b>. ",
      "Este indicador refleja la vulnerabilidad de la población ante eventos de calor extremo, ya que estas condiciones médicas ",
      "suelen agravarse durante las olas de calor.</p>"
    )
    
    # Sección de capacidad adaptativa
    capacidad_adaptativa <- paste0(
      "<h5>Capacidad Adaptativa: Recursos Sanitarios</h5>",
      "<p>La comuna cuenta con <b>", formatear_valor(valores$capacidad_adaptativa, sufijo = " jornadas médicas por 10,000 habitantes"), "</b>. ",
      "Este valor representa la capacidad del sistema de salud local para responder ante aumentos en la demanda de atención médica ",
      "durante eventos de calor extremo. Una mayor disponibilidad de jornadas médicas indica una mayor capacidad de respuesta ante emergencias sanitarias.</p>"
    )
    
    # Sección de índices de riesgo
    indices_riesgo <- paste0(
      "<h5>Índices de Riesgo</h5>",
      "<table style='width:100%; border-collapse: collapse; margin-bottom: 15px;'>",
      "<tr>",
      "<th style='padding: 8px; border: 1px solid #ddd; background-color: #f2f2f2; text-align: left;'>Indicador</th>",
      "<th style='padding: 8px; border: 1px solid #ddd; background-color: #f2f2f2; text-align: left;'>Valor</th>",
      "<th style='padding: 8px; border: 1px solid #ddd; background-color: #f2f2f2; text-align: left;'>Nivel</th>",
      "<th style='padding: 8px; border: 1px solid #ddd; background-color: #f2f2f2; text-align: left;'>Interpretación</th>",
      "</tr>",
      
      "<tr>",
      "<td style='padding: 5px; border: 1px solid #ddd;'><b>Riesgo sin adaptación</b></td>",
      "<td style='padding: 5px; border: 1px solid #ddd;'>", formatear_valor(valores$riesgo_sin_adaptacion), "</td>",
      "<td style='padding: 5px; border: 1px solid #ddd;'>", nivel_riesgo_sin_adapt, "</td>",
      "<td style='padding: 5px; border: 1px solid #ddd;'>Riesgo de mortalidad por calor sin considerar medidas de adaptación</td>",
      "</tr>",
      
      "<tr>",
      "<td style='padding: 5px; border: 1px solid #ddd;'><b>Riesgo con adaptación</b></td>",
      "<td style='padding: 5px; border: 1px solid #ddd;'>", formatear_valor(valores$riesgo_con_adaptacion), "</td>",
      "<td style='padding: 5px; border: 1px solid #ddd;'>", nivel_riesgo_con_adapt, "</td>",
      "<td style='padding: 5px; border: 1px solid #ddd;'>Riesgo de mortalidad por calor considerando medidas de adaptación</td>",
      "</tr>",
      
      "<tr>",
      "<td style='padding: 5px; border: 1px solid #ddd;'><b>Reducción del riesgo</b></td>",
      "<td style='padding: 5px; border: 1px solid #ddd;'>", if(!is.na(porcentaje_reduccion)) paste0(round(porcentaje_reduccion, 2), "%") else "No disponible", "</td>",
      "<td style='padding: 5px; border: 1px solid #ddd;'>", if(!is.na(porcentaje_reduccion)) 
        ifelse(porcentaje_reduccion > 30, "Significativa", 
              ifelse(porcentaje_reduccion > 10, "Moderada", "Baja")) 
      else "No disponible", "</td>",
      "<td style='padding: 5px; border: 1px solid #ddd;'>Porcentaje de reducción del riesgo al implementar medidas de adaptación</td>",
      "</tr>",
      "</table>"
    )
    
    # Sección de evaluación de riesgo
    evaluacion_riesgo <- paste0(
      "<h5>Evaluación del Riesgo</h5>",
      "<p>La comuna de <b>", valores$NOM_COMUNA, "</b> presenta un nivel de riesgo <b>", nivel_riesgo_sin_adapt, 
      "</b> de aumento en la mortalidad por calor extremo para el año 2050. ",
      if (!is.na(porcentaje_reduccion)) {
        paste0("La implementación de medidas de adaptación podría reducir este riesgo en aproximadamente un <b>", 
              round(porcentaje_reduccion, 2), "%</b>, resultando en un nivel de riesgo <b>", 
              nivel_riesgo_con_adapt, "</b>.")
      } else {
        "No se dispone de datos sobre la posible reducción del riesgo mediante medidas de adaptación."
      },
      "</p>"
    )
    
    # Sección de recomendaciones
    generar_recomendaciones <- function(nivel_riesgo, capacidad_adaptativa) {
      recomendaciones <- "<ul>"
      
      # Recomendaciones generales basadas en el nivel de riesgo
      if (nivel_riesgo %in% c("Alto", "Muy Alto")) {
        recomendaciones <- paste0(recomendaciones, 
          "<li><b>Sistemas de Alerta Temprana:</b> Implementar o mejorar los sistemas de alerta temprana para olas de calor, con comunicación efectiva a grupos vulnerables.</li>",
          "<li><b>Infraestructura Sanitaria:</b> Fortalecer la capacidad hospitalaria y de atención primaria para responder a emergencias durante períodos de calor extremo.</li>",
          "<li><b>Espacios Públicos:</b> Crear 'refugios frescos' en espacios públicos con sistemas de refrigeración para la población más vulnerable.</li>",
          "<li><b>Planificación Urbana:</b> Incrementar la cobertura vegetal urbana y superficies reflectantes para reducir las islas de calor.</li>"
        )
      } else if (nivel_riesgo == "Moderado") {
        recomendaciones <- paste0(recomendaciones, 
          "<li><b>Educación Comunitaria:</b> Desarrollar programas educativos sobre los riesgos del calor extremo y medidas preventivas.</li>",
          "<li><b>Monitoreo de Grupos Vulnerables:</b> Establecer sistemas de seguimiento para personas mayores y con condiciones médicas preexistentes.</li>",
          "<li><b>Gestión del Agua:</b> Asegurar el acceso a agua potable en espacios públicos durante períodos de calor.</li>",
          "<li><b>Adaptación de Horarios:</b> Modificar horarios de actividades al aire libre para evitar las horas de mayor temperatura.</li>"
        )
      } else {
        recomendaciones <- paste0(recomendaciones, 
          "<li><b>Monitoreo Preventivo:</b> Mantener un seguimiento de las tendencias de temperatura y establecer umbrales de alerta.</li>",
          "<li><b>Capacitación:</b> Formar al personal de salud y servicios de emergencia en la gestión de casos relacionados con el calor.</li>",
          "<li><b>Planificación a Largo Plazo:</b> Incorporar consideraciones sobre el aumento de temperatura en la planificación urbana y territorial.</li>"
        )
      }
      
      # Recomendaciones específicas basadas en la capacidad adaptativa
      if (!is.na(capacidad_adaptativa$valor) && capacidad_adaptativa$valor < capacidad_adaptativa$stats$promedio) {
        recomendaciones <- paste0(recomendaciones,
          "<li><b>Recursos Sanitarios:</b> Aumentar la disponibilidad de personal médico y recursos sanitarios, que actualmente están por debajo del promedio nacional.</li>",
          "<li><b>Telemedicina:</b> Implementar servicios de telemedicina para ampliar la cobertura sanitaria durante episodios críticos.</li>"
        )
      }
      
      recomendaciones <- paste0(recomendaciones, "</ul>")
      return(recomendaciones)
    }
    
    recomendaciones <- paste0(
      "<h5>Recomendaciones para la Gestión del Riesgo</h5>",
      "<p>Basado en el análisis de riesgo y las características específicas de la comuna, se recomienda implementar las siguientes acciones:</p>",
      generar_recomendaciones(nivel_riesgo_sin_adapt, valores$capacidad_adaptativa)
    )
    
    # Ensamblar el informe completo
    informe_html <- paste0(
      "<h4>Informe de Riesgo de Mortalidad por Calor Extremo (2050) - Comuna de ", valores$NOM_COMUNA, "</h4>",
      "<h5>Contexto</h5>",
      "<p>Este informe presenta un análisis del riesgo de mortalidad asociado a olas de calor para el año 2050, ",
      "considerando proyecciones climáticas, demografía, vulnerabilidad sanitaria y capacidad adaptativa del sistema de salud. ",
      "El aumento de las temperaturas debido al cambio climático incrementa la frecuencia e intensidad de las olas de calor, ",
      "lo que puede provocar un aumento en la mortalidad, especialmente en poblaciones vulnerables como adultos mayores y ",
      "personas con enfermedades cardiovasculares o respiratorias preexistentes.</p>",
      
      "<h5>Ubicación Geográfica</h5>",
      "<p><b>Comuna:</b> ", valores$NOM_COMUNA, "<br>",
      "<b>Provincia:</b> ", valores$NOM_PROVIN, "<br>",
      "<b>Región:</b> ", valores$NOM_REGION, "</p>",
      
      amenaza_climatica,
      exposicion,
      sensibilidad,
      capacidad_adaptativa,
      indices_riesgo,
      evaluacion_riesgo,
      recomendaciones
    )
    
    # Crear datos para visualización
    print("Creando datos para visualización")
    datos_viz <- data.frame(
      indicador = character(),
      valor = numeric(),
      nivel = character(),
      stringsAsFactors = FALSE
    )
    
    print("Estructura inicial de datos_viz:")
    print(str(datos_viz))
    
    # Añadir solo valores no NA a la visualización
    print("Añadiendo indicadores a datos_viz")
    
    # Función auxiliar para añadir datos de forma segura
    add_data_safely <- function(df, new_row) {
      tryCatch({
        # Asegurarse de que todas las columnas están presentes
        for (col in names(df)) {
          if (!(col %in% names(new_row))) {
            new_row[[col]] <- NA
          }
        }
        
        # Asegurarse de que no hay columnas extra
        new_row <- new_row[names(df)]
        
        # Combinar los datos
        result <- rbind(df, new_row)
        return(result)
      }, error = function(e) {
        print(paste("Error al añadir fila:", e$message))
        print("Estructura de df:")
        print(str(df))
        print("Estructura de new_row:")
        print(str(new_row))
        return(df)  # Devolver df sin cambios en caso de error
      })
    }
    
    if (!is.na(valores$amenaza_cambio_temp$valor)) {
      print("Añadiendo indicador: Cambio de temperatura")
      new_row <- data.frame(
        indicador = "Cambio de temperatura (°C)",
        valor = valores$amenaza_cambio_temp$valor,
        nivel = NA_character_,
        stringsAsFactors = FALSE
      )
      datos_viz <- add_data_safely(datos_viz, new_row)
    }
    
    if (!is.na(valores$sensibilidad_egresos$valor)) {
      print("Añadiendo indicador: Tasa de egresos hospitalarios")
      new_row <- data.frame(
        indicador = "Tasa de egresos hospitalarios",
        valor = valores$sensibilidad_egresos$valor,
        nivel = NA_character_,
        stringsAsFactors = FALSE
      )
      datos_viz <- add_data_safely(datos_viz, new_row)
    }
    
    if (!is.na(valores$capacidad_adaptativa_norm$valor)) {
      print("Añadiendo indicador: Capacidad adaptativa")
      new_row <- data.frame(
        indicador = "Capacidad adaptativa",
        valor = valores$capacidad_adaptativa_norm$valor,
        nivel = NA_character_,
        stringsAsFactors = FALSE
      )
      datos_viz <- add_data_safely(datos_viz, new_row)
    }
    
    if (!is.na(valores$riesgo_sin_adaptacion$valor)) {
      print("Añadiendo indicador: Riesgo sin adaptación")
      new_row <- data.frame(
        indicador = "Riesgo sin adaptación",
        valor = valores$riesgo_sin_adaptacion$valor,
        nivel = nivel_riesgo_sin_adapt,
        stringsAsFactors = FALSE
      )
      datos_viz <- add_data_safely(datos_viz, new_row)
    }
    
    if (!is.na(valores$riesgo_con_adaptacion$valor)) {
      print("Añadiendo indicador: Riesgo con adaptación")
      new_row <- data.frame(
        indicador = "Riesgo con adaptación",
        valor = valores$riesgo_con_adaptacion$valor,
        nivel = nivel_riesgo_con_adapt,
        stringsAsFactors = FALSE
      )
      datos_viz <- add_data_safely(datos_viz, new_row)
    }
    
    print("Estructura final de datos_viz:")
    print(str(datos_viz))
    
    return(list(
      informe_html = informe_html,
      datos_visualizacion = datos_viz,
      valores = valores
    ))
    
  }, error = function(e) {
    print(paste("Error en generar_informe_riesgo_salud:", e$message))
    return(list(
      informe_html = paste0(
        "<h4>Informe de Riesgo de Salud por Calor Extremo - Comuna de ", nombre_comuna, "</h4>",
        "<p style='color: red;'><b>Error:</b> ", e$message, "</p>"
      ),
      datos_visualizacion = NULL
    ))
  })
}

#' Función para generar gráfico de indicadores de riesgo de salud
#' @param datos_visualizacion DataFrame con datos para visualización
#' @return Objeto ggplot
generar_grafico_indicadores_salud <- function(datos_visualizacion) {
  # Print debugging information
  print("Debugging generar_grafico_indicadores_salud function")
  print("Input data structure:")
  print(str(datos_visualizacion))
  
  if (is.null(datos_visualizacion) || nrow(datos_visualizacion) == 0) {
    print("No data available for visualization")
    return(ggplot() + 
           annotate("text", x = 0.5, y = 0.5, label = "No hay datos suficientes para generar el gráfico") +
           theme_void() +
           xlim(0, 1) + ylim(0, 1))
  }
  
  # Ensure consistent column structure - create a standard data frame
  # with all necessary columns and copy data into it
  standard_df <- data.frame(
    indicador = character(nrow(datos_visualizacion)),
    valor = numeric(nrow(datos_visualizacion)),
    nivel = character(nrow(datos_visualizacion)),
    stringsAsFactors = FALSE
  )
  
  # Copy data we have
  standard_df$indicador <- datos_visualizacion$indicador
  standard_df$valor <- datos_visualizacion$valor
  
  # If nivel exists in the original data, copy it, otherwise leave as NA
  if ("nivel" %in% names(datos_visualizacion)) {
    standard_df$nivel <- datos_visualizacion$nivel
  }
  
  print("Standardized data frame:")
  print(str(standard_df))
  
  # Use the standardized data frame going forward
  datos_visualizacion <- standard_df
  
  # Colores para los niveles de riesgo
  colores_nivel <- c(
    "Muy Bajo" = "#1a9641",
    "Bajo" = "#a6d96a",
    "Moderado" = "#ffffbf",
    "Alto" = "#fdae61",
    "Muy Alto" = "#d7191c"
  )
  
  # Diferenciar entre indicadores de riesgo y otros indicadores
  risk_indicators <- datos_visualizacion$indicador %in% c("Riesgo sin adaptación", "Riesgo con adaptación")
  non_risk_indicators <- !risk_indicators
  
  print("Risk indicators count:", sum(risk_indicators))
  print("Non-risk indicators count:", sum(non_risk_indicators))
  
  # Initialize plot with full standardized dataset
  print("Creating base plot")
  p <- ggplot(datos_visualizacion, aes(x = reorder(indicador, valor), y = valor)) +
    coord_flip() +
    labs(
      title = "Indicadores de Riesgo de Mortalidad por Calor Extremo",
      x = "",
      y = "Valor del Indicador"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
      axis.title.y = element_text(size = 11),
      axis.text.y = element_text(size = 10),
      legend.title = element_blank()
    )
  
  # Add bars based on indicator type
  print("Adding bars to plot")
  
  # Simple approach - use a single fill aesthetic with a custom color scale
  # This avoids the "numbers of columns of arguments do not match" error
  p <- p + geom_bar(aes(fill = indicador), stat = "identity")
  
  # Add labels
  print("Adding text labels")
  p <- p + geom_text(aes(label = round(valor, 3)), hjust = -0.2, size = 3.5)
  
  # Set y-axis limits
  print("Setting axis limits")
  p <- p + scale_y_continuous(limits = c(0, max(datos_visualizacion$valor) * 1.1))
  
  # Set colors
  print("Setting color scale")
  # Use a simple color scale that works with any data
  p <- p + scale_fill_brewer(palette = "Set3")
  
  print("Returning plot")
  return(p)
}

# Ejemplo de uso
# resultado <- generar_informe_riesgo_salud("Santiago")
# print(resultado$informe_html)
# print(generar_grafico_indicadores_salud(resultado$datos_visualizacion)) 