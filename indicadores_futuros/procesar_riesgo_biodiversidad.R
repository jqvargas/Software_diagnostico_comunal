# Script para procesar indicadores de riesgo para biodiversidad

# Cargar librerías necesarias
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# Lista de tipos de análisis válidos
tipos_biodiversidad <- c(
  "flora_temperatura" = "floramat",
  "flora_precipitacion" = "floramap",
  "fauna_temperatura" = "faunamat",
  "fauna_precipitacion" = "faunamap",
  "humedales" = "humedales"
)

# Textos descriptivos para cada tipo de análisis
descripciones_tipos <- list(
  flora_temperatura = "Pérdida de flora por cambios de temperatura: Estos mapas describen los efectos adversos sobre la distribución de la biodiversidad de especies vegetales producto del cambio futuro de las condiciones de temperatura media anual en Chile continental.",
  flora_precipitacion = "Pérdida de flora por cambios de precipitación: Estos mapas describen los efectos adversos sobre la distribución de la biodiversidad de especies vegetales producto del cambio futuro de las condiciones de precipitación promedio anual en Chile continental.",
  fauna_temperatura = "Pérdida de fauna por cambios de temperatura: Estos mapas describen los efectos adversos sobre la distribución de la biodiversidad de especies animales producto del cambio futuro de las condiciones de temperatura media anual en Chile continental.",
  fauna_precipitacion = "Pérdida de fauna por cambios de precipitación: Estos mapas describen los efectos adversos sobre la distribución de la biodiversidad de especies animales producto del cambio futuro de las condiciones de precipitación promedio anual en Chile continental.",
  humedales = "Degradación de humedales costeros: Los humedales costeros son ecosistemas críticos por sus múltiples servicios ecosistémicos, pero enfrentan amenazas como la anegación permanente por aumento del nivel del mar y la alteración de la columna de agua debido a intrusión salina por marejadas."
)

#' Función para cargar y procesar datos de riesgo para biodiversidad
#' @param ruta_archivo Ruta al archivo Excel
#' @param tipo_analisis Opcional: tipo de análisis para determinar la ruta del archivo
#' @return Lista con metadatos y datos procesados
cargar_datos_riesgo_biodiversidad <- function(ruta_archivo = NULL, tipo_analisis = NULL) {
  print("Cargando datos de riesgo para biodiversidad...")
  
  # Si no se proporciona una ruta explícita pero sí un tipo de análisis, usamos la función auxiliar
  if (is.null(ruta_archivo) && !is.null(tipo_analisis)) {
    ruta_archivo <- obtener_ruta_archivo_biodiversidad(tipo_analisis)
  } else if (is.null(ruta_archivo)) {
    # Si no se proporciona ni ruta ni tipo, usamos una ruta por defecto
    ruta_archivo <- "BBDD/ARCLIM/riesgo/Biodiversidad/ARCLIM_biodiversidad_fauna_temperatura_cbit_comunas.xlsx"
  }
  
  # Intentar cargar las hojas del archivo Excel
  tryCatch({
    print(paste("Intentando cargar archivo:", ruta_archivo))
    
    # Cargar las hojas del archivo Excel
    datos <- read_excel(ruta_archivo, sheet = "DATOS")
    metadatos <- read_excel(ruta_archivo, sheet = "METADATOS")
    
    print(paste("Columnas disponibles:", paste(names(datos), collapse = ", ")))
    
    # Verificar que las columnas necesarias existen
    required_cols <- c("NOM_COMUNA", "NOM_PROVIN", "NOM_REGION")
    
    if (!all(required_cols %in% names(datos))) {
      stop("El archivo no contiene las columnas requeridas para información geográfica")
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
#' @param datos DataFrame con datos de biodiversidad
#' @return Vector con nombres de comunas disponibles
obtener_comunas_disponibles_biodiversidad <- function(datos) {
  comunas <- unique(datos$NOM_COMUNA)
  return(sort(comunas))
}

#' Función para obtener descripciones de indicadores por tipo de análisis
#' @param tipo_analisis Tipo de análisis (flora_temperatura, flora_precipitacion, etc.)
#' @return Lista con descripciones de indicadores
obtener_descripciones_biodiversidad <- function(tipo_analisis) {
  codigo_tipo <- tipos_biodiversidad[tipo_analisis]
  
  if (is.na(codigo_tipo)) {
    stop(paste("Tipo de análisis no válido:", tipo_analisis))
  }
  
  descripciones <- list(
    amenaza = paste0("Índice de amenaza basado en el cambio de condiciones climáticas (", 
                  if(grepl("temperatura", tipo_analisis)) "temperatura" else "precipitación", 
                  ") para la ", 
                  if(grepl("flora", tipo_analisis)) "flora" else "fauna",
                  " en la comuna."),
    exposicion = "Índice de exposición basado en la superficie de vegetación natural disponible.",
    sensibilidad = "Índice de sensibilidad, combinación del Margen de Seguridad y la Capacidad Adaptativa.",
    riesgo = "Índice de riesgo que combina Exposición, Sensibilidad y Amenaza.",
    capacidad_adapt = "Capacidad adaptativa: Amplitud de nicho del conjunto de especies en el área.",
    margen_seguridad = "Margen de seguridad: Distancia entre condiciones históricas y el límite superior de temperatura para las especies.",
    riqueza_especies = "Número de especies presentes en la comuna según modelos de distribución.",
    superficie_protegida = "Porcentaje de superficie con protección oficial (SNASPE).",
    riesgo_recalculado = "Índice de riesgo recalculado considerando la capacidad adaptativa de las especies."
  )
  
  return(descripciones)
}

#' Función para calcular estadísticas para una columna específica
#' @param datos DataFrame con datos
#' @param columna Nombre de la columna
#' @return Lista con estadísticas básicas
calcular_estadisticas_columna <- function(datos, columna) {
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
#' @param tipo_analisis Tipo de análisis (flora_temperatura, flora_precipitacion, etc.)
#' @return Lista con valores y estadísticas
obtener_valores_comuna_biodiversidad <- function(datos, nombre_comuna, tipo_analisis) {
  # Validar tipo de análisis
  if (!tipo_analisis %in% names(tipos_biodiversidad)) {
    stop(paste("Tipo de análisis no válido. Los tipos válidos son:", paste(names(tipos_biodiversidad), collapse = ", ")))
  }
  
  codigo_tipo <- tipos_biodiversidad[tipo_analisis]
  
  # Filtrar datos para la comuna específica
  fila_comuna <- datos %>% 
    filter(tolower(NOM_COMUNA) == tolower(nombre_comuna))
  
  if (nrow(fila_comuna) == 0) {
    stop(paste("No se encontraron datos para la comuna:", nombre_comuna))
  }
  
  # Prefijos para las columnas según el tipo de análisis
  prefijo <- paste0("biodiversidad_", codigo_tipo, "_")
  prefijo_cbit <- paste0("biodiversidad_cbit_", codigo_tipo, "_")
  
  # Obtener valores para la comuna
  obtener_valor <- function(sufijo, prefijo_alt = NULL) {
    columna <- paste0(prefijo, sufijo)
    if (!columna %in% names(datos) && !is.null(prefijo_alt)) {
      columna <- paste0(prefijo_alt, sufijo)
    }
    
    if (columna %in% names(datos)) {
      # Calcular estadísticas para todas las comunas
      stats <- calcular_estadisticas_columna(datos, columna)
      
      # Obtener valor para la comuna específica
      valor <- fila_comuna[[columna]]
      
      return(list(
        valor = valor,
        stats = stats
      ))
    } else {
      return(NULL)
    }
  }
  
  # Obtener valores para cada indicador
  valores <- list(
    amenaza = obtener_valor("amen"),
    exposicion = obtener_valor("expo"),
    sensibilidad = obtener_valor("sens"),
    riesgo = obtener_valor("riesgo"),
    capacidad_adapt = obtener_valor("capacidad_adapt"),
    margen_seguridad = obtener_valor("margen_seguridad"),
    riqueza_especies = obtener_valor("riqueza_especies"),
    superficie_protegida = obtener_valor("sup_prot"),
    riesgo_recalculado = obtener_valor("riesgo_recalculado", prefijo_cbit)
  )
  
  # Adicional: obtener variación porcentual del riesgo si está disponible
  if (tipo_analisis %in% c("fauna_temperatura", "fauna_precipitacion")) {
    columna_variacion <- paste0("biodiversidad_cbit_variacion_", codigo_tipo)
    if (columna_variacion %in% names(datos)) {
      valores$variacion_riesgo <- list(
        valor = fila_comuna[[columna_variacion]],
        stats = calcular_estadisticas_columna(datos, columna_variacion)
      )
    }
  }
  
  return(valores)
}

#' Función para interpretar nivel de riesgo
#' @param valor Valor numérico del riesgo
#' @return Texto con interpretación
interpretar_nivel_riesgo <- function(valor) {
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

#' Función para generar recomendación basada en el nivel de riesgo
#' @param valor_riesgo Valor numérico del riesgo
#' @param tipo_analisis Tipo de análisis
#' @return Texto con recomendación
generar_recomendacion_biodiversidad <- function(valor_riesgo, tipo_analisis) {
  if (is.na(valor_riesgo)) {
    return("No se pueden generar recomendaciones sin información sobre el nivel de riesgo.")
  }
  
  nivel_texto <- interpretar_nivel_riesgo(valor_riesgo)
  es_flora <- grepl("flora", tipo_analisis)
  es_temperatura <- grepl("temperatura", tipo_analisis)
  
  recomendacion <- "Se recomienda: "
  
  if (nivel_texto %in% c("Alto", "Muy Alto")) {
    if (es_flora) {
      recomendacion <- paste0(recomendacion, 
                           "Implementar medidas urgentes de conservación y aumentar las áreas protegidas para las especies vegetales. ",
                           if(es_temperatura) 
                             "Priorizar la protección de hábitats en gradientes altitudinales que permitan la migración vertical de especies ante el aumento de temperaturas."
                           else
                             "Fomentar la conservación de cuencas hidrográficas y sistemas que mantengan la humedad del suelo ante cambios en los patrones de precipitación.")
    } else {
      recomendacion <- paste0(recomendacion, 
                           "Establecer corredores biológicos y aumentar la conectividad entre áreas protegidas para facilitar el desplazamiento de especies animales. ",
                           if(es_temperatura)
                             "Implementar zonas de amortiguamiento térmico y refugios climáticos para especies sensibles al aumento de temperatura."
                           else
                             "Asegurar disponibilidad de cuerpos de agua permanentes y hábitats que provean de recursos hídricos durante períodos prolongados de sequía.")
    }
  } else if (nivel_texto == "Moderado") {
    if (es_flora) {
      recomendacion <- paste0(recomendacion, 
                           "Monitorear cambios en la distribución de especies vegetales y fortalecer las áreas protegidas existentes. ",
                           "Desarrollar programas de restauración ecológica con especies nativas adaptadas a las condiciones climáticas proyectadas.")
    } else {
      recomendacion <- paste0(recomendacion, 
                           "Implementar programas de monitoreo de fauna en riesgo y reducir otras presiones no climáticas como la fragmentación del hábitat. ",
                           "Fomentar prácticas de manejo del territorio que favorezcan la biodiversidad animal.")
    }
  } else {
    recomendacion <- paste0(recomendacion, 
                         "Mantener un monitoreo preventivo de los ecosistemas y especies. ",
                         "Aunque el riesgo actual es bajo, es importante planificar estrategias de adaptación para escenarios futuros más adversos.")
  }
  
  return(recomendacion)
}

#' Función para generar el informe de riesgo para biodiversidad
#' @param nombre_comuna Nombre de la comuna
#' @param tipo_analisis Tipo de análisis (flora_temperatura, flora_precipitacion, etc.)
#' @return Lista con el informe HTML y datos para visualización
generar_informe_riesgo_biodiversidad <- function(nombre_comuna, tipo_analisis) {
  print(paste0("Generando informe de riesgo para biodiversidad (", tipo_analisis, ") en ", nombre_comuna))
  
  # Caso especial para humedales costeros
  if (tipo_analisis == "humedales") {
    return(generar_informe_humedales_costeros(nombre_comuna))
  }
  
  # Cargar datos
  datos_cargados <- cargar_datos_riesgo_biodiversidad(tipo_analisis = tipo_analisis)
  
  # Verificar que la comuna existe en los datos
  comunas_disponibles <- obtener_comunas_disponibles_biodiversidad(datos_cargados$datos)
  if (!tolower(nombre_comuna) %in% tolower(comunas_disponibles)) {
    return(list(
      informe_html = paste0(
        "<h4>Informe de Riesgo para Biodiversidad - Comuna de ", nombre_comuna, "</h4>",
        "<p style='color: red;'><b>Error:</b> No se encontraron datos para la comuna especificada.</p>",
        "<p>Comunas disponibles: ", paste(comunas_disponibles[1:min(10, length(comunas_disponibles))], collapse = ", "),
        if(length(comunas_disponibles) > 10) "..." else "", "</p>"
      ),
      datos_visualizacion = NULL
    ))
  }
  
  # Obtener descripciones y valores para la comuna
  tryCatch({
    descripciones <- obtener_descripciones_biodiversidad(tipo_analisis)
    valores <- obtener_valores_comuna_biodiversidad(datos_cargados$datos, nombre_comuna, tipo_analisis)
    
    # Obtener información geográfica de la comuna
    fila_comuna <- datos_cargados$datos %>% 
      filter(tolower(NOM_COMUNA) == tolower(nombre_comuna))
    provincia <- fila_comuna$NOM_PROVIN[1]
    region <- fila_comuna$NOM_REGION[1]
    
    # Función para formatear sección del informe con valor y estadísticas
    formatear_seccion <- function(nombre, valor_info, descripcion, sufijo = "") {
      if (is.null(valor_info)) {
        return("")
      }
      
      if (is.na(valor_info$valor)) {
        return(paste0(
          "<p><b>", str_to_title(nombre), ":</b> ",
          "No se encontraron datos de ", tolower(nombre), " para la comuna ",
          "</p>"
        ))
      }
      
      valor_formateado <- round(valor_info$valor, 3)
      
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
        
        percentil <- round(sum(datos_cargados$datos[[paste0("biodiversidad_", tipos_biodiversidad[tipo_analisis], "_", ifelse(nombre == "riesgo_recalculado", "riesgo_recalculado", nombre))]] <= valor_info$valor, na.rm = TRUE) / 
                         sum(!is.na(datos_cargados$datos[[paste0("biodiversidad_", tipos_biodiversidad[tipo_analisis], "_", ifelse(nombre == "riesgo_recalculado", "riesgo_recalculado", nombre))]])) * 100)
        
        comparacion_text <- paste0(
          " Este valor es ", comparacion, " al promedio nacional de ", 
          round(valor_info$stats$promedio, 3), sufijo,
          " (percentil ", percentil, " a nivel nacional)."
        )
      }
      
      # Agregar interpretación especial para el riesgo
      interpretacion <- ""
      if (nombre %in% c("riesgo", "riesgo_recalculado")) {
        nivel_riesgo <- interpretar_nivel_riesgo(valor_info$valor)
        interpretacion <- paste0(" Esto corresponde a un nivel de riesgo <b>", nivel_riesgo, "</b>.")
      }
      
      # Formatear texto final
      texto <- paste0(
        "<p><b>", str_to_title(gsub("_", " ", nombre)), ":</b> ",
        valor_formateado, sufijo, ".", 
        interpretacion,
        comparacion_text,
        "</p>",
        "<p><i>", descripcion, "</i></p>"
      )
      
      return(texto)
    }
    
    # Determinar tipo de biodiversidad y factor climático para el título
    tipo_biodiversidad <- if(grepl("flora", tipo_analisis)) "Flora" else "Fauna"
    factor_climatico <- if(grepl("temperatura", tipo_analisis)) "Temperatura" else "Precipitación"
    
    # Generar recomendación basada en el riesgo
    recomendacion <- generar_recomendacion_biodiversidad(
      ifelse(!is.null(valores$riesgo_recalculado), valores$riesgo_recalculado$valor, valores$riesgo$valor),
      tipo_analisis
    )
    
    # Crear el informe en formato HTML
    informe_html <- paste0(
      "<h4>Informe de Riesgo por Pérdida de Biodiversidad - Comuna de ", nombre_comuna, "</h4>",
      
      "<h5>Descripción de los Indicadores</h5>",
      "<p>Este informe presenta indicadores de riesgo para la biodiversidad en Chile continental debido a los efectos del cambio climático. Los indicadores se clasifican según el tipo de biodiversidad (flora o fauna) y el factor climático (cambios en temperatura o precipitación).</p>",
      
      "<p><b>", descripciones_tipos[[tipo_analisis]], "</b> La información se presenta a nivel comunal.</p>",
      
      "<h5>Ubicación Geográfica</h5>",
      "<p><b>Comuna:</b> ", nombre_comuna, "<br>",
      "<b>Provincia:</b> ", provincia, "<br>",
      "<b>Región:</b> ", region, "</p>",
      
      "<h5>Indicadores de Biodiversidad para ", tipo_biodiversidad, " - Factor: ", factor_climatico, "</h5>",
      
      formatear_seccion("riqueza_especies", valores$riqueza_especies, descripciones$riqueza_especies, " especies"),
      formatear_seccion("superficie_protegida", valores$superficie_protegida, descripciones$superficie_protegida, "%"),
      
      "<h5>Indicadores de Vulnerabilidad y Riesgo</h5>",
      formatear_seccion("amenaza", valores$amenaza, descripciones$amenaza),
      formatear_seccion("exposicion", valores$exposicion, descripciones$exposicion),
      formatear_seccion("sensibilidad", valores$sensibilidad, descripciones$sensibilidad),
      formatear_seccion("margen_seguridad", valores$margen_seguridad, descripciones$margen_seguridad, "°C"),
      formatear_seccion("capacidad_adapt", valores$capacidad_adapt, descripciones$capacidad_adapt),
      
      "<h5>Índices de Riesgo</h5>",
      formatear_seccion("riesgo", valores$riesgo, descripciones$riesgo),
      formatear_seccion("riesgo_recalculado", valores$riesgo_recalculado, descripciones$riesgo_recalculado),
      
      if (!is.null(valores$variacion_riesgo)) {
        paste0(
          "<p><b>Variación por Capacidad Adaptativa:</b> ", 
          round(valores$variacion_riesgo$valor, 2), 
          "%. Esta variación representa la reducción en el riesgo al considerar la capacidad adaptativa de las especies.</p>"
        )
      } else {
        ""
      },
      
      "<h5>Recomendaciones para la Gestión</h5>",
      "<p>", recomendacion, "</p>"
    )
    
    # Crear datos para visualización (solo incluir valores no NA)
    indicadores <- c()
    valores_viz <- c()
    nombres_viz <- c()
    
    agregar_indicador <- function(nombre, valor_info, nombre_display = NULL) {
      if (!is.null(valor_info) && !is.na(valor_info$valor)) {
        indicadores <<- c(indicadores, ifelse(is.null(nombre_display), str_to_title(gsub("_", " ", nombre)), nombre_display))
        valores_viz <<- c(valores_viz, valor_info$valor)
      }
    }
    
    agregar_indicador("riesgo", valores$riesgo, "Riesgo Base")
    agregar_indicador("riesgo_recalculado", valores$riesgo_recalculado, "Riesgo Ajustado")
    agregar_indicador("amenaza", valores$amenaza)
    agregar_indicador("exposicion", valores$exposicion)
    agregar_indicador("sensibilidad", valores$sensibilidad)
    
    datos_visualizacion <- data.frame(
      indicador = indicadores,
      valor = valores_viz,
      stringsAsFactors = FALSE
    )
    
    # Añadir interpretación de nivel para indicadores de riesgo
    if ("Riesgo Base" %in% datos_visualizacion$indicador) {
      indice <- which(datos_visualizacion$indicador == "Riesgo Base")
      datos_visualizacion$nivel[indice] <- interpretar_nivel_riesgo(datos_visualizacion$valor[indice])
    }
    
    if ("Riesgo Ajustado" %in% datos_visualizacion$indicador) {
      indice <- which(datos_visualizacion$indicador == "Riesgo Ajustado")
      datos_visualizacion$nivel[indice] <- interpretar_nivel_riesgo(datos_visualizacion$valor[indice])
    }
    
    return(list(
      informe_html = informe_html,
      datos_visualizacion = datos_visualizacion,
      tipo_analisis = tipo_analisis,
      valores = valores
    ))
  }, error = function(e) {
    return(list(
      informe_html = paste0(
        "<h4>Informe de Riesgo para Biodiversidad - Comuna de ", nombre_comuna, "</h4>",
        "<p style='color: red;'><b>Error:</b> ", e$message, "</p>"
      ),
      datos_visualizacion = NULL
    ))
  })
}

#' Función para generar gráfico de indicadores
#' @param datos_visualizacion DataFrame con datos para visualización
#' @param tipo_analisis Tipo de análisis
#' @return Objeto ggplot
generar_grafico_indicadores_biodiversidad <- function(datos_visualizacion, tipo_analisis) {
  if (is.null(datos_visualizacion) || nrow(datos_visualizacion) == 0) {
    return(ggplot() + 
           annotate("text", x = 0.5, y = 0.5, label = "No hay datos suficientes para generar el gráfico") +
           theme_void() +
           xlim(0, 1) + ylim(0, 1))
  }
  
  # Colores según tipo de biodiversidad
  colores_base <- if(grepl("flora", tipo_analisis)) {
    "#1b9e77"  # Verde para flora
  } else if(grepl("fauna", tipo_analisis)) {
    "#d95f02"  # Naranja para fauna
  } else if(tipo_analisis == "humedales") {
    "#0072B2"  # Azul para humedales
  } else {
    "#000000"  # Negro como fallback
  }
  
  # Colores para niveles de riesgo
  colores_nivel <- c(
    "Muy Bajo" = "#1a9641",
    "Bajo" = "#a6d96a",
    "Moderado" = "#ffffbf",
    "Alto" = "#fdae61",
    "Muy Alto" = "#d7191c"
  )
  
  # Determinar título
  if (tipo_analisis == "humedales") {
    titulo <- "Indicadores de Riesgo para Humedales Costeros"
  } else {
    titulo <- paste0("Indicadores de Riesgo para Biodiversidad - ", 
                   if(grepl("flora", tipo_analisis)) "Flora" else "Fauna",
                   " (", 
                   if(grepl("temperatura", tipo_analisis)) "Temperatura" else "Precipitación",
                   ")")
  }
  
  # Generar gráfico de barras
  p <- ggplot(datos_visualizacion, aes(x = reorder(indicador, valor), y = valor)) +
    geom_bar(stat = "identity", aes(fill = if("nivel" %in% names(datos_visualizacion)) nivel else indicador)) +
    geom_text(aes(label = round(valor, 3)), hjust = -0.2, size = 3.5) +
    scale_y_continuous(limits = c(0, max(datos_visualizacion$valor) * 1.1)) +
    coord_flip() +
    labs(
      title = titulo,
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
  
  # Aplicar color según presencia de nivel de riesgo
  if ("nivel" %in% names(datos_visualizacion)) {
    p <- p + scale_fill_manual(values = colores_nivel)
  } else {
    p <- p + scale_fill_manual(values = rep(colores_base, nrow(datos_visualizacion)))
  }
  
  return(p)
}

# Ejemplo de uso
# resultado <- generar_informe_riesgo_biodiversidad("Santiago", "flora_temperatura")
# print(resultado$informe_html)
# print(generar_grafico_indicadores_biodiversidad(resultado$datos_visualizacion, resultado$tipo_analisis)) 

# Función adicional para obtener la ruta del archivo según el tipo de análisis
obtener_ruta_archivo_biodiversidad <- function(tipo_analisis) {
  archivo_base <- "BBDD/ARCLIM/riesgo/Biodiversidad/ARCLIM_biodiversidad"
  
  # Construct file path based on analysis type
  if (tipo_analisis == "flora_temperatura") {
    ruta_archivo <- paste0(archivo_base, "_flora_temperatura_comunas.xlsx")
  } else if (tipo_analisis == "flora_precipitacion") {
    ruta_archivo <- paste0(archivo_base, "_flora_precip_comunas.xlsx")
  } else if (tipo_analisis == "fauna_temperatura") {
    ruta_archivo <- paste0(archivo_base, "_fauna_temperatura_cbit_comunas.xlsx")
  } else if (tipo_analisis == "fauna_precipitacion") {
    ruta_archivo <- paste0(archivo_base, "_fauna_precip_comunas.xlsx")
  } else if (tipo_analisis == "humedales") {
    ruta_archivo <- "BBDD/ARCLIM/riesgo/Biodiversidad/ARCLIM_humedales_costeros_addcbit_comunas.xlsx"
  } else {
    # Default fallback
    ruta_archivo <- paste0(archivo_base, "_fauna_temperatura_cbit_comunas.xlsx")
  }
  
  return(ruta_archivo)
}

#' Función para obtener descripciones específicas para humedales costeros
#' @return Lista con descripciones de indicadores para humedales
obtener_descripciones_humedales <- function() {
  descripciones <- list(
    amenaza = "Aumento de las cotas de inundación costera por cambios proyectados en el nivel del mar y setup del oleaje.",
    exposicion = "Superficie total de humedales costeros en la comuna.",
    sensibilidad = "Depende del tipo, ubicación y tamaño del humedal (ej. humedales pequeños o sin protección son más vulnerables).",
    capacidad_adapt = "Indica si el humedal cuenta con protección legal (SNASPE, RENAMU, Ley de Humedales Costeros).",
    riesgo = "Degradación del humedal debido al aumento de las cotas de inundación y presión antrópica."
  )
  
  return(descripciones)
}

#' Función para obtener valores y estadísticas para humedales en una comuna
#' @param datos DataFrame con datos de humedales
#' @param nombre_comuna Nombre de la comuna
#' @return Lista con valores y estadísticas
obtener_valores_comuna_humedales <- function(datos, nombre_comuna) {
  # Filtrar datos para la comuna específica
  fila_comuna <- datos %>% 
    filter(tolower(NOM_COMUNA) == tolower(nombre_comuna))
  
  if (nrow(fila_comuna) == 0) {
    stop(paste("No se encontraron datos para la comuna:", nombre_comuna))
  }
  
  # Prefijos para las columnas de humedales
  prefijo <- "humedales_costeros_"
  
  # Obtener valores para la comuna
  obtener_valor <- function(sufijo) {
    columna <- paste0(prefijo, sufijo)
    
    if (columna %in% names(datos)) {
      # Calcular estadísticas para todas las comunas
      stats <- calcular_estadisticas_columna(datos, columna)
      
      # Obtener valor para la comuna específica
      valor <- fila_comuna[[columna]]
      
      return(list(
        valor = valor,
        stats = stats
      ))
    } else {
      return(NULL)
    }
  }
  
  # Obtener valores para cada indicador
  valores <- list(
    amenaza = obtener_valor("amen"),
    exposicion = obtener_valor("expo"),
    sensibilidad = obtener_valor("sens"),
    capacidad_adapt = obtener_valor("cap_adapt"),
    riesgo = obtener_valor("riesgo"),
    superficie_humedal = obtener_valor("superficie_ha"),
    superficie_protegida = obtener_valor("sup_prot")
  )
  
  return(valores)
}

#' Función para generar recomendación basada en el nivel de riesgo para humedales
#' @param valor_riesgo Valor numérico del riesgo
#' @return Texto con recomendación
generar_recomendacion_humedales <- function(valor_riesgo) {
  if (is.na(valor_riesgo)) {
    return("No se pueden generar recomendaciones sin información sobre el nivel de riesgo.")
  }
  
  nivel_texto <- interpretar_nivel_riesgo(valor_riesgo)
  
  if (nivel_texto %in% c("Alto", "Muy Alto")) {
    return("Se recomienda: Implementar medidas urgentes de protección legal para los humedales costeros no protegidos. Desarrollar planes de adaptación ante el aumento del nivel del mar incluyendo zonas buffer que permitan la migración del humedal. Establecer programas de monitoreo continuo de intrusión salina y realizar intervenciones para reducir las presiones antrópicas como la contaminación o el relleno de humedales.")
  } else if (nivel_texto == "Moderado") {
    return("Se recomienda: Fortalecer la protección existente de los humedales costeros. Implementar programas de monitoreo de intrusión salina y cambios en el nivel del mar. Desarrollar estudios específicos sobre la vulnerabilidad de cada humedal considerando sus características particulares y elaborar planes de manejo adaptativos.")
  } else {
    return("Se recomienda: Mantener un monitoreo preventivo de los humedales costeros. Educar a la comunidad sobre la importancia de estos ecosistemas y sus servicios ecosistémicos. Aunque el riesgo actual es bajo, es importante planificar estrategias de adaptación para escenarios futuros más adversos, considerando el valor ecológico y económico de estos ecosistemas.")
  }
}

#' Función para generar el informe específico para humedales costeros
#' @param nombre_comuna Nombre de la comuna
#' @return Lista con el informe HTML y datos para visualización
generar_informe_humedales_costeros <- function(nombre_comuna) {
  print(paste0("Generando informe de riesgo para humedales costeros en ", nombre_comuna))
  
  # Cargar datos
  ruta_archivo <- "BBDD/ARCLIM/riesgo/Biodiversidad/ARCLIM_humedales_costeros_addcbit_comunas.xlsx"
  tryCatch({
    datos_cargados <- list(
      datos = read_excel(ruta_archivo, sheet = "DATOS"),
      metadatos = read_excel(ruta_archivo, sheet = "METADATOS")
    )
    
    # Verificar que la comuna existe en los datos
    comunas_disponibles <- obtener_comunas_disponibles_biodiversidad(datos_cargados$datos)
    if (!tolower(nombre_comuna) %in% tolower(comunas_disponibles)) {
      return(list(
        informe_html = paste0(
          "<h4>Informe de Humedales Costeros - Comuna de ", nombre_comuna, "</h4>",
          "<p style='color: red;'><b>Error:</b> No se encontraron datos para la comuna especificada.</p>",
          "<p>Comunas disponibles: ", paste(comunas_disponibles[1:min(10, length(comunas_disponibles))], collapse = ", "),
          if(length(comunas_disponibles) > 10) "..." else "", "</p>"
        ),
        datos_visualizacion = NULL
      ))
    }
    
    # Obtener información de la comuna
    fila_comuna <- datos_cargados$datos %>% 
      filter(tolower(NOM_COMUNA) == tolower(nombre_comuna))
    
    # Información geográfica
    provincia <- fila_comuna$NOM_PROVIN[1]
    region <- fila_comuna$NOM_REGION[1]
    
    # Mapeo de nombres de columnas en el archivo Excel a los nombres usados en el código
    # Basado en la salida de depuración
    mapeo_columnas <- list(
      variacion_riesgo = "zonascosteras_addcbit_variacion",
      indice_sensibilidad = "zonascosteras_addcbit_sen_índicecomunal_norm",
      ubicacion_humedal = "zonascosteras_addcbit_sen_índicecomunal",
      riesgo_futuro_sin_adapt = "zonascosteras_addcbit_riesgo_sinca_norm",
      riesgo_futuro_con_adapt = "zonascosteras_addcbit_riesgo_conca",
      id_humedal = "zonascosteras_addcbit_n", # Usando n como ID 
      capacidad_adaptacion = "zonascosteras_addcbit_exp_a_humedales_norm",
      exposicion_humedales = "zonascosteras_addcbit_exp_a_humedales",
      numero_humedales = "zonascosteras_addcbit_n",
      instrumento_proteccion = "zonascosteras_addcbit_k", # Asumiendo que k podría ser el instrumento de protección
      cota_inundacion = "zonascosteras_addcbit_ca",
      amenaza = "zonascosteras_addcbit_amen_norm"
    )
    
    # Función para obtener valor de columna con manejo de errores
    obtener_valor_columna_mapeada <- function(nombre_atributo) {
      nombre_columna <- mapeo_columnas[[nombre_atributo]]
      if (is.null(nombre_columna) || !nombre_columna %in% names(fila_comuna)) {
        return(NA)
      }
      return(fila_comuna[[nombre_columna]][1])
    }
    
    # Obtener atributos específicos de humedales usando el mapeo
    atributos_humedal <- list(
      NOM_COMUNA = fila_comuna$NOM_COMUNA[1],
      NOM_PROVIN = fila_comuna$NOM_PROVIN[1],
      NOM_REGION = fila_comuna$NOM_REGION[1]
    )
    
    # Completar atributos usando el mapeo
    for (nombre_atributo in names(mapeo_columnas)) {
      atributos_humedal[[nombre_atributo]] <- obtener_valor_columna_mapeada(nombre_atributo)
    }
    
    # Función para formatear los valores
    formatear_valor <- function(valor, tipo = "normal", decimales = 2) {
      if (is.na(valor)) return("No disponible")
      
      if (tipo == "porcentaje") {
        return(paste0(round(valor, decimales), "%"))
      } else if (tipo == "numero") {
        return(as.character(formato_numero(valor, decimales)))
      } else {
        return(as.character(round(valor, decimales)))
      }
    }
    
    # Función auxiliar para dar formato a números (miles con punto)
    formato_numero <- function(x, decimales = 0) {
      format(round(as.numeric(x), decimales), big.mark = ".", decimal.mark = ",", nsmall = decimales)
    }
    
    # Funciones para interpretar niveles
    interpretar_nivel_numerico <- function(valor, breaks = c(0.2, 0.4, 0.6, 0.8), 
                                        labels = c("Muy Bajo", "Bajo", "Moderado", "Alto", "Muy Alto"),
                                        invert = FALSE) {
      if (is.na(valor)) return("No disponible")
      
      cuts <- c(-Inf, breaks, Inf)
      result <- cut(valor, cuts, labels = labels, include.lowest = TRUE)
      
      if (invert) {
        # Invertir para casos donde valores bajos son peores
        labels_inv <- rev(labels)
        result <- cut(1 - valor, cuts, labels = labels_inv, include.lowest = TRUE)
      }
      
      return(as.character(result))
    }
    
    # Interpretar los niveles para cada indicador
    niveles <- list(
      variacion_riesgo = "No aplica", # Es un porcentaje de variación
      indice_sensibilidad = interpretar_nivel_numerico(atributos_humedal$indice_sensibilidad),
      ubicacion_humedal = "No aplica", # Es una descripción
      riesgo_futuro_sin_adapt = interpretar_nivel_numerico(atributos_humedal$riesgo_futuro_sin_adapt),
      riesgo_futuro_con_adapt = interpretar_nivel_numerico(atributos_humedal$riesgo_futuro_con_adapt),
      capacidad_adaptacion = interpretar_nivel_numerico(atributos_humedal$capacidad_adaptacion, invert = TRUE),
      exposicion_humedales = interpretar_nivel_numerico(atributos_humedal$exposicion_humedales)
    )
    
    # Generar descripción de cada indicador
    descripciones_indicadores <- list(
      variacion_riesgo = "Variación porcentual entre el riesgo antes y después de considerar la capacidad de adaptación.",
      indice_sensibilidad = "Índice de sensibilidad normalizado que considera características intrínsecas del humedal.",
      ubicacion_humedal = "Incluye la ubicación, tipo y tamaño del humedal para evaluar su vulnerabilidad.",
      riesgo_futuro_sin_adapt = "Índice de riesgo futuro sin considerar la capacidad de adaptación del humedal.",
      riesgo_futuro_con_adapt = "Índice de riesgo futuro normalizado considerando la capacidad de adaptación del humedal.",
      id_humedal = "Identificador único del humedal dentro de la base de datos nacional.",
      capacidad_adaptacion = "Coeficiente de ponderación de la capacidad de adaptación basado en instrumentos de protección.",
      exposicion_humedales = "Indicador de exposición de humedales normalizado según factores físicos y geográficos.",
      numero_humedales = "Número total de humedales costeros identificados en la comuna.",
      instrumento_proteccion = "Indica si el humedal cuenta con algún instrumento de protección oficial (1=Sí, 0=No).",
      cota_inundacion = "Mayor cota de inundación comunal normalizada por efectos de marejadas y nivel del mar."
    )
    
    # Función para generar recomendaciones basadas en los niveles de riesgo
    generar_recomendaciones <- function(riesgo_nivel, numero_humedales, tiene_proteccion) {
      recomendaciones <- "<ul>"
      
      # Recomendaciones generales
      if (numero_humedales > 0) {
        if (riesgo_nivel %in% c("Alto", "Muy Alto")) {
          recomendaciones <- paste0(recomendaciones, 
            "<li><b>Protección Prioritaria:</b> Implementar con urgencia instrumentos de protección legal para todos los humedales costeros no protegidos de la comuna.</li>",
            "<li><b>Planificación Adaptativa:</b> Desarrollar planes de adaptación que consideren el aumento proyectado del nivel del mar y la mayor frecuencia de marejadas.</li>",
            "<li><b>Zonas de Amortiguamiento:</b> Establecer zonas buffer que permitan la migración natural del humedal hacia el interior.</li>",
            "<li><b>Monitoreo Continuo:</b> Implementar sistemas de monitoreo de intrusión salina y cambios en las cotas de inundación.</li>"
          )
        } else if (riesgo_nivel == "Moderado") {
          recomendaciones <- paste0(recomendaciones, 
            "<li><b>Fortalecimiento de Protección:</b> Reforzar los instrumentos de protección existentes y extenderlos a todos los humedales.</li>",
            "<li><b>Estudios Específicos:</b> Realizar evaluaciones detalladas de vulnerabilidad para cada humedal identificado.</li>",
            "<li><b>Restauración Ecológica:</b> Implementar acciones de restauración en zonas degradadas del humedal.</li>",
            "<li><b>Educación Comunitaria:</b> Desarrollar programas de educación sobre la importancia de los humedales costeros.</li>"
          )
        } else {
          recomendaciones <- paste0(recomendaciones, 
            "<li><b>Monitoreo Preventivo:</b> Establecer un sistema de vigilancia de los humedales costeros.</li>",
            "<li><b>Planificación a Largo Plazo:</b> Considerar escenarios futuros más adversos en la planificación territorial.</li>",
            "<li><b>Conservación del Entorno:</b> Mantener la calidad ambiental de las zonas adyacentes a los humedales.</li>"
          )
        }
        
        # Recomendaciones específicas sobre instrumentos de protección
        if (!tiene_proteccion) {
          recomendaciones <- paste0(recomendaciones,
            "<li><b>Gestión de Protección Legal:</b> Iniciar trámites para incluir los humedales en el Registro Nacional de Humedales o solicitar su protección bajo la Ley de Humedales Urbanos.</li>"
          )
        }
      } else {
        recomendaciones <- paste0(recomendaciones,
          "<li>No se identificaron humedales costeros en esta comuna que requieran acciones específicas.</li>"
        )
      }
      
      recomendaciones <- paste0(recomendaciones, "</ul>")
      return(recomendaciones)
    }
    
    # Preparar valores para el informe
    nivel_riesgo_futuro <- if(!is.na(atributos_humedal$riesgo_futuro_con_adapt)) {
      niveles$riesgo_futuro_con_adapt
    } else if(!is.na(atributos_humedal$riesgo_futuro_sin_adapt)) {
      niveles$riesgo_futuro_sin_adapt
    } else {
      "No disponible"
    }
    
    # Determinar si tiene protección (puede ser NA si no existe el campo)
    tiene_proteccion <- !is.na(atributos_humedal$instrumento_proteccion) && atributos_humedal$instrumento_proteccion > 0
    
    # Crear el informe en formato HTML
    informe_html <- paste0(
      "<h4>Informe de Vulnerabilidad de Humedales Costeros - Comuna de ", nombre_comuna, "</h4>",
      
      "<h5>Contexto e Importancia de los Humedales Costeros</h5>",
      "<p>Los humedales costeros son ecosistemas críticos por sus múltiples servicios ecosistémicos como la regulación hídrica, protección contra inundaciones, captura de carbono y hábitat para biodiversidad. Sin embargo, estos ecosistemas enfrentan diversas amenazas climáticas, destacando:</p>",
      "<ul>",
      "<li><b>Anegación permanente</b> por aumento del nivel del mar.</li>",
      "<li><b>Alteración de la columna de agua</b> debido a intrusión salina por marejadas.</li>",
      "<li><b>Cambios en los patrones de precipitación</b> que afectan su régimen hidrológico.</li>",
      "</ul>",
      
      "<h5>Ubicación Geográfica</h5>",
      "<table style='width:100%; border-collapse: collapse; margin-bottom: 15px;'>",
      "<tr><td style='padding: 5px; border: 1px solid #ddd; width: 30%;'><b>Comuna:</b></td><td style='padding: 5px; border: 1px solid #ddd;'>", nombre_comuna, "</td></tr>",
      "<tr><td style='padding: 5px; border: 1px solid #ddd;'><b>Provincia:</b></td><td style='padding: 5px; border: 1px solid #ddd;'>", provincia, "</td></tr>",
      "<tr><td style='padding: 5px; border: 1px solid #ddd;'><b>Región:</b></td><td style='padding: 5px; border: 1px solid #ddd;'>", region, "</td></tr>",
      "</table>",
      
      "<h5>Caracterización de Humedales Costeros en la Comuna</h5>",
      "<table style='width:100%; border-collapse: collapse; margin-bottom: 15px;'>",
      "<tr>",
      "<th style='padding: 8px; border: 1px solid #ddd; background-color: #f2f2f2; text-align: left;'>Indicador</th>",
      "<th style='padding: 8px; border: 1px solid #ddd; background-color: #f2f2f2; text-align: left;'>Valor</th>",
      "<th style='padding: 8px; border: 1px solid #ddd; background-color: #f2f2f2; text-align: left;'>Descripción</th>",
      "</tr>",
      
      "<tr>",
      "<td style='padding: 5px; border: 1px solid #ddd;'><b>Número de humedales</b></td>",
      "<td style='padding: 5px; border: 1px solid #ddd;'>", formatear_valor(atributos_humedal$numero_humedales, "numero"), "</td>",
      "<td style='padding: 5px; border: 1px solid #ddd;'>", descripciones_indicadores$numero_humedales, "</td>",
      "</tr>",
      
      "<tr>",
      "<td style='padding: 5px; border: 1px solid #ddd;'><b>Instrumento de protección</b></td>",
      "<td style='padding: 5px; border: 1px solid #ddd;'>", ifelse(tiene_proteccion, "Sí", "No"), "</td>",
      "<td style='padding: 5px; border: 1px solid #ddd;'>", descripciones_indicadores$instrumento_proteccion, "</td>",
      "</tr>",
      
      "<tr>",
      "<td style='padding: 5px; border: 1px solid #ddd;'><b>Exposición de humedales</b></td>",
      "<td style='padding: 5px; border: 1px solid #ddd;'>", formatear_valor(atributos_humedal$exposicion_humedales), 
      " (", niveles$exposicion_humedales, ")</td>",
      "<td style='padding: 5px; border: 1px solid #ddd;'>", descripciones_indicadores$exposicion_humedales, "</td>",
      "</tr>",
      
      "<tr>",
      "<td style='padding: 5px; border: 1px solid #ddd;'><b>Cota de inundación</b></td>",
      "<td style='padding: 5px; border: 1px solid #ddd;'>", formatear_valor(atributos_humedal$cota_inundacion), "</td>",
      "<td style='padding: 5px; border: 1px solid #ddd;'>", descripciones_indicadores$cota_inundacion, "</td>",
      "</tr>",
      "</table>",
      
      "<h5>Índices de Vulnerabilidad y Riesgo</h5>",
      "<table style='width:100%; border-collapse: collapse; margin-bottom: 15px;'>",
      "<tr>",
      "<th style='padding: 8px; border: 1px solid #ddd; background-color: #f2f2f2; text-align: left;'>Indicador</th>",
      "<th style='padding: 8px; border: 1px solid #ddd; background-color: #f2f2f2; text-align: left;'>Valor</th>",
      "<th style='padding: 8px; border: 1px solid #ddd; background-color: #f2f2f2; text-align: left;'>Nivel</th>",
      "<th style='padding: 8px; border: 1px solid #ddd; background-color: #f2f2f2; text-align: left;'>Descripción</th>",
      "</tr>",
      
      "<tr>",
      "<td style='padding: 5px; border: 1px solid #ddd;'><b>Índice de sensibilidad</b></td>",
      "<td style='padding: 5px; border: 1px solid #ddd;'>", formatear_valor(atributos_humedal$indice_sensibilidad), "</td>",
      "<td style='padding: 5px; border: 1px solid #ddd;'>", niveles$indice_sensibilidad, "</td>",
      "<td style='padding: 5px; border: 1px solid #ddd;'>", descripciones_indicadores$indice_sensibilidad, "</td>",
      "</tr>",
      
      "<tr>",
      "<td style='padding: 5px; border: 1px solid #ddd;'><b>Capacidad de adaptación</b></td>",
      "<td style='padding: 5px; border: 1px solid #ddd;'>", formatear_valor(atributos_humedal$capacidad_adaptacion), "</td>",
      "<td style='padding: 5px; border: 1px solid #ddd;'>", niveles$capacidad_adaptacion, "</td>",
      "<td style='padding: 5px; border: 1px solid #ddd;'>", descripciones_indicadores$capacidad_adaptacion, "</td>",
      "</tr>",
      
      "<tr>",
      "<td style='padding: 5px; border: 1px solid #ddd;'><b>Riesgo futuro sin adaptación</b></td>",
      "<td style='padding: 5px; border: 1px solid #ddd;'>", formatear_valor(atributos_humedal$riesgo_futuro_sin_adapt), "</td>",
      "<td style='padding: 5px; border: 1px solid #ddd;'>", niveles$riesgo_futuro_sin_adapt, "</td>",
      "<td style='padding: 5px; border: 1px solid #ddd;'>", descripciones_indicadores$riesgo_futuro_sin_adapt, "</td>",
      "</tr>",
      
      "<tr>",
      "<td style='padding: 5px; border: 1px solid #ddd;'><b>Riesgo futuro con adaptación</b></td>",
      "<td style='padding: 5px; border: 1px solid #ddd;'>", formatear_valor(atributos_humedal$riesgo_futuro_con_adapt), "</td>",
      "<td style='padding: 5px; border: 1px solid #ddd;'>", niveles$riesgo_futuro_con_adapt, "</td>",
      "<td style='padding: 5px; border: 1px solid #ddd;'>", descripciones_indicadores$riesgo_futuro_con_adapt, "</td>",
      "</tr>",
      
      "<tr>",
      "<td style='padding: 5px; border: 1px solid #ddd;'><b>Variación del riesgo</b></td>",
      "<td style='padding: 5px; border: 1px solid #ddd;'>", formatear_valor(atributos_humedal$variacion_riesgo, "porcentaje"), "</td>",
      "<td style='padding: 5px; border: 1px solid #ddd;'>", ifelse(is.na(atributos_humedal$variacion_riesgo), "No disponible", 
                                                              ifelse(atributos_humedal$variacion_riesgo > 0, "Favorable", "Desfavorable")), "</td>",
      "<td style='padding: 5px; border: 1px solid #ddd;'>", descripciones_indicadores$variacion_riesgo, "</td>",
      "</tr>",
      "</table>",
      
      "<h5>Evaluación de Riesgo</h5>",
      "<p>La comuna de <b>", nombre_comuna, "</b> presenta un nivel de riesgo <b>", nivel_riesgo_futuro, 
      "</b> para sus humedales costeros, considerando la capacidad de adaptación existente. ",
      if (!is.na(atributos_humedal$variacion_riesgo)) {
        paste0("La implementación de medidas de adaptación podría reducir el riesgo en aproximadamente un <b>", 
              formatear_valor(abs(atributos_humedal$variacion_riesgo), "porcentaje"), "</b>.")
      } else {
        "No se dispone de datos sobre la posible reducción del riesgo mediante medidas de adaptación."
      },
      "</p>",
      
      "<h5>Recomendaciones para la Gestión y Conservación</h5>",
      "<p>Basado en el análisis de riesgo y las características de los humedales costeros en la comuna, se recomienda implementar las siguientes acciones:</p>",
      generar_recomendaciones(nivel_riesgo_futuro, 
                           if(is.na(atributos_humedal$numero_humedales)) 0 else atributos_humedal$numero_humedales,
                           tiene_proteccion)
    )
    
    # Crear datos para visualización
    datos_viz <- data.frame(
      indicador = character(),
      valor = numeric(),
      stringsAsFactors = FALSE
    )
    
    # Añadir solo los valores no NA a la visualización
    if (!is.na(atributos_humedal$riesgo_futuro_sin_adapt)) {
      datos_viz <- rbind(datos_viz, data.frame(
        indicador = "Riesgo futuro sin adaptación",
        valor = atributos_humedal$riesgo_futuro_sin_adapt,
        stringsAsFactors = FALSE
      ))
    }
    
    if (!is.na(atributos_humedal$riesgo_futuro_con_adapt)) {
      datos_viz <- rbind(datos_viz, data.frame(
        indicador = "Riesgo futuro con adaptación",
        valor = atributos_humedal$riesgo_futuro_con_adapt,
        stringsAsFactors = FALSE
      ))
    }
    
    if (!is.na(atributos_humedal$indice_sensibilidad)) {
      datos_viz <- rbind(datos_viz, data.frame(
        indicador = "Índice de sensibilidad",
        valor = atributos_humedal$indice_sensibilidad,
        stringsAsFactors = FALSE
      ))
    }
    
    if (!is.na(atributos_humedal$capacidad_adaptacion)) {
      datos_viz <- rbind(datos_viz, data.frame(
        indicador = "Capacidad de adaptación",
        valor = atributos_humedal$capacidad_adaptacion,
        stringsAsFactors = FALSE
      ))
    }
    
    if (!is.na(atributos_humedal$exposicion_humedales)) {
      datos_viz <- rbind(datos_viz, data.frame(
        indicador = "Exposición de humedales",
        valor = atributos_humedal$exposicion_humedales,
        stringsAsFactors = FALSE
      ))
    }
    
    # Añadir niveles donde corresponda
    if ("Riesgo futuro sin adaptación" %in% datos_viz$indicador) {
      indice <- which(datos_viz$indicador == "Riesgo futuro sin adaptación")
      datos_viz$nivel[indice] <- niveles$riesgo_futuro_sin_adapt
    }
    
    if ("Riesgo futuro con adaptación" %in% datos_viz$indicador) {
      indice <- which(datos_viz$indicador == "Riesgo futuro con adaptación")
      datos_viz$nivel[indice] <- niveles$riesgo_futuro_con_adapt
    }
    
    # Imprimir datos para depuración
    print("Resultado final:")
    print(paste("Número de datos para visualización:", nrow(datos_viz)))
    
    return(list(
      informe_html = informe_html,
      datos_visualizacion = datos_viz,
      tipo_analisis = "humedales",
      valores = atributos_humedal
    ))
  }, error = function(e) {
    # Capturar y mostrar cualquier error en el proceso
    mensaje_error <- paste("Error al generar informe:", e$message)
    print(mensaje_error)
    
    return(list(
      informe_html = paste0(
        "<h4>Informe de Humedales Costeros - Comuna de ", nombre_comuna, "</h4>",
        "<p style='color: red;'><b>Error:</b> ", e$message, "</p>",
        "<p>Detalles adicionales: Verifique que el archivo ARCLIM_humedales_costeros_addcbit_comunas.xlsx esté disponible y contenga los datos necesarios.</p>"
      ),
      datos_visualizacion = NULL
    ))
  })
} 