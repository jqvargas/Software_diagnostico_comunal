# Script para procesar y visualizar datos históricos de clima
# Cargar librerías necesarias
library(dplyr)
library(ggplot2)
library(readr)

# Función para obtener las comunas disponibles
get_available_comunas <- function(variable) {
  # Definir las rutas de los archivos según la variable
  if (variable == "temp") {
    archivo <- paste0(getwd(), "/BBDD/historico/temp/temperaturas_comunales.csv")
  } else if (variable == "pp") {
    archivo <- paste0(getwd(), "/BBDD/historico/pp/precipitacion_comunal.csv")
  } else {
    stop("Variable no válida. Use 'temp' o 'pp'.")
  }
  
  # Verificar si el archivo existe
  if (!file.exists(archivo)) {
    stop(paste("No se encontró el archivo:", archivo))
  }
  
  # Leer los datos y obtener comunas únicas
  datos <- read_csv(archivo)
  comunas_disponibles <- unique(datos$NOM_COM)
  return(sort(comunas_disponibles))
}

# Función para leer y procesar datos históricos
procesar_datos_historicos <- function(variable, nombre_comuna) {
  print(paste0("Comenzando a procesar variable: ", 
               variable, 
               " para la comuna: ",
               nombre_comuna))
  
  # Definir las rutas de los archivos según la variable
  if (variable == "temp") {
    archivo <- paste0(getwd(), "/BBDD/historico/temp/temperaturas_comunales.csv")
  } else if (variable == "pp") {
    archivo <- paste0(getwd(), "/BBDD/historico/pp/precipitacion_comunal.csv")
  } else {
    stop("Variable no válida. Use 'temp' o 'pp'.")
  }
  
  # Verificar si el archivo existe
  if (!file.exists(archivo)) {
    stop(paste("No se encontró el archivo:", archivo))
  }
  
  # Leer los datos
  print(paste("Leyendo archivo:", basename(archivo)))
  datos <- read_csv(archivo)
  
  # Buscar la comuna de manera case-insensitive
  comuna_match <- datos %>%
    filter(tolower(NOM_COM) == tolower(nombre_comuna)) %>%
    pull(NOM_COM) %>%
    unique()
  
  if (length(comuna_match) == 0) {
    # Si no se encuentra la comuna, mostrar las opciones disponibles
    comunas_disponibles <- unique(datos$NOM_COM)
    stop(paste("No se encontró la comuna:", nombre_comuna, 
               "\nComunas disponibles:", 
               paste(sort(comunas_disponibles), collapse=", ")))
  }
  
  # Usar el nombre exacto de la comuna encontrado
  nombre_comuna_exacto <- comuna_match[1]
  
  # Filtrar por comuna y año (hasta 2024)
  datos_comuna <- datos %>%
    filter(NOM_COM == nombre_comuna_exacto,
           Year <= 2024)
  
  print(paste("Procesando datos históricos desde", min(datos_comuna$Year), 
              "hasta 2024"))
  
  return(datos_comuna)
}

# Nueva función para calcular estadísticas climáticas detalladas
compute_climate_statistics <- function(datos, variable, tipo_temp = NULL, nombre_comuna) {
  print(paste0("Calculando estadísticas climáticas para ", nombre_comuna))
  
  # Definir la columna y unidades según la variable
  if (variable == "temp") {
    if (is.null(tipo_temp)) {
      stop("Debe especificar tipo_temp ('max', 'min' o 'mean') para temperatura")
    }
    
    # Seleccionar la columna y unidades según el tipo de temperatura
    if (tipo_temp == "max") {
      y_col <- "temp_max_anual"
      unidad <- "°C"
      var_nombre <- "temperatura máxima"
    } else if (tipo_temp == "min") {
      y_col <- "temp_min_anual"
      unidad <- "°C"
      var_nombre <- "temperatura mínima"
    } else if (tipo_temp == "mean") {
      y_col <- "temp_mean_anual"
      unidad <- "°C"
      var_nombre <- "temperatura media"
    } else {
      stop("tipo_temp no válido. Use 'max', 'min' o 'mean'")
    }
  } else if (variable == "pp") {
    y_col <- "pp_acumulado_anual_comunal"
    unidad <- "mm"
    var_nombre <- "precipitación"
  } else {
    stop("Variable no válida. Use 'temp' o 'pp'")
  }
  
  # Verificar que la columna existe
  if (!y_col %in% names(datos)) {
    stop(paste("La columna", y_col, "no existe en los datos"))
  }
  
  # Calcular estadísticas básicas
  stats_basic <- datos %>%
    summarise(
      valor_max = max(.data[[y_col]], na.rm = TRUE),
      valor_min = min(.data[[y_col]], na.rm = TRUE),
      promedio = mean(.data[[y_col]], na.rm = TRUE),
      mediana = median(.data[[y_col]], na.rm = TRUE),
      desv_est = sd(.data[[y_col]], na.rm = TRUE),
      q25 = quantile(.data[[y_col]], 0.25, na.rm = TRUE),
      q75 = quantile(.data[[y_col]], 0.75, na.rm = TRUE)
    )
  
  # Encontrar años de valores extremos
  year_max <- datos$Year[which.max(datos[[y_col]])]
  year_min <- datos$Year[which.min(datos[[y_col]])]
  
  # Calcular tendencia
  modelo <- lm(reformulate("Year", response = y_col), data = datos)
  tendencia_anual <- coef(modelo)[2]
  tendencia_decadal <- tendencia_anual * 10
  
  # Calcular significancia estadística de la tendencia
  summary_modelo <- summary(modelo)
  p_valor <- summary_modelo$coefficients[2, 4]
  r_cuadrado <- summary_modelo$r.squared
  
  # Crear interpretación de la tendencia
  direccion <- if (tendencia_anual > 0) "aumentado" else "disminuido"
  significancia <- if (p_valor < 0.05) "estadísticamente significativa" else "no estadísticamente significativa"
  
  interpretacion <- paste0(
    "La ", var_nombre, " ha ", direccion, " en promedio ", 
    abs(round(tendencia_anual, 3)), " ", unidad, " por año ",
    "(", abs(round(tendencia_decadal, 3)), " ", unidad, " por década). ",
    "Esta tendencia es ", significancia, " (p-valor = ", round(p_valor, 4), ")."
  )
  
  # Calcular estadísticas por períodos
  años_total <- max(datos$Year) - min(datos$Year) + 1
  n_periodos <- ceiling(años_total / 10)  # Dividir en períodos de aproximadamente 10 años
  
  datos$periodo <- cut(datos$Year, 
                      breaks = n_periodos,
                      labels = paste("Período", 1:n_periodos))
  
  stats_periodos <- datos %>%
    group_by(periodo) %>%
    summarise(
      años = paste(min(Year), "-", max(Year)),
      promedio = mean(.data[[y_col]], na.rm = TRUE),
      desv_est = sd(.data[[y_col]], na.rm = TRUE)
    )
  
  # Crear resumen estructurado
  resumen <- list(
    metadata = list(
      variable = var_nombre,
      comuna = nombre_comuna,
      periodo_analisis = paste(min(datos$Year), "-", max(datos$Year)),
      unidad = unidad
    ),
    estadisticas_basicas = stats_basic,
    valores_extremos = list(
      maximo = list(
        valor = stats_basic$valor_max,
        año = year_max
      ),
      minimo = list(
        valor = stats_basic$valor_min,
        año = year_min
      )
    ),
    analisis_tendencia = list(
      tendencia_anual = tendencia_anual,
      tendencia_decadal = tendencia_decadal,
      p_valor = p_valor,
      r_cuadrado = r_cuadrado,
      interpretacion = interpretacion
    ),
    estadisticas_periodicas = stats_periodos
  )
  
  # Generar texto de resumen
  summary_text <- paste0(
    "<h4>Análisis Histórico de ", var_nombre, " en ", nombre_comuna, "</h4>",
    "<p><b>Período de análisis:</b> ", resumen$metadata$periodo_analisis, "</p>",
    
    "<h5>Estadísticas Generales</h5>",
    "<p>El valor promedio histórico de ", var_nombre, " fue <b>", round(stats_basic$promedio, 2), " ", unidad, 
    "</b>, con una mediana de ", round(stats_basic$mediana, 2), " ", unidad, ". ",
    "La variabilidad de los datos, medida por la desviación estándar, fue de ", round(stats_basic$desv_est, 2), " ", unidad, ".</p>",
    
    "<p>El rango intercuartil muestra que el 50% central de los valores se encuentra entre ",
    "<b>", round(stats_basic$q25, 2), "</b> y <b>", round(stats_basic$q75, 2), " ", unidad, "</b>.</p>",
    
    "<h5>Valores Extremos</h5>",
    "<p>Se registró un valor máximo de <b>", round(stats_basic$valor_max, 2), " ", unidad, "</b> en el año ", year_max,
    ", mientras que el valor mínimo fue de <b>", round(stats_basic$valor_min, 2), " ", unidad, "</b> en ", year_min, ".</p>",
    
    "<h5>Análisis de Tendencia</h5>",
    "<p>", interpretacion, " Esta tendencia tiene un coeficiente de determinación (R-cuadrado) de <b>", 
    round(r_cuadrado, 4), "</b>, lo que indica el grado de ajuste del modelo lineal a los datos.</p>",
    
    "<h5>Análisis por Períodos</h5>"
  )
  
  # Agregar estadísticas por períodos al texto
  for(i in 1:nrow(stats_periodos)) {
    summary_text <- paste0(
      summary_text,
      stats_periodos$periodo[i], " (", stats_periodos$años[i], "): ",
      round(stats_periodos$promedio[i], 2), " ± ",
      round(stats_periodos$desv_est[i], 2), " ", unidad, "\n"
    )
  }
  
  print(summary_text)
  
  return(list(
    resumen = resumen,
    summary_text = summary_text
  ))
}

# Función para generar gráficos de series temporales
plot_serie_temporal <- function(datos, variable, tipo_temp = NULL, nombre_comuna) {
  print("Generando gráfico de serie temporal...")
  
  # Asegurar que no hay datos después de 2024
  datos <- datos %>% filter(Year <= 2024)
  
  # Definir las etiquetas en español según la variable
  if (variable == "temp") {
    if (is.null(tipo_temp)) {
      stop("Debe especificar tipo_temp ('max', 'min' o 'mean') para temperatura")
    }
    
    # Seleccionar la columna y etiquetas según el tipo de temperatura
    if (tipo_temp == "max") {
      y_col <- "temp_max_anual"
      y_label <- "Temperatura Máxima Anual (°C)"
      titulo_var <- "temperatura máxima"
    } else if (tipo_temp == "min") {
      y_col <- "temp_min_anual"
      y_label <- "Temperatura Mínima Anual (°C)"
      titulo_var <- "temperatura mínima"
    } else if (tipo_temp == "mean") {
      y_col <- "temp_mean_anual"
      y_label <- "Temperatura Media Anual (°C)"
      titulo_var <- "temperatura media"
    } else {
      stop("tipo_temp no válido. Use 'max', 'min' o 'mean'")
    }
  } else if (variable == "pp") {
    y_col <- "pp_acumulado_anual_comunal"
    y_label <- "Precipitación Acumulada Anual (mm)"
    titulo_var <- "precipitación"
  } else {
    stop("Variable no válida. Use 'temp' o 'pp'")
  }
  
  # Verificar que la columna existe en los datos
  if (!y_col %in% names(datos)) {
    stop(paste("La columna", y_col, "no existe en los datos"))
  }
  
  # Calcular la línea de tendencia
  modelo <- lm(reformulate("Year", response = y_col), data = datos)
  datos$tendencia <- predict(modelo)
  
  # Crear el gráfico
  p <- ggplot(datos, aes_string(x = "Year", y = y_col)) +
    geom_line(color = "blue", size = 1) +
    geom_point(color = "blue", size = 2) +
    # Agregar línea de tendencia
    geom_line(aes(y = tendencia), color = "red", size = 1) +
    labs(
      title = paste("Serie temporal de", titulo_var, "\npara la comuna", nombre_comuna,
                   "\n(", min(datos$Year), "-2024)"),
      x = "Año",
      y = y_label
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title = element_text(size = 24),  # Aumentar tamaño de títulos de ejes
      axis.text = element_text(size = 20),   # Aumentar tamaño de números en ejes
      axis.text.x = element_text(angle = 45, hjust = 1)  # Rotar etiquetas del eje x
    ) +
    # Ajustar escala de años para mostrar cada 5 años
    scale_x_continuous(
      breaks = seq(
        ceiling(min(datos$Year)/5)*5,  # Redondear al próximo múltiplo de 5
        floor(2024/5)*5,               # Redondear al múltiplo de 5 anterior
        by = 5
      )
    )
  
  # Calcular estadísticas básicas de manera segura
  stats <- datos %>%
    summarise(
      promedio = mean(.data[[y_col]], na.rm = TRUE),
      min = min(.data[[y_col]], na.rm = TRUE),
      max = max(.data[[y_col]], na.rm = TRUE),
      desv_est = sd(.data[[y_col]], na.rm = TRUE),
      # Calcular la tendencia (cambio por década)
      tendencia_decada = coef(modelo)[2] * 10
    )
  
  # Encontrar años de valores mínimos y máximos de manera segura
  year_min <- datos$Year[which.min(datos[[y_col]])]
  year_max <- datos$Year[which.max(datos[[y_col]])]
  
  # Generar resumen estadístico
  summary_text <- paste0(
    "\nResumen Estadístico (", min(datos$Year), "-2024):\n",
    "Promedio: ", round(stats$promedio, 2), "\n",
    "Mínimo: ", round(stats$min, 2), " (", year_min, ")\n",
    "Máximo: ", round(stats$max, 2), " (", year_max, ")\n",
    "Desviación Estándar: ", round(stats$desv_est, 2), "\n",
    "Tendencia: ", round(stats$tendencia_decada, 2), " por década"
  )
  
  print(summary_text)
  
  return(list(
    plot = p,
    stats = stats,
    summary = summary_text
  ))
}

# Ejemplo de uso:
# Para temperatura:
# datos_temp <- procesar_datos_historicos("temp", "Santiago")
# estadisticas_temp <- compute_climate_statistics(datos_temp, "temp", "max", "Santiago")
# resultado_temp <- plot_serie_temporal(datos_temp, "temp", "max", "Santiago")
# print(resultado_temp$plot)
# print(estadisticas_temp$summary_text)
# 
# Para precipitación:
# datos_pp <- procesar_datos_historicos("pp", "Santiago")
# estadisticas_pp <- compute_climate_statistics(datos_pp, "pp", nombre_comuna = "Santiago")
# resultado_pp <- plot_serie_temporal(datos_pp, "pp", nombre_comuna = "Santiago")
# print(resultado_pp$plot)
# print(estadisticas_pp$summary_text) 