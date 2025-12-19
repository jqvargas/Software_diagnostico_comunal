# Script para procesar indicadores de riesgo de acuicultura

# Cargar librerías necesarias
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

#' Función para cargar y procesar datos de riesgo acuícola de agua dulce
#' @param ruta_archivo Ruta al archivo Excel
#' @return Lista con datos procesados
cargar_datos_riesgo_acuicultura <- function(ruta_archivo = "BBDD/ARCLIM/riesgo/Acuicultura/ARCLIM_acuicultura_cuencas_aguadulce_acuicultura_cuencas_172.xlsx") {
  print("Cargando datos de riesgo acuícola...")
  
  # Intentar cargar las hojas del archivo Excel
  tryCatch({
    # Primero leer datos para validar estructura
    datos <- read_excel(ruta_archivo, sheet = "DATOS")
    metadatos <- read_excel(ruta_archivo, sheet = "METADATOS")
    
    print(paste("Columnas disponibles:", paste(names(datos), collapse = ", ")))
    
    # Verificar columnas mínimas necesarias
    cols_requeridas <- c("comuna", "amen_acuicultura_aguadulce", 
                          "sensi_acuicultura_aguadulce", 
                          "riesgo_acuicultura_aguadulce_combinado")
    
    cols_faltantes <- cols_requeridas[!cols_requeridas %in% names(datos)]
    if (length(cols_faltantes) > 0) {
      stop(paste("Columnas faltantes en el archivo:", 
                paste(cols_faltantes, collapse = ", ")))
    }
    
    return(list(
      datos = datos,
      metadatos = metadatos
    ))
  }, error = function(e) {
    stop(paste("Error al cargar los datos:", e$message))
  })
}

#' Función para obtener las comunas disponibles en los datos
#' @param datos DataFrame con datos de acuicultura
#' @return Vector de nombres de comunas
obtener_comunas_disponibles <- function(datos) {
  comunas <- unique(datos$comuna)
  return(sort(comunas))
}

#' Función para contar pisciculturas por comuna
#' @param datos DataFrame con datos de acuicultura
#' @param nombre_comuna Nombre de la comuna
#' @return Número de pisciculturas en la comuna
contar_pisciculturas_comuna <- function(datos, nombre_comuna) {
  # Filtrar registros para la comuna especificada
  datos_comuna <- datos %>% 
    filter(tolower(comuna) == tolower(nombre_comuna))
  
  # Contar número de registros (pisciculturas)
  n_pisciculturas <- nrow(datos_comuna)
  
  return(n_pisciculturas)
}

#' Función para obtener estadísticas de riesgo acuícola para una comuna
#' @param datos DataFrame con datos de acuicultura
#' @param nombre_comuna Nombre de la comuna
#' @return Lista con estadísticas de riesgo
obtener_estadisticas_comuna <- function(datos, nombre_comuna) {
  # Filtrar registros para la comuna especificada
  datos_comuna <- datos %>% 
    filter(tolower(comuna) == tolower(nombre_comuna))
  
  if (nrow(datos_comuna) == 0) {
    stop(paste("No se encontraron datos para la comuna:", nombre_comuna))
  }
  
  # Obtener valores de amenaza, sensibilidad y riesgo
  amenaza_valores <- datos_comuna$amen_acuicultura_aguadulce
  sensibilidad_valores <- datos_comuna$sensi_acuicultura_aguadulce
  riesgo_valores <- datos_comuna$riesgo_acuicultura_aguadulce_combinado
  
  # Función para calcular la distribución de valores
  calcular_distribucion <- function(valores) {
    tabla <- table(valores)
    porcentajes <- round(prop.table(tabla) * 100, 1)
    
    # Determinar el valor predominante
    valor_predominante <- names(tabla)[which.max(tabla)]
    porcentaje_predominante <- porcentajes[which.max(tabla)]
    
    return(list(
      tabla = tabla,
      porcentajes = porcentajes,
      valor_predominante = valor_predominante,
      porcentaje_predominante = porcentaje_predominante
    ))
  }
  
  # Calcular distribuciones
  amenaza_dist <- calcular_distribucion(amenaza_valores)
  sensibilidad_dist <- calcular_distribucion(sensibilidad_valores)
  riesgo_dist <- calcular_distribucion(riesgo_valores)
  
  return(list(
    n_pisciculturas = nrow(datos_comuna),
    amenaza = amenaza_dist,
    sensibilidad = sensibilidad_dist,
    riesgo = riesgo_dist
  ))
}

#' Función para generar texto de recomendación basado en el nivel de riesgo
#' @param nivel_riesgo Nivel de riesgo predominante
#' @return Texto con recomendación
generar_recomendacion <- function(nivel_riesgo) {
  if (nivel_riesgo %in% c("Alto", "Muy Alto")) {
    return("Se recomienda implementar acciones de adaptación urgentes para reducir la vulnerabilidad de las pisciculturas ante los cambios climáticos proyectados. Esto incluye mejoras en sistemas de recirculación, monitoreo de la calidad del agua y diversificación de fuentes de abastecimiento.")
  } else if (nivel_riesgo == "Moderado") {
    return("Se sugiere implementar medidas preventivas de adaptación y mantener un monitoreo regular de las condiciones climáticas y su impacto en las operaciones acuícolas.")
  } else {
    return("Se recomienda mantener un monitoreo continuo de las condiciones climáticas y sus posibles efectos en las operaciones acuícolas, aunque el riesgo actual es bajo.")
  }
}

#' Función para obtener descripción de la amenaza climática
#' @return Texto con descripción
descripcion_amenaza <- function() {
  return("Este indicador considera cambios proyectados en:
  - Días con temperaturas superiores a 25°C.
  - Frecuencia de sequías (déficit de precipitación >75%).
  - Patrones de lluvia y precipitación media anual.")
}

#' Función para obtener descripción de la sensibilidad
#' @return Texto con descripción
descripcion_sensibilidad <- function() {
  return("Esto se debe a la proporción de bosque nativo versus usos agrícolas/forestales en las cuencas abastecedoras, lo que afecta la disponibilidad y calidad del agua.")
}

#' Función para generar el informe de riesgo acuícola
#' @param nombre_comuna Nombre de la comuna
#' @return Lista con el informe HTML y datos para visualización
generar_informe_riesgo_acuicultura <- function(nombre_comuna) {
  print(paste0("Generando informe de riesgo acuícola para ", nombre_comuna))
  
  # Cargar datos
  datos_cargados <- cargar_datos_riesgo_acuicultura()
  
  # Verificar que la comuna existe en los datos
  comunas_disponibles <- obtener_comunas_disponibles(datos_cargados$datos)
  if (!tolower(nombre_comuna) %in% tolower(comunas_disponibles)) {
    return(list(
      informe_html = paste0(
        "<h4>Informe de Riesgo Climático para la Comuna de ", nombre_comuna, "</h4>",
        "<p style='color: red;'><b>Error:</b> No se encontraron datos para la comuna especificada.</p>",
        "<p>Comunas disponibles: ", paste(comunas_disponibles[1:min(10, length(comunas_disponibles))], collapse = ", "),
        if(length(comunas_disponibles) > 10) "..." else "", "</p>"
      ),
      datos_visualizacion = NULL
    ))
  }
  
  # Obtener estadísticas para la comuna
  tryCatch({
    estadisticas <- obtener_estadisticas_comuna(datos_cargados$datos, nombre_comuna)
    
    # Obtener niveles predominantes
    nivel_amenaza <- estadisticas$amenaza$valor_predominante
    nivel_sensibilidad <- estadisticas$sensibilidad$valor_predominante
    nivel_riesgo <- estadisticas$riesgo$valor_predominante
    
    # Generar recomendación basada en el nivel de riesgo
    recomendacion <- generar_recomendacion(nivel_riesgo)
    
    # Extraer información de las pisciculturas para la tabla
    datos_comuna <- datos_cargados$datos %>% 
      filter(tolower(comuna) == tolower(nombre_comuna))
    
    # Crear tabla HTML con información de pisciculturas
    tabla_pisciculturas <- ""
    if ("ID" %in% names(datos_comuna) && "lat_piscicultura" %in% names(datos_comuna) && "lon_piscicultura" %in% names(datos_comuna)) {
      tabla_pisciculturas <- paste0(
        "<table class='table table-striped'>",
        "<thead><tr><th>ID Piscicultura</th><th>Latitud</th><th>Longitud</th><th>Nivel de Riesgo</th></tr></thead>",
        "<tbody>"
      )
      
      for (i in 1:nrow(datos_comuna)) {
        # Determinar el nivel de riesgo como texto
        nivel_riesgo_num <- datos_comuna$riesgo_acuicultura_aguadulce_combinado[i]
        nivel_riesgo_texto <- ""
        
        # Convertir valor numérico a texto descriptivo
        if (!is.na(nivel_riesgo_num)) {
          nivel_riesgo_categorias <- c("Muy Bajo", "Bajo", "Moderado", "Alto", "Muy Alto")
          if (nivel_riesgo_num >= 1 && nivel_riesgo_num <= length(nivel_riesgo_categorias)) {
            nivel_riesgo_texto <- nivel_riesgo_categorias[nivel_riesgo_num]
          } else {
            nivel_riesgo_texto <- as.character(nivel_riesgo_num)
          }
        } else {
          nivel_riesgo_texto <- "No disponible"
        }
        
        tabla_pisciculturas <- paste0(
          tabla_pisciculturas,
          "<tr><td>", datos_comuna$ID[i], "</td><td>", 
          round(datos_comuna$lat_piscicultura[i], 6), "</td><td>", 
          round(datos_comuna$lon_piscicultura[i], 6), "</td><td>", 
          nivel_riesgo_texto, "</td></tr>"
        )
      }
      
      tabla_pisciculturas <- paste0(tabla_pisciculturas, "</tbody></table>")
    } else {
      tabla_pisciculturas <- "<p><i>No hay información de coordenadas disponible para las pisciculturas.</i></p>"
    }
    
    # Crear el informe en formato HTML
    informe_html <- paste0(
      "<h4>Informe de Riesgo Climático para la Comuna de ", nombre_comuna, "</h4>",
      
      "<h5>Salmonicultura: Pérdida de producción por menor provisión de agua dulce</h5>",
      "<p>Los mapas describen el riesgo frente al cambio climático para la producción de huevos y juveniles de salmones en pisciculturas de agua dulce. El riesgo es una función que depende de las futuras tendencias hidro-climáticas (amenazas), la producción reportada en estas unidades de cultivo (exposición) y el tipo de cobertura de suelo presente en las cuencas que abastecen a las pisciculturas (sensibilidad).</p>",
      
      "<p>La salmonicultura en fase de agua dulce es particularmente sensible a los cambios en la disponibilidad de agua. Específicamente:</p>",
      "<ul>",
      "<li><b>Temperatura:</b> El aumento de las temperaturas puede afectar la supervivencia y desarrollo de huevos y alevines de salmón, que requieren aguas frías y bien oxigenadas.</li>",
      "<li><b>Sequías:</b> La reducción en el caudal de los ríos y aporte a las pisciculturas puede generar estrés en los peces, aumentar la concentración de contaminantes y reducir el oxígeno disponible.</li>",
      "<li><b>Cambios en la calidad del agua:</b> La modificación en los patrones de lluvia puede afectar la turbidez y la carga de sedimentos, impactando la calidad del agua necesaria para la producción.</li>",
      "</ul>",
      "<p>Las pisciculturas ubicadas en zonas con alta degradación de cuencas (menos cobertura boscosa y más intervención agrícola/forestal) son más vulnerables a estos cambios climáticos.</p>",
      
      "<hr>",
      
      "<h5>1. Pisciculturas Registradas:</h5>",
      "<p>La comuna cuenta con <b>", estadisticas$n_pisciculturas, " piscicultura(s)</b> en riesgo de ser afectadas por cambios climáticos. Esta es la información con que cuenta el ARClim al 2024.</p>",
      tabla_pisciculturas,
      
      "<h5>2. Amenaza Climática:</h5>",
      "<p>Nivel: <b>", nivel_amenaza, "</b> (", estadisticas$amenaza$porcentaje_predominante, "% de las pisciculturas).</p>",
      "<p>Este indicador considera cambios proyectados en:</p>",
      "<ul>",
      "<li>Días con temperaturas superiores a 25°C.</li>",
      "<li>Frecuencia de sequías (déficit de precipitación >75%).</li>",
      "<li>Patrones de lluvia y precipitación media anual.</li>",
      "</ul>",
      
      "<h5>3. Factores de Vulnerabilidad:</h5>",
      "<p>Sensibilidad del territorio: <b>", nivel_sensibilidad, "</b> (", estadisticas$sensibilidad$porcentaje_predominante, "% de las pisciculturas).</p>",
      "<p>Esto se debe a la proporción de <b>bosque nativo</b> versus <b>usos agrícolas/forestales</b> en las cuencas abastecedoras, lo que afecta la disponibilidad y calidad del agua.</p>",
      
      "<h5>4. Riesgo Climático Combinado:</h5>",
      "<p>Nivel de riesgo: <b>", nivel_riesgo, "</b> (", estadisticas$riesgo$porcentaje_predominante, "% de las pisciculturas).</p>",
      "<p><b>Recomendación:</b> ", recomendacion, "</p>"
    )
    
    # Crear datos para visualización
    datos_visualizacion <- data.frame(
      indicador = c("Amenaza", "Sensibilidad", "Riesgo"),
      nivel = c(nivel_amenaza, nivel_sensibilidad, nivel_riesgo),
      porcentaje = c(
        estadisticas$amenaza$porcentaje_predominante,
        estadisticas$sensibilidad$porcentaje_predominante,
        estadisticas$riesgo$porcentaje_predominante
      ),
      stringsAsFactors = FALSE
    )
    
    # Preparar datos de pisciculturas para el mapa
    datos_pisciculturas_mapa <- NULL
    if ("ID" %in% names(datos_comuna) && "lat_piscicultura" %in% names(datos_comuna) && "lon_piscicultura" %in% names(datos_comuna)) {
      datos_pisciculturas_mapa <- datos_comuna %>%
        select(ID, lat_piscicultura, lon_piscicultura, amen_acuicultura_aguadulce, 
               sensi_acuicultura_aguadulce, riesgo_acuicultura_aguadulce_combinado)
      
      # Convertir valores categóricos de riesgo a factores para asegurar mapeo de colores correcto
      if (!is.null(datos_pisciculturas_mapa) && nrow(datos_pisciculturas_mapa) > 0) {
        # Verificar si las coordenadas son utilizables (no NA)
        datos_pisciculturas_mapa <- datos_pisciculturas_mapa %>%
          filter(!is.na(lat_piscicultura) & !is.na(lon_piscicultura))
        
        # Agregar categorías si son numéricas
        if (is.numeric(datos_pisciculturas_mapa$riesgo_acuicultura_aguadulce_combinado)) {
          nivel_riesgo <- c("Muy Bajo", "Bajo", "Moderado", "Alto", "Muy Alto")
          datos_pisciculturas_mapa$riesgo_acuicultura_aguadulce_combinado <- 
            nivel_riesgo[datos_pisciculturas_mapa$riesgo_acuicultura_aguadulce_combinado]
        }
      }
    }
    
    return(list(
      informe_html = informe_html,
      datos_visualizacion = datos_visualizacion,
      estadisticas = estadisticas,
      pisciculturas = datos_pisciculturas_mapa
    ))
  }, error = function(e) {
    return(list(
      informe_html = paste0(
        "<h4>Informe de Riesgo Climático para la Comuna de ", nombre_comuna, "</h4>",
        "<p style='color: red;'><b>Error:</b> ", e$message, "</p>"
      ),
      datos_visualizacion = NULL,
      pisciculturas = NULL
    ))
  })
}

#' Función para generar gráfico de indicadores de riesgo acuícola
#' @param datos_visualizacion DataFrame con datos para visualización
#' @return Objeto ggplot
generar_grafico_indicadores_acuicultura <- function(datos_visualizacion) {
  # Definir orden de niveles para colores consistentes
  niveles_orden <- c("Muy Bajo", "Bajo", "Moderado", "Alto", "Muy Alto")
  
  # Asignar colores según el nivel
  colores <- c("Muy Bajo" = "#1a9641", "Bajo" = "#a6d96a", "Moderado" = "#ffffbf", 
              "Alto" = "#fdae61", "Muy Alto" = "#d7191c")
  
  # Convertir nivel a factor con orden específico
  datos_visualizacion$nivel <- factor(datos_visualizacion$nivel, levels = niveles_orden)
  
  # Generar gráfico
  p <- ggplot(datos_visualizacion, aes(x = indicador, y = porcentaje, fill = nivel)) +
    geom_bar(stat = "identity", width = 0.7) +
    geom_text(aes(label = paste0(porcentaje, "%")), vjust = -0.5, size = 4) +
    scale_fill_manual(values = colores) +
    labs(
      title = "Indicadores de Riesgo Acuícola",
      x = "",
      y = "Porcentaje de Pisciculturas (%)",
      fill = "Nivel"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title.y = element_text(size = 12),
      axis.text.x = element_text(size = 11, face = "bold"),
      legend.position = "bottom"
    ) +
    ylim(0, 100)
  
  return(p)
}

# Ejemplo de uso
# resultado <- generar_informe_riesgo_acuicultura("Puerto Montt")
# print(resultado$informe_html)
# print(generar_grafico_indicadores_acuicultura(resultado$datos_visualizacion)) 