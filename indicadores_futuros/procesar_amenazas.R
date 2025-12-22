# Explicitly import select from dplyr
select <- dplyr::select

#' Obtiene los valores de amenazas para una comuna específica y todas las categorías
#' @param nombre_comuna Nombre de la comuna
#' @param variable Nombre del indicador de amenaza
#' @return Lista con los valores y metadatos del indicador para todas las categorías
obtener_valores_amenazas <- function(nombre_comuna, variable) {
  # Ruta al archivo de datos
  ruta_archivo <- "BBDD/ARCLIM/indicadores_amenazas/amenazas_comunas_annual.xlsx"
  
  # Leer las hojas del archivo Excel
  variables_df <- read_excel(ruta_archivo, sheet = "VARIABLES")
  features_df <- read_excel(ruta_archivo, sheet = "FEATURES")
  datos_df <- read_excel(ruta_archivo, sheet = "DATOS")
  
  # Validar comuna
  if (!tolower(nombre_comuna) %in% tolower(features_df$NOM_COMUNA)) {
    stop("Comuna no encontrada en la base de datos")
  }
  
  # Obtener FEATURE ID e información de la comuna
  comuna_info <- features_df %>%
    filter(tolower(NOM_COMUNA) == tolower(nombre_comuna)) %>%
    slice(1)
  
  feature_id <- comuna_info$`FEATURE ID`
  nombre_comuna_original <- comuna_info$NOM_COMUNA
  region <- comuna_info$NOM_REGION
  
  # Obtener todas las comunas de la misma región
  comunas_region <- features_df %>%
    filter(NOM_REGION == region) %>%
    pull(`FEATURE ID`)
  
  # Validar variable
  if (!variable %in% variables_df$Nombre) {
    stop("Variable no encontrada en la base de datos")
  }
  
  # Obtener información para todas las categorías
  variable_info <- variables_df %>%
    filter(Nombre == variable)
  
  if (nrow(variable_info) == 0) {
    stop("Variable no encontrada")
  }
  
  # Procesar cada categoría
  resultados <- list()
  for (cat in c("Presente", "Futuro", "Cambio")) {
    info_categoria <- variable_info %>% filter(Categoria == cat)
    if (nrow(info_categoria) > 0) {
      indicador_id <- info_categoria$ID[1]
      valor <- datos_df[[as.character(indicador_id)]][datos_df$`FEATURE ID` == feature_id]
      
      # Calcular promedio nacional (todas las comunas)
      promedio_nacional <- mean(datos_df[[as.character(indicador_id)]], na.rm = TRUE)
      
      # Calcular promedio regional (solo comunas de la misma región)
      valores_region <- datos_df[[as.character(indicador_id)]][datos_df$`FEATURE ID` %in% comunas_region]
      promedio_regional <- mean(valores_region, na.rm = TRUE)
      
      resultados[[cat]] <- list(
        valor = valor,
        promedio_nacional = promedio_nacional,
        promedio_regional = promedio_regional,
        unidad = info_categoria$Unidad[1],
        codigo = info_categoria$Código[1],
        estacion = info_categoria$Estación[1]
      )
    }
  }
  
  # Retornar resultados completos
  list(
    comuna = nombre_comuna_original,
    region = region,
    variable = variable,
    resultados = resultados
  )
}

#' Genera un informe HTML con los resultados del análisis de amenazas
#' @param nombre_comuna Nombre de la comuna
#' @param variable Nombre del indicador de amenaza
#' @return Lista con el informe HTML y los datos utilizados
generar_informe_amenazas <- function(nombre_comuna, variable) {
  # Obtener datos
  datos <- obtener_valores_amenazas(nombre_comuna, variable)
  
  # Crear título para el informe
  titulo <- sprintf("Análisis de %s", datos$variable)
  
  # Crear secciones del informe para cada categoría
  secciones_html <- c()
  
  # Sección de valores actuales
  if (!is.null(datos$resultados$Presente)) {
    presente <- datos$resultados$Presente
    secciones_html <- c(secciones_html, sprintf('
      <div style="background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0;">
        <h3 style="color: #34495e;">Situación Actual (Período 1980-2010)</h3>
        <p>Valor actual: <strong>%.2f %s</strong></p>
        <p>Contexto regional y nacional:</p>
        <ul>
          <li>Promedio regional (%s): %.2f %s</li>
          <li>Promedio nacional: %.2f %s</li>
        </ul>
        <p>Comparación con promedios:</p>
        <ul>
          <li>Diferencia con promedio regional: %.2f %s</li>
          <li>Diferencia con promedio nacional: %.2f %s</li>
        </ul>
      </div>',
      presente$valor, presente$unidad,
      datos$region, presente$promedio_regional, presente$unidad,
      presente$promedio_nacional, presente$unidad,
      presente$valor - presente$promedio_regional, presente$unidad,
      presente$valor - presente$promedio_nacional, presente$unidad
    ))
  }
  
  # Sección de proyecciones futuras
  if (!is.null(datos$resultados$Futuro)) {
    futuro <- datos$resultados$Futuro
    secciones_html <- c(secciones_html, sprintf('
      <div style="background-color: #e9ecef; padding: 15px; border-radius: 5px; margin: 10px 0;">
        <h3 style="color: #34495e;">Proyección Futura (Período 2035-2065)</h3>
        <p>Valor proyectado: <strong>%.2f %s</strong></p>
        <p>Contexto regional y nacional:</p>
        <ul>
          <li>Promedio regional proyectado (%s): %.2f %s</li>
          <li>Promedio nacional proyectado: %.2f %s</li>
        </ul>
        <p>Comparación con promedios:</p>
        <ul>
          <li>Diferencia con promedio regional: %.2f %s</li>
          <li>Diferencia con promedio nacional: %.2f %s</li>
        </ul>
      </div>',
      futuro$valor, futuro$unidad,
      datos$region, futuro$promedio_regional, futuro$unidad,
      futuro$promedio_nacional, futuro$unidad,
      futuro$valor - futuro$promedio_regional, futuro$unidad,
      futuro$valor - futuro$promedio_nacional, futuro$unidad
    ))
  }
  
  # Sección de cambios proyectados
  if (!is.null(datos$resultados$Cambio)) {
    cambio <- datos$resultados$Cambio
    magnitud_regional <- if (abs(cambio$valor) > abs(cambio$promedio_regional)) "mayor" else "menor"
    magnitud_nacional <- if (abs(cambio$valor) > abs(cambio$promedio_nacional)) "mayor" else "menor"
    
    secciones_html <- c(secciones_html, sprintf('
      <div style="background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0;">
        <h3 style="color: #34495e;">Cambio Proyectado (Entre 1980-2010 y 2035-2065)</h3>
        <p>Cambio esperado: <strong>%.2f %s</strong></p>
        <p>Contexto regional y nacional:</p>
        <ul>
          <li>Promedio de cambio regional (%s): %.2f %s</li>
          <li>Promedio de cambio nacional: %.2f %s</li>
        </ul>
        <p>Análisis comparativo:</p>
        <ul>
          <li>El cambio proyectado para %s es <strong>%s</strong> que el promedio de su región (%s)</li>
          <li>El cambio proyectado es <strong>%s</strong> que el promedio nacional</li>
        </ul>
      </div>',
      cambio$valor, cambio$unidad,
      datos$region, cambio$promedio_regional, cambio$unidad,
      cambio$promedio_nacional, cambio$unidad,
      datos$comuna, magnitud_regional, datos$region,
      magnitud_nacional
    ))
  }
  
  # Generar HTML completo
  informe_html <- sprintf('
    <div style="font-family: Arial, sans-serif;">
      <h2 style="color: #2c3e50;">%s</h2>
      <h3 style="color: #34495e;">Comuna: %s (%s)</h3>
      %s
      <div style="background-color: #e9ecef; padding: 15px; border-radius: 5px; margin: 10px 0;">
        <p style="font-size: 0.9em; color: #666;">
          <strong>Nota sobre los períodos y promedios:</strong><br>
          - El período presente corresponde al período histórico 1980-2010.<br>
          - El período futuro corresponde a proyecciones climáticas para 2035-2065.<br>
          - El promedio regional considera solo las comunas de la región %s.<br>
          - El promedio nacional considera todas las comunas del país.
        </p>
      </div>
    </div>
  ', titulo, datos$comuna, datos$region, paste(secciones_html, collapse = "\n"), datos$region)
  
  # Retornar resultados
  list(
    informe_html = informe_html,
    datos = datos
  )
}

# Ejemplos de uso
if (FALSE) {
  # Ejemplo 1: Generar informe completo para Puerto Varas
  resultado <- generar_informe_amenazas(
    nombre_comuna = "PUERTO VARAS",
    variable = "Día más frío"
  )
  cat(resultado$informe_html)
  
  # Ejemplo 2: Obtener datos directamente
  datos <- obtener_valores_amenazas(
    nombre_comuna = "PUERTO VARAS",
    variable = "Precipitación anual"
  )
  print(datos)
} 