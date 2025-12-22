# Cargar librerías necesarias

# Función para obtener los datos de riesgo de incendios forestales
obtener_datos_incendios <- function() {
  ruta_archivo <- "BBDD/ARCLIM/riesgo/Bosques_nativos/ARCLIM_incendios_bosques_nativos_comunas.xlsx"
  
  # Leer hojas del archivo Excel
  datos <- read_excel(ruta_archivo, sheet = "DATOS")
  metadatos <- read_excel(ruta_archivo, sheet = "METADATOS")
  
  list(
    datos = datos,
    metadatos = metadatos
  )
}

# Función para obtener los valores de una comuna específica
obtener_valores_comuna <- function(nombre_comuna, tipo_bosque, periodo) {
  # Obtener datos
  datos_completos <- obtener_datos_incendios()
  datos <- datos_completos$datos
  metadatos <- datos_completos$metadatos
  
  # Filtrar datos por comuna (case insensitive)
  datos_comuna <- datos %>%
    filter(tolower(NOM_COMUNA) == tolower(nombre_comuna))
  
  if (nrow(datos_comuna) == 0) {
    stop(paste("No se encontraron datos para la comuna:", nombre_comuna))
  }
  
  # Guardar el nombre original de la comuna como aparece en los datos
  nombre_comuna_original <- datos_comuna$NOM_COMUNA[1]
  
  # Construir patrones de ID para los diferentes tipos de indicadores
  patrones <- list(
    amenaza = paste0("bosques_incendios_", tipo_bosque, "_amen_", periodo),
    exposicion = paste0("bosques_incendios_", tipo_bosque, "_expo"),  # exposición no tiene período
    sensibilidad = paste0("bosques_incendios_", tipo_bosque, "_sensi"),
    riesgo = paste0("bosques_incendios_", tipo_bosque, "_riesgo_", periodo)
  )
  
  # Obtener valores y metadatos para cada indicador
  valores <- list()
  for (tipo in names(patrones)) {
    patron <- patrones[[tipo]]
    columnas_coincidentes <- grep(patron, names(datos_comuna), value = TRUE)
    
    for (columna in columnas_coincidentes) {
      # Obtener metadatos del indicador
      meta <- metadatos %>%
        filter(`ID Atributo` == columna)
      
      if (nrow(meta) > 0) {
        valores[[columna]] <- list(
          valor = datos_comuna[[columna]],
          nombre = meta$Nombre,
          descripcion = meta$Descripcion,
          unidad = meta$Unidad
        )
      }
    }
  }
  
  # Calcular estadísticas para cada tipo de indicador
  estadisticas <- list()
  for (tipo in names(patrones)) {
    patron <- patrones[[tipo]]
    columnas_coincidentes <- grep(patron, names(datos), value = TRUE)
    
    if (length(columnas_coincidentes) > 0) {
      valores_todas_comunas <- datos %>%
        select(all_of(columnas_coincidentes)) %>%
        summarise(across(everything(), 
                        list(
                          promedio = ~mean(., na.rm = TRUE),
                          n_comunas = ~sum(!is.na(.))
                        )))
      
      estadisticas[[tipo]] <- valores_todas_comunas
    }
  }
  
  list(
    valores = valores,
    estadisticas = estadisticas,
    comuna = nombre_comuna_original,  # Usar el nombre original de la comuna
    tipo_bosque = tipo_bosque,
    periodo = periodo
  )
}

# Función para generar el informe de riesgo
generar_informe_riesgo <- function(nombre_comuna, tipo_bosque, periodo) {
  # Validar parámetros
  if (!tipo_bosque %in% c("bnativos", "plantaciones")) {
    stop("tipo_bosque debe ser 'bnativos' o 'plantaciones'")
  }
  if (!periodo %in% c("fut", "pres")) {
    stop("periodo debe ser 'fut' o 'pres'")
  }
  
  # Obtener datos
  resultado <- obtener_valores_comuna(nombre_comuna, tipo_bosque, periodo)
  
  # Inicializar el informe HTML
  informe_html <- paste0(
    "<h3>Informe de Riesgo de Incendios Forestales</h3>",
    "<p><strong>Comuna:</strong> ", resultado$comuna, "</p>",
    "<p><strong>Tipo de Bosque:</strong> ", 
    if(tipo_bosque == "bnativos") "Bosques Nativos" else "Plantaciones", "</p>",
    "<p><strong>Período:</strong> ", 
    if(periodo == "fut") "Futuro" else "Presente", "</p>"
  )
  
  # Función auxiliar para formatear la comparación con el promedio
  formatear_comparacion <- function(valor, promedio, n_comunas, unidad) {
    if (is.na(valor) || is.na(promedio)) return("No hay datos suficientes para la comparación")
    
    diferencia <- valor - promedio
    comparacion <- if (abs(diferencia) < 0.01) {
      "igual al"
    } else if (diferencia > 0) {
      "superior al"
    } else {
      "inferior al"
    }
    
    sprintf(
      "Este valor es %s promedio de %.2f %s (calculado con datos de %d comunas).",
      comparacion, promedio, unidad, n_comunas
    )
  }
  
  # Procesar cada tipo de indicador
  tipos_indicadores <- c("amenaza", "exposicion", "sensibilidad", "riesgo")
  
  for (tipo in tipos_indicadores) {
    # Encontrar indicadores del tipo actual
    indicadores <- resultado$valores[grep(paste0("_", substr(tipo, 1, 4)), names(resultado$valores))]
    
    if (length(indicadores) > 0) {
      informe_html <- paste0(
        informe_html,
        "<h4>", str_to_title(tipo), "</h4>"
      )
      
      for (nombre_indicador in names(indicadores)) {
        indicador <- indicadores[[nombre_indicador]]
        
        # Obtener estadísticas
        estadisticas_tipo <- resultado$estadisticas[[tipo]]
        if (!is.null(estadisticas_tipo)) {
          promedio_col <- paste0(nombre_indicador, "_promedio")
          n_comunas_col <- paste0(nombre_indicador, "_n_comunas")
          
          promedio <- estadisticas_tipo[[promedio_col]]
          n_comunas <- estadisticas_tipo[[n_comunas_col]]
          
          # Generar texto de comparación
          comparacion <- formatear_comparacion(
            indicador$valor, 
            promedio, 
            n_comunas, 
            indicador$unidad
          )
          
          # Agregar al informe
          informe_html <- paste0(
            informe_html,
            "<p><strong>", indicador$nombre, ":</strong> ",
            sprintf("%.2f %s. ", indicador$valor, indicador$unidad),
            comparacion,
            "<br><em>", indicador$descripcion, "</em></p>"
          )
        }
      }
    }
  }
  
  # Retornar el resultado
  list(
    informe_html = informe_html,
    datos = resultado
  )
}

# Ejemplos de uso del script
if (FALSE) {  # Este bloque no se ejecutará automáticamente
  # Ejemplo 1: Generar informe para bosques nativos en período presente
  resultado_bnativos_pres <- generar_informe_riesgo(
    nombre_comuna = "Puerto Varas",
    tipo_bosque = "bnativos",
    periodo = "fut"
  )
  
  # Mostrar el informe en HTML
  cat(resultado_bnativos_pres$informe_html)
  
  # Ejemplo 2: Generar informe para plantaciones en período futuro
  resultado_plantaciones_fut <- generar_informe_riesgo(
    nombre_comuna = "PUERTO VARAS",
    tipo_bosque = "plantaciones",
    periodo = "fut"
  )
  
  # Mostrar el informe en HTML
  cat(resultado_plantaciones_fut$informe_html)
  
  # Ejemplo 3: Acceder a los datos brutos
  # Obtener valores específicos del resultado
  datos_bnativos <- resultado_bnativos_pres$datos$valores
  estadisticas_bnativos <- resultado_bnativos_pres$datos$estadisticas
  
  # Ejemplo 4: Obtener solo los datos sin generar informe
  datos_comuna <- obtener_valores_comuna(
    nombre_comuna = "PUERTO VARAS",
    tipo_bosque = "bnativos",
    periodo = "pres"
  )
} 