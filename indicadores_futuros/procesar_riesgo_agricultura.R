# Script para procesar indicadores de riesgo agrícola

# Cargar librerías necesarias
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# Lista de sectores productivos válidos
sectores_productivos <- c(
  "almendras",
  "bovinos_carne",
  "bovinos_leche",
  "cereza",
  "frejol",
  "maiz",
  "manzana_roja",
  "nueces",
  "ovinos",
  "papa_riego",
  "papa_secano",
  "pradera",
  "trigo_riego",
  "trigo_secano"
)

#' Función para convertir nombre de comuna a código
#' @param nombre_comuna Nombre de la comuna
#' @return Código de la comuna
convertir_nombre_a_codigo <- function(nombre_comuna) {
  # Ruta al archivo de datos agrícolas que contiene los códigos de comuna
  ruta_archivo <- "BBDD/ARCLIM/riesgo/Agricultura/ARCLIM_agricultura.xlsx"
  
  # Leer la hoja de DATOS que contiene los códigos y nombres de comuna
  datos <- read_excel(ruta_archivo, sheet = "DATOS")
  
  # Buscar el código de la comuna (case insensitive)
  codigo <- datos %>%
    filter(tolower(NOM_COMUNA) == tolower(nombre_comuna)) %>%
    pull(COD_COMUNA)
  
  if (length(codigo) == 0) {
    stop(paste("No se encontró el código para la comuna:", nombre_comuna))
  }
  
  return(codigo)
}

#' Función para cargar y procesar datos de riesgo agrícola
#' @param ruta_archivo Ruta al archivo Excel
#' @return Lista con metadatos y datos procesados
cargar_datos_riesgo <- function(ruta_archivo) {
  print("Cargando datos de riesgo agrícola...")
  
  # Cargar las hojas del archivo Excel
  metadatos <- read_excel(ruta_archivo, sheet = "METADATOS")
  datos <- read_excel(ruta_archivo, sheet = "DATOS")
  
  # Verificar que las columnas necesarias existen
  required_cols_meta <- c("ID Atributo", "Descripcion", "Nombre", "Unidad")
  required_cols_data <- c("COD_COMUNA", "NOM_COMUNA")
  
  if (!all(required_cols_meta %in% names(metadatos))) {
    stop("La hoja METADATOS no contiene las columnas requeridas")
  }
  
  if (!all(required_cols_data %in% names(datos))) {
    stop("La hoja DATOS no contiene las columnas requeridas")
  }
  
  return(list(
    metadatos = metadatos,
    datos = datos
  ))
}

#' Función para obtener descripción de indicadores por categoría
#' @param metadatos DataFrame con metadatos
#' @param sector_productivo Sector productivo seleccionado
#' @return Lista con descripciones por categoría
obtener_descripciones <- function(metadatos, sector_productivo) {
  # Función auxiliar para obtener y limpiar descripción
  obtener_descripcion <- function(tipo) {
    desc <- metadatos %>%
      filter(grepl(sector_productivo, `ID Atributo`) & grepl(tipo, `ID Atributo`)) %>%
      pull(Descripcion)
    
    # Reemplazar "mapa" por "indicador"
    if (length(desc) > 0) {
      desc <- str_replace_all(desc, "(?i)mapa", "indicador")
      desc <- str_replace_all(desc, "(?i)Este indicador representa", "Este valor representa")
    }
    return(desc)
  }
  
  descripciones <- list(
    amenaza = obtener_descripcion("amen"),
    exposicion = obtener_descripcion("expo"),
    oportunidad = obtener_descripcion("oportunidad"),
    riesgo = obtener_descripcion("riesgo"),
    sensibilidad = obtener_descripcion("sensi")
  )
  
  return(descripciones)
}

#' Función para calcular estadísticas de indicadores
#' @param datos DataFrame con datos
#' @param sector_productivo Sector productivo seleccionado
#' @return Lista con estadísticas por categoría
calcular_estadisticas <- function(datos, sector_productivo) {
  # Función auxiliar para calcular estadísticas de un indicador
  calcular_stats <- function(patron_columna) {
    col_idx <- grep(patron_columna, colnames(datos))
    if (length(col_idx) == 0) return(NULL)
    
    # Obtener valores del indicador para todas las comunas
    valores_indicador <- datos[[col_idx]]
    valores_validos <- valores_indicador[!is.na(valores_indicador)]
    
    if (length(valores_validos) == 0) return(NULL)
    
    # Calcular estadísticas básicas
    list(
      promedio = mean(valores_validos),
      n_comunas = length(valores_validos)
    )
  }
  
  # Calcular estadísticas para cada tipo de indicador
  stats <- list(
    amenaza = calcular_stats(paste0("amen_agri_", sector_productivo)),
    exposicion = calcular_stats(paste0("expo_agri_", sector_productivo)),
    oportunidad = calcular_stats(paste0("oportunidad_agri_", sector_productivo)),
    sensibilidad = calcular_stats(paste0("sensi_agri_", sector_productivo)),
    riesgo = calcular_stats(paste0("riesgo_agri_", sector_productivo))
  )
  
  return(stats)
}

#' Función para obtener valores de indicadores para una comuna
#' @param datos DataFrame con datos
#' @param codigo_comuna Código de la comuna
#' @param sector_productivo Sector productivo seleccionado
#' @return Lista con valores de indicadores y estadísticas
obtener_valores_comuna <- function(datos, codigo_comuna, sector_productivo) {
  # Validar que el sector productivo sea válido
  if (!sector_productivo %in% sectores_productivos) {
    stop(paste("Sector productivo no válido. Los sectores válidos son:", paste(sectores_productivos, collapse = ", ")))
  }
  
  # Calcular estadísticas para todos los indicadores
  stats <- calcular_estadisticas(datos, sector_productivo)
  
  # Filtrar datos para la comuna
  fila_comuna <- datos %>% filter(COD_COMUNA == codigo_comuna)
  
  if (nrow(fila_comuna) == 0) {
    stop(paste("No se encontraron datos para la comuna con código:", codigo_comuna))
  }
  
  # Función auxiliar para obtener valor seguro
  obtener_valor_seguro <- function(patron_columna) {
    col_idx <- grep(patron_columna, colnames(datos))
    if (length(col_idx) == 0) return(NA)
    return(fila_comuna[[col_idx]])
  }
  
  # Obtener valores y estadísticas de indicadores
  valores <- list(
    amenaza = list(
      valor = obtener_valor_seguro(paste0("amen_agri_", sector_productivo)),
      stats = stats$amenaza
    ),
    exposicion = list(
      valor = obtener_valor_seguro(paste0("expo_agri_", sector_productivo)),
      stats = stats$exposicion
    ),
    oportunidad = list(
      valor = obtener_valor_seguro(paste0("oportunidad_agri_", sector_productivo)),
      stats = stats$oportunidad
    ),
    sensibilidad = list(
      valor = obtener_valor_seguro(paste0("sensi_agri_", sector_productivo)),
      stats = stats$sensibilidad
    ),
    riesgo = list(
      valor = obtener_valor_seguro(paste0("riesgo_agri_", sector_productivo)),
      stats = stats$riesgo
    )
  )
  
  return(valores)
}

#' Función para obtener unidades de los indicadores
#' @param metadatos DataFrame con metadatos
#' @param sector_productivo Sector productivo seleccionado
#' @return Lista con unidades por categoría
obtener_unidades <- function(metadatos, sector_productivo) {
  obtener_unidad <- function(tipo) {
    metadatos %>%
      filter(grepl(sector_productivo, `ID Atributo`) & grepl(tipo, `ID Atributo`)) %>%
      pull(Unidad)
  }
  
  unidades <- list(
    amenaza = obtener_unidad("amen"),
    exposicion = obtener_unidad("expo"),
    oportunidad = obtener_unidad("oportunidad"),
    sensibilidad = obtener_unidad("sensi"),
    riesgo = obtener_unidad("riesgo")
  )
  
  return(unidades)
}

#' Función para generar el informe de riesgo agrícola
#' @param nombre_comuna Nombre de la comuna
#' @param sector_productivo Sector productivo seleccionado
#' @return Lista con el informe y datos para visualización
generar_informe_riesgo <- function(nombre_comuna, sector_productivo) {
  print(paste0("Generando informe de riesgo agrícola para ", nombre_comuna, " - ", sector_productivo))
  
  # Convertir nombre de comuna a código
  codigo_comuna <- convertir_nombre_a_codigo(nombre_comuna)
  
  # Cargar datos
  ruta_archivo <- "BBDD/ARCLIM/riesgo/Agricultura/ARCLIM_agricultura.xlsx"
  datos_cargados <- cargar_datos_riesgo(ruta_archivo)
  
  # Obtener descripciones, unidades y valores
  descripciones <- obtener_descripciones(datos_cargados$metadatos, sector_productivo)
  unidades <- obtener_unidades(datos_cargados$metadatos, sector_productivo)
  valores <- obtener_valores_comuna(datos_cargados$datos, codigo_comuna, sector_productivo)
  
  # Función auxiliar para formatear sección del informe
  formatear_seccion <- function(tipo, valor_info, descripcion, unidad = "") {
    if (is.na(valor_info$valor)) {
      return(paste0(
        "<p><b>", str_to_title(tipo), ":</b> ",
        "No se encontraron datos de ", tolower(tipo), " para ", sector_productivo, " en la comuna de ", nombre_comuna,
        "</p>"
      ))
    }
    
    # Formatear información comparativa si están disponibles las estadísticas
    comparacion_text <- ""
    if (!is.null(valor_info$stats)) {
      diferencia <- valor_info$valor - valor_info$stats$promedio
      comparacion <- if (diferencia > 0) {
        "superior"
      } else if (diferencia < 0) {
        "inferior"
      } else {
        "igual"
      }
      
      comparacion_text <- paste0(
        " Este valor es ", comparacion, " al promedio de ", 
        round(valor_info$stats$promedio, 2), " ", unidad,
        " calculado para las ", valor_info$stats$n_comunas, 
        " comunas que registran este indicador"
      )
    }
    
    paste0(
      "<p><b>", str_to_title(tipo), ":</b> La ", tolower(tipo), " del sector ", sector_productivo,
      " es ", round(valor_info$valor, 2), " ", unidad, ".", comparacion_text, "</p>",
      "<p><i>", descripcion, "</i></p>"
    )
  }
  
  # Crear el informe en formato HTML
  informe_html <- paste0(
    "<h4>Informe de Riesgo Agrícola para ", nombre_comuna, "</h4>",
    "<p><b>Sector Productivo:</b> ", sector_productivo, "</p>",
    
    "<h5>Indicadores de Riesgo</h5>",
    formatear_seccion("amenaza", valores$amenaza, descripciones$amenaza, unidades$amenaza),
    formatear_seccion("exposicion", valores$exposicion, descripciones$exposicion, unidades$exposicion),
    formatear_seccion("oportunidad", valores$oportunidad, descripciones$oportunidad, unidades$oportunidad),
    formatear_seccion("sensibilidad", valores$sensibilidad, descripciones$sensibilidad, unidades$sensibilidad),
    formatear_seccion("riesgo", valores$riesgo, descripciones$riesgo, unidades$riesgo)
  )
  
  # Crear datos para visualización (solo incluir valores no NA)
  indicadores <- c()
  valores_viz <- c()
  descripciones_viz <- c()
  
  if (!is.na(valores$amenaza$valor)) {
    indicadores <- c(indicadores, "Amenaza")
    valores_viz <- c(valores_viz, valores$amenaza$valor)
    descripciones_viz <- c(descripciones_viz, descripciones$amenaza)
  }
  if (!is.na(valores$exposicion$valor)) {
    indicadores <- c(indicadores, "Exposición")
    valores_viz <- c(valores_viz, valores$exposicion$valor)
    descripciones_viz <- c(descripciones_viz, descripciones$exposicion)
  }
  if (!is.na(valores$oportunidad$valor)) {
    indicadores <- c(indicadores, "Oportunidad")
    valores_viz <- c(valores_viz, valores$oportunidad$valor)
    descripciones_viz <- c(descripciones_viz, descripciones$oportunidad)
  }
  if (!is.na(valores$sensibilidad$valor)) {
    indicadores <- c(indicadores, "Sensibilidad")
    valores_viz <- c(valores_viz, valores$sensibilidad$valor)
    descripciones_viz <- c(descripciones_viz, descripciones$sensibilidad)
  }
  if (!is.na(valores$riesgo$valor)) {
    indicadores <- c(indicadores, "Riesgo")
    valores_viz <- c(valores_viz, valores$riesgo$valor)
    descripciones_viz <- c(descripciones_viz, descripciones$riesgo)
  }
  
  datos_visualizacion <- data.frame(
    indicador = indicadores,
    valor = valores_viz,
    descripcion = descripciones_viz
  )
  
  return(list(
    informe_html = informe_html,
    datos_visualizacion = datos_visualizacion
  ))
}

#' Función para generar gráfico de indicadores
#' @param datos_visualizacion DataFrame con datos para visualización
#' @return Objeto ggplot
generar_grafico_indicadores <- function(datos_visualizacion) {
  p <- ggplot(datos_visualizacion, aes(x = reorder(indicador, valor), y = valor)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(
      title = "Indicadores de Riesgo Agrícola",
      x = "Indicador",
      y = "Valor"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    )
  
  return(p)
}

# Ejemplo de uso
# resultado <- generar_informe_riesgo("Lonquimay", "bovinos_leche")
# print(resultado$informe_html)
# print(generar_grafico_indicadores(resultado$datos_visualizacion)) 