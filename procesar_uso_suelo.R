# Script para procesar datos de uso de suelo
library(sf)
library(dplyr)
library(readr)

# Load metadata once at script level for efficiency
METADATA <- NULL
try({
  METADATA <- read.csv(paste0(getwd(), "/BBDD/ARCLIM/metadatos_comunas.csv"), 
                      sep = ";", 
                      stringsAsFactors = FALSE,
                      encoding = "UTF-8")
}, silent = TRUE)

#' Get region number for a given comuna
#' @param nombre_comuna Character string with the comuna name
#' @return Integer with the region number
get_region_number <- function(nombre_comuna) {
  if (is.null(METADATA)) {
    stop("No se pudo cargar el archivo de metadatos de comunas")
  }
  
  # Clean and standardize input comuna name
  nombre_comuna_clean <- toupper(trimws(nombre_comuna))
  
  # Find exact match first
  region_info <- METADATA[toupper(METADATA$nombre_comuna) == nombre_comuna_clean, ]
  
  if (nrow(region_info) == 0) {
    print("Comunas disponibles en metadata:")
    print(head(sort(unique(METADATA$nombre_comuna)), 10))
    stop(paste("No se encontró la comuna:", nombre_comuna))
  }
  
  if (nrow(region_info) > 1) {
    warning(paste("Se encontró más de una coincidencia para la comuna:", nombre_comuna))
  }
  
  return(region_info$codigo_region[1])
}

#' Find the most recent land use shapefile folder for a given region
#' @param region_number Integer with the region number
#' @return Character string with the folder path
find_land_use_folder <- function(region_number) {
  base_path <- paste0(getwd(), "/BBDD/usos_suelo")
  if (!dir.exists(base_path)) {
    stop("No se encontró el directorio de datos de uso de suelo")
  }
  
  # List and filter folders
  folders <- list.dirs(base_path, full.names = FALSE, recursive = FALSE)
  region_pattern <- paste0("cut_.*_r", region_number, "$")
  region_folders <- folders[grep(region_pattern, folders)]
  
  if (length(region_folders) == 0) {
    stop(paste("No se encontraron datos para la región:", region_number))
  }
  
  # Parse dates and find most recent
  folder_dates <- lapply(region_folders, function(folder) {
    parts <- strsplit(folder, "_")[[1]]
    if (length(parts) < 4) return(NULL)
    
    start_year <- as.numeric(parts[2])
    end_year <- as.numeric(parts[3])
    
    if (is.na(start_year) || is.na(end_year)) return(NULL)
    
    list(
      folder = folder,
      start_year = start_year,
      end_year = end_year
    )
  })
  
  # Remove NULL entries
  folder_dates <- folder_dates[!sapply(folder_dates, is.null)]
  
  if (length(folder_dates) == 0) {
    stop(paste("No se encontraron carpetas con formato válido para la región:", region_number))
  }
  
  # Get most recent
  most_recent <- folder_dates[[which.max(sapply(folder_dates, function(x) x$end_year))]]
  return(file.path(base_path, most_recent$folder))
}

#' Read land use shapefile for a given comuna
#' @param nombre_comuna Character string with the comuna name
#' @return sf object with land use data for the comuna
get_land_use_data <- function(nombre_comuna) {
  print(paste("Procesando datos de uso de suelo para comuna:", nombre_comuna))
  
  # Get region number
  region_number <- tryCatch({
    get_region_number(nombre_comuna)
  }, error = function(e) {
    stop(paste("Error al obtener número de región:", e$message))
  })
  
  print(paste("Comuna pertenece a la región:", region_number))
  
  # Find and validate folder
  folder_path <- tryCatch({
    find_land_use_folder(region_number)
  }, error = function(e) {
    stop(paste("Error al encontrar carpeta de uso de suelo:", e$message))
  })
  
  print(paste("Usando datos de la carpeta:", basename(folder_path)))
  
  # Find shapefile
  shapefile_path <- list.files(folder_path, pattern = "\\.shp$", full.names = TRUE)[1]
  if (is.na(shapefile_path)) {
    stop("No se encontró archivo shapefile en la carpeta")
  }
  
  print(paste("Leyendo shapefile:", basename(shapefile_path)))
  
  # Read and filter data
  land_use_data <- st_read(shapefile_path, quiet = TRUE)
  
  # Verify that the NOM_COM column exists
  if (!"NOM_COM" %in% names(land_use_data)) {
    print("Columnas disponibles en el shapefile:")
    print(names(land_use_data))
    stop("La columna 'NOM_COM' no existe en el shapefile")
  }
  
  # Clean and standardize names for comparison
  nombre_comuna_clean <- toupper(trimws(nombre_comuna))
  comuna_data <- land_use_data %>%
    filter(toupper(trimws(.data$NOM_COM)) == nombre_comuna_clean)
  
  if (nrow(comuna_data) == 0) {
    print("Comunas disponibles en el shapefile:")
    print(head(sort(unique(land_use_data$NOM_COM)), 10))
    stop(paste("No se encontraron datos para la comuna:", nombre_comuna))
  }
  
  print("Datos de uso de suelo cargados exitosamente")
  return(comuna_data)
}

#' Get available comunas for land use data
#' @return Character vector with available comuna names
get_available_comunas_land_use <- function() {
  if (is.null(METADATA)) {
    stop("No se pudo cargar el archivo de metadatos de comunas")
  }
  
  # Get comunas from metadata that have corresponding shapefiles
  available_comunas <- character(0)
  
  for (region_code in unique(METADATA$codigo_region)) {
    tryCatch({
      folder_path <- find_land_use_folder(region_code)
      shapefile_path <- list.files(folder_path, pattern = "\\.shp$", full.names = TRUE)[1]
      
      if (!is.na(shapefile_path)) {
        data <- st_read(shapefile_path, quiet = TRUE)
        if ("NOM_COM" %in% names(data)) {
          available_comunas <- c(available_comunas, unique(data$NOM_COM))
        }
      }
    }, error = function(e) {
      # Skip regions with no data
    })
  }
  
  return(sort(unique(available_comunas)))
}

# Example usage:
# comunas_disponibles <- get_available_comunas_land_use()
# datos_comuna <- get_land_use_data("puerto varas") 
