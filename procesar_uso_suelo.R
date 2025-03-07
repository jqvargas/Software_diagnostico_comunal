# Script para procesar datos de uso de suelo
library(sf)
library(dplyr)
library(readr)

#' Get region number for a given comuna
#' @param nombre_comuna Character string with the comuna name
#' @return Integer with the region number
get_region_number <- function(nombre_comuna) {
  # Read metadata file
  metadata_path <- paste0(getwd(), "/BBDD/ARCLIM/metadatos_comunas.csv")
  if (!file.exists(metadata_path)) {
    stop("No se encontró el archivo de metadatos de comunas")
  }
  
  metadata <- read.csv(metadata_path, sep = ";", stringsAsFactors = FALSE)
  
  # Find region number for the comuna
  region_info <- metadata %>%
    filter(tolower(nombre_comuna) == tolower(nombre_comuna)) %>%
    select(codigo_region)
  
  if (nrow(region_info) == 0) {
    stop(paste("No se encontró la comuna:", nombre_comuna))
  }
  
  return(region_info$codigo_region[1])
}

#' Find the most recent land use shapefile folder for a given region
#' @param region_number Integer with the region number
#' @return Character string with the folder path
find_land_use_folder <- function(region_number) {
  # Base path for land use data
  base_path <- paste0(getwd(), "/BBDD/usos_suelo")
  
  # List all folders
  folders <- list.dirs(base_path, full.names = FALSE, recursive = FALSE)
  
  # Filter folders for the specific region
  region_pattern <- paste0("cut_.*_r", region_number, "$")
  region_folders <- folders[grep(region_pattern, folders)]
  
  if (length(region_folders) == 0) {
    stop(paste("No se encontraron carpetas de uso de suelo para la región:", region_number))
  }
  
  # Extract dates and find most recent
  folder_dates <- lapply(region_folders, function(folder) {
    parts <- strsplit(folder, "_")[[1]]
    list(
      folder = folder,
      start_year = as.numeric(parts[2]),
      end_year = as.numeric(parts[3])
    )
  })
  
  # Sort by end year and get most recent
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
  
  # Find correct folder
  folder_path <- tryCatch({
    find_land_use_folder(region_number)
  }, error = function(e) {
    stop(paste("Error al encontrar carpeta de uso de suelo:", e$message))
  })
  
  print(paste("Usando datos de la carpeta:", basename(folder_path)))
  
  # Read shapefile
  shapefile_path <- list.files(folder_path, pattern = "\\.shp$", full.names = TRUE)[1]
  if (is.na(shapefile_path)) {
    stop("No se encontró archivo shapefile en la carpeta")
  }
  
  print(paste("Leyendo shapefile:", basename(shapefile_path)))
  
  # Read the shapefile
  land_use_data <- st_read(shapefile_path, quiet = TRUE)
  
  # Filter for the specific comuna
  comuna_data <- land_use_data %>%
    filter(tolower(Comuna) == tolower(nombre_comuna))
  
  if (nrow(comuna_data) == 0) {
    stop(paste("No se encontraron datos para la comuna:", nombre_comuna))
  }
  
  print("Datos de uso de suelo cargados exitosamente")
  
  return(comuna_data)
}

#' Get available comunas for land use data
#' @return Character vector with available comuna names
get_available_comunas_land_use <- function() {
  # Base path for land use data
  base_path <- paste0(getwd(), "/BBDD/usos_suelo")
  
  # Get all shapefiles
  all_shapefiles <- list.files(base_path, 
                              pattern = "\\.shp$", 
                              recursive = TRUE, 
                              full.names = TRUE)
  
  # Read and combine comuna names from all shapefiles
  all_comunas <- unique(unlist(lapply(all_shapefiles, function(shapefile) {
    tryCatch({
      data <- st_read(shapefile, quiet = TRUE)
      unique(data$Comuna)
    }, error = function(e) {
      warning(paste("Error reading shapefile:", shapefile, "-", e$message))
      NULL
    })
  })))
  
  return(sort(all_comunas))
}

# Example usage:
# comunas_disponibles <- get_available_comunas_land_use()
# datos_comuna <- get_land_use_data("Santiago") 