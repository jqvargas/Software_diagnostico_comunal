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

#' Read and simplify land use shapefile for a given comuna
#' @param nombre_comuna Character string with the comuna name
#' @return sf object with simplified land use data for the comuna
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
  
  # Read data
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
  
  # Define the IPCC columns we want to keep
  ipcc_columns <- c(
    "SUB_IPCC01",  # 2001
    "SUB_IPCC13",  # 2013
    "SUB_IPCC16",  # 2016
    "SUB_IPCC17",  # 2017
    "SUB_IPCC19",  # 2019
    "SUB_IPCC21",  # 2021
    "SUB_IPCC23"   # 2023
  )
  
  # Verify which IPCC columns exist in the data
  available_ipcc_cols <- ipcc_columns[ipcc_columns %in% names(comuna_data)]
  
  if (length(available_ipcc_cols) == 0) {
    print("Columnas disponibles en el shapefile:")
    print(names(comuna_data))
    stop("No se encontraron columnas de clasificación IPCC en el shapefile")
  }
  
  print("Columnas IPCC encontradas:")
  print(available_ipcc_cols)
  
  # Select only IPCC columns and geometry
  simplified_data <- comuna_data %>%
    select(all_of(available_ipcc_cols), geometry)
  
  # Group by all IPCC columns and merge geometries
  print("Simplificando geometrías...")
  merged_data <- simplified_data %>%
    group_by(across(all_of(available_ipcc_cols))) %>%
    summarise(geometry = st_union(geometry), .groups = "drop")
  
  print("Datos de uso de suelo simplificados exitosamente")
  print(paste("Número de polígonos original:", nrow(comuna_data)))
  print(paste("Número de polígonos después de la simplificación:", nrow(merged_data)))
  
  return(merged_data)
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

#' Plot land use data with comuna boundary
#' @param nombre_comuna Character string with the comuna name
#' @param land_use_data sf object with land use data (output from get_land_use_data)
#' @param year Integer with the year to plot (2001, 2012, 2013, 2015, 2016, 2017, 2019, 2021, or 2023)
#' @return ggplot object with the plot
plot_land_use_with_boundary <- function(nombre_comuna, land_use_data, year) {
  # Validate year input
  valid_years <- c(2001, 2012, 2013, 2015, 2016, 2017, 2019, 2021, 2023)
  if (!year %in% valid_years) {
    stop(paste("Año inválido. Los años disponibles son:", paste(valid_years, collapse = ", ")))
  }
  
  # Convert year to IPCC column name
  year_suffix <- sprintf("%02d", year %% 100)
  ipcc_col <- paste0("SUB_IPCC", year_suffix)
  
  # Check if the column exists in the data
  if (!ipcc_col %in% names(land_use_data)) {
    available_years <- gsub("SUB_IPCC", "", names(land_use_data)[grep("^SUB_IPCC", names(land_use_data))])
    available_years <- paste0("20", available_years)
    stop(paste("No hay datos para el año", year, 
               "\nAños disponibles:", paste(available_years, collapse = ", ")))
  }
  
  # Read comuna boundaries
  comunas_shp <- st_read("BBDD/divisiones_chile/Comunas/comunas.shp", quiet = TRUE)
  
  # Filter for the specific comuna
  nombre_comuna_clean <- toupper(trimws(nombre_comuna))
  comuna_boundary <- comunas_shp %>%
    filter(toupper(trimws(Comuna)) == nombre_comuna_clean)
  
  if (nrow(comuna_boundary) == 0) {
    print("Comunas disponibles en el shapefile de límites:")
    print(head(sort(unique(comunas_shp$Comuna)), 10))
    stop(paste("No se encontró el límite de la comuna:", nombre_comuna))
  }
  
  # Define color mapping for IPCC categories (matching exact names in the data)
  color_mapping <- c(
    "Áreas Desprovistas de Vegetación" = "#A9A9A9",
    "Asentamientos" = "#D73027",
    "Bosque Nativo" = "#1A9850",
    "Cuerpos de Agua" = "#4575B4",
    "Humedales" = "#91BFDB",
    "Matorral" = "#F6E8C3",
    "Matorral Arborescente" = "#A6D96A",
    "Nieves y Glaciares" = "#FFFFFF",
    "Plantación" = "#66A61E",
    "Praderas" = "#FFD700",
    "Tierras de Cultivo" = "#E67E22"
  )
  
  # Get unique categories in the data
  categories <- unique(land_use_data[[ipcc_col]])
  
  # Print available categories for debugging
  print("Categorías presentes en los datos:")
  print(categories)
  
  # Print categories that don't have a color assigned
  missing_categories <- setdiff(categories, names(color_mapping))
  if (length(missing_categories) > 0) {
    print("ADVERTENCIA: Las siguientes categorías no tienen color asignado:")
    print(missing_categories)
  }
  
  # Create the plot
  plot <- ggplot() +
    # Add land use polygons with colors
    geom_sf(data = land_use_data, aes(fill = factor(.data[[ipcc_col]])), alpha = 0.9) +
    # Add comuna boundary
    geom_sf(data = comuna_boundary, fill = NA, color = "black", size = 1) +
    # Customize the appearance
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.position = "right",  # Changed to right for better visibility
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8)
    ) +
    # Add labels
    labs(
      title = paste("Uso de Suelo -", nombre_comuna, "-", year),
      fill = paste("Categoría IPCC", year)
    ) +
    # Use the predefined color mapping
    scale_fill_manual(values = color_mapping, drop = FALSE)
  
  # Force the plot to be displayed
  print(plot)
  
  # Return the plot object
  invisible(plot)
}

# Example usage:
# datos_comuna <- get_land_use_data("puerto varas")
# plot <- plot_land_use_with_boundary("puerto varas", datos_comuna, 2023)  # For 2023 data
# plot <- plot_land_use_with_boundary("puerto varas", datos_comuna, 2001)  # For 2001 data
