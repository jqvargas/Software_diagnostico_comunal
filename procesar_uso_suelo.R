# Script para procesar datos de uso de suelo


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
  
  # Dynamically identify IPCC columns
  ipcc_columns <- grep("^SUB_IPCC\\d{2}$", names(comuna_data), value = TRUE, ignore.case = TRUE)
  
  if (length(ipcc_columns) == 0) {
    print("Columnas disponibles en el shapefile:")
    print(names(comuna_data))
    stop("No se encontraron columnas de clasificación IPCC en el shapefile")
  }
  
  print("Columnas IPCC encontradas:")
  print(ipcc_columns)
  
  # Verify SHAPE_Area column exists
  area_col <- "SHAPE_Area"
  if (!area_col %in% names(comuna_data)) {
    print("ADVERTENCIA: No se encontró la columna SHAPE_Area, se calculará el área usando la geometría")
    # Calculate area if not present
    comuna_data <- comuna_data %>%
      st_transform(32719) %>%  # Transform to UTM zone 19S for accurate area calculation
      mutate(SHAPE_Area = as.numeric(st_area(.)) / 10000)  # Convert to hectares
  } else {
    # Convert existing SHAPE_Area to hectares if it's in square meters
    comuna_data <- comuna_data %>%
      mutate(SHAPE_Area = as.numeric(SHAPE_Area) / 10000)
  }
  
  # Select IPCC columns, SHAPE_Area and geometry
  simplified_data <- comuna_data %>%
    select(all_of(c(ipcc_columns, area_col)), geometry)
  
  # Group by all IPCC columns and sum areas
  print("Simplificando geometrías...")
  merged_data <- simplified_data %>%
    group_by(across(all_of(ipcc_columns))) %>%
    summarise(
      SHAPE_Area = sum(SHAPE_Area),
      geometry = st_union(geometry),
      .groups = "drop"
    )
  
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
#' @param year Integer with the year to plot
#' @return ggplot object with the plot
plot_land_use_with_boundary <- function(nombre_comuna, land_use_data, year) {
  # Convert year to IPCC column name
  year_suffix <- sprintf("%02d", year %% 100)
  ipcc_col <- paste0("SUB_IPCC", year_suffix)
  
  # Check if the column exists in the data
  if (!ipcc_col %in% names(land_use_data)) {
    # Get available years for error message
    available_cols <- names(land_use_data)[grep("^SUB_IPCC", names(land_use_data))]
    available_years <- sort(as.numeric(paste0("20", substr(available_cols, 9, 10))))
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
  
  # Define color mapping for IPCC categories
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
  
  # Print categories that don't have a color assigned
  missing_categories <- setdiff(categories, names(color_mapping))
  if (length(missing_categories) > 0) {
    warning("Las siguientes categorías no tienen color asignado y usarán colores por defecto:\n",
            paste(missing_categories, collapse = ", "))
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
      plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
      axis.title = element_text(size = 24, face = "bold"),
      axis.text = element_text(size = 22),
      legend.position = "right",
      legend.title = element_text(size = 18, face = "bold"),
      legend.text = element_text(size = 16)
    ) +
    # Add labels
    labs(
      title = paste("Uso de Suelo en la comuna de", nombre_comuna, "al año", year),
      fill = "Categoría IPCC"
    ) +
    # Use the predefined color mapping
    scale_fill_manual(values = color_mapping, drop = FALSE)
  
  return(plot)
}

#' Analyze land use changes over time
#' @param land_use_data sf object with land use data (output from get_land_use_data)
#' @param nombre_comuna Character string with the comuna name
#' @return List containing summary statistics and text
analyze_land_use_changes <- function(land_use_data, nombre_comuna) {
  # Get all IPCC columns in chronological order
  ipcc_cols <- sort(names(land_use_data)[grep("^SUB_IPCC", names(land_use_data))])
  
  if (length(ipcc_cols) == 0) {
    stop("No se encontraron columnas de clasificación IPCC en los datos")
  }
  
  # Create year mapping
  year_mapping <- sapply(ipcc_cols, function(col) {
    year_suffix <- substr(col, 9, 10)
    as.numeric(ifelse(as.numeric(year_suffix) > 50, 
                     paste0("19", year_suffix), 
                     paste0("20", year_suffix)))
  })
  
  # Calculate areas for each category in each year
  area_stats <- lapply(ipcc_cols, function(col) {
    # Calculate area for each category using SHAPE_Area
    areas <- land_use_data %>%
      group_by(!!sym(col)) %>%
      summarise(
        area_ha = sum(SHAPE_Area)  # Area is already in hectares
      ) %>%
      st_drop_geometry()
    
    # Calculate total area and percentages
    total_area <- sum(areas$area_ha)
    areas$percentage <- areas$area_ha / total_area * 100
    
    # Rename columns
    names(areas)[1] <- "categoria"
    areas$year <- year_mapping[col]
    
    return(areas)
  })
  
  # Combine all years into one dataframe
  all_stats <- do.call(rbind, area_stats)
  
  # Calculate rates of change
  changes <- list()
  if (length(ipcc_cols) > 1) {
    # Get unique categories
    categories <- unique(all_stats$categoria)
    
    # Calculate changes for each category
    for (cat in categories) {
      cat_data <- all_stats[all_stats$categoria == cat, ]
      if (nrow(cat_data) > 1) {
        # Calculate annual rate of change
        years_diff <- diff(cat_data$year)
        perc_diff <- diff(cat_data$percentage)
        annual_change <- perc_diff / years_diff
        
        changes[[cat]] <- data.frame(
          categoria = cat,
          cambio_anual_promedio = mean(annual_change),
          cambio_total = tail(cat_data$percentage, 1) - cat_data$percentage[1],
          periodo = paste(min(cat_data$year), "-", max(cat_data$year))
        )
      }
    }
    changes_df <- do.call(rbind, changes)
  } else {
    changes_df <- data.frame()
  }
  
  # Generate summary text
  latest_year <- max(all_stats$year)
  earliest_year <- min(all_stats$year)
  latest_stats <- all_stats[all_stats$year == latest_year, ]
  
  # Sort categories by percentage for the latest year
  latest_stats <- latest_stats[order(-latest_stats$percentage), ]
  
  # Create summary paragraph
  summary_text <- paste0(
    "Análisis de Uso de Suelo para la comuna de ", nombre_comuna, ":\n\n",
    "Distribución actual (", latest_year, "):\n"
  )
  
  # Add current distribution
  for (i in 1:nrow(latest_stats)) {
    summary_text <- paste0(
      summary_text,
      "- ", latest_stats$categoria[i], ": ",
      sprintf("%.1f", latest_stats$percentage[i]), "% ",
      "(", sprintf("%.1f", latest_stats$area_ha[i]), " ha)\n"
    )
  }
  
  # Add changes if available
  if (nrow(changes_df) > 0) {
    summary_text <- paste0(
      summary_text,
      "\nCambios principales (", earliest_year, "-", latest_year, "):\n"
    )
    
    # Sort changes by absolute magnitude
    changes_df <- changes_df[order(-abs(changes_df$cambio_total)), ]
    
    # Add top 3 changes
    for (i in 1:min(3, nrow(changes_df))) {
      direction <- ifelse(changes_df$cambio_total[i] > 0, "aumentó", "disminuyó")
      summary_text <- paste0(
        summary_text,
        "- ", changes_df$categoria[i], " ", direction, " ",
        sprintf("%.1f", abs(changes_df$cambio_total[i])),
        "% (", sprintf("%.2f", changes_df$cambio_anual_promedio[i]), "% por año)\n"
      )
    }
  }
  
  # Return results
  return(list(
    summary_text = summary_text,
    area_stats = all_stats,
    changes = changes_df
  ))
}

# Example usage:
# datos_comuna <- get_land_use_data("puerto varas")
# plot<-plot_land_use_with_boundary(nombre_comuna = "puerto varas", land_use_data = datos_comuna, year = 2023)

# analisis <- analyze_land_use_changes(datos_comuna, "Puerto Varas")
# print(analisis$summary_text)  # Print summary text
# View(analisis$area_stats)    # View detailed statistics
# View(analisis$changes)       # View change rates
