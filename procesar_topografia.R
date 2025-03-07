# Script para procesar datos de topografía
library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(viridis)
library(RColorBrewer)

#' Get region number for a given comuna
#' @param nombre_comuna Character string with the comuna name
#' @return Integer with the region number
get_region_number <- function(nombre_comuna_input) {
  # Read metadata file
  metadata <- read.csv(paste0(getwd(), "/BBDD/ARCLIM/metadatos_comunas.csv"), 
                      sep = ";", 
                      stringsAsFactors = FALSE,
                      encoding = "UTF-8")
  
  # Find region number for the comuna (case-insensitive)
  region_info <- metadata %>%
    filter(tolower(nombre_comuna_input) == tolower(nombre_comuna)) %>%
    dplyr::select(codigo_region)
  
  if (nrow(region_info) == 0) {
    stop(paste("No se encontró la comuna", nombre_comuna_input, "en los metadatos"))
  }
  
  return(region_info$codigo_region[1])
}

#' Read topography raster for a given comuna
#' @param nombre_comuna Character string with the comuna name
#' @return RasterLayer object with topography data
read_raster_topo <- function(nombre_comuna) {
  # Get region number
  region_num <- get_region_number(nombre_comuna_input = nombre_comuna)
  
  # Construct path to raster file
  raster_path <- paste0(getwd(), "/BBDD/topografia/", region_num, "/", region_num, ".jp2")
  
  # Check if file exists
  if (!file.exists(raster_path)) {
    stop(paste("No se encontró el archivo raster en:", raster_path))
  }
  
  # Read raster
  tryCatch({
    raster_data <- raster(raster_path)
    return(raster_data)
  }, error = function(e) {
    stop(paste("Error al leer el archivo raster:", e$message))
  })
}

#' Get comuna boundaries from shapefile
#' @param nombre_comuna Character string with the comuna name
#' @return sf object with comuna boundaries
get_comuna_boundaries <- function(nombre_comuna) {
  # Read shapefile
  comunas_shp <- st_read("BBDD/divisiones_chile/Comunas/comunas.shp", quiet = TRUE)
  
  # Find comuna (case-insensitive)
  comuna_polygon <- comunas_shp %>%
    filter(tolower(Comuna) == tolower(nombre_comuna))
  
  if (nrow(comuna_polygon) == 0) {
    stop(paste("No se encontró la comuna", nombre_comuna, "en el shapefile"))
  }
  
  return(comuna_polygon)
}

#' Generate a summary of topographic characteristics
#' @param raster_masked Masked raster containing elevation data
#' @return String containing a descriptive summary of the topography
generate_topo_summary <- function(raster_masked) {
  # Calculate basic elevation statistics
  elevation_stats <- summary(values(raster_masked))
  min_elev <- round(elevation_stats[1], 0)  # Min
  max_elev <- round(elevation_stats[6], 0)  # Max
  mean_elev <- round(elevation_stats[4], 0)  # Mean
  range_elev <- max_elev - min_elev
  
  # Calculate slope
  slope_raster <- terrain(raster_masked, opt = "slope", unit = "degrees")
  slope_values <- values(slope_raster)
  slope_values <- slope_values[!is.na(slope_values)]
  
  mean_slope <- round(mean(slope_values, na.rm = TRUE), 1)
  max_slope <- round(max(slope_values, na.rm = TRUE), 1)
  
  # Calculate slope distribution
  total_cells <- length(slope_values)
  gentle_slopes <- round(sum(slope_values < 10) / total_cells * 100, 1)
  moderate_slopes <- round(sum(slope_values >= 10 & slope_values <= 30) / total_cells * 100, 1)
  steep_slopes <- round(sum(slope_values > 30) / total_cells * 100, 1)
  
  # Generate descriptive text for elevation range
  elevation_description <- if (mean_elev < 500) {
    "predominantemente de baja altitud"
  } else if (mean_elev < 1500) {
    "de altitud media"
  } else {
    "de alta montaña"
  }
  
  # Generate descriptive text for terrain type
  terrain_description <- if (mean_slope < 5) {
    "mayormente plano"
  } else if (mean_slope < 15) {
    "ondulado"
  } else if (mean_slope < 25) {
    "montañoso"
  } else {
    "muy escarpado"
  }
  
  # Create summary text
  summary_text <- sprintf(
    "La zona presenta un terreno %s, %s, con elevaciones que varían desde %d metros hasta %d metros sobre el nivel del mar, 
    abarcando un rango vertical de %d metros. La elevación promedio es de %d metros.
    
    El terreno tiene una pendiente promedio de %.1f grados, con pendientes máximas de hasta %.1f grados. 
    La distribución de pendientes muestra que:
    - %.1f%% del área tiene pendientes suaves (menos de 10 grados)
    - %.1f%% presenta pendientes moderadas (entre 10 y 30 grados)
    - %.1f%% tiene pendientes pronunciadas (más de 30 grados)",
    terrain_description,
    elevation_description,
    min_elev,
    max_elev,
    range_elev,
    mean_elev,
    mean_slope,
    max_slope,
    gentle_slopes,
    moderate_slopes,
    steep_slopes
  )
  
  return(summary_text)
}

#' Plot topography for a given comuna
#' @param nombre_comuna Character string with the comuna name
#' @return List containing plot object and summary text
plot_topografia <- function(nombre_comuna) {
  # Get raster data
  print("Leyendo raster de topografia")
  raster_data <- read_raster_topo(nombre_comuna)
  region_num <- get_region_number(nombre_comuna_input = nombre_comuna)
  # Define CRS based on region_num
if (region_num < 8 || region_num == 13 || region_num == 15) {
  # UTM Zone 19S for regions 1-7, 13, and 15
  crs(raster_data) <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs"
} else if (region_num == 16) {
  # SIRGAS-Chile 2002 for region 16
  crs(raster_data) <- "+init=epsg:5360"
} else if (region_num %in% c(8, 9, 10, 11, 12, 14)) {
  # UTM Zone 18S for regions 8, 9, 10, 11, 12, and 14
  crs(raster_data) <- "+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs"
}
  # Get comuna boundaries
  comuna_boundaries <- get_comuna_boundaries(nombre_comuna)
  
  # Ensure both objects are in the same CRS
  comuna_boundaries <- st_transform(comuna_boundaries, crs(raster_data))
  
  # Crop and mask raster to comuna boundaries
  print("Cortando y enmascarando raster")
  raster_cropped <- crop(raster_data, extent(comuna_boundaries))
  print("Raster cortado")
  raster_masked <- mask(raster_cropped, comuna_boundaries)
  print("Raster enmascarado")
  
  # Generate topographic summary
  print("Generando resumen topográfico")
  summary_text <- generate_topo_summary(raster_masked)
  
  # Create the plot
  print("Creando plot")
  p <- create_elevation_plot(raster_masked, comuna_boundaries)
  print("Plot creado")
  
  # Return both plot and summary
  return(list(
    plot = p,
    summary = summary_text
  ))
}

create_elevation_plot <- function(raster_masked, comuna_boundaries) {
  # Define a custom color palette
elevation_colors <- colorRampPalette(c(
  "#2B3A67",  # 0–100m: Deep blue (deep water)
  "#4A90E2",  # 100–150m: Light blue (shallow water)
  "#63B8EE",  # 150–175m: Very light blue (transition out of water)
  "#87CEEB",  # 175–200m: Pale blue (coastal water/land boundary)
  "#B0E0E6",  # 200–225m: Very pale blue (final transition out of water)
  "#A8D5BA",  # 225–250m: Muted green-blue (coastal transition)
  "#98D1A5",  # 250–275m: Soft green-blue (further land transition)
  "#76C776",  # 275–350m: Soft green (lowlands)
  "#B2D180",  # 350–500m: Brighter green (transition to foothills)
  "#E6D38B",  # 500–700m: Light yellow (foothills)
  "#D9A66C",  # 700–900m: Warm beige-brown (mid-altitude)
  "#C47A4F",  # 900–1,200m: Orange-brown (higher mid-altitude)
  "#A0522D",  # 1,200–1,500m: Rich brown (mountains)
  "#8B7E74",  # 1,500–2,000m: Rocky gray (high peaks)
  "#D0D0D0",  # 2,000–2,500m: Lighter gray (snowline transition)
  "#F0F0F0",  # 2,500–3,000m: Very light gray (near snowline)
  "#FFFFFF"   # 3,000–4,000m: White (snow-capped peaks)
))(256)
  # Start a new plot
  plot.new()

  # Plot the masked raster with the custom palette
  plot(raster_masked, 
       main = "Elevation Map with Comuna Boundary",
       col = elevation_colors,
       colNA = "#F0F0F0",  # Light gray for masked areas
       axes = TRUE)

  # Add the comuna boundary with a black line
  plot(st_geometry(comuna_boundaries), 
       add = TRUE, 
       border = "#6d6868", 
       lwd = 3)

  # Add a legend for elevation
  legend("bottomright",
         title = "Elevation",
         legend = c("Low", "Medium", "High"),
         fill = c("#B0E0E6", "#7CFC00", "#FFFFFF"),
         bty = "n",  # No legend box
         cex = 0.8)  # Adjust legend text size

  # Add a scale bar (in kilometers)
  scalebar(d = 10,  # Length in km
           type = "bar", 
           below = "km",
           xy = c(extent(raster_masked)@xmin + 5000,  # Adjust position
                  extent(raster_masked)@ymin + 5000))


  # Capture the plot and save it as an object
  p <- recordPlot()

  # Return the plot object
  return(p)
}

#' Get list of available comunas with topography data
#' @return Character vector with comuna names
get_available_comunas_topo <- function() {
  # Read metadata file
  metadata <- read.csv(paste0(getwd(), "/BBDD/ARCLIM/metadatos_comunas.csv"), 
                      sep = ";", 
                      stringsAsFactors = FALSE,
                      encoding = "UTF-8")
  
  # Get unique comuna names
  comunas <- unique(metadata$nombre_comuna)
  
  # Sort alphabetically
  return(sort(comunas))
}

# Example usage:
# p <- plot_topografia("Lo Barnechea")

# ggsave("topografia_puerto_varas.png", p, width = 10, height = 8, dpi = 300) 