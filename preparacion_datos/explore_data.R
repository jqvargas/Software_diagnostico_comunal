# Load required libraries
library(sf)
library(dplyr)

# Read the shapefile
comunas_sf <- st_read("BBDD/divisiones_chile/Comunas/comunas.shp")

# Read the CSV file
metadatos_comunas <- read.csv("BBDD/ARCLIM/metadatos_comunas.csv", sep = ";")

# Print structure of both datasets
cat("\nColumns in shapefile:\n")
print(colnames(comunas_sf))
print(head(comunas_sf))

cat("\nColumns in metadatos_comunas:\n")
print(colnames(metadatos_comunas))
print(head(metadatos_comunas))

# Count number of records in each dataset
cat("\nNumber of records:\n")
cat("Shapefile:", nrow(comunas_sf), "\n")
cat("Metadatos:", nrow(metadatos_comunas), "\n")

# Create comparison dataframe
comparison_df <- comunas_sf %>%
  st_drop_geometry() %>%  # Remove geometry for easier viewing
  select(cod_comuna, Comuna) %>%
  full_join(
    metadatos_comunas %>% select(codigo_comuna, nombre_comuna),
    by = c("cod_comuna" = "codigo_comuna")
  )

# Print first 100 rows of comparison
cat("\nFirst 100 rows of comparison (showing both datasets):\n")
print(head(comparison_df, 100))

# Count mismatches
mismatches_shp <- comparison_df %>%
  filter(is.na(nombre_comuna)) %>%
  nrow()

mismatches_csv <- comparison_df %>%
  filter(is.na(Comuna)) %>%
  nrow()

cat("\nMismatches analysis:\n")
cat("Comunas in shapefile but not in CSV:", mismatches_shp, "\n")
cat("Comunas in CSV but not in shapefile:", mismatches_csv, "\n")

# Print specific mismatches
cat("\nComunas in shapefile but not in CSV:\n")
print(comparison_df %>% filter(is.na(nombre_comuna)) %>% select(cod_comuna, Comuna))

cat("\nComunas in CSV but not in shapefile:\n")
print(comparison_df %>% filter(is.na(Comuna)) %>% select(cod_comuna, nombre_comuna))