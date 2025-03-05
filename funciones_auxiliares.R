extraer_modelos_disponibles <- function() {
  # Get the current working directory
  carpeta_base <- getwd()
  
  # Define the variables to process
  variables <- c("pr", "tasmin", "tasmax", "vel", "rsds", "huss")  # Add more variables if needed
  
  # Create an empty data frame to store the results
  modelos_disponibles <- data.frame(variable = character(), modelo = character())
  
  # Loop through each variable
  for (nombre_variable in variables) {
    print(paste0("Procesando variable: ", nombre_variable))
    
    # Define the path to the proyecciones folder for the variable
    carpeta_proyecciones <- paste0(carpeta_base, "/BBDD/ARCLIM/", nombre_variable)
    
    # List all CSV files in the folder
    archivos <- list.files(path = carpeta_proyecciones, pattern = "*.csv", full.names = TRUE)
    
    # Check if there are any files
    if (length(archivos) == 0) {
      warning(paste0("No se encontraron archivos CSV para la variable: ", nombre_variable))
      next
    }
    
    # Extract unique models from the filenames
    modelos <- unique(sapply(archivos, function(archivo) {
      partes_nombre <- strsplit(basename(archivo), "_")
      partes_nombre[[1]][3]  # The model name is in the third position
    }))
    
    # Append the models for this variable to the data frame
    modelos_disponibles <- bind_rows(
      modelos_disponibles,
      data.frame(variable = rep(nombre_variable, length(modelos)), modelo = modelos)
    )
  }
  
  # Write the results to a CSV file
  ruta_csv <- paste0(carpeta_base, "/modelos_disponibles.csv")
  write.csv(modelos_disponibles, file = ruta_csv, row.names = FALSE)
  
  print(paste0("La lista de modelos disponibles ha sido guardada en: ", ruta_csv))
}

