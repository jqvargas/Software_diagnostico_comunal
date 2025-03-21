# Script para leer y procesar archivos CSV de proyecciones climáticas

# Cargar librerías necesarias
library(dplyr)
library(ggplot2)
# Función principal
procesar_variable_comuna <- function(nombre_variable, codigo_comuna, modelo) {
  print(paste0("Comenzando a procesar variable: ", 
        nombre_variable, 
        "en la zona geográfica de la comuna ",
        codigo_comuna))
  # Definir la ruta de la carpeta "proyecciones"
  carpeta_proyecciones <- paste0(getwd(),"/BBDD/ARCLIM/", nombre_variable)
  
  # Listar todos los archivos CSV en la carpeta "proyecciones"
  archivos <- list.files(path = carpeta_proyecciones, pattern = "*.csv", full.names = TRUE)
  
  # Verificar si hay archivos en la carpeta "proyecciones"
  if (length(archivos) == 0) {
    stop("No se encontraron archivos CSV en la carpeta 'proyecciones'.")
  }
  
  # Crear un dataframe vacío para almacenar los datos concatenados
  datos_combinados <- data.frame()
  
  # Iterar sobre cada archivo CSV
  for (archivo in archivos) {
    # Extraer el nombre del modelo y el corrida del nombre del archivo
    partes_nombre <- strsplit(basename(archivo), "_") #<button class="citation-flag" data-index="1">
    #modelo <- partes_nombre[[1]][3]   # El nombre del modelo está en la posición 3
    corrida <- substr(partes_nombre[[1]][4] , start = 2, stop = 2)# El nombre del corrida está en la posición 4
    
    if(modelo == partes_nombre[[1]][3] || modelo == "Todos"){
    print(paste0("Procesando archivo de datos : ", archivo))
    # Leer el archivo CSV (usando "." como separador decimal)
    datos <- read.csv(archivo, sep = ",", dec = ".")
    
    # Corregir los nombres de las columnas
    colnames(datos)[1] <- "año"   # Corregir el nombre de la primera columna
    colnames(datos)[2] <- "mes"   # Corregir el nombre de la segunda columna
    colnames(datos)[3] <- "dia"   # Corregir el nombre de la tercera columna
    colnames(datos) <- gsub("^X", "", colnames(datos))
   
    # Filtrar los datos para la comuna específica
    if (!codigo_comuna %in% colnames(datos)) {
      warning(paste("El código de comuna", codigo_comuna, "no está presente en el archivo:", archivo))
      next
    }
    
    # Crear un dataframe temporal con las columnas necesarias
    datos_temp <- datos %>%
      select(año, mes, dia, all_of(codigo_comuna)) %>%
      rename(valor = !!sym(codigo_comuna)) %>%  # Renombrar la columna de la comuna a "valor"
      mutate(
        modelo = partes_nombre[[1]][3],  # Usar el modelo del archivo
        corrida = corrida,
        variable = nombre_variable
      )
    
    # Concatenar los datos temporales al dataframe combinado
    datos_combinados <- bind_rows(datos_combinados, datos_temp)
  
  # Verificar si se encontraron datos
 
  }
  # Retornar el dataframe final
  print(paste0("La variable ha sido procesada para la zona geográfica"))
  }
  return(datos_combinados)
}


### Función 2) Calcular los valores relativos con respecto a periodo de referencia dinamico

calculate_relative_values <- function(data, periodo_referencia_ini, periodo_referencia_fin, nombre_variable, modelo) {
  print(paste0("Comenzando a calcular valores de referencia de acuerdo al periodo histórico desde ",
        periodo_referencia_ini, " hasta ", periodo_referencia_fin))
  
  # Step 1: Filter the dataframe based on 'nombre_variable' and optionally 'modelo'
  if (modelo == "Todos") {
    # First filter the data
    base_data <- data %>%
      filter(variable == nombre_variable)
    
    # Then apply the appropriate calculation based on variable type
    if(nombre_variable == "pr") {
      # For precipitation: First sum by month, then by year
      filtered_data <- base_data %>%
        group_by(corrida, modelo, año, mes) %>%
        summarise(monthly_sum = sum(valor, na.rm = TRUE), .groups = "drop") %>%
        group_by(corrida, modelo, año) %>%
        summarise(year_mean_value = sum(monthly_sum, na.rm = TRUE), .groups = "drop")
    } else {
      # For other variables: Calculate annual means
      filtered_data <- base_data %>%
        group_by(corrida, modelo, año) %>%
        summarise(year_mean_value = mean(valor, na.rm = TRUE), .groups = "drop")
    }
  } else {
    # First filter the data
    base_data <- data %>%
      filter(variable == nombre_variable & modelo == modelo)
    
    # Then apply the appropriate calculation based on variable type
    if(nombre_variable == "pr") {
      # For precipitation: First sum by month, then by year
      filtered_data <- base_data %>%
        group_by(corrida, modelo, año, mes) %>%
        summarise(monthly_sum = sum(valor, na.rm = TRUE), .groups = "drop") %>%
        group_by(corrida, modelo, año) %>%
        summarise(year_mean_value = sum(monthly_sum, na.rm = TRUE), .groups = "drop")
    } else {
      # For other variables: Calculate annual means
      filtered_data <- base_data %>%
        group_by(corrida, modelo, año) %>%
        summarise(year_mean_value = mean(valor, na.rm = TRUE), .groups = "drop")
    }
  }
  
  # Step 2: Calculate the reference value (mean of annual values) grouped by 'corrida' and 'modelo'
  reference_means <- filtered_data %>%
    filter(año >= periodo_referencia_ini & año <= periodo_referencia_fin) %>% # Restrict to reference period
    group_by(corrida, modelo) %>% # Group by both corrida and modelo
    summarise(
      reference_model_value = mean(year_mean_value, na.rm = TRUE)  # Mean of annual values
    ) %>%
    ungroup()
  
  # Step 3: Add a new column with the relative values
  # Se ocupa el valor absoluto del valor de referencia, de este modo
  # positivo == aumento con respecto a ref.
  updated_data <- filtered_data %>%
    left_join(reference_means, by = c("corrida", "modelo")) %>% # Join by corrida and modelo
    mutate(valor_relativo_anual = (year_mean_value / abs(reference_model_value))) 
  
  # Return the updated dataframe
  print("Los valores históricos de referencia han sido procesados")
  return(updated_data)
}

### Función 3) Graficar resultados

plot_time_series <- function(data, modelo, nombre_variable, periodo_referencia, nombre_comuna = NULL) {
  print("Comenzando a graficar datos ")
  
  # Step 1: Filter data for the specified modelo or all models
  if (modelo == "Todos") {
    filtered_data <- data %>%
      filter(año >= 2035) # Include only years >= 2035
  } else {
    filtered_data <- data %>%
      filter(modelo == !!modelo & año >= 2035) # Include only years >= 2035 for specific model
  }
  
  # Variable names in Spanish
  variable_names_es <- list(
    "tasmax" = "temperatura máxima",
    "tasmin" = "temperatura mínima",
    "pr" = "precipitación",
    "vel" = "velocidad del viento",
    "rsds" = "radiación solar",
    "huss" = "humedad específica"
  )
  
  # Get the Spanish variable name
  var_name_es <- variable_names_es[[nombre_variable]]
  
  # Prepare title with comuna name if provided
  comuna_title <- ifelse(!is.null(nombre_comuna), nombre_comuna)
  
  # Step 2: Calculate summary statistics
  if (modelo == "Todos") {
    # For "Todos", calculate mean, min, and max across all models for each year
    summary_data <- filtered_data %>%
      group_by(año) %>%
      summarise(
        mean_value = mean(valor_relativo_anual, na.rm = TRUE),
        min_value = min(valor_relativo_anual, na.rm = TRUE),
        max_value = max(valor_relativo_anual, na.rm = TRUE)
      ) %>%
      ungroup()
    
    # Step 3: Create the ggplot with min-max range as confidence interval
    p <- ggplot(summary_data, aes(x = año)) +
      geom_ribbon(aes(ymin = min_value, ymax = max_value), fill = "grey", alpha = 0.5) +
      geom_line(aes(y = mean_value), color = "blue", size = 1) +
      labs(
        title = paste("Serie anual ",var_name_es,
                     "periodo ref: ",
                     periodo_referencia),
        x = "Años",
        y = paste0("Cambio relativo en ", var_name_es)
      ) +
      scale_x_continuous(breaks = seq(2035, max(summary_data$año), by = 4)) +
       # Define y-axis limits and breaks with safety checks
 scale_y_continuous()+
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16)
      )
  } else {
    # Check if there are multiple corrida values for the modelo
    unique_corridas <- filtered_data %>%
      distinct(corrida) %>%
      nrow()
    
    # Debugging: Print the number of unique corridas
    cat("Number of unique corridas for modelo", modelo, ":", unique_corridas, "\n")
    
    if (unique_corridas > 1) {
      # Multiple corridas: Calculate confidence intervals
      summary_data <- filtered_data %>%
        group_by(año) %>%
        summarise(
          mean_value = mean(valor_relativo_anual, na.rm = TRUE),
          sd_value = sd(valor_relativo_anual, na.rm = TRUE),
          lower_bound = mean_value - sd_value, # Lower bound of confidence interval
          upper_bound = mean_value + sd_value  # Upper bound of confidence interval
        ) %>%
        ungroup()
      
      # Step 3: Create the ggplot with confidence interval
      p <- ggplot(summary_data, aes(x = año)) +
        geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "grey", alpha = 0.5) +
        geom_line(aes(y = mean_value), color = "blue", size = 1) +
        labs(
        title = paste("Serie anual ",var_name_es,
                     "periodo ref: ",
                     periodo_referencia),
          x = "Años",
          y = paste0("Cambio relativo en ", var_name_es) # Use Spanish variable name for y-axis label
        ) +
        scale_x_continuous(breaks = seq(2035, max(summary_data$año), by = 4)) + # Show every 2 years
        # Define y-axis limits and breaks with safety checks
 scale_y_continuous()+
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 16)
        )
    } else {
      # Single corrida: Plot only the mean line
      summary_data <- filtered_data %>%
        group_by(año) %>%
        summarise(
          mean_value = mean(valor_relativo_anual, na.rm = TRUE)
        ) %>%
        ungroup()
      
      # Step 3: Create the ggplot without confidence interval
      p <- ggplot(summary_data, aes(x = año)) +
        geom_line(aes(y = mean_value), color = "blue", size = 1) +
        labs(
          title = paste("Serie anual ",var_name_es,
                     "periodo ref: ",
                     periodo_referencia),
          x = "Años",
          y = paste0("Cambio relativo en ", var_name_es) # Use Spanish variable name for y-axis label
        ) +
        scale_x_continuous(breaks = seq(2035, max(summary_data$año), by = 4)) + # Show every 2 years
         # Define y-axis limits and breaks with safety checks
  scale_y_continuous()+
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 16)
        )
    }
  }
  
  print("Los datos han sido graficados satisfactoriamente")
  # Return the plot
  return(p)
}

### Función 4) Calcular estadísticas de resumen

compute_summary_statistics <- function(data, modelo, nombre_variable, periodo_referencia_ini, periodo_referencia_fin, nombre_comuna = NULL) {
  print("Comenzando a calcular estadísticas de resumen...")
  
  # Get current year to calculate last 10 years
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  last_10_years_start <- current_year - 10
  
  # Filter data for the reference period
  if (modelo == "Todos") {
    reference_data <- data %>%
      filter(año >= periodo_referencia_ini & 
             año <= periodo_referencia_fin) %>%
      group_by(corrida, modelo) %>%
      summarise(
        reference_mean = mean(year_mean_value, na.rm = TRUE),
        reference_sd = sd(year_mean_value, na.rm = TRUE),
        reference_min = min(year_mean_value, na.rm = TRUE),
        reference_max = max(year_mean_value, na.rm = TRUE)
      ) %>%
      ungroup()
    
    # Filter data for the projected period
    recent_data <- data %>%
      filter(año >= 2035 & año <= 2045) %>%
      group_by(corrida, modelo) %>%
      summarise(
        recent_mean = mean(year_mean_value, na.rm = TRUE),
        recent_sd = sd(year_mean_value, na.rm = TRUE),
        recent_min = min(year_mean_value, na.rm = TRUE),
        recent_max = max(year_mean_value, na.rm = TRUE)
      ) %>%
      ungroup()
    
    # Find which model and run produced the min and max values
    min_model_run <- recent_data %>%
      arrange(recent_mean) %>%
      slice(1) %>%
      select(modelo, corrida, recent_mean)
    
    max_model_run <- recent_data %>%
      arrange(desc(recent_mean)) %>%
      slice(1) %>%
      select(modelo, corrida, recent_mean)
    
  } else {
    # Filter data for the reference period for a specific model
    reference_data <- data %>%
      filter(modelo == !!modelo & 
             año >= periodo_referencia_ini & 
             año <= periodo_referencia_fin) %>%
      group_by(corrida, modelo) %>%
      summarise(
        reference_mean = mean(year_mean_value, na.rm = TRUE),
        reference_sd = sd(year_mean_value, na.rm = TRUE),
        reference_min = min(year_mean_value, na.rm = TRUE),
        reference_max = max(year_mean_value, na.rm = TRUE)
      ) %>%
      ungroup()
    
    # Filter data for the projected period for a specific model
    recent_data <- data %>%
      filter(modelo == !!modelo & año >= 2035 & año <= 2045) %>%
      group_by(corrida, modelo) %>%
      summarise(
        recent_mean = mean(year_mean_value, na.rm = TRUE),
        recent_sd = sd(year_mean_value, na.rm = TRUE),
        recent_min = min(year_mean_value, na.rm = TRUE),
        recent_max = max(year_mean_value, na.rm = TRUE)
      ) %>%
      ungroup()
    
    # Find which run produced the min and max values
    min_model_run <- recent_data %>%
      arrange(recent_mean) %>%
      slice(1) %>%
      select(modelo, corrida, recent_mean)
    
    max_model_run <- recent_data %>%
      arrange(desc(recent_mean)) %>%
      slice(1) %>%
      select(modelo, corrida, recent_mean)
  }
  
  # Join the data and calculate changes
  stats_combined <- reference_data %>%
    left_join(recent_data, by = c("corrida", "modelo")) %>%
    group_by(modelo) %>%
    summarise(
      ref_mean = mean(reference_mean, na.rm = TRUE),
      ref_sd = mean(reference_sd, na.rm = TRUE),
      ref_min = min(reference_min, na.rm = TRUE),
      ref_max = max(reference_max, na.rm = TRUE),
      recent_mean = mean(recent_mean, na.rm = TRUE),
      recent_sd = mean(recent_sd, na.rm = TRUE),
      recent_min = min(recent_min, na.rm = TRUE),
      recent_max = max(recent_max, na.rm = TRUE),
      percent_change = ((recent_mean - ref_mean) / abs(ref_mean)) * 100,
      absolute_change = recent_mean - ref_mean
    ) %>%
    ungroup()
  
  # For "Todos", calculate the average across all models
  if (modelo == "Todos") {
    stats_combined <- stats_combined %>%
      summarise(
        ref_mean = mean(ref_mean, na.rm = TRUE),
        ref_sd = mean(ref_sd, na.rm = TRUE),
        ref_min = min(ref_min, na.rm = TRUE),
        ref_max = max(ref_max, na.rm = TRUE),
        recent_mean = mean(recent_mean, na.rm = TRUE),
        recent_sd = mean(recent_sd, na.rm = TRUE),
        recent_min = min(recent_min, na.rm = TRUE),
        recent_max = max(recent_max, na.rm = TRUE),
        percent_change = mean(percent_change, na.rm = TRUE),
        absolute_change = mean(absolute_change, na.rm = TRUE)
      )
  }
  
  # Create a dataframe with statistic names and values for display
  stats_df <- data.frame(
    statistic = c(
      "Promedio del Período de Referencia",
      "Mínimo del Período de Referencia",
      "Máximo del Período de Referencia",
      "Promedio del Período Reciente (2035-2045)",
      "Mínimo del Período Reciente",
      "Máximo del Período Reciente",
      "Cambio Absoluto",
      "Cambio Porcentual (%)"
    ),
    value = c(
      round(stats_combined$ref_mean[1], 2),
      round(stats_combined$ref_min[1], 2),
      round(stats_combined$ref_max[1], 2),
      round(stats_combined$recent_mean[1], 2),
      round(stats_combined$recent_min[1], 2),
      round(stats_combined$recent_max[1], 2),
      round(stats_combined$absolute_change[1], 2),
      round(stats_combined$percent_change[1], 2)
    )
  )
  
  # Generate a summary paragraph
  variable_names_es <- list(
    "tasmax" = "temperatura máxima",
    "tasmin" = "temperatura mínima",
    "pr" = "precipitación",
    "vel" = "velocidad del viento",
    "rsds" = "radiación solar",
    "huss" = "humedad específica"
  )
  
  variable_units <- list(
    "tasmax" = "°C",
    "tasmin" = "°C",
    "pr" = "mm",
    "vel" = "m/s",
    "rsds" = "W/m²",
    "huss" = "kg/kg"
  )
  
  # Get the full variable name and unit
  var_name_es <- variable_names_es[[nombre_variable]]
  var_unit <- variable_units[[nombre_variable]]
  
  # Prepare comuna text if provided
  comuna_text <- ifelse(!is.null(nombre_comuna), paste0("comuna ", nombre_comuna), "")
  
  # Create the summary paragraph in Spanish
  if (modelo == "Todos") {
    summary_paragraph <- paste0(
      "<h4>Estadísticas de Resumen para ", var_name_es, " (", nombre_variable, ")", comuna_text, "</h4>",
      "<p>Utilizando <b>todos los modelos climáticos disponibles</b>:</p>",
      "<p>Durante el período de referencia (", periodo_referencia_ini, "-", periodo_referencia_fin, "), el promedio anual de ", 
      var_name_es, " fue <b>", round(stats_combined$ref_mean[1], 2), " ", var_unit, "</b>, con un rango desde ", 
      round(stats_combined$ref_min[1], 2), " hasta ", round(stats_combined$ref_max[1], 2), " ", var_unit, ".</p>",
      "<p>Para el período proyectado (2035-2045), los modelos predicen un promedio anual de ", var_name_es, " de <b>", 
      round(stats_combined$recent_mean[1], 2), " ", var_unit, "</b>, con un rango desde ", 
      round(stats_combined$recent_min[1], 2), " hasta ", round(stats_combined$recent_max[1], 2), " ", var_unit, ".</p>",
      "<p>El valor mínimo fue obtenido por el modelo <b>", min_model_run$modelo, "</b> (corrida ", min_model_run$corrida, 
      ") con un valor de <b>", round(min_model_run$recent_mean, 2), " ", var_unit, "</b>.</p>",
      "<p>El valor máximo fue obtenido por el modelo <b>", max_model_run$modelo, "</b> (corrida ", max_model_run$corrida, 
      ") con un valor de <b>", round(max_model_run$recent_mean, 2), " ", var_unit, "</b>.</p>",
      "<p>Esto representa un cambio promedio de <b>", round(stats_combined$percent_change[1], 2), "%</b> comparado con el período de referencia, ",
      "con un cambio absoluto promedio de <b>", round(stats_combined$absolute_change[1], 2), " ", var_unit, "</b>.</p>",
      "<p><i>Nota: Todos los valores representan promedios anuales entre todos los modelos disponibles.</i></p>"
    )
  } else {
    # For a single model, we still show which run produced min and max values
    summary_paragraph <- paste0(
      "<h4>Estadísticas de Resumen para ", var_name_es, " (", nombre_variable, ")", comuna_text, "</h4>",
      "<p>Utilizando el modelo climático <b>", modelo, "</b>:</p>",
      "<p>Durante el período de referencia (", periodo_referencia_ini, "-", periodo_referencia_fin, "), el promedio anual de ", 
      var_name_es, " fue <b>", round(stats_combined$ref_mean[1], 2), " ", var_unit, "</b>, con un rango desde ", 
      round(stats_combined$ref_min[1], 2), " hasta ", round(stats_combined$ref_max[1], 2), " ", var_unit, ".</p>",
      "<p>Para el período proyectado (2035-2045), el modelo predice un promedio anual de ", var_name_es, " de <b>", 
      round(stats_combined$recent_mean[1], 2), " ", var_unit, "</b>, con un rango desde ", 
      round(stats_combined$recent_min[1], 2), " hasta ", round(stats_combined$recent_max[1], 2), " ", var_unit, ".</p>"
    )
    
    # Only add run information if there are multiple runs
    if (nrow(recent_data) > 1) {
      summary_paragraph <- paste0(
        summary_paragraph,
        "<p>El valor mínimo fue obtenido en la corrida <b>", min_model_run$corrida, 
        "</b> con un valor de <b>", round(min_model_run$recent_mean, 2), " ", var_unit, "</b>.</p>",
        "<p>El valor máximo fue obtenido en la corrida <b>", max_model_run$corrida, 
        "</b> con un valor de <b>", round(max_model_run$recent_mean, 2), " ", var_unit, "</b>.</p>"
      )
    }
    
    # Add the change information
    summary_paragraph <- paste0(
      summary_paragraph,
      "<p>Esto representa un cambio de <b>", round(stats_combined$percent_change[1], 2), "%</b> comparado con el período de referencia, ",
      "con un cambio absoluto de <b>", round(stats_combined$absolute_change[1], 2), " ", var_unit, "</b>.</p>",
      "<p><i>Nota: Todos los valores representan promedios anuales.</i></p>"
    )
  }
  
  # Return both the statistics dataframe and the summary paragraph
  return(list(
    stats_df = stats_df,
    summary_paragraph = summary_paragraph
  ))
}

# Ejemplo de uso

#1) Procesar variable
#nombre_variable <- "pr"  # Nombre de la variable (input)
#codigo_comuna <- "2201"           # Código de la comuna (input)
#modelo = "ACCESS1-0"
#resultado <- procesar_variable_comuna(nombre_variable, codigo_comuna, modelo)


#2) Calcular periodo de referencia
#relative_values <- calculate_relative_values(
#  data = resultado,
#  periodo_referencia_ini = 1980,
#  periodo_referencia_fin = 2010,
#  nombre_variable,
# modelo
#)

#3) Graficar resultados
#plot_time_series(data = relative_values, 
#                 modelo,
#                 nombre_variable,
#                 periodo_referencia = paste0(periodo_referencia_ini, 
#                                             "-", 
#                                         periodo_referencia_fin)
#                )

#4) Calcular estadísticas de resumen
#summary_stats <- compute_summary_statistics(
#  data = relative_values,
#  modelo = modelo,
#  nombre_variable = nombre_variable,
#  periodo_referencia_ini = periodo_referencia_ini,
#  periodo_referencia_fin = periodo_referencia_fin
#)