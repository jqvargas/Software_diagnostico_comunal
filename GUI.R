####### Source data
source(paste0(getwd(), "/indicadores_futuros/procesar_arclim.R"))

# Read the CSV file with available models
available_models_df <- read.csv(paste0(getwd(), "/BBDD/ARCLIM/modelos_disponibles.csv"))
available_models <- split(available_models_df$modelo, available_models_df$variable)

# Read the CSV file with comuna metadata
comunas_df <- read.csv(paste0(getwd(), "/BBDD/ARCLIM/metadatos_comunas.csv"), sep = ";")
# Create a named vector for the dropdown (nombre_comuna as names, codigo_comuna as values)
comunas_choices <- setNames(comunas_df$codigo_comuna, comunas_df$nombre_comuna)

# Read the shapefile
comunas_shp <- st_read("BBDD/divisiones_chile/Comunas/comunas.shp")
# Join with metadata to get nombre_comuna
comunas_shp <- comunas_shp %>%
  left_join(comunas_df, by = c("cod_comuna" = "codigo_comuna"))

############ Libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(shinyFiles)
library(shinydashboard)
library(shinydashboardPlus)
library(sf)
##########

# Define GUI
ui <- shinydashboardPlus::dashboardPage(
  header = shinydashboardPlus::dashboardHeader(
    title = "Panel de Datos Climáticos",
    titleWidth = 300
  ),
  sidebar = shinydashboardPlus::dashboardSidebar(
    width = 300,
    shinydashboard::sidebarMenu(  # Use shinydashboard::sidebarMenu
      shinydashboard::menuItem("Selección de Datos", tabName = "data_selection", icon = icon("chart-line")),
      shinydashboard::menuSubItem("Datos Futuros", tabName = "future_data", icon = icon("arrow-right")),
      shinydashboard::menuItem("Datos Históricos", tabName = "historical_data", icon = icon("chart-bar"), disabled = TRUE) # Placeholder
    )
  ),
  body = shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      shinydashboard::tabItem(tabName = "future_data",
                              # First row: Control panel, Time series plot, and Map
                              fluidRow(
                                # Control panel (1/4 of width)
                                column(width = 3,
                                  shinydashboard::box(
                                    title = "Panel de Control", width = NULL, solidHeader = TRUE, status = "primary",
                                    selectInput("nombre_variable", "Seleccionar Variable:", 
                                                choices = c("tasmax", "tasmin", "pr", "vel", "rsds", "huss")),
                                    selectInput("nombre_comuna", "Nombre de Comuna:", choices = comunas_choices),
                                    numericInput("periodo_referencia_ini", "Año Inicial del Período de Referencia:", value = 1980, min = 1950, max = 2020),
                                    numericInput("periodo_referencia_fin", "Año Final del Período de Referencia:", value = 2010, min = 1950, max = 2020),
                                    selectInput("modelo", "Seleccionar Modelo:", choices = NULL),
                                    div(style = "margin-top: 20px;",
                                      actionButton("generate_plot", "Generar Gráfico", class = "btn-primary", style = "width: 100%; margin-bottom: 10px;"),
                                      actionButton("save_plot", "Guardar Resultados", class = "btn-success", style = "width: 100%;")
                                    )
                                  )
                                ),
                                
                                # Time series plot (1/2 of width)
                                column(width = 6,
                                  shinydashboard::box(
                                    title = "Gráfico", width = NULL, solidHeader = TRUE, status = "info",
                                    plotOutput("time_series_plot")
                                  )
                                ),
                                
                                # Map (1/4 of width)
                                column(width = 3,
                                  shinydashboard::box(
                                    title = "Mapa de Comuna", width = NULL, solidHeader = TRUE, status = "info",
                                    plotOutput("spatial_plot")
                                  )
                                )
                              ),
                              
                              # Second row: Summary stats and logs
                              fluidRow(
                                # Summary statistics (2/3 of width)
                                column(width = 8,
                                  shinydashboard::box(
                                    title = "Estadísticas de Resumen", width = NULL, height = "300px", solidHeader = TRUE, status = "success",
                                    div(style = "height: 230px; overflow-y: auto;",
                                      htmlOutput("summary_stats")
                                    )
                                  )
                                ),
                                
                                # Logs (1/3 of width)
                                column(width = 4,
                                  shinydashboard::box(
                                    title = "Registros", width = NULL, height = "300px", solidHeader = TRUE, status = "warning",
                                    div(style = "height: 230px; overflow-y: auto;",
                                      textOutput("console_logs")
                                    )
                                  )
                                )
                              )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive value to store the selected comuna code and name
  selected_comuna <- reactiveVal(list(codigo = NULL, nombre = NULL))
  
  # Update selected_comuna when nombre_comuna changes
  observeEvent(input$nombre_comuna, {
    codigo_comuna <- input$nombre_comuna
    nombre_comuna <- names(which(comunas_choices == codigo_comuna))
    selected_comuna(list(codigo = codigo_comuna, nombre = nombre_comuna))
  })
  
  # Render the spatial plot
  output$spatial_plot <- renderPlot({
    # Only render when generate button is pressed
    req(input$generate_plot)
    
    # Get the selected comuna info
    comuna_info <- selected_comuna()
    
    # Filter to show only the selected comuna
    selected_polygon <- comunas_shp %>%
      filter(cod_comuna == comuna_info$codigo)
    
    # Create the plot
    ggplot() +
      geom_sf(data = selected_polygon, fill = "lightblue", color = "white") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.margin = margin(0, 0, 0, 0, "pt")
      ) +
      ggtitle(comuna_info$nombre) +
      coord_sf(expand = FALSE)
  }, height = function() 300, width = function() 300)
  
  # Dynamically update the modelo dropdown based on the selected variable
  observeEvent(input$nombre_variable, {
    # Clear the modelo dropdown if no variable is selected
    if (is.null(input$nombre_variable)) {
      updateSelectInput(session, "modelo", choices = NULL)
      return()
    }
    
    # Get the available models for the selected variable
    selected_models <- available_models[[input$nombre_variable]]
    
    # Add "Todos" as the first option
    model_choices <- c("Todos", selected_models)
    
    # Update the modelo dropdown with the available models
    updateSelectInput(session, "modelo", choices = model_choices)
  })
  
  # Reactive expression to process data and generate plot
  processed_data <- eventReactive(input$generate_plot, {
    # Get the selected comuna code and name
    comuna_info <- selected_comuna()
    codigo_comuna <- comuna_info$codigo
    nombre_comuna <- comuna_info$nombre
    
    # Initialize logs
    output$console_logs <- renderText(paste0("Procesamiento iniciado para la comuna: ", nombre_comuna, " (", codigo_comuna, ")"))
    
    # Step 1: Procesar variable
    output$console_logs <- renderText("Paso 1: Procesando variable...")
    
    # If "Todos" is selected, we need to process all models for the variable
    if (input$modelo == "Todos") {
      # Get all models for the selected variable
      all_models <- available_models[[input$nombre_variable]]
      
      # Initialize an empty dataframe to store combined results
      resultado_combinado <- data.frame()
      
      # Process each model and combine the results
      for (modelo_actual in all_models) {
        output$console_logs <- renderText(paste("Procesando modelo:", modelo_actual))
        
        # Process the current model
        resultado_modelo <- procesar_variable_comuna(
          nombre_variable = input$nombre_variable,
          codigo_comuna = codigo_comuna,
          modelo = modelo_actual
        )
        
        # Combine with previous results
        resultado_combinado <- bind_rows(resultado_combinado, resultado_modelo)
      }
      
      resultado <- resultado_combinado
    } else {
      # Process a single model as before
      resultado <- procesar_variable_comuna(
        nombre_variable = input$nombre_variable,
        codigo_comuna = codigo_comuna,
        modelo = input$modelo
      )
    }
    
    # Step 2: Calcular periodo de referencia
    output$console_logs <- renderText("Paso 2: Calculando valores relativos...")
    relative_values <- calculate_relative_values(
      data = resultado,
      periodo_referencia_ini = input$periodo_referencia_ini,
      periodo_referencia_fin = input$periodo_referencia_fin,
      nombre_variable = input$nombre_variable,
      modelo = input$modelo
    )
    
    # Step 3: Graficar resultados
    output$console_logs <- renderText("Paso 3: Generando gráfico...")
    plot <- plot_time_series(
      data = relative_values,
      modelo = input$modelo,
      nombre_variable = input$nombre_variable,
      periodo_referencia = paste0(input$periodo_referencia_ini,
                                  "-",
                                  input$periodo_referencia_fin),
      nombre_comuna = nombre_comuna
    )
    
    # Step 4: Calculate summary statistics
    output$console_logs <- renderText("Paso 4: Calculando estadísticas de resumen...")
    summary_stats <- compute_summary_statistics(
      data = relative_values,
      modelo = input$modelo,
      nombre_variable = input$nombre_variable,
      periodo_referencia_ini = input$periodo_referencia_ini,
      periodo_referencia_fin = input$periodo_referencia_fin,
      nombre_comuna = nombre_comuna
    )
    
    # Render the summary statistics paragraph
    output$summary_stats <- renderUI({
      # Add the comuna name to the beginning of the summary paragraph
      comuna_info_html <- paste0("<p>Para la comuna <b>", nombre_comuna, "</b> (código: ", codigo_comuna, "):</p>")
      HTML(paste0(comuna_info_html, summary_stats$summary_paragraph))
    })
    
    # Final log message
    output$console_logs <- renderText("Procesamiento completado exitosamente!")
    
    return(list(
      plot = plot,
      summary_stats = summary_stats,
      codigo_comuna = codigo_comuna,
      nombre_comuna = nombre_comuna
    ))
  })
  
  
  # Render the plot
  output$time_series_plot <- renderPlot({
    processed_data()$plot
  })
  
  
  # Save plot and summary functionality
  observeEvent(input$save_plot, {
    # Check if a plot has been generated
    req(processed_data())
    
    # Get the selected comuna code
    codigo_comuna <- processed_data()$codigo_comuna
    nombre_comuna <- processed_data()$nombre_comuna
    
    # Create the folder structure
    folder_path <- paste0(getwd(), "/BBDD/resultados/", codigo_comuna, "/", input$nombre_variable, "/", input$modelo, "/")
    
    # Create the directory if it doesn't exist
    if (!dir.exists(folder_path)) {
      dir.create(folder_path, recursive = TRUE)
    }
    
    # Define the file names
    plot_file_name <- paste0("grafico_", input$nombre_variable, "_", input$modelo, ".png")
    summary_file_name <- paste0("resumen_", input$nombre_variable, "_", input$modelo, ".txt")
    
    # Full paths
    plot_full_path <- file.path(folder_path, plot_file_name)
    summary_full_path <- file.path(folder_path, summary_file_name)
    
    # Save the plot as a .png file
    ggsave(plot_full_path, plot = processed_data()$plot, width = 12, height = 8, units = "in", dpi = 300)
    
    # Save the summary as a .txt file
    # Convert HTML to plain text by removing HTML tags
    summary_text <- gsub("<.*?>", "", processed_data()$summary_stats$summary_paragraph)
    summary_text <- gsub("&nbsp;", " ", summary_text)
    
    # Add comuna information to the summary text
    summary_text <- paste0("Resumen para la comuna ", nombre_comuna, " (", codigo_comuna, "):\n\n", summary_text)
    
    # Write to file
    writeLines(summary_text, summary_full_path)
    
    # Log the success message
    output$console_logs <- renderText(paste("Resultados guardados exitosamente en:", folder_path))
  })
  
}


# Run the application
shinyApp(ui, server)