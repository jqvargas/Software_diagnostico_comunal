####### Source data
source(paste0(getwd(), "/indicadores_futuros/procesar_arclim.R"))
source(paste0(getwd(), "/procesar_historico.R"))  # Add source for historical processing

# Read configuration files
available_models_df <- read.csv(paste0(getwd(), "/BBDD/ARCLIM/modelos_disponibles.csv"))
available_models <- split(available_models_df$modelo, available_models_df$variable)

# Read comuna metadata and create choices
comunas_df <- read.csv(paste0(getwd(), "/BBDD/ARCLIM/metadatos_comunas.csv"), sep = ";")
comunas_choices <- setNames(comunas_df$codigo_comuna, comunas_df$nombre_comuna)

# Read the shapefile
print("Loading shapefile...")
comunas_shp <- st_read("BBDD/divisiones_chile/Comunas/comunas.shp", quiet = TRUE)
print("Shapefile columns:")
print(names(comunas_shp))

# Join shapefile with metadata
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
library(stringi)
##########

# Define GUI
ui <- shinydashboardPlus::dashboardPage(
  header = shinydashboardPlus::dashboardHeader(
    title = "Panel de Datos Climáticos",
    titleWidth = 300
  ),
  sidebar = shinydashboardPlus::dashboardSidebar(
    width = 300,
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Selección de Datos", tabName = "data_selection", icon = icon("chart-line")),
      shinydashboard::menuSubItem("Datos Futuros", tabName = "future_data", icon = icon("arrow-right")),
      shinydashboard::menuSubItem("Datos Históricos", tabName = "historical_data", icon = icon("clock"))
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
      ),
      
      # Historical Data Tab (new)
      shinydashboard::tabItem(tabName = "historical_data",
        # First row: Control panel, Time series plot, and Map
        fluidRow(
          # Control panel (1/4 of width)
          column(width = 3,
            shinydashboard::box(
              title = "Panel de Control", width = NULL, solidHeader = TRUE, status = "primary",
              selectInput("hist_variable", "Seleccionar Variable:",
                        choices = c("Temperatura" = "temp", "Precipitación" = "pp")),
              conditionalPanel(
                condition = "input.hist_variable == 'temp'",
                selectInput("tipo_temp", "Tipo de Temperatura:",
                          choices = c("Máxima" = "max", "Mínima" = "min", "Media" = "mean"))
              ),
              uiOutput("hist_comuna_selector"),  # Dynamic comuna selector
              div(style = "margin-top: 20px;",
                actionButton("generate_hist_plot", "Generar Gráfico", 
                           class = "btn-primary", style = "width: 100%; margin-bottom: 10px;"),
                actionButton("save_hist_plot", "Guardar Resultados", 
                           class = "btn-success", style = "width: 100%;")
              )
            )
          ),
          
          # Time series plot (1/2 of width)
          column(width = 6,
            shinydashboard::box(
              title = "Gráfico", width = NULL, solidHeader = TRUE, status = "info",
              plotOutput("hist_time_series_plot")
            )
          ),
          
          # Map (1/4 of width)
          column(width = 3,
            shinydashboard::box(
              title = "Mapa de Comuna", width = NULL, solidHeader = TRUE, status = "info",
              plotOutput("hist_spatial_plot")
            )
          )
        ),
        
        # Second row: Summary stats and logs
        fluidRow(
          # Summary statistics (2/3 of width)
          column(width = 8,
            shinydashboard::box(
              title = "Estadísticas de Resumen", width = NULL, height = "300px", 
              solidHeader = TRUE, status = "success",
              div(style = "height: 230px; overflow-y: auto;",
                htmlOutput("hist_summary_stats")
              )
            )
          ),
          
          # Logs (1/3 of width)
          column(width = 4,
            shinydashboard::box(
              title = "Registros", width = NULL, height = "300px", 
              solidHeader = TRUE, status = "warning",
              div(style = "height: 230px; overflow-y: auto;",
                textOutput("hist_console_logs")
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
  
  # Reactive value to store the selected comuna info
  selected_comuna <- reactiveVal(list(codigo = NULL, nombre = NULL))
  
  # Update selected_comuna when nombre_comuna changes
  observeEvent(input$nombre_comuna, {
    selected_comuna(list(
      codigo = input$nombre_comuna,
      nombre = names(which(comunas_choices == input$nombre_comuna))
    ))
  })
  
  # Render the spatial plot
  output$spatial_plot <- renderPlot({
    req(input$generate_plot)
    comuna_info <- selected_comuna()
    
    selected_polygon <- comunas_shp %>%
      filter(cod_comuna == comuna_info$codigo)
    
    ggplot() +
      geom_sf(data = selected_polygon, fill = "lightblue", color = "darkblue") +
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
  
  # Process data and generate plot
  processed_data <- eventReactive(input$generate_plot, {
    comuna_info <- selected_comuna()
    
    # Initialize logs
    output$console_logs <- renderText(paste0("Procesamiento iniciado para la comuna: ", 
                                           comuna_info$nombre, " (", comuna_info$codigo, ")"))
    
    # Step 1: Process variable
    output$console_logs <- renderText("Paso 1: Procesando variable...")
    
    resultado <- if (input$modelo == "Todos") {
      all_models <- available_models[[input$nombre_variable]]
      bind_rows(lapply(all_models, function(modelo_actual) {
        output$console_logs <- renderText(paste("Procesando modelo:", modelo_actual))
        procesar_variable_comuna(
          nombre_variable = input$nombre_variable,
          codigo_comuna = comuna_info$codigo,
          modelo = modelo_actual
        )
      }))
    } else {
      procesar_variable_comuna(
        nombre_variable = input$nombre_variable,
        codigo_comuna = comuna_info$codigo,
        modelo = input$modelo
      )
    }
    
    # Step 2: Calculate relative values
    output$console_logs <- renderText("Paso 2: Calculando valores relativos...")
    relative_values <- calculate_relative_values(
      data = resultado,
      periodo_referencia_ini = input$periodo_referencia_ini,
      periodo_referencia_fin = input$periodo_referencia_fin,
      nombre_variable = input$nombre_variable,
      modelo = input$modelo
    )
    
    # Step 3: Generate plot
    output$console_logs <- renderText("Paso 3: Generando gráfico...")
    plot <- plot_time_series(
      data = relative_values,
      modelo = input$modelo,
      nombre_variable = input$nombre_variable,
      periodo_referencia = paste0(input$periodo_referencia_ini, "-", input$periodo_referencia_fin),
      nombre_comuna = comuna_info$nombre
    )
    
    # Step 4: Calculate summary statistics
    output$console_logs <- renderText("Paso 4: Calculando estadísticas de resumen...")
    summary_stats <- compute_summary_statistics(
      data = relative_values,
      modelo = input$modelo,
      nombre_variable = input$nombre_variable,
      periodo_referencia_ini = input$periodo_referencia_ini,
      periodo_referencia_fin = input$periodo_referencia_fin,
      nombre_comuna = comuna_info$nombre
    )
    
    # Render summary statistics
    output$summary_stats <- renderUI({
      HTML(paste0(
        "<p>Para la comuna <b>", comuna_info$nombre, "</b> (código: ", comuna_info$codigo, "):</p>",
        summary_stats$summary_paragraph
      ))
    })
    
    output$console_logs <- renderText("Procesamiento completado exitosamente!")
    
    list(
      plot = plot,
      summary_stats = summary_stats,
      codigo_comuna = comuna_info$codigo,
      nombre_comuna = comuna_info$nombre
    )
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
  
  # Reactive value for historical data
  selected_hist_comuna <- reactiveVal(list(codigo = NULL, nombre = NULL))
  
  # Update selected_hist_comuna when hist_nombre_comuna changes
  observeEvent(input$hist_nombre_comuna, {
    selected_hist_comuna(list(
      codigo = input$hist_nombre_comuna,
      nombre = names(which(comunas_choices == input$hist_nombre_comuna))
    ))
  })
  
  # Render the historical spatial plot
  output$hist_spatial_plot <- renderPlot({
    req(input$generate_hist_plot, input$hist_nombre_comuna)
    
    print("Attempting to create historical spatial plot...")
    print(paste("Selected comuna:", input$hist_nombre_comuna))
    
    selected_polygon <- comunas_shp %>%
      filter(Comuna == input$hist_nombre_comuna |
             tolower(Comuna) == tolower(input$hist_nombre_comuna))
    
    if (nrow(selected_polygon) == 0) {
      print("ERROR: No matching polygon found!")
      print("Input comuna:", input$hist_nombre_comuna)
      print("Sample of available comuna names:")
      print(head(sort(unique(comunas_shp$Comuna)), 10))
      return(ggplot() +
        annotate("text", x = 0, y = 0, 
                label = "No se encontró la comuna en el mapa",
                size = 5) +
        theme_void())
    }
    
    print("Found matching polygon")
    
    ggplot() +
      geom_sf(data = selected_polygon, fill = "lightblue", color = "darkblue") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.margin = margin(0, 0, 0, 0, "pt")
      ) +
      ggtitle(input$hist_nombre_comuna) +
      coord_sf(expand = FALSE)
  }, height = function() 300, width = function() 300)
  
  # Process historical data
  processed_hist_data <- eventReactive(input$generate_hist_plot, {
    req(input$hist_nombre_comuna)
    nombre_comuna <- input$hist_nombre_comuna
    
    output$hist_console_logs <- renderText(paste0("Procesando datos históricos para la comuna: ", nombre_comuna))
    
    # Step 1: Process historical data
    output$hist_console_logs <- renderText("Paso 1: Leyendo datos históricos...")
    datos_hist <- procesar_datos_historicos(input$hist_variable, nombre_comuna)
    
    # Step 2: Calculate statistics
    output$hist_console_logs <- renderText("Paso 2: Calculando estadísticas...")
    estadisticas <- compute_climate_statistics(
      datos = datos_hist,
      variable = input$hist_variable,
      tipo_temp = if(input$hist_variable == "temp") input$tipo_temp else NULL,
      nombre_comuna = nombre_comuna
    )
    
    # Step 3: Generate plot
    output$hist_console_logs <- renderText("Paso 3: Generando gráfico...")
    plot <- plot_serie_temporal(
      datos = datos_hist,
      variable = input$hist_variable,
      tipo_temp = if(input$hist_variable == "temp") input$tipo_temp else NULL,
      nombre_comuna = nombre_comuna
    )
    
    output$hist_summary_stats <- renderUI({
      HTML(estadisticas$summary_text)
    })
    
    output$hist_console_logs <- renderText("Procesamiento completado exitosamente!")
    
    list(
      plot = plot$plot,
      summary_stats = estadisticas,
      codigo_comuna = nombre_comuna,
      nombre_comuna = nombre_comuna
    )
  })
  
  # Render the historical time series plot
  output$hist_time_series_plot <- renderPlot({
    processed_hist_data()$plot
  })
  
  # Save historical plot and summary
  observeEvent(input$save_hist_plot, {
    req(processed_hist_data())
    
    folder_path <- paste0(getwd(), "/BBDD/resultados/historico/", 
                         processed_hist_data()$codigo_comuna, "/", 
                         input$hist_variable, "/")
    
    if (!dir.exists(folder_path)) {
      dir.create(folder_path, recursive = TRUE)
    }
    
    # Define file names
    base_name <- paste0(
      if(input$hist_variable == "temp") 
        paste0("_", input$tipo_temp) 
      else 
        ""
    )
    
    plot_file <- paste0("grafico_historico_", input$hist_variable, base_name, ".png")
    summary_file <- paste0("resumen_historico_", input$hist_variable, base_name, ".txt")
    
    # Save files
    ggsave(file.path(folder_path, plot_file), 
           plot = processed_hist_data()$plot, 
           width = 12, height = 8, units = "in", dpi = 300)
    
    writeLines(processed_hist_data()$summary_stats$summary_text, 
              file.path(folder_path, summary_file))
    
    output$hist_console_logs <- renderText(paste("Resultados guardados en:", folder_path))
  })

  # Dynamic comuna selector for historical data
  output$hist_comuna_selector <- renderUI({
    req(input$hist_variable)
    comunas_disponibles <- get_available_comunas(input$hist_variable)
    selectInput("hist_nombre_comuna", "Nombre de Comuna:", 
                choices = sort(comunas_disponibles),
                selected = NULL)
  })
}

# Run the application
shinyApp(ui, server)