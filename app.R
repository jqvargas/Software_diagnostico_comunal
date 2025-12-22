# Load all required packages
library(shiny)
library(dplyr)
library(ggplot2)
library(sf)
library(raster)
library(viridis)
library(RColorBrewer)
library(readr)
library(readxl)
library(stringr)
library(stringi)
library(knitr)
library(fs)
library(rmarkdown)
library(webshot2)


####### Source data
source(paste0(getwd(), "/indicadores_futuros/procesar_arclim.R"))
source(paste0(getwd(), "/procesar_historico.R"))  # Add source for historical processing
source(paste0(getwd(), "/procesar_uso_suelo.R"))  # Add source for land use processing
source(paste0(getwd(), "/procesar_topografia.R"))  # Add source for topography processing
source(paste0(getwd(), "/indicadores_futuros/procesar_riesgo_agricultura.R"))  # Add source for agricultural risk processing
source(paste0(getwd(), "/indicadores_futuros/procesar_riesgo_incendio_bosque.R"))  # Add source for forest fire risk processing
source(paste0(getwd(), "/indicadores_futuros/procesar_amenazas.R"))  # Add source for climate threats processing

# Explicitly import select from dplyr
select <- dplyr::select

# Read configuration files
print("Loading available models...")
available_models_df <- read.csv(paste0(getwd(), "/BBDD/ARCLIM/modelos_disponibles.csv"))
print("Available models columns:")
print(names(available_models_df))
available_models <- split(available_models_df$modelo, available_models_df$variable)
print("Available models by variable:")
print(str(available_models))

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
library(dplyr, warn.conflicts = FALSE)  # Load dplyr first and suppress conflict warnings
library(ggplot2)
library(shinyFiles)
library(shinydashboard)
library(shinydashboardPlus)
library(sf)
library(stringi)
library(tidyr)  # Add tidyr for data manipulation
##########

# Define GUI
ui <- shinydashboardPlus::dashboardPage(
  header = shinydashboardPlus::dashboardHeader(
    title = "Herramienta de Diagnóstico Comunal",
    titleWidth = 300
  ),
  sidebar = shinydashboardPlus::dashboardSidebar(
    width = 300,
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Selección de Diagnóstico", tabName = "data_selection", icon = icon("chart-line")),
      shinydashboard::menuSubItem("Diagnóstico Clima Histórico", tabName = "historical_data", icon = icon("clock")),
      shinydashboard::menuSubItem("Diagnóstico Clima Futuro", tabName = "future_data", icon = icon("arrow-right")),
      shinydashboard::menuSubItem("Usos de la Tierra", tabName = "land_use_data", icon = icon("tree")),
      shinydashboard::menuSubItem("Topografía", tabName = "topography_data", icon = icon("mountain")),
      shinydashboard::menuSubItem("Riesgo Agrícola", tabName = "agricultural_risk", icon = icon("tractor")),
      shinydashboard::menuSubItem("Riesgo de Incendios Forestales", tabName = "forest_fire_risk", icon = icon("fire")),
      shinydashboard::menuSubItem("Amenazas Climáticas", tabName = "climate_threats", icon = icon("cloud"))
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
      ),

      # Land Use Data Tab (new)
      shinydashboard::tabItem(tabName = "land_use_data",
        # First row: Control panel, Time series plot, and Map
        fluidRow(
          # Control panel (1/4 of width)
          column(width = 3,
            shinydashboard::box(
              title = "Panel de Control", width = NULL, solidHeader = TRUE, status = "primary",
              uiOutput("land_use_comuna_selector"),  # Dynamic comuna selector
              div(style = "margin-top: 20px;",
                actionButton("read_land_use", "Leer mapas uso de suelo", 
                           class = "btn-info", style = "width: 100%; margin-bottom: 10px;"),
                uiOutput("land_use_year_selector"),  # Dynamic year selector
                actionButton("generate_land_use", "Generar Mapa", 
                           class = "btn-primary", style = "width: 100%; margin-bottom: 10px;"),
                actionButton("save_land_use", "Guardar Resultados", 
                           class = "btn-success", style = "width: 100%;")
              )
            )
          ),
          
          # Map (2/3 of width)
          column(width = 9,
            shinydashboard::box(
              title = "Mapa de Uso de Suelo", width = NULL, solidHeader = TRUE, status = "info",
              plotOutput("land_use_plot", height = "500px")
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
                verbatimTextOutput("land_use_summary")
              )
            )
          ),
          
          # Logs (1/3 of width)
          column(width = 4,
            shinydashboard::box(
              title = "Registros", width = NULL, height = "300px", 
              solidHeader = TRUE, status = "warning",
              div(style = "height: 230px; overflow-y: auto;",
                textOutput("land_use_logs")
              )
            )
          )
        )
      ),

      # Topography Data Tab
      shinydashboard::tabItem(tabName = "topography_data",
        # First row: Control panel and Map
        fluidRow(
          # Control panel (1/4 of width)
          column(width = 3,
            shinydashboard::box(
              title = "Panel de Control", width = NULL, solidHeader = TRUE, status = "primary",
              uiOutput("topo_comuna_selector"),  # Dynamic comuna selector
              div(style = "margin-top: 20px;",
                actionButton("generate_topo", "Generar Análisis", 
                           class = "btn-primary", style = "width: 100%; margin-bottom: 10px;"),
                actionButton("save_topo", "Guardar Resultados", 
                           class = "btn-success", style = "width: 100%;")
              )
            )
          ),
          
          # Map (3/4 of width)
          column(width = 9,
            shinydashboard::box(
              title = "Mapa Topográfico", width = NULL, solidHeader = TRUE, status = "info",
              plotOutput("topo_plot", height = "500px")
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
                verbatimTextOutput("topo_summary")
              )
            )
          ),
          
          # Logs (1/3 of width)
          column(width = 4,
            shinydashboard::box(
              title = "Registros", width = NULL, height = "300px", 
              solidHeader = TRUE, status = "warning",
              div(style = "height: 230px; overflow-y: auto;",
                textOutput("topo_logs")
              )
            )
          )
        )
      ),

      # Agricultural Risk Tab
      shinydashboard::tabItem(tabName = "agricultural_risk",
        # First row: Control panel, Report, and Map
        fluidRow(
          # Control panel (1/4 of width)
          column(width = 3,
            shinydashboard::box(
              title = "Panel de Control", width = NULL, solidHeader = TRUE, status = "primary",
              selectInput("agri_sector", "Sector Productivo:", 
                        choices = c(
                          "Almendras" = "almendras",
                          "Bovinos Carne" = "bovinos_carne",
                          "Bovinos Leche" = "bovinos_leche",
                          "Cereza" = "cereza",
                          "Frejol" = "frejol",
                          "Maíz" = "maiz",
                          "Manzana Roja" = "manzana_roja",
                          "Nueces" = "nueces",
                          "Ovinos" = "ovinos",
                          "Papa Riego" = "papa_riego",
                          "Papa Secano" = "papa_secano",
                          "Pradera" = "pradera",
                          "Trigo Riego" = "trigo_riego",
                          "Trigo Secano" = "trigo_secano"
                        )),
              uiOutput("agri_comuna_selector"),  # Dynamic comuna selector
              div(style = "margin-top: 20px;",
                actionButton("generate_agri_report", "Generar Informe", 
                           class = "btn-primary", style = "width: 100%; margin-bottom: 10px;"),
                actionButton("save_agri_report", "Guardar Resultados", 
                           class = "btn-success", style = "width: 100%;")
              )
            )
          ),
          
          # Report (1/2 of width)
          column(width = 6,
            shinydashboard::box(
              title = "Informe de Riesgo Agrícola", width = NULL, solidHeader = TRUE, status = "info",
              div(style = "height: 500px; overflow-y: auto;",
                htmlOutput("agri_report")
              )
            )
          ),
          
          # Map (1/4 of width)
          column(width = 3,
            shinydashboard::box(
              title = "Mapa de Comuna", width = NULL, solidHeader = TRUE, status = "info",
              plotOutput("agri_spatial_plot", height = "300px")
            )
          )
        ),
        
        # Logs row
        fluidRow(
          column(width = 12,
            shinydashboard::box(
              title = "Registros", width = NULL, height = "200px", solidHeader = TRUE, status = "warning",
              div(style = "height: 130px; overflow-y: auto;",
                textOutput("agri_logs")
              )
            )
          )
        )
      ),

      # Forest Fire Risk Tab
      shinydashboard::tabItem(tabName = "forest_fire_risk",
        # First row: Control panel, Report, and Map
        fluidRow(
          # Control panel (1/4 of width)
          column(width = 3,
            shinydashboard::box(
              title = "Panel de Control", width = NULL, solidHeader = TRUE, status = "primary",
              selectInput("forest_type", "Tipo de Bosque:", 
                        choices = c(
                          "Bosque Nativo" = "bnativos",
                          "Plantaciones" = "plantaciones"
                        )),
              selectInput("forest_period", "Período:", 
                        choices = c(
                          "Presente" = "pres",
                          "Futuro" = "fut"
                        )),
              uiOutput("forest_comuna_selector"),  # Dynamic comuna selector
              div(style = "margin-top: 20px;",
                actionButton("generate_forest_report", "Generar Informe", 
                           class = "btn-primary", style = "width: 100%; margin-bottom: 10px;"),
                actionButton("save_forest_report", "Guardar Resultados", 
                           class = "btn-success", style = "width: 100%;")
              )
            )
          ),
          
          # Report (1/2 of width)
          column(width = 6,
            shinydashboard::box(
              title = "Informe de Riesgo de Incendios Forestales", width = NULL, solidHeader = TRUE, status = "info",
              div(style = "height: 500px; overflow-y: auto;",
                htmlOutput("forest_report")
              )
            )
          ),
          
          # Map (1/4 of width)
          column(width = 3,
            shinydashboard::box(
              title = "Mapa de Comuna", width = NULL, solidHeader = TRUE, status = "info",
              plotOutput("forest_spatial_plot", height = "300px")
            )
          )
        ),
        
        # Logs row
        fluidRow(
          column(width = 12,
            shinydashboard::box(
              title = "Registros", width = NULL, height = "200px", solidHeader = TRUE, status = "warning",
              div(style = "height: 130px; overflow-y: auto;",
                textOutput("forest_logs")
              )
            )
          )
        )
      ),

      # Climate Threats Tab
      shinydashboard::tabItem(tabName = "climate_threats",
        # First row: Control panel, Report, and Map
        fluidRow(
          # Control panel (1/4 of width)
          column(width = 3,
            shinydashboard::box(
              title = "Panel de Control", width = NULL, solidHeader = TRUE, status = "primary",
              uiOutput("threats_variable_selector"),  # Dynamic variable selector
              uiOutput("threats_comuna_selector"),  # Dynamic comuna selector
              div(style = "margin-top: 20px;",
                actionButton("generate_threats_report", "Generar Informe", 
                           class = "btn-primary", style = "width: 100%; margin-bottom: 10px;"),
                actionButton("save_threats_report", "Guardar Resultados", 
                           class = "btn-success", style = "width: 100%;")
              )
            )
          ),
          
          # Report (1/2 of width)
          column(width = 6,
            shinydashboard::box(
              title = "Informe de Amenazas Climáticas", width = NULL, solidHeader = TRUE, status = "info",
              div(style = "height: 500px; overflow-y: auto;",
                htmlOutput("threats_report")
              )
            )
          ),
          
          # Map (1/4 of width)
          column(width = 3,
            shinydashboard::box(
              title = "Mapa de Comuna", width = NULL, solidHeader = TRUE, status = "info",
              plotOutput("threats_spatial_plot", height = "300px")
            )
          )
        ),
        
        # Logs row
        fluidRow(
          column(width = 12,
            shinydashboard::box(
              title = "Registros", width = NULL, height = "200px", solidHeader = TRUE, status = "warning",
              div(style = "height: 130px; overflow-y: auto;",
                textOutput("threats_logs")
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
  
  # Dynamically update the modelo dropdown based on the selected variable
  observeEvent(input$nombre_variable, {
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
  
  # Save plot and summary functionality
  observeEvent(input$save_plot, {
    # Check if a plot has been generated
    req(processed_data())
    
    # Get the selected comuna info
    codigo_comuna <- processed_data()$codigo_comuna
    nombre_comuna <- processed_data()$nombre_comuna
    
    # Create the folder structure with the new path
    folder_path <- paste0(getwd(), "/BBDD/resultados/proyecciones/", 
                         nombre_comuna, "/",
                         input$nombre_variable, "/",
                         input$modelo, "/")
    
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

  # Dynamic comuna selector for land use data
  output$land_use_comuna_selector <- renderUI({
    tryCatch({
      available_comunas<-get_available_comunas(variable="temp")
      if (length(available_comunas) > 0) {
        selectInput("land_use_comuna", "Nombre de Comuna:",
                   choices = sort(available_comunas),
                   selected = NULL)
      } else {
        div(
          style = "color: red;",
          "No se encontraron comunas con datos de uso de suelo disponibles"
        )
      }
    }, error = function(e) {
      div(
        style = "color: red;",
        paste("Error al cargar comunas:", e$message)
      )
    })
  })

  # Reactive value to store land use data
  land_use_data <- reactiveVal(NULL)
  available_years <- reactiveVal(NULL)

  # Read land use data when button is clicked
  observeEvent(input$read_land_use, {
    req(input$land_use_comuna)
    
    output$land_use_logs <- renderText("Leyendo datos de uso de suelo...")
    
    # Get the data
    tryCatch({
      data <- get_land_use_data(input$land_use_comuna)
      land_use_data(data)
      
      # Extract available years from IPCC columns
      ipcc_cols <- sort(names(data)[grep("^SUB_IPCC", names(data))])
      years <- as.numeric(paste0("20", substr(ipcc_cols, 9, 10)))
      available_years(years)
      
      output$land_use_logs <- renderText("Datos leídos exitosamente. Seleccione un año para generar el análisis.")
    }, error = function(e) {
      output$land_use_logs <- renderText(paste("Error:", e$message))
    })
  })

  # Dynamic year selector UI
  output$land_use_year_selector <- renderUI({
    req(available_years())
    years <- available_years()
    
    selectInput("land_use_year", "Seleccionar año para mapear:",
                choices = years,
                selected = max(years))
  })

  # Reactive values to store current plot and analysis
  current_land_use_plot <- reactiveVal(NULL)
  current_land_use_analysis <- reactiveVal(NULL)

  # Process land use data and generate outputs
  observeEvent(input$generate_land_use, {
    req(land_use_data(), input$land_use_year)
    
    output$land_use_logs <- renderText("Generando mapa...")
    
    tryCatch({
      # Generate plot with selected year
      plot <- plot_land_use_with_boundary(
        nombre_comuna = input$land_use_comuna,
        land_use_data = land_use_data(),
        year = as.numeric(input$land_use_year)
      )
      
      # Generate analysis
      analysis <- analyze_land_use_changes(land_use_data(), input$land_use_comuna)
      
      # Store current plot and analysis
      current_land_use_plot(plot)
      current_land_use_analysis(analysis)
      
      # Update outputs
      output$land_use_plot <- renderPlot({ plot })
      output$land_use_summary <- renderText({ analysis$summary_text })
      output$land_use_logs <- renderText("Mapa generado exitosamente!")
      
    }, error = function(e) {
      output$land_use_logs <- renderText(paste("Error:", e$message))
    })
  })

  # Save land use results
  observeEvent(input$save_land_use, {
    req(current_land_use_plot(), current_land_use_analysis(), input$land_use_comuna, input$land_use_year)
    
    # Create the folder structure
    folder_path <- paste0(getwd(), "/BBDD/resultados/usos_suelo/", input$land_use_comuna, "/")
    
    if (!dir.exists(folder_path)) {
      dir.create(folder_path, recursive = TRUE)
    }
    
    # Save plot
    plot_file <- paste0(folder_path, "mapa_uso_suelo_", input$land_use_year, ".png")
    ggsave(plot_file, plot = current_land_use_plot(), 
           width = 12, height = 8, units = "in", dpi = 300)
    
    # Save summary
    summary_file <- paste0(folder_path, "resumen_uso_suelo_", input$land_use_year, ".txt")
    writeLines(current_land_use_analysis()$summary_text, summary_file)
    
    output$land_use_logs <- renderText(paste("Resultados guardados en:", folder_path))
  })

  # Dynamic comuna selector for topography data
  output$topo_comuna_selector <- renderUI({
    tryCatch({
      available_comunas <- get_available_comunas(variable="temp")
      if (length(available_comunas) > 0) {
        selectInput("topo_comuna", "Nombre de Comuna:",
                   choices = sort(available_comunas),
                   selected = NULL)
      } else {
        div(
          style = "color: red;",
          "No se encontraron comunas con datos topográficos disponibles"
        )
      }
    }, error = function(e) {
      div(
        style = "color: red;",
        paste("Error al cargar comunas:", e$message)
      )
    })
  })

  # Process topography data and generate outputs
  topo_data <- eventReactive(input$generate_topo, {
    req(input$topo_comuna)
    
    output$topo_logs <- renderText("Procesando datos topográficos...")
    
    # Get the data and generate plot
    tryCatch({
      print(paste("Processing comuna:", input$topo_comuna))
      
      result <- plot_topografia(input$topo_comuna)
      
      output$topo_logs <- renderText("Procesamiento completado exitosamente!")
      
      return(result)
    }, error = function(e) {
      output$topo_logs <- renderText(paste("Error:", e$message))
      return(NULL)
    })
  })
  
  # Render the topography plot
  output$topo_plot <- renderPlot({
    req(topo_data())
    topo_data()$plot
  })
  
  # Render the summary text
  output$topo_summary <- renderText({
    req(topo_data())
    topo_data()$summary
  })
  
  # Save topography results
  observeEvent(input$save_topo, {
    req(topo_data(), input$topo_comuna)
    
    # Create the folder structure
    folder_path <- paste0(getwd(), "/BBDD/resultados/topografia/", input$topo_comuna, "/")
    
    if (!dir.exists(folder_path)) {
      dir.create(folder_path, recursive = TRUE)
    }
    
    # Save plot
    plot_file <- paste0(folder_path, "mapa_topografico.png")
    # For base R plots, we need to use png() and dev.off()
    png(plot_file, width = 12, height = 8, units = "in", res = 300)
    replayPlot(topo_data()$plot)
    dev.off()
    
    # Save summary
    summary_file <- paste0(folder_path, "resumen_topografico.txt")
    writeLines(topo_data()$summary, summary_file)
    
    output$topo_logs <- renderText(paste("Resultados guardados en:", folder_path))
  })

  # Agricultural Risk Section
  
  # Update comuna selector with available comunas
  output$agri_comuna_selector <- renderUI({
    tryCatch({
      # Get available comunas from the ARCLIM agriculture data
      ruta_archivo <- "BBDD/ARCLIM/riesgo/Agricultura/ARCLIM_agricultura.xlsx"
      datos <- read_excel(ruta_archivo, sheet = "DATOS")
      available_comunas <- sort(unique(datos$NOM_COMUNA))
      
      if (length(available_comunas) > 0) {
        selectInput("agri_comuna", "Nombre de Comuna:",
                   choices = available_comunas,
                   selected = NULL)
      } else {
        div(
          style = "color: red;",
          "No se encontraron comunas con datos de riesgo agrícola disponibles"
        )
      }
    }, error = function(e) {
      div(
        style = "color: red;",
        paste("Error al cargar comunas:", e$message)
      )
    })
  })
  
  # Reactive values to store current report and plot
  current_agri_report <- reactiveVal(NULL)
  current_agri_plot <- reactiveVal(NULL)
  
  # Process agricultural risk data and generate outputs
  observeEvent(input$generate_agri_report, {
    req(input$agri_comuna, input$agri_sector)
    
    output$agri_logs <- renderText("Generando informe de riesgo agrícola...")
    
    tryCatch({
      # Generate report and visualization
      result <- generar_informe_riesgo_agricola(input$agri_comuna, input$agri_sector)
      
      # Store current report and plot
      current_agri_report(result)
      
      # Update outputs
      output$agri_report <- renderUI({ HTML(result$informe_html) })
      
      # Generate and display spatial plot
      output$agri_spatial_plot <- renderPlot({
        # Get comuna boundaries
        selected_polygon <- comunas_shp %>%
          filter(tolower(Comuna) == tolower(input$agri_comuna))
        
        if (nrow(selected_polygon) == 0) {
          return(ggplot() +
            annotate("text", x = 0, y = 0, 
                    label = "No se encontró la comuna en el mapa",
                    size = 5) +
            theme_void())
        }
        
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
          ggtitle(input$agri_comuna) +
          coord_sf(expand = FALSE)
      })
      
      output$agri_logs <- renderText("Informe generado exitosamente!")
      
    }, error = function(e) {
      output$agri_logs <- renderText(paste("Error:", e$message))
    })
  })
  
  # Save agricultural risk results
  observeEvent(input$save_agri_report, {
    req(current_agri_report(), input$agri_comuna, input$agri_sector)
    
    # Create the folder structure
    folder_path <- paste0(getwd(), "/BBDD/resultados/riesgo_agricola/", 
                         input$agri_comuna, "/",
                         input$agri_sector, "/")
    
    if (!dir.exists(folder_path)) {
      dir.create(folder_path, recursive = TRUE)
    }
    
    # Save report as HTML
    report_file <- paste0(folder_path, "informe_riesgo_agricola.html")
    writeLines(current_agri_report()$informe_html, report_file)
    
    # Save spatial plot
    spatial_plot_file <- paste0(folder_path, "mapa_comuna.png")
    selected_polygon <- comunas_shp %>%
      filter(tolower(Comuna) == tolower(input$agri_comuna))
    
    spatial_plot <- ggplot() +
      geom_sf(data = selected_polygon, fill = "lightblue", color = "darkblue") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()
      ) +
      ggtitle(input$agri_comuna) +
      coord_sf(expand = FALSE)
    
    ggsave(spatial_plot_file, plot = spatial_plot,
           width = 8, height = 8, units = "in", dpi = 300)
    
    output$agri_logs <- renderText(paste("Resultados guardados en:", folder_path))
  })

  # Forest Fire Risk Section
  
  # Update comuna selector with available comunas
  output$forest_comuna_selector <- renderUI({
    tryCatch({
      # Get available comunas from the forest fire risk data
      ruta_archivo <- "BBDD/ARCLIM/riesgo/Bosques_nativos/ARCLIM_incendios_bosques_nativos_comunas.xlsx"
      datos <- read_excel(ruta_archivo, sheet = "DATOS")
      available_comunas <- sort(unique(datos$NOM_COMUNA))
      
      if (length(available_comunas) > 0) {
        selectInput("forest_comuna", "Nombre de Comuna:",
                   choices = available_comunas,
                   selected = NULL)
      } else {
        div(
          style = "color: red;",
          "No se encontraron comunas con datos de riesgo de incendios forestales disponibles"
        )
      }
    }, error = function(e) {
      div(
        style = "color: red;",
        paste("Error al cargar comunas:", e$message)
      )
    })
  })
  
  # Reactive values to store current report and plot
  current_forest_report <- reactiveVal(NULL)
  current_forest_plot <- reactiveVal(NULL)
  
  # Process forest fire risk data and generate outputs
  observeEvent(input$generate_forest_report, {
    req(input$forest_comuna, input$forest_type, input$forest_period)
    
    output$forest_logs <- renderText("Generando informe de riesgo de incendios forestales...")
    
    tryCatch({
      # Generate report using the forest fire risk function
      result <- generar_informe_riesgo(
        nombre_comuna = input$forest_comuna,
        tipo_bosque = input$forest_type,
        periodo = input$forest_period
      )
      
      # Store current report
      current_forest_report(result)
      
      # Update outputs
      output$forest_report <- renderUI({ HTML(result$informe_html) })
      
      # Generate and display spatial plot
      output$forest_spatial_plot <- renderPlot({
        # Get comuna boundaries
        selected_polygon <- comunas_shp %>%
          filter(tolower(Comuna) == tolower(input$forest_comuna))
        
        if (nrow(selected_polygon) == 0) {
          return(ggplot() +
            annotate("text", x = 0, y = 0, 
                    label = "No se encontró la comuna en el mapa",
                    size = 5) +
            theme_void())
        }
        
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
          ggtitle(input$forest_comuna) +
          coord_sf(expand = FALSE)
      })
      
      output$forest_logs <- renderText("Informe generado exitosamente!")
      
    }, error = function(e) {
      output$forest_logs <- renderText(paste("Error:", e$message))
    })
  })
  
  # Save forest fire risk results
  observeEvent(input$save_forest_report, {
    req(current_forest_report(), input$forest_comuna, input$forest_type, input$forest_period)
    
    # Create the folder structure
    folder_path <- paste0(getwd(), "/BBDD/resultados/riesgo_incendios/", 
                         input$forest_comuna, "/",
                         input$forest_type, "/",
                         input$forest_period, "/")
    
    if (!dir.exists(folder_path)) {
      dir.create(folder_path, recursive = TRUE)
    }
    
    # Save report as HTML
    report_file <- paste0(folder_path, "informe_riesgo_incendios.html")
    writeLines(current_forest_report()$informe_html, report_file)
    
    # Save spatial plot
    spatial_plot_file <- paste0(folder_path, "mapa_comuna.png")
    selected_polygon <- comunas_shp %>%
      filter(tolower(Comuna) == tolower(input$forest_comuna))
    
    spatial_plot <- ggplot() +
      geom_sf(data = selected_polygon, fill = "lightblue", color = "darkblue") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()
      ) +
      ggtitle(input$forest_comuna) +
      coord_sf(expand = FALSE)
    
    ggsave(spatial_plot_file, plot = spatial_plot,
           width = 8, height = 8, units = "in", dpi = 300)
    
    output$forest_logs <- renderText(paste("Resultados guardados en:", folder_path))
  })

  # Climate Threats Section
  
  # Update variable selector with available variables
  output$threats_variable_selector <- renderUI({
    tryCatch({
      # Get available variables from the threats data
      ruta_archivo <- "BBDD/ARCLIM/indicadores_amenazas/amenazas_comunas_annual.xlsx"
      variables_df <- read_excel(ruta_archivo, sheet = "VARIABLES")
      available_variables <- sort(unique(variables_df$Nombre))
      
      selectInput("threats_variable", "Variable:",
                 choices = available_variables,
                 selected = NULL)
    }, error = function(e) {
      div(
        style = "color: red;",
        "Error al cargar variables de amenazas"
      )
    })
  })
  
  # Update comuna selector with available comunas
  output$threats_comuna_selector <- renderUI({
    tryCatch({
      # Get available comunas from the threats data
      ruta_archivo <- "BBDD/ARCLIM/indicadores_amenazas/amenazas_comunas_annual.xlsx"
      features_df <- read_excel(ruta_archivo, sheet = "FEATURES")
      available_comunas <- sort(unique(features_df$NOM_COMUNA))
      
      selectInput("threats_comuna", "Nombre de Comuna:",
                 choices = available_comunas,
                 selected = NULL)
    }, error = function(e) {
      div(
        style = "color: red;",
        "Error al cargar comunas"
      )
    })
  })
  
  # Reactive values to store current report and plot
  current_threats_report <- reactiveVal(NULL)
  
  # Process threats data and generate outputs
  observeEvent(input$generate_threats_report, {
    req(input$threats_comuna, input$threats_variable)
    
    output$threats_logs <- renderText("Generando informe de amenazas climáticas...")
    
    tryCatch({
      # Generate report
      result <- generar_informe_amenazas(
        nombre_comuna = input$threats_comuna,
        variable = input$threats_variable
      )
      
      # Store current report
      current_threats_report(result)
      
      # Update outputs
      output$threats_report <- renderUI({ HTML(result$informe_html) })
      
      # Generate and display spatial plot
      output$threats_spatial_plot <- renderPlot({
        # Get comuna boundaries
        selected_polygon <- comunas_shp %>%
          filter(tolower(Comuna) == tolower(input$threats_comuna))
        
        if (nrow(selected_polygon) == 0) {
          return(ggplot() +
            annotate("text", x = 0, y = 0, 
                    label = "No se encontró la comuna en el mapa",
                    size = 5) +
            theme_void())
        }
        
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
          ggtitle(input$threats_comuna) +
          coord_sf(expand = FALSE)
      })
      
      output$threats_logs <- renderText("Informe generado exitosamente!")
      
    }, error = function(e) {
      output$threats_logs <- renderText(paste("Error:", e$message))
    })
  })
  
  # Save threats results
  observeEvent(input$save_threats_report, {
    req(current_threats_report(), input$threats_comuna, input$threats_variable)
    
    # Create the folder structure
    folder_path <- paste0(getwd(), "/BBDD/resultados/amenazas/", 
                         input$threats_comuna, "/")
    
    if (!dir.exists(folder_path)) {
      dir.create(folder_path, recursive = TRUE)
    }
    
    # Save report as HTML
    report_file <- paste0(folder_path, "informe_amenazas_", 
                         gsub(" ", "_", input$threats_variable), ".html")
    writeLines(current_threats_report()$informe_html, report_file)
    
    # Save spatial plot
    spatial_plot_file <- paste0(folder_path, "mapa_comuna.png")
    selected_polygon <- comunas_shp %>%
      filter(tolower(Comuna) == tolower(input$threats_comuna))
    
      geom_sf(data = selected_polygon, fill = "lightblue", color = "darkblue") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()
      ) +
      ggtitle(input$threats_comuna) +
      coord_sf(expand = FALSE)
    
    ggsave(spatial_plot_file, plot = spatial_plot,
           width = 8, height = 8, units = "in", dpi = 300)
    
    output$threats_logs <- renderText(paste("Resultados guardados en:", folder_path))
  })
}

# Run the application
# Run the application (only if running directly, not via runApp)
if (!exists("shiny_running")) {
  shinyApp(ui, server)
}