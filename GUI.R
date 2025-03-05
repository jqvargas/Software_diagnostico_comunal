####### Source data
source(paste0(getwd(), "/indicadores_futuros/procesar_arclim.R"))

# Read the CSV file with available models
available_models_df <- read.csv(paste0(getwd(), "/BBDD/ARCLIM/modelos_disponibles.csv"))
available_models <- split(available_models_df$modelo, available_models_df$variable)

############ Libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(shinyFiles)
library(shinydashboard)
library(shinydashboardPlus)
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
                              fluidRow(
                                shinydashboard::box(
                                  title = "Panel de Control", width = 4, solidHeader = TRUE, status = "primary",
                                  selectInput("nombre_variable", "Seleccionar Variable:", 
                                              choices = c("tasmax", "tasmin", "pr", "vel", "rsds", "huss")),
                                  textInput("codigo_comuna", "Código de Comuna:", value = "2201"),
                                  numericInput("periodo_referencia_ini", "Año Inicial del Período de Referencia:", value = 1980, min = 1950, max = 2020),
                                  numericInput("periodo_referencia_fin", "Año Final del Período de Referencia:", value = 2010, min = 1950, max = 2020),
                                  selectInput("modelo", "Seleccionar Modelo:", choices = NULL),  # Initially empty
                                  actionButton("generate_plot", "Generar Gráfico", class = "btn-primary"),
                                  br(),
                                  br(),
                                  actionButton("save_plot", "Guardar Resultados", class = "btn-success")  # Save button
                                ),
                                shinydashboard::box(
                                  title = "Gráfico", width = 8, solidHeader = TRUE, status = "info",
                                  plotOutput("time_series_plot", height = "600px")
                                ),
                                shinydashboard::box(
                                  title = "Estadísticas de Resumen", width = 12, solidHeader = TRUE, status = "success",
                                  htmlOutput("summary_stats")  # Display summary statistics here
                                ),
                                shinydashboard::box(
                                  title = "Registros", width = 12, solidHeader = TRUE, status = "warning",
                                  textOutput("console_logs")  # Display logs here
                                )
                              )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
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
    # Initialize logs
    output$console_logs <- renderText("Procesamiento iniciado...")
    
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
          codigo_comuna = input$codigo_comuna,
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
        codigo_comuna = input$codigo_comuna,
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
                                  input$periodo_referencia_fin)
    )
    
    # Step 4: Calculate summary statistics
    output$console_logs <- renderText("Paso 4: Calculando estadísticas de resumen...")
    summary_stats <- compute_summary_statistics(
      data = relative_values,
      modelo = input$modelo,
      nombre_variable = input$nombre_variable,
      periodo_referencia_ini = input$periodo_referencia_ini,
      periodo_referencia_fin = input$periodo_referencia_fin
    )
    
    # Render the summary statistics paragraph
    output$summary_stats <- renderUI({
      # Add the comuna code to the beginning of the summary paragraph
      comuna_info <- paste0("<p>Para el código de comuna <b>", input$codigo_comuna, "</b>:</p>")
      HTML(paste0(comuna_info, summary_stats$summary_paragraph))
    })
    
    # Final log message
    output$console_logs <- renderText("Procesamiento completado exitosamente!")
    
    return(list(
      plot = plot,
      summary_stats = summary_stats
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
    
    # Create the folder structure
    folder_path <- paste0(getwd(), "/BBDD/resultados/", input$codigo_comuna, "/", input$nombre_variable, "/", input$modelo, "/")
    
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
    summary_text <- paste0("Resumen para la comuna ", input$codigo_comuna, ":\n\n", summary_text)
    
    # Write to file
    writeLines(summary_text, summary_full_path)
    
    # Log the success message
    output$console_logs <- renderText(paste("Resultados guardados exitosamente en:", folder_path))
  })
  
}

# Run the application
shinyApp(ui, server)