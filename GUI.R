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
    title = "Climate Data Dashboard",
    titleWidth = 300
  ),
  sidebar = shinydashboardPlus::dashboardSidebar(
    width = 300,
    shinydashboard::sidebarMenu(  # Use shinydashboard::sidebarMenu
      shinydashboard::menuItem("Data Selection", tabName = "data_selection", icon = icon("chart-line")),
      shinydashboard::menuSubItem("Future Data", tabName = "future_data", icon = icon("arrow-right")),
      shinydashboard::menuItem("Historical Data", tabName = "historical_data", icon = icon("chart-bar"), disabled = TRUE) # Placeholder
    )
  ),
  body = shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      shinydashboard::tabItem(tabName = "future_data",
                              fluidRow(
                                shinydashboard::box(
                                  title = "Control Panel", width = 4, solidHeader = TRUE, status = "primary",
                                  selectInput("nombre_variable", "Select Variable:", 
                                              choices = c("tasmax", "tasmin", "pr", "vel", "rsds", "huss")),
                                  textInput("codigo_comuna", "Enter Comuna Code:", value = "2201"),
                                  numericInput("periodo_referencia_ini", "Reference Period Start Year:", value = 1980, min = 1950, max = 2020),
                                  numericInput("periodo_referencia_fin", "Reference Period End Year:", value = 2010, min = 1950, max = 2020),
                                  selectInput("modelo", "Select Model:", choices = NULL),  # Initially empty
                                  actionButton("generate_plot", "Generate Plot", class = "btn-primary"),
                                  br(),
                                  br(),
                                  actionButton("save_plot", "Save Plot", class = "btn-success")  # Save button
                                ),
                                shinydashboard::box(
                                  title = "Plot Output", width = 8, solidHeader = TRUE, status = "info",
                                  plotOutput("time_series_plot", height = "600px")
                                ),
                                shinydashboard::box(
                                  title = "Summary Statistics", width = 12, solidHeader = TRUE, status = "success",
                                  htmlOutput("summary_stats")  # Display summary statistics here
                                ),
                                shinydashboard::box(
                                  title = "Logs", width = 12, solidHeader = TRUE, status = "warning",
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
    
    # Update the modelo dropdown with the available models
    updateSelectInput(session, "modelo", choices = selected_models)
  })
  
  # Reactive expression to process data and generate plot
  processed_data <- eventReactive(input$generate_plot, {
    # Initialize logs
    output$console_logs <- renderText("Processing started...")
    
    # Step 1: Procesar variable
    output$console_logs <- renderText("Step 1: Processing variable...")
    resultado <- procesar_variable_comuna(
      nombre_variable = input$nombre_variable,
      codigo_comuna = input$codigo_comuna,
      modelo = input$modelo
    )
    
    # Step 2: Calcular periodo de referencia
    output$console_logs <- renderText("Step 2: Calculating relative values...")
    relative_values <- calculate_relative_values(
      data = resultado,
      periodo_referencia_ini = input$periodo_referencia_ini,
      periodo_referencia_fin = input$periodo_referencia_fin,
      nombre_variable = input$nombre_variable,
      modelo = input$modelo
    )
    
    # Step 3: Graficar resultados
    output$console_logs <- renderText("Step 3: Generating plot...")
    plot <- plot_time_series(
      data = relative_values,
      modelo = input$modelo,
      nombre_variable = input$nombre_variable,
      periodo_referencia = paste0(input$periodo_referencia_ini,
                                  "-",
                                  input$periodo_referencia_fin)
    )
    
    # Step 4: Calculate summary statistics
    output$console_logs <- renderText("Step 4: Calculating summary statistics...")
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
      comuna_info <- paste0("<p>For comuna code <b>", input$codigo_comuna, "</b>:</p>")
      HTML(paste0(comuna_info, summary_stats$summary_paragraph))
    })
    
    # Final log message
    output$console_logs <- renderText("Processing completed successfully!")
    
    return(list(
      plot = plot,
      summary_stats = summary_stats
    ))
  })
  
  
  # Render the plot
  output$time_series_plot <- renderPlot({
    processed_data()$plot
  })
  
  
  # Folder selection using shinyFiles
  volumes <- c(Home = fs::path_home())  # Default to the user's home directory
  shinyFileChoose(input, "folder", roots = volumes, session = session, filetypes = NULL)
  
  # Save plot functionality
  observeEvent(input$save_plot, {
    # Check if a plot has been generated
    req(processed_data())
    
    # Get the selected folder path
    folder_path <- parseFilePaths(volumes, input$folder)$datapath
    if (is.null(folder_path)) {
      output$console_logs <- renderText("Error: No folder selected. Please select a folder to save the plot.")
      return()
    }
    
    # Define the file name
    file_name <- paste0("plot_", input$nombre_variable, "_", input$modelo, ".png")
    full_path <- file.path(folder_path, file_name)
    
    # Save the plot as a .png file
    ggsave(full_path, plot = processed_data()$plot, width = 12, height = 8, units = "in", dpi = 300)
    
    # Log the success message
    output$console_logs <- renderText(paste("Plot saved successfully at:", full_path))
  })
  
  
}

# Run the application
shinyApp(ui, server)