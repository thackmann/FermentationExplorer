# Main Shiny App Script
# This script sets up the system locale, loads external R scripts, and defines the user interface (UI)
# and server components for the Shiny app. The app includes modules for 
# database searching, predictions, and user help, all organized within a Bootstrap-based layout.
# Author: Timothy Hackmann
# Date: 15 April 2025

# === Set system locale ===
  Sys.setlocale("LC_ALL", "C")

# === Set CRAN mirror ===
  options(repos = c(CRAN = "https://cloud.r-project.org"))

# === Load external R files ===
  # Define parent directories
  directories <- c("install", "functions", "variables", "modules")
  
  # Directories to exclude
  exclude_old <- function(dir) {
    files <- list.files(path = dir, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
    files[!grepl("/old/", files)]  # Exclude files inside "old" directories
  }
  
  # Source all R files in directories
  for (dir in directories) {
    files <- exclude_old(dir)
    
    for (file in files) {
      message("Sourcing: ", file)
        source(file, local = TRUE)
    }
  }
  
# === Define user interface (UI) ===
  ui <- bslib::page_fluid(
    title = "Fermentation Explorer",
    
    # --- Set style ---
    # Set Bootstrap version and theme
    theme = bslib::bs_theme(version = 5, preset = "shiny"),
    
    # Set theme for query builder
    jqbr::useQueryBuilder(bs_version = "5"),
    
    # Load JavaScript file from custom.js (/www folder)
    shinyjs::useShinyjs(),
    
    # Load custom CSS and favicon from /www folder
    tags$head(
      tags$script(src = "custom.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$link(rel = "shortcut icon", href = "favicon.svg")
    ),
      
    # --- Loading screen ---
    create_loading_screen("app-loading-screen", navbar_height_px = 0),
    
    # --- Main app UI (initially hidden ) ---
    shinyjs::hidden(
      div(id = "app-wrapper",
          # Navigation bar
          bslib::page_navbar(
            id = "tabs",
            selected = "home",  # placeholder, gets overridden by query
            
            # Home
            bslib::nav_panel(
              value = "home",
              title = tagList(icon("home"), "Home"),
              homeUI("home")
            ),
            
            # Database
            bslib::nav_menu(
              title = tagList(icon("database"), "Database"),
              # Search database
              bslib::nav_panel(
                value = "databaseSearch",
                title = "Search",
                databaseSearchUI("databaseSearch")
              ),
              # Download database
              bslib::nav_panel(
                value = "databaseDownload",
                title = "Download",
                databaseDownloadUI("databaseDownload")
              )
            ),
            
            # Predict
            bslib::nav_menu(
              title = tagList(icon("desktop"), "Predict"),
              # Predict from taxonomy
              bslib::nav_panel(
                value = "predictionsTaxonomy",
                title = "From taxonomy",
                predictionsTaxonomyUI("predictionsTaxonomy")
              ),
              # Predict from metabolic networks
              bslib::nav_panel(
                value = "predictionsNetwork",
                title = "With metabolic networks",
                predictionsNetworkUI("predictionsNetwork")
              ),
              # Predict with machine learning
              bslib::nav_panel(
                value = "predictionsMachineLearning",
                title = "With machine learning",
                predictionsMachineLearningUI("predictionsMachineLearning")
              )
            ),
            
            # Prediction history
            bslib::nav_panel(
              value = "history",
              title = tagList(icon("clock-rotate-left"), "History"),
              historyUI("history")
            ),
            
            # Help
            bslib::nav_panel(
              value = "help",
              title = tagList(icon("question-circle"), "Help"),
              helpUI("help")
            ),
            
            # About (right-aligned)
            bslib::nav_spacer(),
            bslib::nav_panel(
              value = "about",
              title = tagList(icon("circle-info"), "About"),
              aboutUI("about")
            )
          )
      )
    )
  )

# === Define server ===
  server <- function(input, output, session) {
      # Uncomment to adjust theming
      # bslib::bs_themer()
    
      # Set maximum file upload size
      options(shiny.maxRequestSize=100*1024^2)

      # Set variables
      session$userData$modal_open <- reactiveVal(FALSE) # For tracking if modals are open

      # Call server modules
      shiny::callModule(homeServer, "home", x=session)
      shiny::callModule(databaseSearchServer, "databaseSearch", x = session, selected_tab = reactive(input$tabs))
      shiny::callModule(databaseDownloadServer, "databaseDownload")
      shiny::callModule(predictionsTaxonomyServer, "predictionsTaxonomy", x = session, selected_tab = reactive(input$tabs))
      shiny::callModule(predictionsNetworkServer, "predictionsNetwork", x = session, selected_tab = reactive(input$tabs))
      shiny::callModule(predictionsMachineLearningServer, "predictionsMachineLearning", x = session, selected_tab = reactive(input$tabs))
      shiny::callModule(historyServer, "history", selected_tab = reactive(input$tabs))
      shiny::callModule(helpServer, "help", selected_tab = reactive(input$tabs))
      shiny::callModule(aboutServer, "about")

      # Update URL based on selected tab
      sync_tabs_with_query()
      
      # Clear old/large computation jobs
      setup_auto_cleanup()
  }
  
# === Run app ===
  shiny::shinyApp(ui = ui, server = server)