# Main Shiny App Script
# This script sets up the system locale, loads external R scripts, and defines the user interface (UI)
# and server components for the Shiny app. The app includes modules for 
# database searching, predictions, and user help, all organized within a Bootstrap-based layout.
# Author: Timothy Hackmann
# Date: 26 February 2025

# === Set system locale ===
Sys.setlocale("LC_ALL", "C")

# === Set CRAN mirror ===
options(repos = c(CRAN = "https://cloud.r-project.org"))

# === Load external R files ===
source("installPackages.R", local = TRUE)
source("utilityFunctions.R", local = TRUE)
source("loadDataFunctions.R", local = TRUE)
source("plotFunctions.R", local = TRUE)
source("userInterfaceFunctions.R", local = TRUE)
source("variables.R", local = TRUE)
source("homeModule.R", local = TRUE)
source("databaseSearchModule.R", local = TRUE)
source("databaseDownloadModule.R", local = TRUE)
source("predictionsTaxonomyModule.R", local = TRUE)
source("predictionsNetworkModule.R", local = TRUE)
source("predictionsMachineLearningModule.R", local = TRUE)
source("aboutModule.R", local = TRUE)
source("helpModule.R", local = TRUE)

# === Define user interface (UI) ===
ui <- 
  htmltools::tagList(
    bslib::page_fluid(
      title = "Fermentation Explorer",
      # --- Set style ---
      #Set Bootstrap version and theme
      theme = bslib::bs_theme(version = 5, preset = "shiny", 
                              font_scale = 1, 
                              spacer = "0.5rem"
                              ),
      
      #Set theme for query builder
      jqbr::useQueryBuilder(bs_version = "5"),
      
      #Get JavaScript file from custom.js (/www folder)
      shinyjs::useShinyjs(),
      htmltools::tags$head(tags$script(src="custom.js")),
      
      #Get style from style.css (/www folder)
      htmltools::tags$head(
        htmltools::tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
        htmltools::tags$link(rel = "shortcut icon", href = "favicon.svg")
      ),

      # --- Define layout of tabs ---
        # Navigation bar
        bslib::page_navbar(
          id = "tabs",
      
          # Home
          bslib::nav_panel(
            value = "home",
            title = htmltools::div(shiny::icon("home"), "Home"),
            homeUI("home")
          ),

          # Database
          bslib::nav_menu(title = htmltools::div(shiny::icon("database"), "Database"),
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
          bslib::nav_menu(title = htmltools::div(shiny::icon("desktop"), "Predict"),
                            #Predict from taxonomy
                            bslib::nav_panel(
                              value = "predictionsTaxonomy",
                              title = "From taxonomy",
                              predictionsTaxonomyUI("predictionsTaxonomy")
                            ),

                            # Predict from metabolic networks
                            bslib::nav_panel(
                              value = "predictionsNetwork",
                              title = "With metabolic networks",
                              predictionsNetworkUI("predictionsNetwork"),
                            ),

                            # Predict with machine learning
                            bslib::nav_panel(
                              value = "predictionsMachineLearning",
                              title = "With machine learning",
                              predictionsMachineLearningUI("predictionsMachineLearning")
                            )
          ),

          # Help
          bslib::nav_panel(
            value = "help",
            title = htmltools::div(shiny::icon("question-circle"), "Help"),
            helpUI("help")
          ),

          # About
          bslib::nav_spacer(), #Justify right
          bslib::nav_panel(
            value = "about",
            title = htmltools::div(shiny::icon("circle-info"), "About"),
            aboutUI("about")
          ),
          
          # Navigation bar options
          selected = "home"  
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
  shiny::callModule(helpServer, "help", x = session, selected_tab = reactive(input$tabs))
  shiny::callModule(aboutServer, "about")
}

# Uncomment to allow reactlog
# reactlog::reactlog_enable()

# === Run app ===
shiny::shinyApp(ui = ui, server = server)