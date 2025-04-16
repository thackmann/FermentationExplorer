# Define the Database Download Module in Shiny App
# This script defines the user interface (UI) and server for the database download module
# Author: Timothy Hackmann
# Date: 18 February 2025

# === Define user interface (UI) ===
  databaseDownloadUI <- function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(width = 2),
        shiny::column(
          create_title_div("Download database"),
          p(""),
          p(""),
          p(h5("Click below to download the full database in csv format.")),
          create_download_button(ns('download_data'), label = "Download"),
          width = 8
        ),
        shiny::column(width = 2)
      )
    )
  }
  
# === Define server ===
  databaseDownloadServer <- function(input, output, session) {
    # --- Generate outputs ---
    #Output downloadable csv of full database
    output$download_data <- create_download_handler(
      filename_prefix = "database",
      data_source = function() {
        load_raw_database()
      }
    )
  }
