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
          create_download_button(ns('databaseDownload_full'), label = "Download"),
          width = 8
        ),
        shiny::column(width = 2)
      )
    )
  }
  
# === Define server ===
  databaseDownloadServer <- function(input, output, session) {
    #Get data
    data <- load_raw_database()
    
    #Output downloadable csv of full database
    output$databaseDownload_full <- shiny::downloadHandler(
      filename = function() {
        paste("database", "csv", sep = ".")
      },
      content = function(file) {
        sep <- switch("csv", "csv" = ",", "tsv" = "\t")
        
        # Write to a file specified by the 'file' argument
        utils::write.table(data, file, sep = sep, row.names = FALSE)
      }
    )
  }
