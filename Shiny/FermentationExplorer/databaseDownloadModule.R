# Define the Database Download Module in Shiny App
# This script defines the user interface (UI) and server for the database download module
# Author: Timothy Hackmann
# Date: 6 September 2024

# === Define user interface (UI) ===
databaseDownloadUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(width = 2),
      shiny::column(
        p(h3("Download database")),
        p(""),
        p(""),
        p(h5("Click below to download the full database in csv format.")),
        p(shiny::downloadButton(ns('databaseDownload_full'), 'Download')),
        width = 8
      ),
      shiny::column(width = 2)
    )
  )
}

# === Define server ===
databaseDownloadServer <- function(input, output, session) {
  #Get data
  data <- raw_data
  
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
