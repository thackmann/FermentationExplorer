###########################
#Define user interface (UI)
###########################
databaseDownloadUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width=2),
      column(
        p(h3("Download database")),
        p(""),
        p(""),
        p(h5("Click below to download the full database in csv format.")),
        p(downloadButton(ns('databaseDownload_full'), 'Download')),
        width=8),
      column(width=2)
    )
  )
}

##############
#Define server
##############
databaseDownloadServer <- function(input, output, session) {
  #Get data
  data = raw_data
  
  #Output downloadable csv of full database
  output$databaseDownload_full <- downloadHandler(
    filename = function() {
      paste("database", "csv", sep = ".")
    },
    content = function(file) {
      sep <- switch("csv", "csv" = ",", "tsv" = "\t")
      
      # Write to a file specified by the 'file' argument
      write.table(data, file, sep = sep, row.names = FALSE)
    }
  )
} 