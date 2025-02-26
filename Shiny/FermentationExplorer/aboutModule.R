# Define the About Module in Shiny App
# This script defines the user interface (UI) and server for the about module
# Author: Timothy Hackmann
# Date: 26 February 25

# === Define user interface (UI) ===
aboutUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    div(
      shiny::uiOutput(ns("about_content"))
    )
  )
}

# === Define server ===
aboutServer <- function(input, output, session) {
  ns <- session$ns
  
  output$about_content <- renderUI({
    shiny::tagList(
      create_title_div("About"),
      p(""),
      p(""),
      p(h5("Development of v. 1.0 of this resource is described in ", url_FermentationExplorer, ".")),
      p(""),
      p(h5(shiny::tagList(
        "We acknowledge ", 
        url_BacDive, ", ", 
        url_FAPROTAX, ", ",
        url_GOLD, ", ",
        url_GTDB, ", ",
        url_IMG, ", ", 
        url_LPSN, ", and ", 
        url_NCBI, 
        " databases for use of their data. Data from ", 
        url_BacDive, ", ", 
        url_GTDB, ", and ", 
        url_LPSN, 
        " appear under the terms of a ", url_CC, ".  Data from ", 
        url_NCBI, 
        " appear under the terms of a ", url_MIT, ". Data from ",
        url_FAPROTAX, 
        "appear under the terms of ", 
        url_FAPROTAX_license, ". Data from ", 
        url_GOLD, " and ", url_IMG, 
        " appear under the terms of ", url_JGI, "."
      ))),
      p(""),
      p(h5(shiny::tagList(
        "We also acknowledge ", 
        url_AnaerobeManual, ", ", 
        url_Bergey, ", and authors of the primary literature for use of their data. Data from these sources appear under the doctrine of ", 
        url_fairuse, "."
      ))),
      p(""),
      p(h5("This work was supported by an Agriculture and Food Research Initiative Competitive Grant [grant no. 2018-67015-27495] and Hatch Project [accession no. 1019985] from the United States Department of Agriculture National Institute of Food and Agriculture.")),
      p("")
    )
  })
}
