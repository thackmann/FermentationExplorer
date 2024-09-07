# Define the About Module in Shiny App
# This script defines the user interface (UI) and server for the about module
# Author: Timothy Hackmann
# Date: 16 August 2024

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
            p(h3("About")),
            p(""),
            p(""),
            p(h5("Development of v. 1.0 of this resource is described in", url_FermentationExplorer, ".")),
            p(""),
            p(h5(shiny::tagList("We acknowledge ", url_BacDive, ", ", url_GOLD, ", and ", url_IMG, " for use of their data.  Data from ", url_BacDive, " appear under the terms of a ", url_CC, ".  Data from ", url_GOLD, " and ", url_IMG, " appear under the terms of ", url_JGI))),
            p(""),
            p(h5("This work was supported by an Agriculture and Food Research Initiative Competitive Grant [grant no. 2018-67015-27495] and Hatch Project [accession no. 1019985] from the United States Department of Agriculture National Institute of Food and Agriculture.")),
            p("")
        )
  })
}
