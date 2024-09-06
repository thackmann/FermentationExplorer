# Define the Home Module in Shiny App
# This script defines the user interface (UI) and server for the home module
# It also includes functions and variables specific to this module.  
# Author: Timothy Hackmann
# Date: 16 August 2024

# === Define functions ===
  #' Create a Home Button UI Element
  #'
  #' This function creates a UI element for a home button in a Shiny app. 
  #' The button includes an image, title, subtitle, and a customizable action button.
  #'
  #' @param image_name A character string specifying the name of the image file (without extension) to be displayed on the button.
  #' @param title A character string specifying the title text to be displayed on the button.
  #' @param subtitle A character string specifying the subtitle text to be displayed on the button.
  #' @param button_name A character string specifying the ID for the Shiny action button.
  #' @param icon_background_color A character string specifying the background color of the icon. Default is "red".
  #' @param position A character string specifying the position class for the action button. Default is "left".
  #' @return A Shiny UI element representing a home button with the specified properties.
  #' @export
  #' @importFrom shiny div tags actionButton
  home_button <- function(image_name, title, subtitle, button_name, icon_background_color = "red", position = "left") {
    div(
      class = "home-button-box",
      div(
        class = "home-button-grid",
        div(
          class = "home-button-icon",
          tags$img(
            src = paste0(image_name, ".svg"),
            style = paste0(
              'background-color:', icon_background_color, ';', 
              'border-radius: 5px; display: block; max-width: 100%; max-height: 75px; height: auto; margin: 0 auto;'
            )
          )
        ),
        div(
          div(
            class = "home-button-title",
            title
          ),
          div(
            class = "home-button-subtitle",
            subtitle
          )
        )
      ),
      shiny::actionButton(button_name, title, class = paste0("home-action-button-", position))
    )
  }
  
  # === Define user interface (UI) ===
  homeUI <- function(id) {
    ns <- shiny::NS(id)
    htmltools::tagList(
      div(
        tags$img(src = 'FermentationExplorerLogo.svg', width = 75),
        shiny::br(),
        tags$img(src = 'FermentationExplorerText.svg', width = 400),
        shiny::p(
          "Get started by choosing an option below",
          style = "color: grey; font-size: 20px;"
        ),
        align = "center"
      ),
      bslib::layout_column_wrap(
        width = 1/2, height = 325,
        div(
          home_button(button_name = ns('jump_databaseSearch'), position = "left", icon_background_color = "#6d54a3", image_name = "databaseSearch", title = "Search database", subtitle = "Find data for thousands of organisms"),
          align = "right"
        ),
        div(
          home_button(button_name = ns('jump_databaseDownload'), position = "right", icon_background_color = "#ef4146", image_name = "databaseDownload", title = "Download database", subtitle = "For use in Excel or other programs"),
          align = "left"
        ),
        div(
          home_button(button_name = ns('jump_predictionsTaxonomy'), position = "left", icon_background_color = "#26b784", image_name = "predictionsTaxonomy", title = "Predict traits from taxonomy", subtitle = "Just provide names of taxa"),
          align = "right"
        ),
        div(
          home_button(button_name = ns('jump_predictionsNetwork'), position = "right", icon_background_color = "#bb65a8", image_name = "predictionsNetwork", title = "Predict traits with metabolic networks", subtitle = "Build networks on the fly"),
          align = "left"
        ),
        div(
          home_button(button_name = ns('jump_predictionsMachineLearning'), position = "left", icon_background_color = "#f3a73f", image_name = "predictionsMachineLearning", title = "Predict traits with machine learning", subtitle = "Use random forests"),
          align = "right"
        ),
        div(
          home_button(button_name = ns('jump_help'), position = "right", icon_background_color = "#808285", image_name = "help", title = "Help", subtitle = "Tutorials and documentation"),
          align = "left"
        ),
        div(
          home_button(button_name = ns('jump_about'), position = "right", icon_background_color = "#6d54a3", image_name = "about", title = "About", subtitle = "How this resource was developed"),
          align = "right"
        ),
        div(
        )
      )
    )
  }

# === Define server ===
homeServer <- function(input, output, session, x) {
  #Set namespace
  ns <- session$ns
  
  #Navigate to tab selected by navigation button
  shiny::observeEvent(input$jump_databaseSearch, {
    shinyjs::runjs("shinyjs.goToTab('databaseSearch');")
  })
  shiny::observeEvent(input$jump_databaseDownload, {
    shinyjs::runjs("shinyjs.goToTab('databaseDownload');")
  })
  shiny::observeEvent(input$jump_predictionsTaxonomy, {
    shinyjs::runjs("shinyjs.goToTab('predictionsTaxonomy');")
  })
  shiny::observeEvent(input$jump_predictionsNetwork, {
    shinyjs::runjs("shinyjs.goToTab('predictionsNetwork');")
  })
  shiny::observeEvent(input$jump_predictionsMachineLearning, {
    shinyjs::runjs("shinyjs.goToTab('predictionsMachineLearning');")
  })
  shiny::observeEvent(input$jump_help, {
    shinyjs::runjs("shinyjs.goToTab('help');")
  })
  shiny::observeEvent(input$jump_about, {
    shinyjs::runjs("shinyjs.goToTab('about');")
  })
}
