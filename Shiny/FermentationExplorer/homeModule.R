#################
#Define functions
#################
home_button <- function(image_name, title, subtitle, button_name, icon_background_color="red", position="left") {
  div(
    class = "home-button-box",
    div(
      class = "home-button-grid",
      div(
        class = "home-button-icon",
        tags$img(
          src=paste0(image_name, ".svg"),
          style=paste0(
            'background-color:',icon_background_color, ';', 
            'border-radius: 5px; display: block; max-width: 100%; max-height: 60px; height: auto; margin: 0 auto;'
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
    actionButton(button_name, title, class=paste0("home-action-button-", position))
  )
}

###########################
#Define user interface (UI)
###########################
homeUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      tags$img(src='FermentationExplorerLogo.svg', width=75),
      br(),
      tags$img(src='FermentationExplorerText.svg', width=400),
      p(
        "Get started by choosing an option below",
        style = "color: grey; font-size: 20px;"
      ),
      align="center"
    ),
    
    fluidRow(
      p(""),
      p("")
    ),
    
    fluidRow(
      column(
        div(
          home_button(button_name = ns('jump_databaseSearch'), position="left", icon_background_color="#6d54a3", image_name="databaseSearch", title = "Search database", subtitle="Find data for thousands of organisms")
        ),
        width=6, align= "right"
      ),
      column(
        div(
          home_button(button_name = ns('jump_databaseDownload'), position="right", icon_background_color="#ef4146", image_name="databaseDownload", title = "Download database", subtitle="For use in Excel or other programs")
        ),
        width=6, align= "left"
      )
    ),
    
    fluidRow(
      p("")
    ),
    
    fluidRow(
      column(
        div(
          home_button(button_name = ns('jump_predictionsTaxonomy'), position="left", icon_background_color="#26b784", image_name="predictionsTaxonomy", title = "Predict traits from taxonomy", subtitle="Just provide names of taxa")
        ),
        width=6, align= "right"
      ),
      column(
        div(
          home_button(button_name = ns('jump_predictionsGenome'), position="right", icon_background_color="#bb65a8", image_name="predictionsGenome", title = "Predict traits from genome", subtitle="Use our library of genomes or BYO")
        ),
        width=6, align= "left"
      )
    ),
    
    fluidRow(
      p("")
    ),
    
    fluidRow(
      column(
        div(
          home_button(button_name = ns('jump_help'), position="left", icon_background_color="#f3a73f", image_name="help", title = "Help", subtitle="Tutorials and documentation")
        ),
        width=6, align= "right"
      ),
      column(
        div(
          home_button(button_name = ns('jump_about'), position="right", icon_background_color="#808285", image_name="about", title = "About", subtitle="How this resource was developed")
        ),
        width=6, align= "left"
      )
    )
  )
}

##############
#Define server
##############
homeServer <- function(input, output, session, x) {
  #Set namespace
  ns <- session$ns
  
  #Navigate to tab selected by navigation button
  observeEvent(input$jump_databaseSearch, {
    updateNavbarPage(session=x, inputId="tabs", selected = "databaseSearch")
  })
  observeEvent(input$jump_databaseDownload, {
    updateNavbarPage(session=x, inputId="tabs", selected = "databaseDownload")
  })
  observeEvent(input$jump_predictionsTaxonomy, {
    updateNavbarPage(session=x, inputId="tabs", selected = "predictionsTaxonomy")
  })
  observeEvent(input$jump_predictionsGenome, {
    updateNavbarPage(session=x, inputId="tabs", selected = "predictionsGenome")
  })
  observeEvent(input$jump_help, {
    updateNavbarPage(session=x, inputId="tabs", selected = "help")
  })
  observeEvent(input$jump_about, {
    updateNavbarPage(session=x, inputId="tabs", selected = "about")
  })
}
