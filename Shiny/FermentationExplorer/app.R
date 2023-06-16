##############
#Load packages
##############
library(colorspace)
library(dplyr)
library(fbar)
library(ggplot2)
library(ggtree)
library(htmltools)
library(igraph)
library(plotly)
library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(stringr)
library(tidyr)
library(treemap)
library(queryBuilder)

##################
#Set system locale
##################
Sys.setlocale("LC_ALL","C")

######################
#Load external R files
######################
source("loadInternalData.R", local = TRUE)
source("homeModule.R", local = TRUE)
source("databaseSearchModule.R", local = TRUE)
source("databaseDownloadModule.R", local = TRUE)
source("predictionsTaxonomyModule.R", local = TRUE)
source("predictionsGenomeModule.R", local = TRUE)
source("aboutModule.R", local = TRUE)
source("helpModule.R", local = TRUE)
source("miscFunctions.R", local = TRUE)
source("miscVariables.R", local = TRUE)

###########################
#Define user interface (UI)
###########################
  ui <- 
  tagList(
    fluidPage(
      
      #*********
      #Set style
      #*********
      useShinydashboard(), 
      useShinyjs(),
      #Get style from style.css (/www folder)
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
        tags$link(rel = "shortcut icon", href = "favicon.svg")
      ),

      #*********************
      #Define layout of tabs
      #*********************
      fluidRow(
        #Navigation bar
        navbarPage(
          id="tabs", windowTitle="Fermentation Explorer", 
          title="",
          
        #Home
          tabPanel(
            value="home",
            title=div(icon("home"), "Home"),
            homeUI("home")
          ),
          
          #Database
          navbarMenu(title=div(icon("database"), "Database"),
            #Search database
             tabPanel(
               value="databaseSearch",
               title="Search",
               databaseSearchUI("databaseSearch")
              ),
            #Download database     
              tabPanel(
                value="databaseDownload", 
                title="Download",
                databaseDownloadUI("databaseDownload")
              )
            ),
            
          #Predict 
          navbarMenu(title=div(icon("desktop"), "Predict"),
                     #Predict from taxonomy
                     tabPanel(
                       value="predictionsTaxonomy",
                       title="From taxonomy",
                       predictionsTaxonomyUI("predictionsTaxonomy")
                     ),
                     
                     #Predict from genome
                     tabPanel(
                         value="predictionsGenome",
                         title="From genome",
                         predictionsGenomeUI("predictionsGenome"),
                     )
          ),
          
          #Help     
          tabPanel(
            value="help", 
            title=div(icon("question-circle"), "Help"),
            helpUI("help")
          ),
        
          #About 
          tabPanel(
            value="about",
            title=div(icon("circle-info"), "About"),
            aboutUI("about")
          ),
          
          #Navigation bar options
          selected="home"
        )
      )
    ),
    
  )

##############
#Define server
##############
server <- function(input, output, session) {
  #Set maximum file upload size
  options(shiny.maxRequestSize=30*1024^2)

  #Call server modules
  callModule(homeServer, "home", x=session)
  callModule(databaseSearchServer, "databaseSearch")  
  callModule(predictionsTaxonomyServer, "predictionsTaxonomy", x=session)
  callModule(predictionsGenomeServer, "predictionsGenome", x=session)   
  callModule(databaseDownloadServer, "databaseDownload")
  callModule(aboutServer, "about")
  callModule(helpServer, "help", selected_section = selected_section)
}

########
#Run app
########
shinyApp(ui = ui, server = server)