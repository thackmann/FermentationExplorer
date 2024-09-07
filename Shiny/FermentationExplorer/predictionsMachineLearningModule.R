# Define the Predictions Using Machine Learning Module in Shiny App
# This script defines the user interface (UI) and server for the predictions using machine learning module.  
# It also includes functions and variables specific to this module.  
# Author: Timothy Hackmann
# Date: 5 September 2024

# === Define functions ===
  # --- Functions for loading internal data ---
    #' Load Selected Models
    #'
    #' This function loads a list of models based on the selected model names provided.
    #' The function checks and loads each model file from the specified paths.
    #'
    #' @param selected_models A character vector of selected model names to be loaded.
    #' @return A named list of loaded model objects.
    #' @export
    #' @importFrom base lapply
    load_models <- function(selected_models) {
      model_list <- lapply(selected_models, function(x) {
          obj <- check_and_load(file_path = model_paths[[x]])
      })
      
      names(model_list) <- selected_models
      
      return(model_list)
    }

  # --- Other functions ---
    #' Run Random Forest Model Predictions
    #'
    #' This function makes predictions using a list of pre-trained random forest models. 
    #' It ensures that the necessary predictors are present in the dataframe and in the correct order.
    #'
    #' @param df A dataframe containing the predictor variables.
    #' @param models A named list of random forest models to use for predictions.
    #' @return A dataframe containing the predicted probabilities for each model.
    #' @export
    #' @importFrom base lapply setdiff
    run_random_forest <- function(df, models) {
      predictions <- lapply(models, function(model) {
        predictors <- rownames(model$importance)
        
        # Ensure all necessary predictors are present
        missing_predictors <- setdiff(predictors, colnames(df))
        if (length(missing_predictors) > 0) {
          df[missing_predictors] <- 0
        }
        
        # Ensure the predictors are in the same order as the model expects
        df_ordered <- df[, predictors, drop = FALSE]
    
        # Predict using the loaded model
        randomForest:::predict.randomForest(object = model, newdata = df_ordered, type = "prob")[, 2] # Assuming the second column is the probability of the positive class
      })
      
      prediction_df <- data.frame(Organism = row.names(df))
      for (i in seq_along(models)) {
        prediction_df[[names(models)[i]]] <- predictions[[i]]
      }
      
      return(prediction_df)
    }
    
    #' Format Machine Learning Results for Plots
    #'
    #' This function formats the results from machine learning predictions into a format suitable 
    #' for different types of plots such as summary, heatmap, or treemap.
    #'
    #' @param df A dataframe containing the machine learning prediction results.
    #' @param plot_type A character string specifying the type of plot ("summary", "heatmap", or "treemap").
    #' @return A formatted dataframe ready for plotting.
    #' @export
    #' @importFrom dplyr group_by summarize ungroup filter mutate
    #' @importFrom tidyr pivot_longer pivot_wider complete
    ML_results_to_plot <- function(df, plot_type) {
      # Pivot longer and rename columns
      df <- df %>% tidyr::pivot_longer(-Organism)
      colnames(df) <- c("x", "y", "z")
      
      if (plot_type == "summary") {
        # Convert to binary
        df$z <- make_binary(df$z)
        
        # Scale values
        df$z <- df$z * 100
        
        # Summarize by y
        df <- df %>% dplyr::group_by(y) %>% dplyr::summarize(z = mean(z), .groups = 'drop')
        
        #Pivot wider
        df <- df %>%
          tidyr::pivot_wider(names_from = y, values_from = z, values_fill = list(z = 0))
      }else if (plot_type == "heatmap") {
        # Scale values
        df$z <- df$z * 100
        
        # Factorize the x column
        df$x <- factor(df$x, levels = unique(df$x))
        
        # Fill in missing combinations of x and y
        df <- df %>%
          dplyr::group_by(y) %>%
          tidyr::complete(x = unique(df$x), fill = list(z = 0)) %>%
          dplyr::ungroup()
        
        # Pivot wider
        df <- df %>%
          tidyr::pivot_wider(names_from = y, values_from = z)
        
        # Add asterisk to x labels for clarity
        df$x = paste0(df$x, "*")
      }else if (plot_type == "treemap") {
        # Scale values
        df$z <- df$z * 100
        
        # Convert to binary
        df$z <- make_binary(df$z)
        
        # Filter rows where z is 1
        df <- df %>% dplyr::filter(z == 1)
        
        # Select relevant columns
        df <- df %>% dplyr::select(-z)
        
        # Summarize by y
        df <- df %>%
          dplyr::group_by(y) %>%
          dplyr::summarise(z = dplyr::n(), .groups = 'drop') %>%
          dplyr::mutate(z = z / sum(z))
        
        # Convert to percentage
        df$z = df$z*100
      }
      
      return(df)
    }

# === Set variables ===
#File paths for random forest models
model_paths <- list(
  fermentation = "data/random_forest_fermentation.rds",
  methanogenesis = "data/random_forest_methanogenesis.rds"
)

# === Define user interface (UI) ===
predictionsMachineLearningUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    #Call functions in custom.js
    tags$script(HTML(sprintf("
        $(document).ready(function(){
          shinyjs.resizeWidthFromHeight('%s', 1.045296);
        });
      ", ns("treemap-container")))),
    
    #Title
    div(
                  shiny::h3("Predict traits with machine learning")
    ),

    bslib::layout_sidebar(
      #Sidebar
      sidebar = bslib::sidebar(
                  width = "30%",
                  fileInput_modal(ns("file_gene_functions"), "Upload predicted gene functions", multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"), modalId = ns("genome_file_functions_modal"), modalLabel = "Download example"),
                  shinyWidgets::pickerInput(inputId = ns("models"),
                                            label = "Choose traits",
                                            choices = names(model_paths),
                                            selected = names(model_paths),
                                            multiple = TRUE, options = list(`actions-box` = TRUE)),
                  shiny::actionButton(ns("make_predictions"), "Make predictions", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                ),
      #Main content area
      div(
                 id = ns("results_page"),

                 shiny::conditionalPanel(
                   condition = "input.make_predictions == 0 | output.check_file_genome",
                   ns = ns,
                   shiny::h4("Please upload files and make selections at left")
                 ),

                 shiny::conditionalPanel(
                   condition = "input.make_predictions > 0 & !output.check_file_genome",
                   ns = ns,
                   bslib::card(
                     bslib::card_header(shiny::textOutput(ns("summary_text"))),
                     full_screen = TRUE,
                     shiny::downloadButton(ns('download_data'), 'Download results') %>% shinycssloaders::withSpinner(color = "#3C8DBC"),
                   ),

                   bslib::navset_card_underline(
                     title = "Prediction Results",
                     full_screen = TRUE,
                   bslib::nav_panel(
                     title = "Summary",
                     div(
                       id = ns("summary-container"),
                       class = "summary-container-style",
                       plotly::plotlyOutput(ns("summary"), width = "100%", height = "40vh") %>% shinycssloaders::withSpinner(color = "#3C8DBC")
                     )
                   ),
                   bslib::nav_panel(
                     title = "Treemap",
                     div(
                       id = ns("treemap-container"),
                       class = "treemap-container-style",
                       plotly::plotlyOutput(ns("treemap"), width = "100%", height = "40vh") %>% shinycssloaders::withSpinner(color = "#3C8DBC")
                     )
                   ),
                   bslib::nav_panel(
                     "Heatmap",
                     div(
                       id = ns("heatmap-container"),
                       class = "heatmap-container-style",
                       plotly::plotlyOutput(ns("heatmap"), width = "100%", height = "40vh") %>% shinycssloaders::withSpinner(color = "#3C8DBC")
                     )
                   )
                 )
               )
      )
    )
  )
}

# === Define server ===
predictionsMachineLearningServer <- function(input, output, session, x, selected_section) {
  # Set namespace
  ns <- session$ns
  
  #***********************
  #Get user input (events)
  #***********************
  get_gene_functions <- shiny::reactive({
    # Validate, read, and process the gene functions file
    gene_functions <- validate_and_read_csv(input$file_gene_functions$datapath)
    gene_functions <- process_uploaded_gene_functions(gene_functions)

    # Do further formatting
    gene_functions <- as.data.frame(gene_functions)
    
    # Replace default column name with a more descriptive one
    if (any(c("database_ID", "Database_ID") %in% colnames(gene_functions))) {
      colnames(gene_functions)[colnames(gene_functions) %in% c("database_ID", "Database_ID")] <- "Organism"
    }
    
    runValidationModal(need(gene_functions != "", "Please check the format of your predicted gene functions file and try again."))
    
    return(gene_functions)
  })
  
  #Get models
  get_models <- shiny::eventReactive({input$make_predictions}, {
    selected_models <- input$models
    shiny::validate(shiny::need(selected_models != "", "Please choose a trait to predict"))
    
    return(selected_models)
  }, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  #--- Process input ---
  #Get probabilities of traits
  get_probabilities <- shiny::eventReactive({input$make_predictions}, {
    
    #Launch modal with progress bar
    launch_modal_with_progress(session, ns("pb"), message = "Loading machine learning models")
    
    #Get inputs
    df <- get_gene_functions()
    
    models <- load_models(selected_models = get_models())
    
    # Update modal with progress bar
    shinyjs::runjs("document.getElementById('modal-text').innerText = 'Prediction in progress';")
    shinyWidgets::updateProgressBar(session = session, id = ns("pb"), value = 50)
    
    #Print status to log
    cat(file = stderr(), paste0("Started prediction at ", Sys.time(), "\n"))
    
    #Format inputs
    df <- df %>% tidyr::pivot_longer(cols = tidyselect::everything(), names_to = "Organism", values_to = "Database_ID") %>%
      dplyr::mutate(value = 1)
    df <- df %>% dplyr::select(Organism, Database_ID) %>% dplyr::distinct() %>% 
      dplyr::mutate(value = 1) %>% tidyr::pivot_wider(names_from = Database_ID, values_from = value, values_fill = list(value = 0))
    df <- as.data.frame(df)
    row.names(df) <- df$Organism
    
    #Make predictions
    prediction_df <- run_random_forest(df, models)
    colnames(prediction_df) <- gsub(pattern = "\\.rds", replacement = "", x = colnames(prediction_df))
    
    # Hide the modal with progress bar
    hide_modal_with_progress(session, ns("pb"))
    
    #Print status to log
    cat(file = stderr(), paste0("Ended prediction at ", Sys.time(), "\n"))
    
    return(prediction_df)
  }
  , ignoreNULL = FALSE, ignoreInit = FALSE)
  
  #Get traits from probabilities
  get_traits <- shiny::eventReactive({input$make_predictions}, {
    traits_df <- get_probabilities()
    
    traits_df <- traits_df %>% dplyr::mutate_at(dplyr::vars(-1), make_binary)

    return(traits_df)
  }
  , ignoreNULL = FALSE,  ignoreInit = FALSE)

  # Count organisms with at least one predicted trait
  count_organisms <-  shiny::eventReactive({input$make_predictions}, {
    # Get inputs
    df = get_traits()
    
    df = df %>% dplyr::select(-Organism)
    
    n_organisms = sum(rowSums(df) > 0)
    
    return(n_organisms)
  }
  , ignoreNULL = FALSE,  ignoreInit=FALSE)
  
  #--- Generate outputs ---
  # Output downloadable csv with example gene functions
  output$downloadFunctions_1 <- shiny::downloadHandler(
    filename = function() {
      paste("gene_functions_e_coli", "csv", sep = ".")
    },
    content = function(file) {
      table <- gene_functions_e_coli
      utils::write.csv(table, file, row.names = FALSE)
    }
  )  
  
  output$downloadFunctions_2 <- shiny::downloadHandler(
    filename = function() {
      paste("gene_functions_uncharacterized", "csv", sep = ".")
    },
    content = function(file) {
      table <- gene_functions_uncharacterized
      utils::write.csv(table, file, row.names = FALSE)
    }
  )  
  
  output$downloadFunctions_3 <- shiny::downloadHandler(
    filename = function() {
      paste("gene_functions_rumen_cultured", "csv", sep = ".")
    },
    content = function(file) {
      table <- gene_functions_Hungate
      utils::write.csv(table, file, row.names = FALSE)
    }
  )  
  
  output$downloadFunctions_4 <- shiny::downloadHandler(
    filename = function() {
      paste("gene_functions_rumen_MAGs", "csv", sep = ".")
    },
    content = function(file) {
      table <- gene_functions_RUG
      utils::write.csv(table, file, row.names = FALSE)
    }
  )  

  # Output modal with example files
  # Create modal
  shiny::observeEvent(input$genome_file_functions_modal, ignoreInit = TRUE, {
    shiny::showModal(shiny::modalDialog(
      shiny::h3("Example files"),
      tags$ol(class = "circled-list",
              tags$li(shiny::downloadLink(outputId = ns("downloadFunctions_1"), label = "E. coli")),
              tags$li(shiny::downloadLink(outputId = ns("downloadFunctions_2"), label = "Previously uncharacterized bacteria")),
              tags$li(shiny::downloadLink(outputId = ns("downloadFunctions_3"), label = "Cultured prokaryotes from rumen")),
              tags$li(shiny::downloadLink(outputId = ns("downloadFunctions_4"), label = "MAGs from rumen"))
      ),
      shiny::div("Click ",
                 shiny::actionLink(ns("go_to_help"), "here"),
                 " to see detailed guidelines."),
      easyClose = TRUE, footer = NULL
    ))
  })
  
  shiny::observeEvent(input$go_to_help, {
    shiny::updateNavbarPage(session = x, inputId = "tabs", selected = "help")
    shiny::updateNavlistPanel(session = x, inputId = "navlist_panel", selected = "Predict traits using machine learning")
    shiny::removeModal()
  })
  
  # Alternate syntax for updateNavlistPanel, which results in error
  # shiny::observeEvent(input$go_to_help, {
  #   shiny::updateNavbarPage(session = x, inputId = "tabs", selected = "help")
  #   shiny::navlistPanel(session = session, inputId = "subtabs", selected = "Predict traits with machine learning")
  #   shiny::removeModal()
  # })
  
  # Output file upload status
  output$check_file_genome <- shiny::reactive({
    is.null(input$file_gene_functions$datapath)
  })
  shiny::outputOptions(output, "check_file_genome", suspendWhenHidden = FALSE)
  
  # Output summary text
  output$summary_text = shiny::renderText(
    if(count_organisms() > 0 & nrow(get_probabilities()) > 1) {
      paste0("Traits predicted for ", count_organisms(), " out of ", nrow(get_probabilities()), " organisms")
    }else if(count_organisms() > 0 & nrow(get_probabilities()) == 1) {
      paste0("Traits predicted for ", nrow(get_probabilities()), " organism")
    }else if(count_organisms() == 0 & nrow(get_probabilities()) > 1) {
      paste0("No traits predicted for ", nrow(get_probabilities()), " organisms")
    }else{
      paste0("No traits predicted for ", nrow(get_probabilities()), " organism")
    }
  )

  # Output downloadable csv of results
  output$download_data <- shiny::downloadHandler(
    filename = function() {
      paste("results", "csv", sep = ".")
    },
    content = function(file) {
      sep <- switch("csv", "csv" = ",", "tsv" = "\t")
      
      table <- get_probabilities()
      if(length(unique(table$Organism)) == 1) {
        table <- table %>% dplyr::select(-Organism)
      }
      
      utils::write.table(table, file, sep = sep, row.names = FALSE)
    }
  ) 
  
  # Output overview plots
  shiny::observe({
    # Summary plot
  
    output$summary <- plotly::renderPlotly({
      df = get_probabilities()
      df <- ML_results_to_plot(df = df, plot_type="summary")
      plot = plot_summary(df, 
                          coord_fixed = TRUE, 
                          hovertemplate = "<b>Trait: %{x}</b><br><b>% organisms positive: %{z:.2f}</b><br><extra></extra>",
                          legend_labels = c("0", "25", "50", "75", "100"), 
                          legend_title = "% organisms positive")
      plot
    })
    
    # Treemap
    output$treemap <- plotly::renderPlotly({    
      df = get_probabilities()
      df = ML_results_to_plot(df = df, plot_type="treemap")
      plot = plot_treemap(df=df,
                   hovertemplate = "<b>Trait: %{label}</b><br><b>% total: %{value:.2f}</b><br><extra></extra>")
      plot
    })
    
    # Heatmap
    output$heatmap <- plotly::renderPlotly({
      df = get_probabilities()
      df <- ML_results_to_plot(df = df, plot_type="heatmap")
      plot <- plot_heatmap(df, 
                           hovertemplate = "<b>Trait: %{x}</b><br><b>Organism: %{y}</b><br><b>% probability: %{z:.2f}</b><br><extra></extra>",
                           legend_labels = c("0", "25", "50", "75", "100"), 
                           legend_title = "% probability")
      plot
    })
  })
}
