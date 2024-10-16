# Define the Predictions Using Machine Learning Module in Shiny App
# This script defines the user interface (UI) and server for the predictions using machine learning module.  
# It also includes functions and variables specific to this module.  
# Author: Timothy Hackmann
# Date: 14 October 2024

# === Define functions ===
  # --- Functions for loading internal data ---
    # Load Selected Models
    #'
    #' This function loads a list of models based on the selected model names and paths provided.
    #' The function checks and loads each model file from the specified paths.
    #'
    #' @param model_names A character vector of selected model names to be loaded.
    #' @param model_paths A vector of file paths corresponding to the models.
    #' @param file_upload A Boolean describing if files are uploaded. 
    #' @return A named list of loaded model objects.
    #' @export
    #' @importFrom base lapply
    load_models <- function(model_names, model_paths, file_upload=FALSE) {
      
      if(!file_upload) {
        model_list <- lapply(seq_along(model_names), function(i) {
          obj <- check_and_load(file_path = model_paths[[i]])
          return(obj)
        })
      } else if(file_upload) {
        model_list <- lapply(seq_along(model_names), function(i) {
          obj <- validate_and_read_rds(file_path = model_paths[[i]])
          return(obj)
        })
      }
      
      names(model_list) <- model_names
      return(model_list)
    }
        
    #' Load Response Variable for Fermentation
    #'
    #' This function loads an example response variable (for fermnetation) from a CSV file.
    #' The data is loaded and stored in the environment if it is not already present.
    #'
    #' @return A data frame of the response variable
    #' @export
    load_response_fermentation <- function() {
      data_fp <- "data/response_fermentation.csv"
      obj <- check_and_load(data_fp)
      
      return(obj)
    }
    
    #' Load Response Variable for Methanogenesis
    #'
    #' This function loads an example response variable (for methanogenesis) from a CSV file.
    #' The data is loaded and stored in the environment if it is not already present.
    #'
    #' @return A data frame of the response variable
    #' @export
    load_response_methanogenesis <- function() {
      data_fp <- "data/response_methanogenesis.csv"
      obj <- check_and_load(data_fp)
      
      return(obj)
    }
    
    #' Load Predictors Variables for Fermentation
    #'
    #' This function loads an example set of predictors (for fermentation) from a CSV file.
    #' The data is loaded and stored in the environment if it is not already present.
    #'
    #' @return A data frame of the predictor variables
    #' @export
    load_predictors_fermentation <- function() {
      data_fp <- "data/predictors_fermentation.csv"
      obj <- check_and_load(data_fp)
      
      return(obj)
    }
    
    #' Load Predictors Variables for Methanogenesis
    #'
    #' This function loads an example set of predictors (for methananogesis) from a CSV file.
    #' The data is loaded and stored in the environment if it is not already present.
    #'
    #' @return A data frame of the predictor variables
    #' @export
    load_predictors_methanogenesis <- function() {
      data_fp <- "data/predictors_methanogenesis.csv"
      obj <- check_and_load(data_fp)
      
      return(obj)
    }
    
    #' Load Random Forest Model for Fermentation
    #'
    #' This function loads an example random forest model (for fermentation) from a rds file.
    #' The data is loaded and stored in the environment if it is not already present.
    #'
    #' @return A data frame of the predictor variables
    #' @export
    load_model_fermentation <- function() {
      data_fp <- "data/random_forest_fermentation.rds"
      obj <- check_and_load(data_fp)
      
      return(obj)
    }
    
    #' Load Random Forest Model for Methanogenesis
    #'
    #' This function loads an example random forest model (for methanogenesis) from a rds file.
    #' The data is loaded and stored in the environment if it is not already present.
    #'
    #' @return A data frame of the predictor variables
    #' @export
    load_model_methanogenesis <- function() {
      data_fp <- "data/random_forest_methanogenesis.rds"
      obj <- check_and_load(data_fp)
      
      return(obj)
    }
    
  # --- Other functions ---
    #' Run Random Forest Model Predictions
    #'
    #' This function makes predictions using a list of pre-trained random forest models. 
    #' It ensures that the necessary predictors are present in the dataframe and in the correct order.
    #'
    #' @param df A dataframe containing the predictor variables.
    #' @param models A named list of random forest models to use for predictions.  Non-lists will be wrapped into lists.
    #' @return A dataframe containing the predicted probabilities for each model.
    #' @export
    #' @importFrom base lapply setdiff
    run_random_forest <- function(df, models) {
      predictions <- lapply(models, function(model) {
        predictors <- rownames(model$importance)
        
        # Check if model has 'importance' before proceeding
        if (is.null(model$importance)) {
          stop("Model importance is NULL. Check model structure.")
        }
        
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

# Filters for query builder
query_filters_ML = load_query_filters()
traits_to_keep = c(
                  # Physiology/Function
                  "Type of metabolism", "End products", "Major end products", "Minor end products", 
                  "Substrates for end products", "Oxygen tolerance", "Pathogenicity", 
                  "Temperature category", "Temperature for growth in degrees", "pH for growth", 
                  "Incubation period in days", "Indole test", "Salt in moles per liter", 
                  "Antibiotic resistance", "Antibiotic sensitivity", "FAPROTAX predicted metabolism",
                  
                  # Morphology
                  "Cell length in microns", "Cell width in microns", "Cell shape", "Colony size", 
                  "Flagellum arrangement", "Gram stain", "Spore formation",
                  
                  # Isolation traits
                  "Isolation source category 1", "Isolation source category 2", "Isolation source category 3"
                )

query_filters_ML = purrr::keep(query_filters_ML, ~ .x$id %in% traits_to_keep)

# Rules for query builder
query_rules_ML <- list(
  condition = "AND",
  rules = list(
    list(
      id = "Type of metabolism",
      operator = "in"
    )
  )
)

# Variables showing conditional panels --- 
ML_hide_results = "(input.subtabs == 'Standard traits' & (input.make_predictions_standard == 0 | output.check_file_gene_functions_standard))|
                    (input.subtabs == 'Other traits' & (input.make_predictions_other == 0 | output.check_file_gene_functions_other))|
                    (input.subtabs == 'Data upload' & (input.make_predictions_data_upload == 0 | output.check_file_gene_functions_data_upload|output.check_file_response_data_upload|output.check_file_predictors_data_upload))|
                    (input.subtabs == 'Model upload' & (input.make_predictions_model_upload == 0 | output.check_file_gene_functions_model_upload | output.check_file_model_model_upload))"

ML_show_results = "(input.subtabs == 'Standard traits' & (input.make_predictions_standard > 0 & !output.check_file_gene_functions_standard))|
                  (input.subtabs == 'Other traits' & (input.make_predictions_other > 0 & !output.check_file_gene_functions_other))|
                  (input.subtabs == 'Data upload' & (input.make_predictions_data_upload > 0 & !output.check_file_gene_functions_data_upload & !output.check_file_response_data_upload & !output.check_file_predictors_data_upload))|
                  (input.subtabs == 'Model upload' & (input.make_predictions_model_upload > 0 & !output.check_file_gene_functions_model_upload & !output.check_file_model_model_upload))"

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
    
    #Define additional javascript (does not work if called in custom.js)
    tags$head(
      tags$script(
        sprintf(
          "
                  $( document ).ready(function() {
                  $('#%s').on('afterCreateRuleInput.queryBuilder', function(e, rule) {
                if (rule.filter.plugin == 'selectize') {
                  rule.$el.find('.rule-value-container').css('min-width', '10vw')
                    .find('.selectize-control').removeClass('form-select');
                    rule.$el.find('.rule-value-container').find('.selectize-dropdown').removeClass('form-select');
                }});
              });",
          ns("query_builder")
        )
      )
    ),
    
    #Title
    div(
                  shiny::h3("Predict traits with machine learning")
    ),

    bslib::layout_sidebar(
      #Sidebar
      sidebar = bslib::sidebar(
                  width = "30%",
                  bslib::navset_tab(id = ns("subtabs"),
                                    bslib::nav_panel(title = "Standard traits",
                                                    fileInput_modal(ns("file_gene_functions_standard"), "Upload predicted gene functions", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"), modalId = ns("gene_functions_modal_standard"), modalLabel = "Download example"),
                                                    shinyWidgets::pickerInput(inputId = ns("models"),
                                                                                label = "Choose traits",
                                                                                choices = names(model_paths),
                                                                                selected = names(model_paths),
                                                                                multiple = TRUE, options = list(`actions-box` = TRUE)
                                                                              ),
                                                    shiny::actionButton(ns("make_predictions_standard"), "Make predictions", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                    ),
                                    
                                     bslib::nav_panel(title = "Other traits",
                                                      fileInput_modal(ns("file_gene_functions_other"), "Upload predicted gene functions", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"), modalId = ns("gene_functions_modal_other"), modalLabel = "Download example"),
                                                      div(
                                                            "Choose trait",
                                                            jqbr::queryBuilderInput(
                                                              inputId = ns("query_builder"),
                                                              filters = query_filters_ML,
                                                              return_value = "r_rules",
                                                              display_errors = TRUE,
                                                              rules = query_rules_ML,
                                                              add_na_filter = FALSE
                                                            )
                                                          ),
                                                      shiny::checkboxInput(ns("show_advanced_other"), "Show advanced settings", value = FALSE),
                                                      shiny::conditionalPanel(
                                                        condition = "input.show_advanced_other == true",
                                                        ns = ns,
                                                        shiny::div(class = "vertical-container",
                                                                   "Ignore missing values in database",
                                                                   shinyWidgets::switchInput(inputId = ns("ignore_missing"), value = TRUE,  size = "small", inline = TRUE)
                                                        ),
                                                        # Advanced inputs
                                                        shiny::sliderInput(ns("proportion_to_keep"), "Proportion of predictors to keep", min = 1e-3, max = 1, value = 0.1),
                                                        shiny::sliderInput(ns("training_split_other"), "Proportion of data for model training", min = 1e-3, max = 1, value = 0.7),
                                                        shiny::numericInput(ns("seed_other"), "Set seed for subsampling", value = 123, min = 1, step = 1),
                                                        shiny::numericInput(ns("ntree_other"), "Set number of trees", value = 50, min = 1, step = 1),
                                                        shiny::numericInput(ns("maxnodes_other"), "Set maximum nodes", value = 30, min = 1, step = 1),
                                                        shiny::sliderInput(ns("positive_class_weight_other"), "Weight for positive classes of responses", min = 1e-3, max = 1, value = 0.5),
                                                        shiny::textInput(ns("trait_name_other"), "Name of trait (alphanumeric characters only)", value = "Custom trait", placeholder = "Enter an alphanumeric value"),
                                                      ),
                                                    shiny::actionButton(ns("make_predictions_other"), "Make predictions", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                     ),
                                    
                                    bslib::nav_panel(title = "Data upload",
                                                     fileInput_modal(ns("file_gene_functions_data_upload"), "Upload predicted gene functions", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"), modalId = ns("gene_functions_modal_data_upload"), modalLabel = "Download example"),
                                                     fileInput_modal(ns("file_response_data_upload"), "Upload response variable", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"), modalId = ns("response_modal"), modalLabel = "Download example"),
                                                     fileInput_modal(ns("file_predictors_data_upload"), "Upload predictor variables", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"), modalId = ns("predictors_modal"), modalLabel = "Download example"),
                                                     shiny::checkboxInput(ns("show_advanced_data_upload"), "Show advanced settings", value = FALSE),
                                                     shiny::conditionalPanel(
                                                       condition = "input.show_advanced_data_upload == true",
                                                       ns = ns,
                                                       
                                                       # Advanced inputs
                                                       shiny::sliderInput(ns("training_split_data_upload"), "Proportion of data for model training", min = 1e-3, max = 1, value = 0.7),
                                                       shiny::numericInput(ns("seed_data_upload"), "Set seed for subsampling", value = 123, min = 1, step = 1),
                                                       shiny::numericInput(ns("ntree_data_upload"), "Set number of trees", value = 50, min = 1, step = 1),
                                                       shiny::numericInput(ns("maxnodes_data_upload"), "Set maximum nodes", value = 30, min = 1, step = 1),
                                                       shiny::sliderInput(ns("positive_class_weight_data_upload"), "Weight for positive classes of responses", min = 1e-3, max = 1, value = 0.5),
                                                       shiny::textInput(ns("trait_name_data_upload"), "Name of trait (alphanumeric characters only)", value = "Custom trait", placeholder = "Enter an alphanumeric value"),
                                                     ),
                                                     shiny::actionButton(ns("make_predictions_data_upload"), "Make predictions", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                    ),
                                    
                                    bslib::nav_panel(title = "Model upload",
                                                    fileInput_modal(ns("file_gene_functions_model_upload"), "Upload predicted gene functions", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"), modalId = ns("gene_functions_modal_model_upload"), modalLabel = "Download example"),
                                                    fileInput_modal(ns("file_model_model_upload"), "Upload random forest models", multiple = TRUE, accept = c(".rds"), modalId = ns("model_modal"), modalLabel = "Download example"),
                                                    shiny::actionButton(ns("make_predictions_model_upload"), "Make predictions", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                            )
                                          
                )
      ),
      #Main content area
      div(
        id = ns("results_page"),

        shiny::conditionalPanel(
          condition = ML_hide_results,
          ns = ns,
          shiny::h4("Please upload files and make selections at left")
        ),
        
        shiny::conditionalPanel(
          condition = ML_show_results,
          ns = ns,
          bslib::card(
            bslib::card_header(shiny::textOutput(ns("prediction_summary"))),
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
          ),
          
          bslib::card(
              bslib::card_header("Model training and evaluation"),
              full_screen = TRUE,
              shiny::selectInput(ns("model_to_display"), "Select a model", choices = "", selected = NULL, multiple = FALSE, selectize = TRUE, width = "100%"),
              div(
                shiny::textOutput(ns("training_summary")),
                shiny::downloadButton(ns('download_model'), 'Download model') %>% shinycssloaders::withSpinner(color = "#3C8DBC")
              ),
              div(
                id = ns("confusion_matrix-container"),
                class = "confusion_matrix-container-style",
                "Confusion matrix",
                plotly::plotlyOutput(ns("confusion_matrix"), width = "100%", height = "30vh") %>% shinycssloaders::withSpinner(color = "#3C8DBC"),
                "Values in cells refer to number of organisms in the evaluation set.  Higher values in green cells are better."
              ),
              div(
                id = ns("metrics-container"),
                class = "metrics-container-style",
                "Detailed metrics",
                plotly::plotlyOutput(ns("metrics"), width = "100%", height = "180px") %>% shinycssloaders::withSpinner(color = "#3C8DBC"),
                "Higher values are better."
              ),
              div(
                 shiny::downloadButton(ns('download_confusion_matrix'), 'Download evaluation metrics') %>% shinycssloaders::withSpinner(color = "#3C8DBC")
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

  # --- Define triggers for reactive expressions ---
  make_predictions_trigger <- reactive({
    input$make_predictions_standard |
      input$make_predictions_other |
      input$make_predictions_data_upload |
      input$make_predictions_model_upload
  })
  
  get_query_string_trigger <- reactive({
    input$make_predictions_other 
  })
  
  train_and_evaluate_rf_trigger <- reactive({
      input$make_predictions_other |
      input$make_predictions_data_upload 
  })
  
  get_pretrained_info_trigger <- reactive({
    input$make_predictions_standard |
      input$make_predictions_model_upload 
  })
  
  get_traits_trigger <- reactive({
    get_probabilities()
  })
  
  # --- Get user input (events) ----
  # Get gene functions
  get_gene_functions <- shiny::eventReactive({make_predictions_trigger()},
  {
    # Get file path
    if (input$subtabs == "Standard traits") {
      fp = input$file_gene_functions_standard$datapath
    }else if(input$subtabs == "Other traits"){
      fp = input$file_gene_functions_other$datapath
    }else if(input$subtabs == "Data upload"){
      fp = input$file_gene_functions_data_upload$datapath
    }else if(input$subtabs == "Model upload"){
      fp = input$file_gene_functions_model_upload$datapath
    }

    # Validate and read file
    gene_functions <- validate_and_read_csv(fp)
    
    # Launch modal
    display_modal(session, ns("pb"), message = "Loading gene functions")
    
    # Process file
    gene_functions <- process_uploaded_gene_functions(gene_functions)
    
    # Do further formatting
    gene_functions <- as.data.frame(gene_functions)
    
    # Replace default column name with a more descriptive one
    if (any(c("database_ID", "Database_ID") %in% colnames(gene_functions))) {
      colnames(gene_functions)[colnames(gene_functions) %in% c("database_ID", "Database_ID")] <- "Organism"
    }

    runValidationModal(need(gene_functions != "", "Please check the format of your predicted gene functions file and try again."))

    return(gene_functions)
  },  
  ignoreNULL = TRUE, ignoreInit = TRUE, label="get_gene_functions")  # Must have ignoreInit = TRUE, or module runs on app startup

  # Get query string (from query builder)
  get_query_string <- shiny::eventReactive({get_query_string_trigger()}, 
  {
    query_string  = input$query_builder

    runValidationModal(need(query_string != "", "Please build a valid query."))
    
    return(query_string)
  },
  ignoreNULL = TRUE,  ignoreInit = FALSE, label="get_query_string")
  
  # Get response variable
  get_response <- shiny::eventReactive({train_and_evaluate_rf_trigger()},
  {
    if (input$subtabs == "Standard traits"){
      response = NULL
    }else if (input$subtabs == "Other traits"){
      # Get inputs
      data = clean_data
      query_string = get_query_string()
      ignore_NA = input$ignore_missing
      
      # Format response
      response <- format_response(data = data, query_string = query_string, ignore_NA=ignore_NA)
    }else if(input$subtabs == "Data upload"){
      fp = input$file_response_data_upload$datapath
      response <- validate_and_read_csv(fp)
      
      runValidationModal(need(response != "", "Please check the format of your response variable file and try again."))
      
    }else if(input$subtabs == "Model upload"){
      response <- NULL
    }

    n_response = length(unique(response$Response))
    
    runValidationModal(need(nrow(response)>0, "Please ensure the dataset has at least one response."))
    runValidationModal(need(n_response == 2, "Please ensure that the response variable has exactly two classes."))
    
    return(response)
  },
  ignoreNULL = TRUE, ignoreInit = FALSE, label="get_reponse")

  # Get predictors
  get_predictors <- shiny::eventReactive({train_and_evaluate_rf_trigger()},
  {
    if (input$subtabs == "Standard traits"){
      predictors = NULL
    }else if (input$subtabs == "Other traits"){
      # Get inputs
      data = clean_data
      gene_functions = load_gene_functions()
      proportion_to_keep = input$proportion_to_keep
      seed = input$seed
      
      gene_functions = gene_functions %>% dplyr::filter(grepl("^K", Database_ID)) # Keep only KO IDs
      
      # Format predictors
      predictors <- format_predictors(gene_functions = gene_functions, proportion_to_keep = proportion_to_keep, seed = seed)
    }else if(input$subtabs == "Data upload"){
      fp = input$file_predictors_data_upload$datapath
      predictors <- validate_and_read_csv(fp)
      
      runValidationModal(need(predictors != "", "Please check the format of your predictor variables file and try again."))
      
    }else if(input$subtabs == "Model upload"){
      predictors <- NULL
    }

    runValidationModal(need(ncol(predictors)>1, "Please ensure the dataset has at least one predictor"))

    return(predictors)
  },
  ignoreNULL = TRUE, ignoreInit = FALSE, label="get_predictors")

  # --- Process input ---
  # Get data for random forest model
  get_rf_data = shiny::eventReactive({train_and_evaluate_rf_trigger()}, 
  {
    #Get inputs
    ignore_NA = input$ignore_missing
    proportion_to_keep = input$proportion_to_keep
    seed = input$seed

    # Update modal with progress bar
    display_modal(session, ns("pb"), message = "Loading variables for model", value = 0)
    
    response = get_response()
    predictors = get_predictors()

    # Format data
    rf_data = format_rf_data(predictors = predictors, response = response)

    runValidationModal(shiny::need(rf_data != "", "Error in formatting data for model. Please try again."))

    return(rf_data)
  },
  ignoreNULL = TRUE,  ignoreInit=FALSE, label="get_rf_data")

  # Train and evaluate random forest model
  train_and_evaluate_rf = shiny::eventReactive({train_and_evaluate_rf_trigger()}, 
  {
    # Get inputs    
    if (input$subtabs == "Other traits"){
      rf_data = get_rf_data()
      seed = input$seed_other
      ntree = input$ntree_other
      maxnodes = input$maxnodes_other
      positive_class_weight = input$positive_class_weight_other
      training_split = input$training_split_other
    }else if(input$subtabs == "Data upload"){
      # Get inputs
      rf_data = get_rf_data()
      seed = input$seed_data_upload
      ntree = input$ntree_data_upload
      maxnodes = input$maxnodes_data_upload
      positive_class_weight = input$positive_class_weight_data_upload
      training_split = input$training_split_other
    }

    # Update modal with progress bar
    display_modal(session, ns("pb"), message = "Training random forest model", value = 0)

    cat(file = stderr(), paste0("Started training at ", Sys.time(), "\n"))
    
    # Build model
    rf <- build_rf(data = rf_data, seed = seed, training_split = training_split, ntree = ntree, maxnodes = maxnodes, positive_class_weight = positive_class_weight)

    cat(file = stderr(), paste0("Ended training at ", Sys.time(), "\n"))
    
    runValidationModal(need(rf != "", "Error in training random forest model. Please check formatting of data and try again."))

    return(rf)
  },
  ignoreNULL = TRUE,  ignoreInit=FALSE, label="train_and_evaluate_rf")
  
  # Get random forest model
  get_models <- shiny::eventReactive({make_predictions_trigger()},
   {
     # Update modal with progress bar
     display_modal(session = session, id = ns("pb"), message = "Loading machine learning models", value = 0)
     
     # Initialize models variable
     models <- NULL
     
     # Fetch models and paths based on subtabs
     if (input$subtabs == "Standard traits") {
       model_names <- input$models
       runValidationModal(need(model_names != "", "Please choose a trait to predict"))
       
       model_paths <- lapply(model_names, function(model_name) {
         model_paths[[model_name]]
       })
       
       models <- load_models(model_names = model_names, model_paths = model_paths, file_upload=FALSE)
     } else if(input$subtabs == "Model upload") {
       model_names <- input$file_model_model_upload$name

       if (is.null(model_names)) {
         model_names <- gsub("\\.rds$", "", model_names)
       }
       
       model_paths <- input$file_model_model_upload$datapath
       
       runValidationModal(need(model_names != "", "Please upload a model file"))
       
       models <- load_models(model_names = model_names, model_paths = model_paths, file_upload=TRUE)
     } else if (input$subtabs == "Other traits") {
       models <- train_and_evaluate_rf()
       models <- list(models)
       names(models) <- input$trait_name_other
       
       runValidationModal(need(grepl("^[a-zA-Z0-9_ ]*$", names(models)), "Please enter a valid trait name and try again."))
       runValidationModal(need(names(models)!="", "Please enter a valid trait name and try again."))
     } else if (input$subtabs == "Data upload") {
       models <- train_and_evaluate_rf()
       models <- list(models)
       names(models) <- input$trait_name_data_upload
       
       runValidationModal(need(grepl("^[a-zA-Z0-9_ ]*$", names(models)), "Please enter a valid trait name and try again."))
       runValidationModal(need(names(models)!="", "Please enter a valid trait name and try again."))
     }
     
     runValidationModal(shiny::need(models != "", "Error in loading models. Please try again."))
     
     return(models)
   },
   ignoreNULL = TRUE, ignoreInit = TRUE, label="get_models")
  
  # Get probabilities of traits
  get_probabilities <- shiny::eventReactive({make_predictions_trigger()},
  {
    #Get inputs
    df <- get_gene_functions()
    models <- get_models()
    
    # Update modal with progress bar
    display_modal(session, ns("pb"), message = "Prediction in progress", value = 50)
    
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
    prediction_df <- run_random_forest(df = df, models = models)
    colnames(prediction_df) <- gsub(pattern = "\\.rds", replacement = "", x = colnames(prediction_df))
    
    # Hide the modal with progress bar
    hide_modal_with_progress(session, ns("pb"))
    
    #Print status to log
    cat(file = stderr(), paste0("Ended prediction at ", Sys.time(), "\n"))
    
    runValidationModal(shiny::need(prediction_df != "", "Error in getting probabilities for traits. Please try again."))

    return(prediction_df)
  },
   ignoreNULL = TRUE, ignoreInit = FALSE, label="get_probabilities")
  
  # Get traits from probabilities
  get_traits <- shiny::eventReactive({get_traits_trigger()},
  {
    traits_df <- get_probabilities()
    
    traits_df <- traits_df %>% dplyr::mutate_at(dplyr::vars(-1), make_binary)

    runValidationModal(shiny::need(traits_df != "", "Error in getting traits. Please try again."))
    
    return(traits_df)
  },
  ignoreNULL = TRUE,  ignoreInit = FALSE, label="get_traits")
  
  # Count organisms with at least one predicted trait
  count_organisms <- shiny::eventReactive({get_traits_trigger()},
  {
    # Get inputs
    df = get_traits()
    
    df = df %>% dplyr::select(-Organism)
    
    n_organisms = sum(rowSums(df) > 0)
    
    runValidationModal(shiny::need(n_organisms != "", "Error in counting organisms. Please try again."))

    return(n_organisms)
  },
  ignoreNULL = TRUE,  ignoreInit=FALSE, label="count_organisms")
  
  # --- Update selections ---
  # Update choices for model to display
  shiny::observeEvent({make_predictions_trigger()},
  {
      x = names(get_models())
      
      if (is.null(x))
        x <- character(0)
      
      shiny::updateSelectInput(session, inputId = "model_to_display", choices = x, selected = head(x, 1))
  })
  
  # Update modal state
  observeEvent(input$modal_closed, {
    # Set modal_open to FALSE
    session$userData$modal_open(FALSE)
   
    print("The modal has been closed.")
  })
  
  # --- Generate outputs ---
  # Output example data for download
  output$downloadFunctions_1 <- create_download_handler("gene_functions_e_coli", function() gene_functions_e_coli)
  output$downloadFunctions_2 <- create_download_handler("gene_functions_uncharacterized", function() gene_functions_uncharacterized)
  output$downloadFunctions_3 <- create_download_handler("gene_functions_rumen_cultured", function() gene_functions_rumen_cultured)
  output$downloadFunctions_4 <- create_download_handler("gene_functions_rumen_MAGs", function() gene_functions_rumen_MAGs)

  output$downloadResponse_1 <- create_download_handler("response_fermentation", load_response_fermentation)
  output$downloadResponse_2 <- create_download_handler("response_methanogenesis", load_response_methanogenesis)
  
  output$downloadPredictors_1 <- create_download_handler("predictors_fermentation", load_predictors_fermentation)
  output$downloadPredictors_2 <- create_download_handler("predictors_methanogenesis", load_predictors_methanogenesis)

  output$downloadModel_1 <- create_download_handler("random_forest_fermentation", load_model_fermentation, file_type = "rds")
  output$downloadModel_2 <- create_download_handler("random_forest_methanogenesis", load_model_methanogenesis, file_type = "rds")
  
  # Output modal with example files
  shiny::observeEvent(input$gene_functions_modal_standard, ignoreInit = TRUE, {
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
  
  shiny::observeEvent(input$gene_functions_modal_other, ignoreInit = TRUE, {
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
  
  shiny::observeEvent(input$gene_functions_modal_data_upload, ignoreInit = TRUE, {
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
  
  shiny::observeEvent(input$gene_functions_modal_model_upload, ignoreInit = TRUE, {
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
  
  shiny::observeEvent(input$response_modal, ignoreInit = TRUE, {
    shiny::showModal(shiny::modalDialog(
      shiny::h3("Example files"),
      tags$ol(class = "circled-list",
              tags$li(shiny::downloadLink(outputId = ns("downloadResponse_1"), label = "Fermentation")),
              tags$li(shiny::downloadLink(outputId = ns("downloadResponse_2"), label = "Methanogenesis")),
      ),
      shiny::div("Click ",
                 shiny::actionLink(ns("go_to_help"), "here"),
                 " to see detailed guidelines."),
      easyClose = TRUE, footer = NULL
    ))
  })
  
  shiny::observeEvent(input$predictors_modal, ignoreInit = TRUE, {
    shiny::showModal(shiny::modalDialog(
      shiny::h3("Example files"),
      tags$ol(class = "circled-list",
              tags$li(shiny::downloadLink(outputId = ns("downloadPredictors_1"), label = "Fermentation")),
              tags$li(shiny::downloadLink(outputId = ns("downloadPredictors_2"), label = "Methanogenesis")),
      ),
      shiny::div("Click ",
                 shiny::actionLink(ns("go_to_help"), "here"),
                 " to see detailed guidelines."),
      easyClose = TRUE, footer = NULL
    ))
  })
  
  shiny::observeEvent(input$model_modal, ignoreInit = TRUE, {
    shiny::showModal(shiny::modalDialog(
      shiny::h3("Example files"),
      tags$ol(class = "circled-list",
              tags$li(shiny::downloadLink(outputId = ns("downloadModel_1"), label = "Fermentation")),
              tags$li(shiny::downloadLink(outputId = ns("downloadModel_2"), label = "Methanogenesis")),
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
  output$check_file_gene_functions_standard <- shiny::reactive({
    is.null(input$file_gene_functions_standard$datapath)
  })
  shiny::outputOptions(output, "check_file_gene_functions_standard", suspendWhenHidden = FALSE)
  
  output$check_file_gene_functions_other <- shiny::reactive({
    is.null(input$file_gene_functions_other$datapath)
  })
  shiny::outputOptions(output, "check_file_gene_functions_other", suspendWhenHidden = FALSE)
  
  output$check_file_gene_functions_data_upload <- shiny::reactive({
    is.null(input$file_gene_functions_data_upload$datapath)
  })
  shiny::outputOptions(output, "check_file_gene_functions_data_upload", suspendWhenHidden = FALSE)
  
  output$check_file_response_data_upload <- shiny::reactive({
    is.null(input$file_response_data_upload$datapath)
  })
  shiny::outputOptions(output, "check_file_response_data_upload", suspendWhenHidden = FALSE)
  
  output$check_file_predictors_data_upload <- shiny::reactive({
    is.null(input$file_predictors_data_upload$datapath)
  })
  shiny::outputOptions(output, "check_file_predictors_data_upload", suspendWhenHidden = FALSE)
  
  output$check_file_gene_functions_model_upload <- shiny::reactive({
    is.null(input$file_gene_functions_model_upload$datapath)
  })
  shiny::outputOptions(output, "check_file_gene_functions_model_upload", suspendWhenHidden = FALSE)
  
  output$check_file_model_model_upload <- shiny::reactive({
    is.null(input$file_model_model_upload$datapath)
  })
  shiny::outputOptions(output, "check_file_model_model_upload", suspendWhenHidden = FALSE)
  
  # Output summary text for predictions
  output$prediction_summary = shiny::renderText(
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
    
    df = get_probabilities()
    
    # Summary plot
    output$summary <- plotly::renderPlotly({
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
      df = ML_results_to_plot(df = df, plot_type="treemap")
      plot = plot_treemap(df=df,
                          hovertemplate = "<b>Trait: %{label}</b><br><b>% total: %{value:.2f}</b><br><extra></extra>")
      plot
    })
    
    # Heatmap
    output$heatmap <- plotly::renderPlotly({
      df <- ML_results_to_plot(df = df, plot_type="heatmap")
      plot <- plot_heatmap(df, 
                           hovertemplate = "<b>Trait: %{x}</b><br><b>Organism: %{y}</b><br><b>% probability: %{z:.2f}</b><br><extra></extra>",
                           legend_labels = c("0", "25", "50", "75", "100"), 
                           legend_title = "% probability")
      plot
    })
  })
  
  # Plot model evaluation plots
  shiny::observe({
    models <- get_models()
    
    model_to_display <- input$model_to_display
    selected_model <- models[[model_to_display]]
    df <- selected_model$evaluation_results
    
    # Plot confusion matrix
    output$confusion_matrix <- plotly::renderPlotly({
      if(!is.null(df)){
      # Format the matrix for plotting
      df$table = df$table
      rownames(df$table) <- c("Negative", "Positive")
      colnames(df$table) <- c("Negative", "Positive")
      df$table <- df$table[c("Positive", "Negative"), c("Positive", "Negative")]
      }
      
      plot <- plot_confusion_matrix(df = df)

      plot
    })

    # Plot metrics table
    output$metrics <- plotly::renderPlotly({
      plot <- plot_metrics_table(df = df)
      
      plot
    })
  })
  
  # Output summary text for training
  shiny::observe({
    models <- get_models()
    model_to_display <- input$model_to_display
    selected_model <- models[[model_to_display]]
    df <- selected_model$evaluation_results
    
    # Extract number of responses and predictors from the model
    if ("randomForest" %in% class(selected_model)) {
      n_predictors <- length(selected_model$forest$ncat)
      n_responses_training <- length(selected_model$y)
      n_responses_evaluation <- sum(df$table)
    } else {
      # Fallback or handle other model types appropriately
      return("Unsupported model type for extracting training summary.")
    }
    
    output$training_summary <- shiny::renderText(
      paste0("Model trained with ", n_responses_training, " responses (organisms) and ", 
             n_predictors, " predictors (gene functions). Model evaluated with an additional ",
             n_responses_evaluation, " responses (organisms).")
             )
  })
  
  # Output downloadable rds for model
  output$download_model <- shiny::downloadHandler(
    filename = function() {
      model_to_display <- input$model_to_display
      paste(model_to_display, "rds", sep = ".")
    },
    content = function(file) {
      # Get inputs
      models <- get_models()
      model_to_display <- input$model_to_display
      
      selected_model <- models[[model_to_display]]
      
      # Save model
      if (!is.null(selected_model)) {
        saveRDS(selected_model, file)  # Save the selected model as an RDS file
      } else {
        stop("No model available to download.")
      }
    }
  )
  
  # Output downloadable csv for confusion matrix 
  output$download_confusion_matrix <- shiny::downloadHandler(
    filename = function() {
      model_to_display <- input$model_to_display
      paste(model_to_display, "txt", sep = ".")
    },
    content = function(file) {
      models <- get_models()
      model_to_display <- input$model_to_display
      selected_model <- models[[model_to_display]]
      df <- selected_model$evaluation_results

      if (!is.null(df)) {
        output = capture.output(print(df))
        writeLines(output, file)
      } else {
        stop("No confusion matrix available to download.")
      }
    }
  )
  
}


# Main Shiny App Script
# This script sets up the system locale, loads external R scripts, and defines the user interface (UI)
# and server components for the Shiny app. The app includes modules for 
# database searching, predictions, and user help, all organized within a Bootstrap-based layout.
# Author: Timothy Hackmann
# Date: 6 September 2024

# === Set system locale ===
Sys.setlocale("LC_ALL", "C")

# === Load external R files ===
source("homeModule.R", local = TRUE)

# === Define user interface (UI) ===
ui <- 
  htmltools::tagList(
    bslib::page_fluid(
      title = "Fermentation Explorer",
      # --- Set style ---
      #Set Bootstrap version and theme
      theme = bslib::bs_theme(version = 5, preset = "shiny", 
                              font_scale = 1, 
                              spacer = "0.5rem"
      ),
      
      #Set theme for query builder
      jqbr::useQueryBuilder(bs_version = "5"),
      
      #Get JavaScript file from custom.js (/www folder)
      shinyjs::useShinyjs(),
      htmltools::tags$head(tags$script(src="custom.js")),
      
      #Get style from style.css (/www folder)
      htmltools::tags$head(
        htmltools::tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
        htmltools::tags$link(rel = "shortcut icon", href = "favicon.svg")
      ),
      
      # # --- Define layout of tabs ---
      # # Navigation bar
      bslib::page_navbar(
        id = "tabs",
      #   
        # Home
        bslib::nav_panel(
          value = "home",
          title = htmltools::div(shiny::icon("home"), "Home"),
          homeUI("home")
        ),
        
        # Predict
        bslib::nav_menu(title = htmltools::div(shiny::icon("desktop"), "Predict"),
                        # Predict with machine learning
                        bslib::nav_panel(
                          value = "predictionsMachineLearning",
                          title = "With machine learning",
                          predictionsMachineLearningUI("predictionsMachineLearning"),
                        )
        ),
        
        # Navigation bar options
        selected = "predictionsMachineLearning"  
      )
    )
  )