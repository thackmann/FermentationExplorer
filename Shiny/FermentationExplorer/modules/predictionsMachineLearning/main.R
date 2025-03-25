# Define the Predictions Using Machine Learning Module in Shiny App
# This script defines the user interface (UI) and server for the predictions using machine learning module.  
# Author: Timothy Hackmann
# Date: 23 Mar 2025

# === Define user interface (UI) ===
	predictionsMachineLearningUI <- function(id) {
	  ns <- shiny::NS(id)
	  shiny::tagList(
		#Call JavaScript functions
		inject_js_resize(ns, "treemap-container"),
		inject_query_builder_js(ns, "query_builder"),
		
		#Title
		create_title_div("Predict traits with machine learning"),

		bslib::layout_sidebar(
		  #Sidebar
		  sidebar = bslib::sidebar(
					  width = "30%",
					  
					  # Select data
					  div("Choose organisms (gene functions)"),
					  bslib::navset_tab(id = ns("function_tabs"),
										bslib::nav_panel(title = "Database",
														 create_selectize_input(inputId = ns("gene_functions_database"))
										),
										bslib::nav_panel(title = "File upload",
														 fileInput_modal(ns("gene_functions_upload"), modalId = ns("gene_functions_modal"))
										)
					  ),
					  
					  div("Choose traits, data, or models"),
					  bslib::navset_tab(id = ns("subtabs"),
										bslib::nav_panel(title = "Standard traits",
														 create_selectize_input(inputId = ns("model_names"), choices = NULL, selected = NULL)
										),
										bslib::nav_panel(title = "Other traits",
														create_query_builder(ns = ns, input_id = "query_builder"),
										),
										bslib::nav_panel(title = "Data upload",
													   fileInput_modal(ns("response_upload"), "Upload response variable", multiple = FALSE, modalId = ns("response_modal")),
													   fileInput_modal(ns("predictors_upload"), "Upload predictor variables", multiple = FALSE, modalId = ns("predictors_modal"))
										),
										bslib::nav_panel(title = "Model upload",
													  fileInput_modal(ns("model_upload"), accept = c(".rds"), modalId = ns("model_modal"))
										),
										
										# Advanced inputs
										shiny::checkboxInput(ns("show_advanced"), "Show advanced settings", value = FALSE),
										shiny::conditionalPanel(
										  condition = "input.show_advanced",
										  ns = ns,
										  shiny::sliderInput(ns("threshold"), "Probability threshold", min = 0, max = 1, value = 0.5),
										),
										shiny::conditionalPanel(
										  condition = "input.show_advanced && input.subtabs == `Other traits`",
										  ns = ns,
										  create_switch_input(inputId = ns("ignore_missing"), label = "Ignore missing values in database"),
										  shiny::sliderInput(ns("predictors_to_keep"), "Proportion of predictors to keep", min = 1e-3, max = 1, value = 0.1),
										  shiny::sliderInput(ns("responses_to_keep"), "Proportion of responses to keep", min = 1e-3, max = 1, value = 1),
										),
										shiny::conditionalPanel(
										  condition = "input.show_advanced && (input.subtabs == `Other traits`|input.subtabs == `Data upload`)",
										  ns = ns,
										  shiny::sliderInput(ns("training_split"), "Proportion of responses for model training", min = 1e-3, max = 1, value = 0.7),
										  shiny::numericInput(ns("seed"), "Set seed for subsampling", value = 123, min = 1, step = 1),
										  shiny::numericInput(ns("ntree"), "Set number of trees", value = 50, min = 1, step = 1),
										  shiny::numericInput(ns("maxnodes"), "Set maximum nodes", value = 30, min = 1, step = 1),
										  shiny::sliderInput(ns("positive_class_weight"), "Weight for positive classes of responses", min = 1e-3, max = 1, value = 0.5),
										  shiny::textInput(ns("trait_name"), "Name of trait (alphanumeric characters only)", value = "Custom trait", placeholder = "Enter an alphanumeric value")
										)
						),
					
					# Make predictions
					shiny::actionButton(ns("make_predictions"), "Make predictions", class = "btn btn-primary")
		  ),
		  #Main content area
		  div(
			id = ns("results_page"),

			shiny::conditionalPanel(
			  condition = "!output.flag_results",
			  ns = ns,
			  shiny::h4("Please upload files and make selections at left")
			),
			
			shiny::conditionalPanel(
			  condition = "output.flag_results",
			  ns = ns,
			  
			  # Summary and download button
			  bslib::card(
				bslib::card_header(shiny::textOutput(ns("summary_text"))),
				create_download_button(ns('download_data'))
			  ),
			  
			  # Tabs for plots
			  bslib::navset_card_underline(
				id = ns("results_tabs"),
				title = "Prediction Results",
				
				# Panels
				create_plot_panel(ns, "summary", "Summary"),
				create_plot_panel(ns, "treemap", "Treemap", centered = TRUE),
				create_plot_panel(ns, "heatmap", "Heatmap")
			  ),
			  
			  # Model details
			  bslib::card(
				  bslib::card_header("Model training and evaluation"),
				  full_screen = TRUE,
				  create_picker_input(ns("model_to_display"), label = "Select a model", multiple = FALSE),
				  div(
					shiny::textOutput(ns("training_summary")),
					create_download_button(ns('download_model'), label = "Download model")
				  ),
				  div(
					"Confusion matrix",
					create_plot_div(ns = ns, plot_type = "confusion_matrix", height = "30vh"),
					"Values in cells refer to number of organisms in the evaluation set.  Higher values in green cells are better."
				  ),
				  div(
					"Detailed metrics",
					create_plot_div(ns = ns, plot_type = "metrics", height = "220px"),
					"Higher values are better."
				  ),
				  div(
					create_download_button(ns('download_confusion_matrix'), label = "Download evaluation metrics")
				  )
			  )
			)
		  )
		)
	  )
	}

# === Define server ===
	predictionsMachineLearningServer <- function(input, output, session, x, selected_tab) {
	  # Set namespace
	  ns <- session$ns

	  # --- Define triggers for reactive expressions ---
	  tab_selected_trigger <- make_tab_trigger(selected_tab, "predictionsMachineLearning")
	  
	  make_predictions_trigger <- make_action_button_trigger("make_predictions")
	  
	  get_query_string_trigger <- make_other_trigger(
	    input$subtabs == "Other traits" && input$make_predictions
	  )
	  
	  train_and_evaluate_rf_trigger <- make_other_trigger(
	    input$subtabs %in% c("Other traits", "Data upload") && input$make_predictions
	  )
	  
	  get_pretrained_info_trigger <- make_other_trigger(
	    input$subtabs %in% c("Standard traits", "Model upload") && input$make_predictions
	  )
	  
	  # --- Get user input (events) ----
	  # Get gene functions
	  get_gene_functions <- shiny::eventReactive({make_predictions_trigger()},
	  {
		# Get data
		database <- load_database()
			
		# Get file path
		if (input$function_tabs == "Database") {
		  # Get gene functions for selected organisms in the database
		  gene_functions <- load_gene_functions()
		  organism_by_genome <- get_organism_by_genome(database = database)
		  selected_organisms <- input$gene_functions_database
		  
		  gene_functions <- process_database_gene_functions(gene_functions, organism_by_genome, selected_organisms)
		  
		  runValidationModal(session = session, need(gene_functions != "", "Please choose at least one organism."))
		} else if (input$function_tabs == "File upload") {
		  # Validate, read, and process the gene functions file
		  gene_functions <- validate_and_read_file(session = session, file_path = input$gene_functions_upload$datapath)
		  gene_functions <- process_uploaded_gene_functions(gene_functions)
		  
		  runValidationModal(session = session, need(gene_functions != "", "Please check the format of your predicted gene functions file and try again."))
		}
		
		# Launch modal
		display_modal(session, ns("pb"), message = "Loading gene functions")

		return(gene_functions)
	  },  
		label="get_gene_functions")

	  # Get query string (from query builder)
	  get_query_string <- shiny::eventReactive({get_query_string_trigger()}, 
	  {
		# Get input
		query_string  = input$query_builder

		# Process query string for more precise matching
		query_string <- process_query_string(query_string)
		
		runValidationModal(session = session, need(query_string != "", "Please build a valid query."))
		
		return(query_string)
	  },
	  label="get_query_string")
	  
	  # Get response variable
	  get_response <- shiny::eventReactive({train_and_evaluate_rf_trigger()},
	  {
		if (input$subtabs == "Standard traits"){
		  response = NULL
		}else if (input$subtabs == "Other traits"){
		  # Get inputs
		  data = load_database()
		  query_string = get_query_string()
		  ignore_NA = input$ignore_missing
		
		  # Format response
		  response <- format_response(data = data, 
									  query_string = query_string, 
									  ignore_NA = ignore_NA)
		}else if(input$subtabs == "Data upload"){
		  fp = input$response_upload$datapath
		  response <- validate_and_read_file(session = session, file_path = fp)
		  
		  runValidationModal(session = session, need(response != "", "Please check the format of your response variable file and try again."))
		  
		}else if(input$subtabs == "Model upload"){
		  response <- NULL
		}

		n_response = length(unique(response$Response))
		
		runValidationModal(session = session, need(nrow(response)>0, "Please ensure the dataset has at least one response."))
		runValidationModal(session = session, need(n_response == 2, "Please ensure that the response variable has exactly two classes."))
		
		return(response)
	  },
	  , label="get_reponse")

	  # Get predictors
	  get_predictors <- shiny::eventReactive({train_and_evaluate_rf_trigger()},
	  {
		if (input$subtabs == "Standard traits"){
		  predictors = NULL
		}else if (input$subtabs == "Other traits"){
		  # Get inputs
		  data = load_database()
		  gene_functions = load_gene_functions()
		  responses_to_keep = input$responses_to_keep
		  predictors_to_keep = input$predictors_to_keep
		  seed = input$seed
		  
		  # Format predictors
		  predictors <- format_predictors(gene_functions = gene_functions, 
										  responses_to_keep = responses_to_keep, 
										  predictors_to_keep = predictors_to_keep,
										  seed = seed)
		}else if(input$subtabs == "Data upload"){
		  fp = input$predictors_upload$datapath
		  predictors <- validate_and_read_file(session = session, file_path = fp)
		  
		  runValidationModal(session = session, need(predictors != "", "Please check the format of your predictor variables file and try again."))
		  
		}else if(input$subtabs == "Model upload"){
		  predictors <- NULL
		}

		runValidationModal(session = session, need(ncol(predictors)>1, "Please ensure the dataset has at least one predictor"))

		return(predictors)
	  },
		label="get_predictors")

	  # --- Process input ---
	  # Get data for random forest model
	  get_rf_data = shiny::eventReactive({train_and_evaluate_rf_trigger()}, 
	  {
		#Get inputs
		ignore_NA = input$ignore_missing
		seed = input$seed

		# Update modal with progress bar
		display_modal(session, ns("pb"), message = "Loading variables for model", value = 0)
		
		response = get_response()
		predictors = get_predictors()

		# Format data
		rf_data = format_rf_data(predictors = predictors, response = response)

		runValidationModal(session = session, shiny::need(rf_data != "", "Error in formatting data for model. Please try again."))

		return(rf_data)
	  },
	  label="get_rf_data")

	  # Train and evaluate random forest model
	  train_and_evaluate_rf = shiny::eventReactive({train_and_evaluate_rf_trigger()}, 
	  {
		# Get inputs    
		rf_data = get_rf_data()
		seed = input$seed
		ntree = input$ntree
		maxnodes = input$maxnodes
		positive_class_weight = input$positive_class_weight
		training_split = input$training_split

		# Update modal with progress bar
		display_modal(session, ns("pb"), message = "Training random forest model", value = 0)

		cat(file = stderr(), paste0("Started training at ", Sys.time(), "\n"))
		
		# Build model
		rf <- build_rf(data = rf_data, seed = seed, training_split = training_split, ntree = ntree, maxnodes = maxnodes, positive_class_weight = positive_class_weight)

		cat(file = stderr(), paste0("Ended training at ", Sys.time(), "\n"))
		
		runValidationModal(session = session, need(rf != "", "Error in training random forest model. Please check formatting of data and try again."))

		return(rf)
	  },
	  label="train_and_evaluate_rf")
	  
	  # Get random forest model
	  get_models <- shiny::eventReactive({make_predictions_trigger()},
	  {
		 # Update modal with progress bar
		 display_modal(session = session, id = ns("pb"), message = "Loading machine learning models", value = 0)
		 
		 # Initialize models variable
		 models <- NULL
		 
		 # Fetch models and paths based on subtabs
		 if (input$subtabs == "Standard traits") {
		   model_names <- input$model_names
		   runValidationModal(session = session, need(model_names != "", "Please choose at least one trait to predict"))
		   
		   model_paths <- lapply(model_names, function(model_name) {
			 model_paths[[model_name]]
		   })
		   
		   models <- load_models(session = session, model_names = model_names, model_paths = model_paths, file_upload=FALSE)
		 } else if(input$subtabs == "Model upload") {
		   model_names <- input$model_upload$name

		   if (is.null(model_names)) {
			 model_names <- gsub("\\.rds$", "", model_names)
		   }
		   
		   model_paths <- input$model_upload$datapath
		   
		   runValidationModal(session = session, need(model_names != "", "Please upload a model file"))
		   
		   models <- load_models(session = session, model_names = model_names, model_paths = model_paths, file_upload=TRUE)
		 } else if (input$subtabs == "Other traits"|input$subtabs == "Data upload") {
		   models <- train_and_evaluate_rf()
		   models <- list(models)
		   names(models) <- input$trait_name
		   
		   runValidationModal(session = session, need(grepl("^[a-zA-Z0-9_ ]*$", names(models)), "Please enter a valid trait name and try again."))
		   runValidationModal(session = session, need(names(models)!="", "Please enter a valid trait name and try again."))
		 }
		 
		 runValidationModal(session = session, shiny::need(models != "", "Error in loading models. Please try again."))
		 
		 return(models)
	   },
	   , label="get_models")
	  
	  # Predict probabilities of traits
	  predict_traits <- shiny::eventReactive({make_predictions_trigger()},
	  {
		# Get inputs
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
		
		runValidationModal(session = session, shiny::need(prediction_df != "", "Error in getting probabilities for traits. Please try again."))

		return(prediction_df)
	  },
	   , label="predict_traits")
	  
	  # --- Update selections ---
	  # Update choices for gene functions (organisms)
	  shiny::observeEvent({tab_selected_trigger()},
	  {
		  database <- load_database()
		  choices <- get_organism_choices(database)
		  update_select_input(session = session, inputId = "gene_functions_database", choices = choices)
	  }, 
	  , label="update_gene_function_choices")

	  # Update query builder
	  shiny::observeEvent({tab_selected_trigger()},
	  {
		update_query_builder(inputId = "query_builder", choices = choices_traits_ML)
	  },
	  label="update_query_builder")
	  
	  # Update choices for models
	  shiny::observeEvent({tab_selected_trigger()},
	  {
		choices = names(model_paths)
		selected = c(
					  "Fermentation (type of metabolism)",
					  "Methanogenesis (type of metabolism)"
					 )
		update_select_input(session = session, inputId = "model_names", choices = choices, selected = selected)
	  }, 
	  , label="update_model_choices")
	  
	  
	  # Update choices for model to display
	  shiny::observeEvent({make_predictions_trigger()},
	  {
		choices = names(get_models())
		update_picker_input(session = session, inputId = "model_to_display", choices = choices)
	  },
	  label="update_model_to_display")
	  
	  # --- Generate outputs ---
	  # Output modals with example data
	  output_download_modal(
	    input_id = "gene_functions_modal",
	    object_ids = c(
	      "gene_functions_e_coli",
	      "gene_functions_uncharacterized",
	      "gene_functions_rumen_cultured",
	      "gene_functions_rumen_MAGs"
	    ),
	    labels = c(
	      "E. coli",
	      "Previously uncharacterized bacteria",
	      "Cultured prokaryotes from rumen",
	      "MAGs from rumen"
	    ),
	    ns = ns,
	    label = "show_gene_functions_modal"
	  )
	  
	  output_download_modal(
	    input_id = "response_modal",
	    object_ids = c(
	      "response_fermentation",
	      "response_methanogenesis"
	    ),
	    labels = c(
	      "Fermentation",
	      "Methanogenesis"
	    ),
	    ns = ns,
	    label = "show_response_modal"
	  )
	  
	  output_download_modal(
	    input_id = "predictors_modal",
	    object_ids = c(
	      "predictors_fermentation",
	      "predictors_methanogenesis"
	    ),
	    labels = c(
	      "Fermentation",
	      "Methanogenesis"
	    ),
	    ns = ns,
	    label = "show_predictors_modal"
	  )
	  
	  output_download_modal(
	    input_id = "model_modal",
	    object_ids = c(
	      "model_fermentation",
	      "model_methanogenesis"
	    ),
	    labels = c(
	      "Fermentation",
	      "Methanogenesis"
	    ),
	    file_types = c("rds", "rds"),
	    ns = ns,
	    label = "show_model_modal"
	  )
	  
	  # Create observer to direct user to Help
	  navigate_to_help(session = x, selected_tab = "help", selected_panel = "Predict traits with machine learning")
	  
	  # Create output flags
	  flag_if_not_null(output, "flag_results", trigger = make_predictions_trigger, value_fun = predict_traits)
	  
	  # Output summary text
	  output$summary_text <- shiny::renderText({
		df <- predict_traits()
		threshold <- input$threshold
		
		counts <- count_predictions(df, 
									organism_col = "Organism number", 
									trait_col = "Model", 
									value_col = "Probability", 
									threshold = threshold)
		
		format_summary_text(
		  count1 = counts$traits_predictions, 
		  count2 = counts$organisms_predictions, 
		  label1 = "traits", 
		  label2 = "organisms", 
		  total2 = counts$organisms_total
		)
	  })
	  
	  # Output downloadable csv of results
	  output$download_data <- create_download_handler(
		filename_prefix = "results",
		data_source = function() {
		  table <- predict_traits()
		  table
		},
	  )
	  
	  # Output overview plots
	  shiny::observeEvent({list(make_predictions_trigger(), input$threshold)},
	  {
		df <- predict_traits()
		df <- format_organism_names(df, cols = "Organism name", abbreviate_names = FALSE)
		threshold <- input$threshold
		
		# Summary plot
		output$summary_plot <- plotly::renderPlotly({
		  df <- results_to_plot(df = df, plot_type="summary",
								x_col = "Organism name", y_col = "Model", z_col = "Probability", 
								var_col = NULL, var_to_keep = NULL, 
								z_threshold = threshold, drop_extra_y = FALSE, z_percentage = TRUE) 
		  plot = plot_summary(df, 
							  coord_fixed = TRUE, 
							  hovertemplate = "<b>Trait: %{x}</b><br><b>% organisms positive: %{z:.2f}</b><br><extra></extra>",
							  legend_labels = c("0", "25", "50", "75", "100"), 
							  legend_title = "% organisms positive")
		  plot
		})
		
		# Treemap plot
		output$treemap_plot <- plotly::renderPlotly({    
		  df <- results_to_plot(df = df, plot_type="treemap",
								x_col = "Organism name", y_col = "Model", z_col = "Probability", 
								var_col = NULL, var_to_keep = NULL, 
								z_threshold = threshold, drop_extra_y = TRUE, z_percentage = TRUE) 
		  dims <- calculate_treemap_dimensions(session, ns, "treemap_plot")
		  hovertemplate <- "<b>Trait: %{label}</b><br><b>% total: %{value:.2f}</b><br><extra></extra>"
		  plot = plot_treemap(df,
							   hovertemplate = hovertemplate,
							   width = dims$width, height = dims$height)
		  plot
		})
		
		# Heatmap plot
		output$heatmap_plot <- plotly::renderPlotly({
		  df <- results_to_plot(df = df, plot_type="heatmap",
								x_col = "Organism name", y_col = "Model", z_col = "Probability", 
								var_col = NULL, var_to_keep = NULL, 
								z_threshold = threshold, drop_extra_y = FALSE, z_percentage = TRUE) 
		  scale_ratio <- calculate_heatmap_scale(session, ns, "heatmap_plot", df)
		  hovertemplate <- "<b>Trait: %{x}</b><br><b>Organism: %{y}</b><br><b>% probability: %{z:.2f}</b><br><extra></extra>"
		  plot <- plot_heatmap(df, 
							   hovertemplate = hovertemplate,
							   legend_labels = c("0", "25", "50", "75", "100"), 
							   legend_title = "% probability",
							   x_to_y_ratio = scale_ratio)
		  plot
		})
	  })
	  
	  # Plot model evaluation plots and summary text for training
	  shiny::observeEvent({list(make_predictions_trigger(), input$model_to_display)},
	  {
		models <- get_models()
		model_to_display <- input$model_to_display
		model_to_display <- assign_if_invalid(model_to_display, names(get_models()[1]))
		selected_model <- models[[model_to_display]]
		df <- selected_model$evaluation_results

		# Plot confusion matrix
		output$confusion_matrix_plot <- plotly::renderPlotly({
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
		output$metrics_plot <- plotly::renderPlotly({
		  plot <- plot_metrics_table(df = df)
		  
		  plot
		})

		# Output training summary
		if ("randomForest" %in% class(selected_model)) {
		  n_predictors <- length(selected_model$forest$ncat)
		  n_responses_training <- length(selected_model$y)
		  n_responses_evaluation <- sum(df$table)
		} else {
		  return("Unsupported model type for extracting training summary.")
		}
		
		output$training_summary <- shiny::renderText(
		  paste0("Model trained with ", n_responses_training, " responses (organisms) and ", 
				 n_predictors, " predictors (gene functions). Model evaluated with an additional ",
				 n_responses_evaluation, " responses (organisms).")
				 )
	  })
	  
	  # Output downloadable rds for model
	  output$download_model <- create_download_handler(
		filename_prefix = reactive(input$model_to_display),
		data_source = function() {
		  models <- get_models()
		  model_to_display <- input$model_to_display
		  selected_model <- models[[model_to_display]]
		  
		  if (is.null(selected_model)) {
			stop("No model available to download.")
		  }
		  
		  selected_model  # Return the model for download
		},
		file_type = "rds"
	  )
	  
	  # Output downloadable csv for confusion matrix 
	  output$download_confusion_matrix <- create_download_handler(
		filename_prefix = reactive(input$model_to_display),
		data_source = function() {
		  models <- get_models()
		  model_to_display <- input$model_to_display
		  selected_model <- models[[model_to_display]]
		  
		  if (is.null(selected_model)) {
			stop("No confusion matrix available to download.")
		  }
		  
		  df <- selected_model$evaluation_results
		  capture.output(print(df))  
		}
	  )
	}