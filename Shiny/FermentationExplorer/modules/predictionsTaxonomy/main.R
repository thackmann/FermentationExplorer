# Define the Predictions from Taxonomy Module in Shiny App
# This script defines the user interface (UI) and server for the predictions from taxonomy module.
# Author: Timothy Hackmann
# Date: 23 Mar 2025

# === Define user interface (UI) ===
  predictionsTaxonomyUI <- function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
      #Call JavaScript functions
      inject_js_resize(ns, "treemap-container"),
      inject_query_builder_js(ns, "query_builder"),
      
      #Title
      create_title_div("Predict traits from taxonomy"),
  
      #Sidebar
      bslib::layout_sidebar(
        #Sidebar
        sidebar = bslib::sidebar(
          width = "30%",
          
          # Select data
          div("Choose organisms (taxa)"),
          bslib::navset_tab(id = ns("taxonomy_tabs"),
                            bslib::nav_panel(title = "Database",
                                             create_selectize_input(inputId = ns("taxonomy_database"))
                            ),
                            bslib::nav_panel(title = "File upload",
                                             fileInput_modal(ns("taxonomy_upload"), modalId = ns("taxonomy_file_modal")),
                            )
          ),
  
          # Select traits
          div("Choose traits"),
          bslib::navset_tab(id = ns("trait_tabs"),
                            bslib::nav_panel(title = "Standard traits",
                                             create_selectize_input(inputId = ns("set_traits")),
                            ),
                            bslib::nav_panel(title = "Other traits", 
                                              create_query_builder(ns = ns, input_id = "query_builder")
                            )
                    ),
         
          # Advanced inputs
          shiny::checkboxInput(ns("show_advanced"), "Show advanced settings", value = FALSE),
          
          shiny::conditionalPanel(
            condition = "input.show_advanced",
            ns = ns,
            shiny::sliderInput(ns("threshold"), "Probability threshold", min = 0, max = 1, value = 0.5),
            create_switch_input(inputId = ns("simple_names"), label = "Simplify names of taxa"),
            create_switch_input(inputId = ns("ignore_missing"), label = "Ignore missing values in database"),
            create_selectize_input(inputId = ns("system_taxonomy"), label = "Taxonomy", multiple = FALSE),
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
            shiny::h4("Please make selections at left")
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
              title = "Prediction results",
              
              # Panels
              create_plot_panel(ns, "summary", "Summary"),
              create_plot_panel(ns, "treemap", "Treemap", centered = TRUE),
              create_plot_panel(ns, "heatmap", "Heatmap"),
              
              # Plot options
              div(
                shiny::conditionalPanel(
                  condition = "output.flag_multiple_traits",
                  ns = ns,
                  create_picker_input(inputId = ns("trait_to_display"), label = "Trait")
                )
              )
            )
          )
          )
      )
    )
  }

# === Define server ===
predictionsTaxonomyServer <- function(input, output, session, x, selected_tab) {
  #Set namespace
  ns <- session$ns
  
  # --- Define triggers for reactive expressions ---
  tab_selected_trigger <- make_tab_trigger(selected_tab, "predictionsTaxonomy")
  
  make_predictions_trigger <- make_action_button_trigger("make_predictions")
  
  # --- Get user input (events) ---
  # Get query taxa
  get_query_taxa <- shiny::eventReactive({make_predictions_trigger()},
  {
    if (input$taxonomy_tabs == "Database") {
      # Get inputs
      selected_organisms <- input$taxonomy_database
      
      query <- process_query_taxa(selected_organisms)

      runValidationModal(session = session, need((!is.null(query)) && (nrow(query) > 0), "Please choose an taxon"))
      
    } else if (input$taxonomy_tabs == "File upload") {
      # Validate, read, and process the taxonomy file
      query <- validate_and_read_file(session = session, file_path = input$taxonomy_upload$datapath)
      query <- process_uploaded_taxonomy(query)
      
      runValidationModal(session = session, need((!is.null(query)) && (nrow(query) > 0), "Please check the format of the taxonomy file and try again."))
    }

    runValidationModal(session = session, need((!is.null(query)) && (nrow(query) > 0), "Please check the format of your uploaded file and try again."))
    
    return(query)
  },
  label="get_query_taxa")
  
  # Get query string (from query builder)
  get_query_string <- shiny::eventReactive({make_predictions_trigger()},
  {
    # Get input
    query_string  = input$query_builder

    # Process query string for more precise matching
    query_string <- process_query_string(query_string)

    runValidationModal(session = session, need(query_string != "", "Please build a valid query."))

    return(query_string)
  },
  label="get_query_string")
  
  # Get table of organisms in database
  get_table <- shiny::eventReactive({make_predictions_trigger()},
  {
    # Get data
    data = load_database()
    
    # Get inputs
    system_taxonomy = input$system_taxonomy
    ignore_NA = input$ignore_missing
    
    # Add taxonomy
    col_name <- paste0(system_taxonomy, " Taxonomy")
    data <- expand_and_merge_taxonomy(data = data, col_name = col_name)

    # Add custom traits
    if(input$trait_tabs == "Other traits") {
      query_string = get_query_string()
      data <- add_custom_traits(data = data, query_string = query_string, ignore_NA = ignore_NA)
    }

    return(data)
  },
  label="get_table")
  
  # Get traits
  get_traits_to_predict <- shiny::eventReactive({make_predictions_trigger()},
  {
    # Get inputs
    if (input$trait_tabs == "Standard traits") {
      traits_to_predict = input$set_traits
    }else if(input$trait_tabs == "Other traits"){
      traits_to_predict = "Custom trait"
    }
    
    runValidationModal(session = session, need(traits_to_predict != "", "Please choose a trait"))
    
    return(traits_to_predict)
  },
  label="get_traits_to_predict")
  
  #--- Process input ---
  # Format query taxa
  format_query_taxa <- shiny::eventReactive({make_predictions_trigger()},
  {
    # Get data
    query = get_query_taxa()
    
    # Get inputs
    simple_names = input$simple_names
    
    #Launch modal with progress bar
    display_modal(session, ns("pb"), message = "Retrieving data")
    
    # Replace missing values with NA
    query[query == ""] = NA
    query[query == "?"] = NA
    query[query == "unclassified"] = NA
    
    # Remove brackets
    query = data.frame(lapply(query, function(x) gsub("\\[|\\]", "", x)))
    
    # Simplify names
    if(simple_names == TRUE) {
      query = as.data.frame(t(apply(query, 1, simplify_names, colnames(query))))
    }
    
    return(query)
  },
  label="format_query_taxa")
  
  # Get predictions
  predict_traits <- shiny::eventReactive({make_predictions_trigger()},
  {
    # Get inputs
    ignore_NA = input$ignore_missing
    table = get_table()
    traits_to_predict = get_traits_to_predict()
    query = format_query_taxa()

    # Format table
    table = table %>% 
      dplyr::select(Phylum, Class, Order, Family, Genus, Species, dplyr::all_of(traits_to_predict))
    
    if (ignore_NA == TRUE) {
      table = table %>%
        dplyr::filter(!dplyr::if_all(dplyr::all_of(traits_to_predict), is.na))
    }
    # Update modal with progress bar
    display_modal(session, ns("pb"), message = "Prediction in progress", value = 0)
    
    # Start prediction
    cat(file = stderr(), paste0("Started prediction at ", Sys.time(), "\n"))
    start_time <- Sys.time()
    
    unique_cols <- c("Phylum", "Class", "Order", "Family", "Genus", "Species")
    
    query_data <- precompute_unique_queries(query, unique_cols)
        
    patterns <- precompute_patterns(query_data$query_unique, traits_to_predict, table)
    
    matching_organisms <- compute_matching_organisms(query_data$query_unique, table, patterns$query_patterns)
    
    results_unique <- compute_results(query_data$query_unique, traits_to_predict, 
                                      patterns$unique_traits, patterns$trait_patterns, 
                                      matching_organisms, ignore_NA, 
                                      session = session, ns = ns)
    
    cat(file = stderr(), paste0("Ended prediction at ", Sys.time(), "\n"))
    
    results <- restore_duplicates(results_unique, query_data$query_map) 

    # Hide modal with progress bar
    hide_modal_with_progress(session, ns("pb"), delay_time = 1000)

    return(results)
  },
  label="predict_traits")

  # --- Update selections ---
  # Update choices for traits
  shiny::observeEvent({tab_selected_trigger()},
  {
    choices <- choices_traits_taxonomy
    selected <-  c("Type of metabolism", "Substrates for end products", "End products")
    update_select_input(session = session,  inputId = "set_traits", choices = choices, selected = selected)
  },
  label="update_set_traits")
  
  # Update choices for system for taxonomy
  shiny::observeEvent({tab_selected_trigger()}, 
  {
      choices <- choices_system_taxonomy
      update_select_input(session = session,  inputId = "system_taxonomy", choices = choices)
  },
  label="update_system_taxonomy")
  
  # Update choices for taxa
  shiny::observeEvent({input$system_taxonomy}, 
  {
        # Get data
        database <- load_database()

        # Get inputs
        system_taxonomy <- input$system_taxonomy
        system_taxonomy <- assign_if_invalid(system_taxonomy, "LPSN")
        
        # Get choices
        col_name <- paste0(system_taxonomy, " Taxonomy")
        choices <- expand_and_merge_taxonomy(data = database, col_name = col_name)
        choices <- get_taxon_choices(choices)
        
        selected <- "Escherichia (Genus)"
        
        update_select_input(session = session,  inputId = "taxonomy_database", 
                            choices = choices, selected = selected)
  },
  ignoreInit = TRUE, label="update_choices_taxa")

  # Update choices for trait to display
  shiny::observeEvent({make_predictions_trigger()},
  {
    choices = get_traits_to_predict()
    update_picker_input(session = session,  inputId = "trait_to_display", choices = choices)
  },
  label="update_traits_to_display")
  
  # Update query builder
  shiny::observeEvent({tab_selected_trigger()},
  {
    update_query_builder("query_builder", choices_traits_taxonomy)
  },
  ignoreInit = TRUE, label="update_query_builder")
  
  # --- Generate outputs ---
  # Output modal with example data
  output_download_modal(
    input_id = "taxonomy_file_modal",
    object_ids = c(
      "taxa_uncharacterized",
      "taxa_rumen_cultured",
      "taxa_rumen_MAGs",
      "taxa_infant"
    ),
    labels = c(
      "Previously uncharacterized bacteria",
      "Cultured prokaryotes from rumen",
      "MAGs from rumen",
      "OTUs from infant gut"
    ),
    ns = ns,
    label = "show_data_modal"
  )

  # Create observer to navigate user to Help
  navigate_to_help(session = x, selected_tab = "help", selected_panel = "Predict traits from taxonomy")

  # Create output flags
  flag_if_multiple(output, "flag_multiple_traits", trigger = make_predictions_trigger, value_fun = get_traits_to_predict)
  flag_if_not_null(output, "flag_results", trigger = make_predictions_trigger, value_fun = predict_traits)
  
  # Output summary text
  output$summary_text <- shiny::renderText({
    df <- predict_traits()
    threshold <- input$threshold
    
    counts <- count_predictions(df, 
                organism_col = "Organism number", 
                trait_col = "Trait category", 
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
  
  # Output downloadable csv with matching results
  output$download_data <- create_download_handler(
    filename_prefix = "results",
    data_source = function() predict_traits()
  )

  # Output overview plots
  shiny::observe({
    # Get data
    df <- predict_traits()
    threshold <- input$threshold
    trait_to_display = input$trait_to_display

    # Format organism names
    df <- format_organism_names(df, cols = c("Phylum", "Class", "Order", "Family", "Genus", "Species"))
    
    # Summary plot
    output$summary_plot <- plotly::renderPlotly({
      df <- results_to_plot(df = df, plot_type="summary",
                            x_col = "Organism name", y_col = "Trait name", z_col = "Probability", 
                            var_col = "Trait category", var_to_keep = trait_to_display, 
                            z_threshold = threshold, drop_extra_y = TRUE, z_percentage = TRUE) 
      hovertemplate = paste0("<b>",trait_to_display,": %{x}</b><br><b>% organisms positive: %{z:.2f}</b><br><extra></extra>")
      plot = plot_summary(df,
                          coord_fixed = TRUE,
                          hovertemplate = hovertemplate,
                          legend_labels = c("0", "25", "50", "75", "100"),
                          legend_title = "% organisms positive")
      plot
    })
    
    # Treemap plot
    output$treemap_plot <- plotly::renderPlotly({
      df <- results_to_plot(df = df, plot_type="treemap",
                            x_col = "Organism name", y_col = "Trait name", z_col = "Probability", 
                            var_col = "Trait category", var_to_keep = trait_to_display, 
                            z_threshold = threshold, drop_extra_y = TRUE, z_percentage = TRUE) 
      dims <- calculate_treemap_dimensions(session, ns, "treemap_plot")
      hovertemplate = paste0("<b>",trait_to_display,": %{label}</b><br><b>% total: %{value:.2f}</b><br><extra></extra>")
      plot = plot_treemap(df,
                          hovertemplate = hovertemplate,
                          width = dims$width, height = dims$height)
      plot
    })
    
    # Heatmap plot
    output$heatmap_plot <- plotly::renderPlotly({
      df <- results_to_plot(df = df, plot_type="heatmap",
                            x_col = "Organism name", y_col = "Trait name", z_col = "Probability", 
                            var_col = "Trait category", var_to_keep = trait_to_display, 
                            z_threshold = threshold, drop_extra_y = TRUE, z_percentage = TRUE) 
      scale_ratio <- calculate_heatmap_scale(session, ns, "heatmap_plot", df)
      hovertemplate <- "<b>Trait: %{x}</b><br><b>Organism: %{y}</b><br><b>% probability: %{z:.2f}</b><br><extra></extra>"
      plot <- plot_heatmap(df,
                          hovertemplate = hovertemplate,
                          data_are_binary = TRUE,
                          legend_labels = c("0", "25", "50", "75", "100"), 
                          legend_title = "% probability",
                          x_to_y_ratio = scale_ratio)
      plot
    })
  })
}