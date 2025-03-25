# Define the Predictions with Metabolic Networks Module in Shiny App
# This script defines the user interface (UI) and server for the predictions with metabolic networks module.
# Author: Timothy Hackmann
# Date: 9 Mar 2025

# === Define user interface (UI) ===
  predictionsNetworkUI <- function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
      #Call JavaScript functions
      inject_js_resize(ns, "treemap-container"),
      
      #Title
      create_title_div("Predict traits with metabolic networks"),
      
      #Content
      bslib::layout_sidebar(
        #Sidebar
        sidebar = bslib::sidebar(
              width = "30%",
              
              # Select data
              div("Choose gene functions"),
              bslib::navset_tab(id = ns("function_tabs"),
                                bslib::nav_panel(title = "Database",
                                                 create_selectize_input(inputId = ns("gene_functions_database")),
                                ),
                                bslib::nav_panel(title = "File upload",
                                                 fileInput_modal(ns("gene_functions_upload"), modalId = ns("gene_functions_modal"))
                                )
              ),
              div("Choose reference reactions"),
              bslib::navset_tab(id = ns("reaction_tabs"),
                                bslib::nav_panel(title = "Database",
                                                 create_selectize_input(inputId = ns("reference_reactions_database"), multiple = FALSE),
                                 ),
                                bslib::nav_panel(title = "File upload",
                                                 fileInput_modal(ns("reference_reactions_upload"), modalId = ns("reference_reactions_modal"))
                                 )
                ),
              
              # Set parameters
              shiny::conditionalPanel(
                condition = "input.reaction_tabs == 'Database'",
                ns = ns, 
                create_selectize_input(inputId = ns("substrates_database"), label = "Substrates"), 
                create_selectize_input(inputId = ns("products_database"), label = "End products"),
              ),
              shiny::conditionalPanel(
                condition = "input.reaction_tabs == 'File upload'",
                ns = ns, 
                create_selectize_input(inputId = ns("substrates_upload"), label = "Substrates"), 
                create_selectize_input(inputId = ns("products_upload"), label = "End products"),
              ),
              
              # Advanced inputs
              shiny::checkboxInput(ns("show_advanced"), "Show advanced settings", value = FALSE),
              shiny::conditionalPanel(
                condition = "input.show_advanced & input.reaction_tabs == 'Database'",
                ns = ns,
                create_selectize_input(inputId = ns("unbalanced_intermediates_database"), label = "Unbalanced intermediates"),
              ),
              shiny::conditionalPanel(
                condition = "input.show_advanced & input.reaction_tabs == 'File upload'",
                ns = ns,
                create_selectize_input(inputId = ns("unbalanced_intermediates_upload")),
              ),
              shiny::conditionalPanel(
                condition = "input.show_advanced",
                ns = ns,
                shiny::sliderInput(ns("threshold"), "Flux threshold", min = 0, max = 1000, value = 1),
              ),
              
              # Make predictions
              shiny::actionButton(ns("make_predictions"), "Make predictions", class = "btn btn-primary")
          ),
        
        #Main content area
        div(
           id = ns("results_page"),
           
           # Message for missing selections
           shiny::conditionalPanel(
             condition = "!output.flag_results",
             ns = ns,
             shiny::h4("Please make selections at left")
           ),
           
           # Results panel
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
                  
                # Plot panels
                  create_plot_panel(ns, "summary", "Summary"),
                  create_plot_panel(ns, "treemap", "Treemap", centered = TRUE),
                  create_plot_panel(ns, "heatmap", "Heatmap"),
                  create_plot_panel(ns, "network", "Metabolic network"),
                
                # Plot options
                  div(
                    class = "flex-container",
                    shiny::conditionalPanel(
                      condition = "input.results_tabs == 'Metabolic network' && output.flag_multiple_organisms",
                      ns = ns, 
                      div(
                        class = "flex-item",
                        create_picker_input(inputId = ns("organism_to_display"), label = "Organism")
                      )
                    ),
                    shiny::conditionalPanel(
                      condition = "output.flag_multiple_substrates",
                      ns = ns, 
                      div(
                        class = "flex-item",
                        create_picker_input(inputId = ns("substrate_to_display"), label = "Substrate")
                      )
                    ),
                    shiny::conditionalPanel(
                      condition = "input.results_tabs == 'Metabolic network' && output.flag_multiple_products",
                      ns = ns, 
                      div(
                        class = "flex-item",
                        create_picker_input(inputId = ns("product_to_display"), label = "End product")
                      )
                    ),
                    shiny::conditionalPanel(
                      condition = "input.results_tabs == 'Metabolic network'",
                      ns = ns, 
                      div(
                        class = "flex-item",
                        create_picker_input(inputId = ns("set_network_layout"), label = "Layout")
                      )
                    ),
                    shiny::conditionalPanel(
                      condition = "input.results_tabs == 'Metabolic network'",
                      ns = ns, 
                      div(
                        class = "flex-item",
                        create_picker_input(inputId = ns("set_network_dimensions"), label = "Dimensions", choices = c("2", "3"), selected = "3")
                      )
                    )
                  ),
                  div(
                    class = "flex-container",
                    shiny::conditionalPanel(
                      condition = "input.results_tabs == 'Metabolic network'",
                      ns = ns, 
                      create_download_button(ns('download_network_model'), label = "Download network model")
                  )
              )
            )
          )
        )
      )
    )
  }
  
  # === Define server ===
  predictionsNetworkServer <- function(input, output, session, x, selected_tab) {
    #Set namespace
    ns <- session$ns
    
    # --- Define triggers for reactive expressions ---
    tab_selected_trigger <- make_tab_trigger(selected_tab, "predictionsNetwork")
    
    make_predictions_trigger <- make_action_button_trigger("make_predictions")
    
    get_graph_trigger <- make_other_trigger(
      make_predictions_trigger(),
      input$substrate_to_display, input$product_to_display,
      input$set_network_layout, input$set_network_dimensions,
      input$organism_to_display
    )
    
    get_layout_trigger <- make_other_trigger(
      make_predictions_trigger(),
      input$set_network_layout, input$set_network_dimensions,
      input$organism_to_display
    )
    
    updated_reference_reactions_trigger <- make_other_trigger(
      input$reference_reactions_database,
      input$reference_reactions_upload
    )
    
    updated_products_trigger <- make_other_trigger(
      input$products_database,
      input$products_upload
    )
    
    get_reference_reactions_trigger <- make_other_trigger(
      input$make_predictions,
      updated_reference_reactions_trigger()
    )
    
    # --- Get user input (events) ---
    #Get gene functions
    get_gene_functions <- shiny::eventReactive({make_predictions_trigger()},
    {
      # Get data
      database <- load_database()
      
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
    
    #Get reference reactions
    get_reference_reactions <- shiny::eventReactive({get_reference_reactions_trigger()},
    {
      if (input$reaction_tabs == "Database") {
        # Get data
        reference_reactions_glucose_fermentation <- load_reference_reactions_glucose_fermentation()
        reference_reactions_fructose_fermentation <- load_reference_reactions_fructose_fermentation()
        reference_reactions_methanogenesis <- load_reference_reactions_methanogenesis()
        
        # Get reactions
        reference_reactions <- switch(
          input$reference_reactions_database,
          "Glucose fermentation" = reference_reactions_glucose_fermentation,
          "Fructose fermentation" = reference_reactions_fructose_fermentation,
          "Methanogenesis" = reference_reactions_methanogenesis,
          NULL
        )
      } else if (input$reaction_tabs == "File upload") {
        # Get data
        reference_reactions = validate_and_read_file(session = session, file_path = input$reference_reactions_upload$datapath)

        # Get reactions
        reference_reactions <- validate_reference_reactions(reference_reactions)
        
        runValidationModal(session = session, need(reference_reactions != "", "Please check the format of the reference reactions file and try again."))
      }

      return(reference_reactions)
    }, 
    ignoreInit = TRUE, label="get_reference_reactions")  # Must have ignoreInit = TRUE, or module runs on app start up
    
    #Get substrate
    get_input_substrates <- shiny::eventReactive({make_predictions_trigger()},
    {
      substrates <- switch(
        input$reaction_tabs,
        "Database" = input$substrates_database,
        "File upload" = input$substrates_upload,
        NULL
      )
      
      runValidationModal(session = session, need(substrates != "", "Please choose at least one substrate"))
      
      return(substrates)  
    }, 
    label="get_input_substrates")
    
    #Get products
    get_input_products <- shiny::eventReactive({make_predictions_trigger()},
    {
      products <- switch(
        input$reaction_tabs,
        "Database" = input$products_database,
        "File upload" = input$products_upload,
        NULL
      )
        
      runValidationModal(session = session, need(products != "", "Please choose at least one product"))
      
      return(products)  
    }, 
    label="get_input_products")
    
    #Get unbalanced intermediates
    get_unbalanced_intermediates <- shiny::eventReactive({make_predictions_trigger()},
    {
      unbalanced_intermediates <- switch(
        input$reaction_tabs,
        "Database" = input$unbalanced_intermediates_database,
        "File upload" = input$unbalanced_intermediates_upload,
        NULL
      )
      
      return(unbalanced_intermediates)  
    }, 
    label="get_unbalanced_intermediates")
    
    # --- Process input ---
    # Build reference network model
    build_reference_network_model <- shiny::eventReactive({make_predictions_trigger()},
    {
      reference_reactions = get_reference_reactions()
      
      abbreviation = reference_reactions$abbreviation
      equation = reference_reactions$equation
      direction = reference_reactions$direction
      officialName = reference_reactions$officialName
      geneAssociation = reference_reactions$geneAssociation
      subsystem = reference_reactions$subsystem
      unbalanced_intermediates = get_unbalanced_intermediates()
      unbalanced_products = get_input_products()[get_input_products() %nin% get_unbalanced_intermediates()]
      
      # Build metabolic model
      reference_network_model = build_network_model(equation = equation, direction = direction, abbreviation = abbreviation, officialName = officialName, geneAssociation = geneAssociation, subsystem = subsystem, starting_metabolite = NULL, ending_metabolite = NULL, unbalanced_intermediates = unbalanced_intermediates, unbalanced_products = unbalanced_products, remove_redundant_reactions = FALSE)
      
      return(reference_network_model)
    }, 
    label="build_reference_network_model")
    
    # Build and solve organism-specific model
    get_solved_model <- shiny::eventReactive({make_predictions_trigger()},
    {
      # Get inputs
      reference_network_model <- build_reference_network_model()
      gene_functions <- get_gene_functions()
      products <- get_input_products()
      substrates <- get_input_substrates() 

      # Set variables
      if (length(products) == 0) {
        products <- "None"
      }
      
      # Initialize values
      s <- lapply(1:ncol(gene_functions), function(x) {
        vector("list", length(substrates))
      })
      
      #Launch modal with progress bar
      display_modal(session, ns("pb"), message = "Loading gene functions")
      
      # Print status to log
      cat(file = stderr(), paste0("Started prediction at ", Sys.time(), "\n"))
      
      for (i in seq_len(ncol(gene_functions))) {
        # Filter database IDs
        filtered_gene_functions <- gene_functions[, i][!is.na(gene_functions[, i]) & gene_functions[, i] != ""]
        # Solve the network model
        s[[i]] <- solve_network_model(df = reference_network_model, gene_functions = filtered_gene_functions, substrates = substrates, products = products)
        
        # Update modal with progress bar
        display_modal(session, ns("pb"), message = "Prediction in progress", value = 1 / ncol(gene_functions) * 100 * i)
      }
      
      # Hide the modal with progress bar
      hide_modal_with_progress(session, ns("pb"), delay_time = 1000)
      
      # Print status to log
      cat(file = stderr(), paste0("Ended prediction at ", Sys.time(), "\n"))
      
      return(s)
    }, 
    label="get_solved_model")
    
    # Get fluxes from network models
    predict_fluxes <- shiny::eventReactive({make_predictions_trigger()},
    {
      # Get inputs
      s = get_solved_model()
      substrates = get_input_substrates()
      products = get_input_products()
      organism = colnames(get_gene_functions())

      # Set variables
      if (length(products) > 0) {
        products = products
      } else {
        products = "None"
      }
      ending_metabolite = products
      
      # Get fluxes
      flux <- data.frame(matrix(NA, nrow = length(organism) * length(substrates) * length(ending_metabolite), ncol = 5))
      colnames(flux) <- c("Organism number", "Organism name", "Substrate", "End product", "Flux")
      row_counter <- 1

      for (i in seq_along(organism)) {
        for (j in seq_along(substrates)) {
          substrate <- substrates[j]
          
          for (k in seq_along(ending_metabolite)) {
            product <- ending_metabolite[k]
            match_index <- which(s[[i]][[j]][[k]]$abbreviation == "Ending_metabolite")
            flux_value <- if (length(match_index) > 0) s[[i]][[j]][[k]]$flux[match_index] else NA
            
            # Fill the data frame
            flux[row_counter, ] <- list(i, organism[i], substrate, product, flux_value)
            
            # Increment the row counter
            row_counter <- row_counter + 1
          }
        }
      }
      
      return(flux)
      
    }, 
    label="predict_fluxes")

    # Make network graph
    get_network_graph <- shiny::eventReactive({get_graph_trigger()},
    {
      # Get unbalanced intermediates
      unbalanced_intermediates = get_unbalanced_intermediates()
      threshold <- input$threshold
      
      # Get index of selected organism
      if (!is.null(input$organism_to_display)) {
        i = which(colnames(get_gene_functions()) == input$organism_to_display)
      } else {
        i = 1
      }

      # Get index of selected substrates
      j = which(get_input_substrates() == input$substrate_to_display)
      
      # Get index of selected products
      k = which(get_input_products() == input$product_to_display)
      
      # Get model
      s = get_solved_model()[[i]][[j]][[k]]
      
      # Change fluxes to 0 if product has flux less than threshold
      if (s$flux[which(s$abbreviation == "Ending_metabolite")] < threshold) {
        s$flux = 0
      }
      
      # Make graph
      g = make_network_graph(s = s, to_remove = unbalanced_intermediates)
      
      return(g)
    }, 
    label="get_network_graph")
  
    # Set layout for graph
    get_network_layout <- shiny::eventReactive(get_layout_trigger(),
    {
      g <- get_network_graph()

      layout <- set_network_layout(graph = g, type = input$set_network_layout, dimensions = input$set_network_dimensions)
      
      return(layout)
    },
    label="get_network_layout")

    # --- Update selections ---
    # Update choices for gene functions (organisms)
    shiny::observeEvent({list(tab_selected_trigger(), input$reference_reactions_database, input$reaction_tabs)}, 
    {
      database <- load_database()
      
      if (input$reaction_tabs == "Database") {
        choices <- switch(
          input$reference_reactions_database,
          "Glucose fermentation" = get_organism_choices(database = database, metabolism_type = "Fermentation"),
          "Fructose fermentation" = get_organism_choices(database = database, metabolism_type = "Fermentation"),
          "Methanogenesis" = get_organism_choices(database = database, metabolism_type = "Methanogenesis"),
          NULL
        )
      } else if (input$reaction_tabs == "File upload") {
        choices <- get_organism_choices(database)
      }
      
      update_select_input(session = session,  inputId = "gene_functions_database", choices = choices)
    },
    label="update_gene_function_choices")
    
    # Update choices for substrates
    shiny::observeEvent({list(tab_selected_trigger(), updated_reference_reactions_trigger())},
    {
      choices <- get_metabolite_names(equation = get_reference_reactions()$equation)

      if (input$reaction_tabs == "Database") {
        inputId = "substrates_database"
        selected <- switch(input$reference_reactions_database,
                           "Glucose fermentation" = "D-Glucose",
                           "Fructose fermentation" = "D-Fructose",
                           "Methanogenesis" = c("CO2", "Formate"),
                           NULL)
      } else if (input$reaction_tabs == "File upload") {
        inputId = "substrates_upload"
        selected <- "D-Glucose"
      }
      
      update_select_input(session = session,  inputId = inputId, choices = choices, selected = selected)
    }, 
    label="update_substrate_choices")
    
    # Update choices for reference reactions
    shiny::observeEvent({tab_selected_trigger()},
    {
      choices <- c("Glucose fermentation", "Fructose fermentation", "Methanogenesis")
      update_select_input(session = session,  inputId = "reference_reactions_database", choices = choices)
      
    },
    label="update_reference_reactions_choices")
    
    # Update choices for products
    shiny::observeEvent({list(tab_selected_trigger(), updated_reference_reactions_trigger())},             
    {
      choices <- get_metabolite_names(equation = get_reference_reactions()$equation)
      
      if (input$reaction_tabs == "Database") {
        inputId <- "products_database"
        selected <- switch(input$reference_reactions_database,
                           "Glucose fermentation" = selected_products_fermentation,
                           "Fructose fermentation" = selected_products_fermentation,
                           "Methanogenesis" = selected_products_methanogenesis,
                           NULL)
      } else if (input$reaction_tabs == "File upload") {
        inputId <- "products_upload"
        selected <- selected_products_fermentation
      }
      
      update_select_input(session = session,  inputId = inputId, choices = choices, selected = selected)
    },
    label="update_product_choices")
    
    # Update choices for unbalanced intermediates
    observeEvent(list(tab_selected_trigger(), updated_reference_reactions_trigger(), updated_products_trigger()),
     {
       # Get choices
       choices <- get_metabolite_names(equation = get_reference_reactions()$equation)
       
       # Determine selected products
       selected_products <- switch(input$reaction_tabs,
                                   "Database" = input$products_database,
                                   "File upload" = input$products_upload,
                                   character(0)
       )
       
       # Determine inputId and selected intermediates
       if (input$reaction_tabs == "Database") {
         inputId <- "unbalanced_intermediates_database"

         selected <- switch(input$reference_reactions_database,
                            "Glucose fermentation" = selected_unbalanced_intermediates_fermentation[selected_unbalanced_intermediates_fermentation %nin% selected_products],
                            "Fructose fermentation" = selected_unbalanced_intermediates_fermentation[selected_unbalanced_intermediates_fermentation %nin% selected_products],
                            "Methanogenesis" = selected_unbalanced_intermediates_methanogenesis,
                            NULL)
         
       } else if (input$reaction_tabs == "File upload") {
         inputId <- "unbalanced_intermediates_upload"
         
         selected <- selected_unbalanced_intermediates_fermentation[
           selected_unbalanced_intermediates_fermentation %in% choices &
             selected_unbalanced_intermediates_fermentation %nin% selected_products
         ]
       }
       
       update_select_input(session = session, inputId = inputId, choices = choices, selected = selected)
     },
     label = "update_unbalanced_intermediate_choices")
    
    # Update choices for substrates to display
    shiny::observeEvent({make_predictions_trigger()},
    {
      choices <- get_input_substrates()
      update_picker_input(session = session,  inputId = "substrate_to_display", choices = choices)
    },
    label="update_substrate_display")
    
    # Update choices for products to display
    shiny::observeEvent({make_predictions_trigger()},
    {
      choices <- get_input_products()
      update_picker_input(session = session,  inputId = "product_to_display", choices = choices)
    },
    label="update_product_display")
    
    # Update choices for organisms to display
    shiny::observeEvent({make_predictions_trigger()},
    {
      choices <- colnames(get_gene_functions())
      update_picker_input(session = session,  inputId = "organism_to_display", choices = choices)
    },
    label="update_organism_display")
    
    # Update choices for network layout
    shiny::observeEvent({input$set_network_dimensions},
    {
      choices <- switch(input$set_network_dimensions,
                        "3" = c("FR", "KK", "DH", "GEM", "DRL", "MDS"),
                        "2" = c("FR", "KK", "DH", "GEM", "DRL", "MDS", "Graphopt"),
                        NULL)
      update_picker_input(session = session,  inputId = "set_network_layout", choices = choices)
    },
    label="update_layout_display")

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
    input_id = "reference_reactions_modal",
    object_ids = c(
      "reference_reactions_glucose_fermentation",
      "reference_reactions_fructose_fermentation",
      "reference_reactions_methanogenesis"
    ),
    labels = c(
      "Glucose fermentation",
      "Fructose fermentation",
      "Methanogenesis"
    ),
    ns = ns,
    label = "show_reference_reactions_modal"
  )

  # Create observer to direct user to Help
  navigate_to_help(session = x, selected_tab = "help", selected_panel = "Predict traits with metabolic networks")
  
  # Create output flags
  flag_if_multiple(output, "flag_multiple_organisms", trigger = make_predictions_trigger, value_fun = get_gene_functions)
  flag_if_multiple(output, "flag_multiple_substrates", trigger = make_predictions_trigger, value_fun = get_input_substrates)
  flag_if_multiple(output, "flag_multiple_products", trigger = make_predictions_trigger, value_fun = get_input_products)
  flag_if_not_null(output, "flag_results", trigger = make_predictions_trigger, value_fun = predict_fluxes)
  
  # Output summary text
  output$summary_text <- shiny::renderText({
    df <- predict_fluxes()
    threshold <- input$threshold
    
    counts <- count_predictions(df, 
                organism_col = "Organism number", 
                trait_col = "End product", 
                value_col = "Flux", 
                threshold = threshold)
    
    format_summary_text(
      count1 = counts$traits_predictions, 
      count2 = counts$organisms_predictions, 
      label1 = "end products", 
      label2 = "organisms", 
      total2 = counts$organisms_total
    )
  })
  
  # Output downloadable csv of fluxes
  output$download_data <- create_download_handler(
    filename_prefix = "fluxes",
    data_source = function() predict_fluxes()
  )
  
  # Output overview plots
  shiny::observe({
    df = predict_fluxes()
    substrate_to_display = input$substrate_to_display
    threshold <- input$threshold
    
    df <- format_organism_names(df, cols = "Organism name", abbreviate_names = FALSE)
    
    #Summary plot
    output$summary_plot <- plotly::renderPlotly({
      df <- results_to_plot(df = df, plot_type="summary",
                            x_col = "Organism name", y_col = "End product", z_col = "Flux", 
                            var_col = "Substrate", var_to_keep = substrate_to_display, 
                            z_threshold = threshold, drop_extra_y = FALSE, z_percentage = TRUE) 
      plot = plot_summary(df, 
                          coord_fixed = TRUE, 
                          hovertemplate = "<b>Endproduct: %{x}</b><br><b>% organisms positive: %{z:.2f}</b><br><extra></extra>",
                          legend_labels = c("0", "25", "50", "75", "100"), 
                          legend_title = "% organisms positive")
      plot
    })
    
    # Treemap plot
    output$treemap_plot <- plotly::renderPlotly({
      df <- results_to_plot(df = df, plot_type="treemap",
                            x_col = "Organism name", y_col = "End product", z_col = "Flux", 
                            var_col = "Substrate", var_to_keep = substrate_to_display, 
                            z_threshold = threshold, drop_extra_y = TRUE, z_percentage = TRUE) 
      dims <- calculate_treemap_dimensions(session, ns, "treemap_plot")
      hovertemplate <- "<b>Endproduct: %{label}</b><br><b>% total: %{value:.2f}</b><br><extra></extra>"
      plot = plot_treemap(df,
                           hovertemplate = hovertemplate,
                           width = dims$width, height = dims$height)
      plot
    })
    
    # Heatmap plot
    output$heatmap_plot <- plotly::renderPlotly({
      df <- results_to_plot(df = df, plot_type="heatmap",
                            x_col = "Organism name", y_col = "End product", z_col = "Flux", 
                            var_col = "Substrate", var_to_keep = substrate_to_display, 
                            z_threshold = threshold, drop_extra_y = FALSE, z_percentage = FALSE) 
      scale_ratio <- calculate_heatmap_scale(session, ns, "heatmap_plot", df)
      hovertemplate <- "<b>Endproduct: %{x}</b><br><b>Organism: %{y}</b><br><b>Flux: %{z:.0f}</b><br><extra></extra>"
      plot = plot_heatmap(df, 
                          coord_fixed = TRUE,
                          hovertemplate = hovertemplate,
                          legend_labels = c("0", "250", "500", "750", "1000"), 
                          legend_title = "Flux", 
                          zmax = 1000,
                          x_to_y_ratio = scale_ratio)
      plot
    })
  })
  
  # Output network graph
  output$network_plot <- plotly::renderPlotly(exp = {
    g = get_network_graph()
    layout = get_network_layout()
    
    vertices_to_highlight = c(format_metabolite_name(input$substrate_to_display), format_metabolite_name(input$product_to_display))
    
    g = format_network_graph(graph = g, show_flux = TRUE, show_subsystems = TRUE, vertices_to_highlight = vertices_to_highlight)

    plot = plot_network(graph = g, layout = layout, network_legend_key = network_legend_key, spread = 0.05)

    plot
  })
  
  # Output downloadable csv of results
  output$download_network_model <- create_download_handler(
    filename_prefix = "model",
    data_source = function() {
      #Get index of selected organism
      if (!is.null(input$organism_to_display)) {
        i = which(colnames(get_gene_functions()) == input$organism_to_display)
      } else {
        i = 1
      }
      
      #Get index of selected substrates
      j = which(get_input_substrates() == input$substrate_to_display)
      
      #Get index of selected products
      k = which(get_input_products() == input$product_to_display)
      
      #Get network model
      s = get_solved_model()[[i]][[j]][[k]]
      s
    },
  )
}