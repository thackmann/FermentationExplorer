# Define the Search Database Module in Shiny App
# This script defines the user interface (UI) and server for the search database module.
# Author: Timothy Hackmann
# Date: 23 Mar 25

# === Define user interface (UI) ===
  # Search database tab
  databaseSearchUI <- function(id) {
    ns <- NS(id)
    shiny::tagList(
        #Call JavaScript functions
        inject_js_resize(ns, "treemap-container"),

        #Call JavaScript functions
        inject_js_resize(ns, "treemap-container"),
        inject_query_builder_js(ns, "query_builder"),
        
        # --- Loading screen ---
        create_loading_screen("search-loading-screen"), 
        
        # --- Main UI (initially hidden ) ---
        shinyjs::hidden(
          div(id = "search-wrapper",
  
          #Title
          create_title_div("Search database"),
          
          bslib::layout_sidebar(
            #Sidebar
            sidebar = bslib::sidebar(
              id = ns("sidebar"),
              width = "30%",
              
              create_query_builder(ns = ns, input_id = "query_builder", label = "Build query"),
              
              shiny::actionButton(ns("make_predictions"), "Perform search", class = "btn btn-primary")
            ),
            #Main content area
            shiny::div(
              id = ns("results_page"),
    
              shiny::conditionalPanel(
                condition = "!output.flag_results",
                ns = ns,
                h4("Please make selections at left")
              ),
              
              shiny::conditionalPanel(
                condition = "output.flag_results",
                ns = ns,
                
                # Summary and download button
                bslib::card(
                  bslib::card_header(textOutput(ns("summary_text"))), 
                  create_download_button(ns('download_data'))
                ),
                
                # Tabs for plots
                bslib::navset_card_underline(
                  id = ns("results_tabs"),
                  title = "Plots",
    
                  # Panels
                  create_plot_panel(ns, "treemap", "Treemap", centered = TRUE),
                  create_plot_panel(ns, "tree", "Tree"),
                  create_plot_panel(ns, "tsne", "t-SNE"),
                  
                  # Plot options
                  shiny::conditionalPanel(
                    condition = "input.results_tabs == 'Treemap'",
                    ns = ns, 
                    create_picker_input(inputId = ns("variable_to_display"), label = "Variable")
                  ),
                  shiny::conditionalPanel(
                    condition = "input.results_tabs == 'Tree'",
                    ns = ns, 
                    create_picker_input(inputId = ns("set_tree_layout"), label = "Layout")
                  ),
                  shiny::conditionalPanel(
                    condition = "input.results_tabs == 'Tree' | input.results_tabs == `t-SNE`",
                    ns = ns, 
                    div(
                      "Matching organisms are those that are fully colored. Only organisms with genome sequences are shown."
                    )
                  )
                ),
    
                # Detailed results
                bslib::card(
                  bslib::card_header("Detailed results"),  
                  full_screen = TRUE,
                  create_data_table(inputId = ns("table")),
                  bslib::layout_column_wrap(
                    width = 1/4, 
                    shiny::checkboxGroupInput(inputId = ns("checkboxes_info_organism"), label = "Organism"),
                    shiny::checkboxGroupInput(inputId = ns("checkboxes_info_databases"), label = "Databases"),
                    shiny::checkboxGroupInput(inputId = ns("checkboxes_info_metabolism"), label = "Metabolism"),
                    shiny::checkboxGroupInput(inputId = ns("checkboxes_info_traits"), label = "Metabolism")
                  )
                )
              )
            )
        )
      )
      )
    )
  }

# === Define server ===
  databaseSearchServer <- function(input, output, session, x, selected_tab) {
    # Set namespace
    ns <- session$ns
    
    # --- Define triggers for reactive expressions ---
    tab_selected_trigger <- make_tab_trigger(selected_tab, "databaseSearch")
    
    make_predictions_trigger <- make_action_button_trigger("make_predictions")

    url_change_trigger <- make_url_trigger(param_name = "job")
    
    build_table_trigger <- make_other_trigger(
      url_change_trigger(),
      input$checkboxes_info_organism,
      input$checkboxes_info_databases,
      input$checkboxes_info_metabolism,
      input$checkboxes_info_traits
    )
    
    get_tree_trigger <- make_other_trigger(url_change_trigger(), 
                                           input$set_tree_layout)
    
    # --- Get user input (events) ---
    # Get query
    get_query_string <- shiny::eventReactive({make_predictions_trigger()},
    {
      # Get input
      query_string  = input$query_builder

      # Process query string for more precise matching
      query_string <- process_query_string(query_string)

      runValidationModal(need(query_string != "", "Please build a valid query."))

      return(query_string)
    },
    label = "get_query_string")
    
    # --- Process input ---
    # Create job for computation
    create_job <- shiny::eventReactive(make_predictions_trigger(), {
      # Create job ID
      job_id <- create_job_id()
      
      # Update URL with the  ID
      url <- create_job_url(job_id = job_id, tab = "databaseSearch")
      shiny::updateQueryString(sub(".*\\?", "?", url), mode = "push")
      
      # Launch modal   
      display_modal(id = ns("pb"), message = "Creating job for computation", value = 0, url = url)
      
      cat("Job created:", job_id)
      
      return(job_id)
      
    }, label = "create_job")
    
    # Add taxonomy of organisms to data
    add_taxonomy = shiny::eventReactive({make_predictions_trigger()},
    {
     # Get data and variables
     data = load_database()
     
     col_name <- "LPSN Taxonomy"
     
     # Add taxonomy to data
     data <- expand_and_merge_taxonomy(data = data, col_name = col_name)
     
     return(data)
    },
    label="add_taxonomy")
     
    # Filter data according to query
    filter_data = shiny::eventReactive({make_predictions_trigger()},
    {
      #Get data
      data = add_taxonomy()
      query_string = get_query_string()
      
      # Print status to log
      cat(file = stderr(), paste0("Started search at ", Sys.time(), "\n"))

      # Launch modal with progress bar
      display_modal(id = ns("pb"), message = "Performing search")

      # Perform filtering
      data =  filter_data_by_query(data = data, query_string = query_string)

      return(data)
    },
    label="filter_data")

    # --- Save and get results ---
    # Save results
    shiny::observeEvent({make_predictions_trigger()},
    {
      job_id <- create_job()
      job_dir <- get_job_dir(tab = "databaseSearch")
      
      results <-
        list(
          filter_data = filter_data()
        )
     
      # Update modal with progress bar
      display_modal(id = ns("pb"), message = "Saving results", value = 100)
      
      # Save result
      save_job_result(job_id = job_id, result = results, job_dir = job_dir)
      
      # Hide modal with progress bar
      hide_modal_with_progress(id = ns("pb"), delay_time = 1000)
    },
    label="save_results")
    
    # Get results
    get_results <- eventReactive({url_change_trigger()}, {
      # Get job ID from URL
      job_id <- get_query_param()
      job_dir <- get_job_dir(tab = "databaseSearch")
      
      if (job_result_exists(job_id, job_dir)) {
        load_job_result(job_id, job_dir)
      } else {
        NULL
      }
    })

    # --- Process results ----
    # Build data table
    build_table <- shiny::eventReactive({build_table_trigger()}, {
      # Get data
      data <- get_results()$filter_data
      
      # Rename columns with links
      data <- data %>%
        rename_and_overwrite(" link$", "") # Remove " link" and overwrite
      
      # Dynamically get names of selected columns from checkboxes
      selected_columns <- lapply(names(choices_checkboxes_search), function(category) {
        selected_vals <- input[[paste0("checkboxes_info_", category)]]
        selected_vals[!is.na(selected_vals)]
      }) %>% unlist()
      
      # Keep only selected columns
      data <- data %>% dplyr::select(dplyr::all_of(selected_columns))

      # Print status to log
      cat(file = stderr(), paste0("Ended search at ", Sys.time(), "\n"))
      
      return(data)
    },
    label = "build_table")
    
    # Plot phylogenetic tree
    plot_tree <- shiny::eventReactive({get_tree_trigger()}, {
      layout_type <- input$set_tree_layout
      
      # Get layouts
      layout <- switch(layout_type,
                       "Daylight" = load_layout_tree_daylight(),
                       "Equal angle" = load_layout_tree_equal_angle(),
                       "Rectangular" = load_layout_tree_rectangular())
      
      branches_all <- switch(layout_type,
                             "Daylight" = load_plot_branches_all_daylight(),
                             "Equal angle" = load_plot_branches_all_equal_angle(),
                             "Rectangular" = load_plot_branches_all_rectangular())
      
      tips_all <- switch(layout_type,
                         "Daylight" = load_plot_tips_all_daylight(),
                         "Equal angle" = load_plot_tips_all_equal_angle(),
                         "Rectangular" = load_plot_tips_all_rectangular())
      
      # Get matching data and filter layout
      data <- get_results()$filter_data
      nodes_to_root <- load_nodes_to_root()
      layout_filtered <- filter_tree_layout(layout, data, nodes_to_root, id_column = "IMG Genome ID max genes")
      
      # Parameters for matching plots
      layout_param <- get_layout_param(layout_type)
      coord_fixed <- layout_param$coord_fixed
      x_to_y_ratio <- layout_param$x_to_y_ratio
      
      # Plot matching branches
      branches_matching <- ggtree_to_plotly(
        layout = layout_filtered,
        type = "daylight",  # assumed to match your actual layout logic
        coord_fixed = coord_fixed,
        x_to_y_ratio = x_to_y_ratio,
        color = green_color,
        linewidth = 1
      )
      
      # Format tips layout
      tips_matching_layout <- layout %>%
        dplyr::filter(isTip == TRUE) %>%
        add_taxonomy_to_layout(layout_ID = "label", taxonomy = data, taxonomy_ID = "IMG Genome ID max genes") %>%
        add_fill_to_layout(group = "Phylum", lighten_amount = 0.2) %>%
        add_color_to_layout(group = "Phylum", lighten_amount = 0)
      
      # Plot matching tips
      tips_matching <- plot_scatterplot(
        df = tips_matching_layout,
        color = tips_matching_layout$color,
        fill = tips_matching_layout$fill,
        stroke = 1,
        size = 5,
        shape = "circle",
        alpha = 1,
        label = c("Phylum", "Class", "Order", "Family", "Genus", "Species"),
        coord_fixed = coord_fixed,
        x_to_y_ratio = x_to_y_ratio
      )
      
      # Combine plots
      combined_plot <- overlay_plots(branches_all, tips_all, branches_matching, tips_matching)
      return(combined_plot)
    }, label = "plot_tree")

    # Plot t-SNE scatterplot
    plot_tsne <- shiny::eventReactive({url_change_trigger()}, {
      # Load full plot
      plot_all <- load_plot_tsne_all()
      
      # Load layout and data for matching plot
      layout <- load_layout_tsne()
      data <- get_results()$filter_data
      
      # Format layout with taxonomy and colors
      layout <- add_taxonomy_to_layout(layout = layout, layout_ID = "IMG_Genome_ID_max_genes", taxonomy = data, taxonomy_ID = "IMG Genome ID max genes")
      layout <- add_fill_to_layout(layout = layout, group = "Phylum", lighten_amount = 0.2)
      layout <- add_color_to_layout(layout = layout, group = "Phylum", lighten_amount = 0)
      
      # Create matching plot
      plot_matching <- plot_scatterplot(
        df = layout,
        color = layout$color,
        fill = layout$fill,
        stroke = 1,
        size = 5,
        shape = "circle",
        alpha = 1,
        label = c("Phylum", "Class", "Order", "Family", "Genus", "Species"),
        ticklen.x = 4, ticklen.y = 4,
        showticklabels.x = TRUE, showticklabels.y = TRUE,
        title.x = "Dimension 1", title.y = "Dimension 2",
        coord_fixed = TRUE, x_to_y_ratio = 1
      )
      
      # Overlay plots
      plot_combined <- overlay_plots(plot_all, plot_matching)
      
      return(plot_combined)
    }, label = "plot_tsne")
    
    # --- Update user interface (UI) elements ---
    # Update query builder
    shiny::observeEvent({tab_selected_trigger()},
    {
      update_query_builder(inputId = "query_builder", choices = choices_traits_search)
      
      # Hide loading screen
      shinyjs::runjs("shinyjs.hide('search-loading-screen'); shinyjs.show('search-wrapper');")
    },
    label="update_query_builder")

    # Update variable to display
    shiny::observeEvent({tab_selected_trigger()},
    {
      choices <- choices_traits_search
      selected = "Phylum"
      update_picker_input(inputId = "variable_to_display", choices = choices, selected = selected)
    },
    label="update_variable_to_display")
    
    # Update tree layout
    shiny::observeEvent({tab_selected_trigger()},
    {
      choices <- c("Equal angle", "Daylight", "Rectangular")
      update_picker_input(inputId = "set_tree_layout", choices = choices)
    },
    label="update_set_tree_layout")

    # Update checkboxes
    shiny::observeEvent({tab_selected_trigger()},
    {
      choices = choices_checkboxes_search$organism$choices
      selected = intersect(c("Genus", "Species"),
                           choices_checkboxes_search$organism$choices)
      update_checkbox_group(inputId = "checkboxes_info_organism", choices = choices, selected = selected)

      choices = choices_checkboxes_search$databases$choices
      selected = intersect(c(
                            "LPSN Page", "Bergey Article",
                            "GTDB ID", "NCBI Taxonomy ID", "GOLD Organism ID",
                            "IMG Genome ID", "BacDive ID"
                            ),
                            choices_checkboxes_search$database$choices)
      update_checkbox_group(inputId = "checkboxes_info_databases", choices = choices, selected = selected)
      
      choices = choices_checkboxes_search$metabolism$choices
      update_checkbox_group(inputId = "checkboxes_info_metabolism", choices = choices)

      choices = choices_checkboxes_search$traits$choices
      update_checkbox_group(inputId = "checkboxes_info_traits", choices = choices)
    },
    label="update_checkboxes")
    
    # Toggle sidebar closed (when loading saved job)
    shiny::observeEvent(tab_selected_trigger(), 
    {
      if (isTRUE(session$userData$loaded_job_on_init)) {
        # Toggle side bar closed
        bslib::sidebar_toggle("sidebar")
        
        # Reset restore so it does not toggle side bar closed again
        session$userData$loaded_job_on_init <- FALSE
      }
    })
    
    # --- Generate outputs ---
    # Output number of matching organisms
    output$summary_text <- shiny::renderText(
      paste0("Query matched ", nrow(get_results()$filter_data), " organisms")
    )
       
    # Create output flags
    flag_if_not_null(output, "flag_results", trigger = url_change_trigger, 
         value_fun = function() get_results())
    
    # Output overview plots
      # Treemap plot
      output$treemap_plot <- plotly::renderPlotly({
        #Get data
        df = get_results()$filter_data
        
        #Format variable name
        var_name = input$variable_to_display
        var_name_display = var_name
        
        df = search_results_to_plot(df = df, plot_type="treemap", var_name = var_name)
        
        hovertemplate = paste0("<b>",var_name_display,": %{label}</b><br><b>% of matching organisms: %{value:.2f}</b><br><extra></extra>")
        dims <- calculate_treemap_dimensions(ns = ns, plot_id = "treemap_plot")
        plot = plot_treemap(df, 
                            hovertemplate = hovertemplate,
                            width = dims$width, height = dims$height)
      })
      
      # Tree plot
      output$tree_plot = plotly::renderPlotly({
        plot = plot_tree()
      })
      
      # t-SNE plot
      output$tsne_plot <- plotly::renderPlotly({
         plot = plot_tsne()
      })

    # Output table with matching organisms and columns
    output$table <- DT::renderDataTable({
      table = build_table()
    }, escape = FALSE, options = list(scrollX = TRUE))
    
    # Output downloadable csv with matching results
    output$download_data <- create_download_handler(
      filename_prefix = "results",
      data_source = function() {
        get_results()$filter_data %>%
        dplyr::select(-dplyr::ends_with("link"))
      }
    )
}