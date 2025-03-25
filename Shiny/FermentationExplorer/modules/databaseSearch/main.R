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
        inject_query_builder_js(ns, "query_builder"),

      #Title
      create_title_div("Search database"),
      
      bslib::layout_sidebar(
        #Sidebar
        sidebar = bslib::sidebar(
          width = "30%",
          create_query_builder(ns = ns, input_id = "query_builder", label = "Build query"),
          shiny::actionButton(ns("perform_search"), "Perform search", class = "btn btn-primary")
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
              # create_plot_panel(ns, "tsne", "t-SNE"),

              bslib::nav_panel(
                title = "t-SNE",
                create_plot_div(ns = ns, plot_type = "tsne"),
              ),
              
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
  }

# === Define server ===
  databaseSearchServer <- function(input, output, session, x, selected_tab) {
    # Set namespace
    ns <- session$ns
    
    # --- Define triggers for reactive expressions ---
    tab_selected_trigger <- make_tab_trigger(selected_tab, "databaseSearch")
    
    make_search_trigger <- make_action_button_trigger("perform_search")

    get_tree_trigger <- make_other_trigger(make_search_trigger(), input$set_tree_layout)
    
    build_table_trigger <- make_other_trigger(
      make_search_trigger(),
      input$checkboxes_info_organism,
      input$checkboxes_info_databases,
      input$checkboxes_info_metabolism,
      input$checkboxes_info_traits
    )
    
    # --- Get user input (events) ---
    # Get query
    get_query_string <- shiny::eventReactive({make_search_trigger()},
    {
      # Get input
      query_string  = input$query_builder

      # Process query string for more precise matching
      query_string <- process_query_string(query_string)

      runValidationModal(session = session, need(query_string != "", "Please build a valid query."))

      return(query_string)
    },
    label = "get_query_string")
    
    # --- Process input ---
    # Add taxonomy of organisms to data
    add_taxonomy = shiny::eventReactive({make_search_trigger()},
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
    filter_data = shiny::eventReactive({make_search_trigger()},
    {
      #Get data
      data = add_taxonomy()
      query_string = get_query_string()
      
      # Print status to log
      cat(file = stderr(), paste0("Started search at ", Sys.time(), "\n"))

      # Launch modal with progress bar
      display_modal(session, ns("pb"), message = "Performing search")

      # Perform filtering
      data =  filter_data_by_query(data = data, query_string = query_string)

      return(data)
    },
    label="filter_data")
    
    # Build data table
    build_table <- shiny::eventReactive({build_table_trigger()}, {
      # Get data
      data <- filter_data()
      
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
      
      # Hide the modal with progress bar
      hide_modal_with_progress(session, ns("pb"), delay_time = 1000)
      
      # Print status to log
      cat(file = stderr(), paste0("Ended search at ", Sys.time(), "\n"))
      
      return(data)
    },
    label = "build_table")
    
    # Get layout of phylogenetic tree
    get_tree_layout <- shiny::eventReactive({get_tree_trigger()},
    {
      #Get layout
      layout <- switch(input$set_tree_layout,
                       "Daylight" = load_layout_tree_daylight(),
                       "Equal angle" = load_layout_tree_equal_angle(),
                       "Rectangular" = load_layout_tree_rectangular()
      )

      return(layout)
    },
    label="get_tree_layout")
    
    # Plot tree branches for all organisms
    plot_branches_all = shiny::eventReactive({get_tree_trigger()},
    {
      plot <- switch(input$set_tree_layout,
                     "Daylight" = load_plot_branches_all_daylight(),
                     "Equal angle" = load_plot_branches_all_equal_angle(),
                     "Rectangular" = load_plot_branches_all_rectangular()
      )

      return(plot)
    },
    label="plot_branches_all")
    
    # Plot tree branches for matching organisms
    plot_branches_matching = shiny::eventReactive({get_tree_trigger()},
    {
      # Get data and layout
      layout = get_tree_layout()
      data = filter_data()
      nodes_to_root = load_nodes_to_root()

      # Filter tree layout
      layout <- filter_tree_layout(layout, data, nodes_to_root, id_column = "IMG Genome ID max genes")

      # Get layout parameters
      layout_param <- get_layout_param(input$set_tree_layout)
      coord_fixed <- layout_param$coord_fixed
      x_to_y_ratio <- layout_param$x_to_y_ratio
      
      # Plot branches
      plot <- ggtree_to_plotly(layout = layout, 
                               type = "daylight", coord_fixed = coord_fixed, x_to_y_ratio = x_to_y_ratio, color = green_color, linewidth = 1)
      
      return(plot)
    },
    label="plot_branches_matching")
    
    # Plot tip points for all organisms 
    plot_tips_all = shiny::eventReactive({get_tree_trigger()},
    {
      plot <- switch(input$set_tree_layout,
                     "Daylight" = load_plot_tips_all_daylight(),
                     "Equal angle" = load_plot_tips_all_equal_angle(),
                     "Rectangular" = load_plot_tips_all_rectangular()
      )

      return(plot)
    },
    label="plot_tips_all")
    
    # Plot tip points for matching organisms 
    plot_tips_matching = shiny::eventReactive({get_tree_trigger()},
    {
      # Get layout and data
      layout = get_tree_layout()
      data = filter_data()
      
      # Format layout
      layout = layout %>% dplyr::filter(isTip==TRUE)
      layout <- add_taxonomy_to_layout(layout = layout, layout_ID = "label", taxonomy = data, taxonomy_ID = "IMG Genome ID max genes")
      layout <- add_fill_to_layout(layout = layout, group = "Phylum", lighten_amount = 0.2)
      layout <- add_color_to_layout(layout = layout, group = "Phylum", lighten_amount = 0)
      
      # Get layout parameters
      layout_param <- get_layout_param(input$set_tree_layout)
      coord_fixed <- layout_param$coord_fixed
      x_to_y_ratio <- layout_param$x_to_y_ratio

      # Plot branches
      plot <- plot_scatterplot(df = layout, 
                               color = layout$color, fill = layout$fill, stroke = 1, size = 5, shape = "circle", alpha = 1,
                               label = c("Phylum", "Class", "Order", "Family", "Genus", "Species"), 
                               coord_fixed = coord_fixed, x_to_y_ratio = x_to_y_ratio)
      
      return(plot)
    },
    label="plot_tips_matching")
    
    # Combine plots for phylogenetic tree
    combine_tree_plots = shiny::eventReactive({get_tree_trigger()},
    {
      plot1 = plot_branches_all()
      plot2 = plot_tips_all()
      plot3 = plot_branches_matching()
      plot4 = plot_tips_matching()
      
      plot = overlay_plots(plot1, plot2, plot3, plot4)
    
      return(plot)
    },
    label = "combine_tree_plots")
    
    # Plot t-SNE scatterplot for all organisms 
    plot_scatter_all = shiny::eventReactive({make_search_trigger()},
    {
      plot = load_plot_tsne_all()

      return(plot)
    },
    label="combine_tree_plots")
    
    # Plot t-SNE scatterplot for  matching organisms 
    plot_scatter_matching = shiny::eventReactive({make_search_trigger()},
    {
      # Get layout and data
      layout = load_layout_tsne()
      data = filter_data()
      
      # Format layout
      layout <- add_taxonomy_to_layout(layout = layout, layout_ID = "IMG_Genome_ID_max_genes", taxonomy = data, taxonomy_ID = "IMG Genome ID max genes")
      layout <- add_fill_to_layout(layout = layout, group = "Phylum", lighten_amount = 0.2)
      layout <- add_color_to_layout(layout = layout, group = "Phylum", lighten_amount = 0)

      #Create scatter plot
      plot = plot_scatterplot(df = layout, 
                              color = layout$color, fill =  layout$fill, stroke = 1, size = 5, shape = "circle", alpha=1,
                              label = c("Phylum", "Class", "Order", "Family", "Genus", "Species"), 
                              ticklen.x = 4, ticklen.y = 4, showticklabels.x = TRUE, showticklabels.y = TRUE, title.x = "Dimension 1", title.y = "Dimension 2",
                              coord_fixed=TRUE, x_to_y_ratio=1)
      
      return(plot)
    },
    label="plot_scatter_matching")
    
    #Combine plots for t-SNE
    combine_tsne_plots = shiny::eventReactive({make_search_trigger()},
    {
      plot1 = plot_scatter_all()
      plot2 = plot_scatter_matching()
      plot = overlay_plots(plot1, plot2)
      
      return(plot)
    },
    label="combine_tsne_plots")
    
    # --- Update selections ---
    # Update query builder
    shiny::observeEvent({tab_selected_trigger()},
    {
      update_query_builder(inputId = "query_builder", choices = choices_traits_search)
    },
    label="update_query_builder")

    # Update variable to display
    shiny::observeEvent({tab_selected_trigger()},
    {
      choices <- choices_traits_search
      selected = "Phylum"
      update_picker_input(session, "variable_to_display", choices = choices, selected = selected)
    },
    label="update_variable_to_display")
    
    # Update tree layout
    shiny::observeEvent({tab_selected_trigger()},
    {
      choices <- c("Equal angle", "Daylight", "Rectangular")
      update_picker_input(session, "set_tree_layout", choices = choices)
    },
    label="update_set_tree_layout")

    # Update checkboxes
    shiny::observeEvent({tab_selected_trigger()},
    {
      choices = choices_checkboxes_search$organism$choices
      selected = intersect(c("Genus", "Species"),
                           choices_checkboxes_search$organism$choices)
      update_checkbox_group(session = session, inputId = "checkboxes_info_organism", choices = choices, selected = selected)

      choices = choices_checkboxes_search$databases$choices
      selected = intersect(c(
                            "LPSN Page", "Bergey Article",
                            "GTDB ID", "NCBI Taxonomy ID", "GOLD Organism ID",
                            "IMG Genome ID", "BacDive ID"
                            ),
                            choices_checkboxes_search$database$choices)
      update_checkbox_group(session = session, inputId = "checkboxes_info_databases", choices = choices, selected = selected)
      
      choices = choices_checkboxes_search$metabolism$choices
      update_checkbox_group(session = session, inputId = "checkboxes_info_metabolism", choices = choices)

      choices = choices_checkboxes_search$traits$choices
      update_checkbox_group(session = session, inputId = "checkboxes_info_traits", choices = choices)
    },
    label="update_checkboxes")

    # --- Generate outputs ---
    # Output number of matching organisms
    output$summary_text <- shiny::renderText(
      paste0("Query matched ", nrow(filter_data()), " organisms")
    )
       
    # Create output flags
    flag_if_not_null(output, "flag_results", trigger = make_search_trigger, value_fun = filter_data)
    
    # Output overview plots
      # Treemap plot
      output$treemap_plot <- plotly::renderPlotly({
        #Get data
        df = filter_data()
  
        #Format variable name
        var_name = input$variable_to_display
        var_name_display = var_name
        
        df = search_results_to_plot(df = df, plot_type="treemap", var_name = var_name)
        
        hovertemplate = paste0("<b>",var_name_display,": %{label}</b><br><b>% of matching organisms: %{value:.2f}</b><br><extra></extra>")
        dims <- calculate_treemap_dimensions(session, ns, "treemap_plot")
        plot = plot_treemap(df, 
                            hovertemplate = hovertemplate,
                            width = dims$width, height = dims$height)
        plot
      })
      
      # Tree plot
      output$tree_plot = plotly::renderPlotly({
        plot = combine_tree_plots()
        plot
      })
      
      # t-SNE plot
      output$tsne_plot <- plotly::renderPlotly({
         plot = combine_tsne_plots()
         plot
      })

    # Output table with matching organisms and columns
    output$table <- DT::renderDataTable({
      table = build_table()
      table
    }, escape = FALSE, options = list(scrollX = TRUE))
    
    # Output downloadable csv with matching results
    output$download_data <- create_download_handler(
      filename_prefix = "results",
      data_source = function() {
        filter_data() %>%
        dplyr::select(-dplyr::ends_with("link"))
      }
    )
}