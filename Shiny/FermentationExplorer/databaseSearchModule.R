# Define the Search Database Module in Shiny App
# This script defines the user interface (UI) and server for the search database module.
# It also includes functions and variables specific to this module.  
# Author: Timothy Hackmann
# Date: 18 February 2025

# === Define functions ===
  #' Rename and Overwrite Column Names
  #'
  #' This function renames columns in a dataframe by replacing a specified pattern with a replacement string.
  #' If the renaming results in duplicate column names, only the last occurrence of each name is retained.
  #'
  #' @param data A dataframe whose column names need to be renamed.
  #' @param pattern A character string containing a regular expression to match in column names.
  #' @param replacement A character string that will replace the matched pattern in column names.
  #'
  #' @return The input dataframe with renamed column names. If duplicates arise after renaming, earlier occurrences are removed, keeping only the last occurrence of each duplicated column name.
  #' @export
  #'
  #' @examples
  rename_and_overwrite <- function(data, pattern, replacement) {
    colnames(data) <- colnames(data) %>%
      stringr::str_replace(pattern, replacement)
    
    data <- data[, !duplicated(names(data), fromLast = TRUE)]
    
    data
  }

  #' Format Search Results for Plots
  #'
  #' This function formats taxonomy results into a format suitable 
  #' for different types of plots, though only treemap plots are
  #' supported at present.   
  #'
  #' @param df A dataframe containing the taxonomy results.
  #' @param plot_type A character string specifying the type of plot ("treemap").
  #' @param var_name Optional. A character string specifying the variable name to filter by.
  #' @return A formatted dataframe ready for plotting.
  #' @export
  #' @importFrom dplyr  mutate select group_by n summarize
  search_results_to_plot <- function(df, plot_type, var_name = NULL) {
    if (plot_type == "treemap") {
      df = df %>% 
        dplyr::select(all_of(var_name)) %>% 
        dplyr::rename(y = all_of(var_name)) %>%
        tidyr::drop_na() %>%  
        dplyr::filter(y != "NA") 
      
      df <- df %>%
        dplyr::group_by(y) %>%
        dplyr::summarise(z = dplyr::n(), .groups = 'drop') %>%
        dplyr::mutate(z = z / sum(z))
      
      # Convert to percentage
      df$z = df$z * 100
    }
    
    return(df)
  }
  
  #' Filter Tree Layout Based on Selected Organisms
  #'
  #' This function filters a tree layout to retain only the branches that lead to 
  #' organisms matching a given set of IDs from a specified column. It ensures that
  #' only relevant parent-child relationships are kept, while preserving root nodes.
  #'
  #' @param layout A dataframe representing the phylogenetic tree layout, 
  #'        containing columns `node`, `label`, `parent`, etc.
  #' @param data A dataframe containing the selected organisms.
  #' @param nodes_to_root A dataframe mapping tip nodes to their parent nodes, 
  #'        containing columns `tip_node`, `parent_node`, and `child_node`.
  #' @param id_column A string specifying the column in `data` that contains the organism IDs.
  #'        Defaults to `"IMG Genome ID max genes"`.
  #'
  #' @return A filtered version of `layout` containing only the branches leading 
  #'         to the selected organisms, along with root nodes.
  #' 
  #' @export
  filter_tree_layout <- function(layout, data, nodes_to_root, id_column = "IMG Genome ID max genes") {
    if (!id_column %in% names(data)) {
      stop(glue::glue("Column '{id_column}' not found in data"))
    }
    
    # Select matching tip nodes based on the given ID column
    selected_tips <- layout$node[which(layout$label %in% data[[id_column]])]
    
    # Find matching parent-child node pairs
    match <- nodes_to_root %>%
      dplyr::filter(tip_node %in% selected_tips) %>%
      dplyr::distinct(parent_node, child_node)
    
    # Retain only relevant branches and root nodes
    layout_filtered <- layout %>%
      dplyr::semi_join(match, by = c("parent" = "parent_node", "node" = "child_node")) %>%
      dplyr::bind_rows(layout %>% dplyr::filter(parent == node))
    
    return(layout_filtered)
  }
  
  #' Get Tree Layout Parameters
  #'
  #' This function returns a list of parameters based on the selected tree layout type.
  #' It determines whether the coordinate system should be fixed and assigns an appropriate 
  #' x-to-y ratio for different layout types.
  #'
  #' @param layout_type A character string specifying the tree layout type. 
  #'        Possible values: "Daylight", "Equal angle", or "Rectangular".
  #'
  #' @return A list containing:
  #'   \item{coord_fixed}{Logical value indicating whether the coordinate system should be fixed.}
  #'   \item{x_to_y_ratio}{Numeric value specifying the x-to-y ratio for the layout, or NULL if not applicable.}
  #'
  #' @examples
  #' get_layout_param("Daylight")
  #' get_layout_param("Rectangular")
  #'
  #' @export
  get_layout_param <- function(layout_type) {
    list(
      coord_fixed = switch(layout_type,
                           "Daylight" = TRUE,
                           "Equal angle" = TRUE,
                           "Rectangular" = FALSE
      ),
      x_to_y_ratio = switch(layout_type,
                     "Daylight" = 0.8,
                     "Equal angle" = 0.8,
                     "Rectangular" = NULL
      )
    )
  }
  
# === Set variables ===
  # Choices for variables for searching
  choices_traits_search = c(
    taxonomy_var,
    # database_var, # commenting this out speeds up execution by ~20 s
    metabolism_var,
    physiology_var,
    morphology_var,
    isolation_var)
   
  # Choices for checkboxes
  choices_checkboxes_search <- list(
    organism = list(names = taxonomy_var),
    databases = list(names = database_var),
    metabolism = list(names = metabolism_var),
    traits = list(names = c(physiology_var, morphology_var, isolation_var))
  )
  
  choices_checkboxes_search <- lapply(choices_checkboxes_search, function(category) {
    category$choices <- setNames(category$names, category$names)
    return(category)
  })
  
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
              full_screen = TRUE,
              bslib::nav_panel(
                title = "Treemap",
                create_plot_div(ns = ns, plot_type = "treemap")
              ),
              bslib::nav_panel(
                title = "Tree",
                create_plot_div(ns = ns, plot_type = "tree"),
              ),
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
    make_search_trigger <- reactive({
      input$perform_search
    })

    get_tree_trigger <- reactive({
      list(input$perform_search,input$set_tree_layout)
    })
    
    build_table_trigger <- reactive({
      list(input$perform_search,
           input$checkboxes_info_organism,
           input$checkboxes_info_databases,
           input$checkboxes_info_metabolism,
           input$checkboxes_info_traits)
    })
    
    tab_selected_trigger <- reactive({
      if (selected_tab() == "databaseSearch") {
        return(TRUE)
      }
    })
    
    # --- Get user input (events) ---
    # Get query
    get_query <- shiny::eventReactive({make_search_trigger()},
    {
      query_string  = input$query_builder
      
      runValidationModal(session = session, need(query_string != "", "Please build a valid query."))

      return(query_string)
    },
    ignoreNULL = TRUE,  ignoreInit=FALSE, label = "get_query")
    
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
    ignoreNULL = TRUE,  ignoreInit=FALSE, label="add_taxonomy") # Must have ignoreInit = TRUE, or module runs on app start up
     
    # Filter data according to query
    filter_data = shiny::eventReactive({make_search_trigger()},
    {
      #Get data
      data = add_taxonomy()
      query_string = get_query()
      
      # Print status to log
      cat(file = stderr(), paste0("Started search at ", Sys.time(), "\n"))

      # Launch modal with progress bar
      display_modal(session, ns("pb"), message = "Performing search")
      
      # Perform filtering
      data =  filter_data_by_query(data = data, query_string = query_string)

      return(data)
    },
    ignoreNULL = TRUE,  ignoreInit=FALSE, label="filter_data") # Must have ignoreInit = TRUE, or module runs on app start up
    
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
    }, ignoreNULL = TRUE, ignoreInit = FALSE, label = "build_table")
    
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
    ignoreNULL = TRUE,  ignoreInit=FALSE, label="get_Tree_layout")
    
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
    ignoreNULL = TRUE,  ignoreInit=FALSE, label="plot_branches_all")
    
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
    ignoreNULL = TRUE,  ignoreInit=FALSE, label="plot_branches_matching")
    
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
    ignoreNULL = TRUE,  ignoreInit=FALSE, label="plot_tips_all")
    
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
    ignoreNULL = TRUE,  ignoreInit=FALSE, label="plot_tips_matching")
    
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
    ignoreNULL = TRUE,  ignoreInit=FALSE, label = "combine_tree_plots")
    
    # Plot t-SNE scatterplot for all organisms 
    plot_scatter_all = shiny::eventReactive({make_search_trigger()},
    {
      plot = load_plot_tsne_all()
      
      return(plot)
    },
    ignoreNULL = TRUE,  ignoreInit=FALSE, label="combine_tree_plots")
    
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
    ignoreNULL = TRUE,  ignoreInit=FALSE, label="plot_scatter_matching")
    
    #Combine plots for t-SNE
    combine_tsne_plots = shiny::eventReactive({make_search_trigger()},
    {
      plot1 = plot_scatter_all()
      plot2 = plot_scatter_matching()
      
      plot = overlay_plots(plot1, plot2)
      
      return(plot)
    },
    ignoreNULL = TRUE,  ignoreInit=FALSE, label="combine_tsne_plots")
    
    # --- Update selections ---
    # Update query builder
    shiny::observeEvent({tab_selected_trigger()},
    {
      update_query_builder(inputId = "query_builder", choices = choices_traits_search)
    },
    ignoreNULL = TRUE,  ignoreInit = TRUE, label="update_query_builder")

    # Update variable to display
    shiny::observeEvent({tab_selected_trigger()},
    {
      choices <- choices_traits_search
      selected = "Phylum"
      update_picker_input(session, "variable_to_display", choices = choices, selected = selected)
    },
    ignoreNULL = TRUE,  ignoreInit = TRUE, label="update_variable_to_display")
    
    # Update tree layout
    shiny::observeEvent({tab_selected_trigger()},
    {
      choices <- c("Equal angle", "Daylight", "Rectangular")
      update_picker_input(session, "set_tree_layout", choices = choices)
    },
    ignoreNULL = TRUE,  ignoreInit = TRUE, label="update_set_tree_layout")

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
    ignoreNULL = TRUE,  ignoreInit = TRUE, label="update_checkboxes")

    # --- Generate outputs ---
    # Output number of matching organisms
    output$summary_text <- shiny::renderText(
      paste0("Query matched ", nrow(filter_data()), " organisms")
    )
       
    # Output flag for results
    output$flag_results = shiny::reactive({
      !is.null(filter_data())
    })
    shiny::outputOptions(output, "flag_results", suspendWhenHidden = FALSE)
    
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
        
        plot = plot_treemap(df, 
                            hovertemplate = hovertemplate)
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