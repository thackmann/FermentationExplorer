# Define the Search Database Module in Shiny App
# This script defines the user interface (UI) and server for the search database module.
# It also includes functions and variables specific to this module.  
# Author: Timothy Hackmann
# Date: 6 September 2024

# === Define functions ===
  # --- Functions for loading internal data ---
    #' Load Layout Tree Data in Daylight Format
    #'
    #' This function loads the layout tree data in daylight format from a CSV file.
    #' The data is loaded and stored in the environment if it is not already present.
    #'
    #' @return A data frame containing the layout tree data in daylight format.
    #' @export
    load_layout_tree_daylight <- function() {
      data_fp <- "data/layout_tree_daylight.csv"
      obj <- check_and_load(data_fp)
  
      return(obj)
    }
  
    #' Load Layout Tree Data in Equal Angle Format
    #'
    #' This function loads the layout tree data in equal angle format from a CSV file.
    #' The data is loaded and stored in the environment if it is not already present.
    #'
    #' @return A data frame containing the layout tree data in equal angle format.
    #' @export
    load_layout_tree_equal_angle <- function() {
      data_fp <- "data/layout_tree_equal_angle.csv"
      obj <- check_and_load(data_fp)
  
      return(obj)
    }
  
    #' Load Layout Tree Data in Rectangular Format
    #'
    #' This function loads the layout tree data in rectangular format from a CSV file.
    #' The data is loaded and stored in the environment if it is not already present.
    #'
    #' @return A data frame containing the layout tree data in rectangular format.
    #' @export
    #' @importFrom utils read.csv
    load_layout_tree_rectangular <- function() {
      data_fp <- "data/layout_tree_rectangular.csv"
      obj <- check_and_load(data_fp)
  
      return(obj)
    }
  
    #' Load Plot Branches Data in Daylight Format
    #'
    #' This function loads the plot branches data in daylight format from an RDS file.
    #' The data is loaded and stored in the environment if it is not already present.
    #'
    #' @return An object containing the plot branches data in daylight format.
    #' @export
    load_plot_branches_all_daylight <- function() {
      data_fp <- "data/plot_branches_all_daylight.rds"
      obj <- check_and_load(data_fp)
  
      return(obj)
    }
  
    #' Load Plot Branches Data in Equal Angle Format
    #'
    #' This function loads the plot branches data in equal angle format from an RDS file.
    #' The data is loaded and stored in the environment if it is not already present.
    #'
    #' @return An object containing the plot branches data in equal angle format.
    #' @export
    load_plot_branches_all_equal_angle <- function() {
      data_fp <- "data/plot_branches_all_equal_angle.rds"
      obj <- check_and_load(data_fp)
  
      return(obj)
    }
  
    #' Load Plot Branches Data in Rectangular Format
    #'
    #' This function loads the plot branches data in rectangular format from an RDS file.
    #' The data is loaded and stored in the environment if it is not already present.
    #'
    #' @return An object containing the plot branches data in rectangular format.
    #' @export
    load_plot_branches_all_rectangular <- function() {
      data_fp <- "data/plot_branches_all_rectangular.rds"
      obj <- check_and_load(data_fp)
  
      return(obj)
    }
  
    #' Load Plot Tips Data in Daylight Format
    #'
    #' This function loads the plot tips data in daylight format from an RDS file.
    #' The data is loaded and stored in the environment if it is not already present.
    #'
    #' @return An object containing the plot tips data in daylight format.
    #' @export
    load_plot_tips_all_daylight <- function() {
      data_fp <- "data/plot_tips_all_daylight.rds"
      obj <- check_and_load(data_fp)
  
      return(obj)
    }
  
    #' Load Plot Tips Data in Equal Angle Format
    #'
    #' This function loads the plot tips data in equal angle format from an RDS file.
    #' The data is loaded and stored in the environment if it is not already present.
    #'
    #' @return An object containing the plot tips data in equal angle format.
    #' @export
    load_plot_tips_all_equal_angle <- function() {
      data_fp <- "data/plot_tips_all_equal_angle.rds"
      obj <- check_and_load(data_fp)
  
      return(obj)
    }
  
    #' Load Plot Tips Data in Rectangular Format
    #'
    #' This function loads the plot tips data in rectangular format from an RDS file.
    #' The data is loaded and stored in the environment if it is not already present.
    #'
    #' @return An object containing the plot tips data in rectangular format.
    #' @export
    load_plot_tips_all_rectangular <- function() {
      data_fp <- "data/plot_tips_all_rectangular.rds"
      obj <- check_and_load(data_fp)
  
      return(obj)
    }
  
    #' Load t-SNE Layout Data
    #'
    #' This function loads the t-SNE layout data from a CSV file.
    #' The data is loaded and stored in the environment if it is not already present.
    #'
    #' @return A data frame containing the t-SNE layout data.
    #' @export
    load_layout_tsne <- function() {
      data_fp <- "data/layout_tsne.csv"
      obj <- check_and_load(data_fp)
  
      return(obj)
    }
  
    #' Load t-SNE Plot Data
    #'
    #' This function loads the t-SNE plot data from an RDS file.
    #' The data is loaded and stored in the environment if it is not already present.
    #'
    #' @return An object containing the t-SNE plot data.
    #' @export
    load_plot_tsne_all <- function() {
      data_fp <- "data/plot_tsne_all.rds"
      obj <- check_and_load(data_fp)
  
      return(obj)
    }
  
    #' Load Nodes to Root Data
    #'
    #' This function loads the nodes to root data from a CSV file.
    #' The data is loaded and stored in the environment if it is not already present.
    #'
    #' @return A data frame containing the nodes to root data.
    #' @export
    load_nodes_to_root <- function() {
      data_fp <- "data/nodes_to_root.csv"
      obj <- check_and_load(data_fp)
  
      return(obj)
    }
    
    #' Load Query Filters for Query Builder
    #'
    #' This function loads the query filters for the query builder from an RDS file.
    #' The data is loaded and stored in the environment if it is not already present.
    #'
    #' @return A list containing the query filters
    #' @export
    load_query_filters <- function() {
      data_fp <- "data/query_filters.rds"
      obj <- check_and_load(data_fp)
      
      return(obj)
    }
    
  # --- Other functions ---
    #' Simplify Taxonomy Names
    #'
    #' This function simplifies taxonomy names by replacing high-level taxonomic ranks with NA.
    #' The last non-NA rank is preserved, along with the species name.
    #'
    #' @param row A vector representing a row of taxonomy data.
    #' @param col_names A character vector of column names corresponding to the taxonomy ranks.
    #' @return A simplified vector with high-level ranks replaced by NA.
    #' @export
    simplify_names <- function(row, col_names) {
      species_column_idx <- which(col_names == "Species")
      
      if (all(is.na(row[-species_column_idx]))) {
        row
      } else {
        last_non_na_idx <- max(which(!is.na(row[-species_column_idx])))
        row[-c(last_non_na_idx, species_column_idx)] <- NA
      }
      
      return(row)
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
          dplyr::rename(y = var_name) %>%
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
 
  # === Set variables ===
  #Choices for variables to display
    choices_variables = c(
      "Phylum", "Class", "Order", "Family", "Genus", 
      "Type of metabolism", "Major end products", "Minor end products", "Substrates for end products", 
      "NCBI Phylum", "NCBI Class", "NCBI Order", "NCBI Family", 
      "NCBI Genus", 
      "Cell shape", "Flagellum arrangement", "Gram stain", "Indole test", 
      "Isolation source category 1", "Isolation source category 2", "Isolation source category 3", 
      "Oxygen tolerance", "Spore formation", "FAPROTAX predicted metabolism"
    )
    
  # Choices for columns of data to display
  choices_info_organism = vector()
  names_info_organism = colnames(dplyr::select(clean_data, Phylum:`Article link`))
  choices_info_organism = seq_along(names_info_organism)
  names(choices_info_organism) = names_info_organism

  choices_info_fermentation = vector()
  names_info_fermentation = colnames(dplyr::select(clean_data, `Type of metabolism`:`Substrates for end products`))
  choices_info_fermentation = seq_along(names_info_fermentation)
  names(choices_info_fermentation) = names_info_fermentation

  choices_info_JGI = vector()
  names_info_JGI = colnames(dplyr::select(clean_data, `GOLD Organism ID`:`IMG Genome ID max genes`))
  choices_info_JGI = seq_along(names_info_JGI)
  names(choices_info_JGI) = names_info_JGI

  choices_info_NCBI = vector()
  names_info_NCBI = colnames(dplyr::select(clean_data, `NCBI Taxonomy ID`:`NCBI Species`))
  choices_info_NCBI = seq_along(names_info_NCBI)
  names(choices_info_NCBI) = names_info_NCBI
  
  choices_info_BacDive = vector()
  names_info_BacDive = colnames(dplyr::select(clean_data, `BacDive Organism ID`:`Salt in moles per liter`))
  choices_info_BacDive = seq_along(names_info_BacDive)
  names(choices_info_BacDive) = names_info_BacDive
  
  choices_info_FAPROTAX = vector()
  names_info_FAPROTAX = colnames(dplyr::select(clean_data, `FAPROTAX predicted metabolism`))
  choices_info_FAPROTAX = seq_along(names_info_FAPROTAX)
  names(choices_info_FAPROTAX) = names_info_FAPROTAX
  
  # Filters for query builder
  query_filters = load_query_filters()
  
  # Rules for query builder
  query_rules <- list(
    condition = "AND",
    rules = list(
      list(
        id = "Type of metabolism",
        operator = "in"
      )
    )
  )
  
# === Define user interface (UI) ===
  # Search database tab
  databaseSearchUI <- function(id) {
    ns <- NS(id)
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
        shiny::h3("Search database")
      ),
      
      bslib::layout_sidebar(
        #Sidebar
        sidebar = bslib::sidebar(
          width = "30%",
          div(
            "Build query",
            jqbr::queryBuilderInput(
              inputId = ns("query_builder"),
              filters = query_filters,
              return_value = "r_rules",
              display_errors = TRUE,
              rules = query_rules,
              add_na_filter = FALSE
            )
          ),
          shiny::actionButton(ns("perform_search"), "Perform search", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
        ),
        #Main content area
        shiny::div(
          id = ns("results_page"),

          shiny::conditionalPanel(
            condition = "input.perform_search == 0",
            ns = ns,
            h4("Please make selections at left")
          ),
          
          shiny::conditionalPanel(
            condition = "input.perform_search > 0",
            ns = ns,
            
            bslib::card(
              bslib::card_header(textOutput(ns("n_match"))), downloadButton(ns('download_data'), 'Download results') %>% shinycssloaders::withSpinner(color = "#3C8DBC"),
            ),
            
            bslib::navset_card_underline(
              title = "Plots",
              full_screen = TRUE,
              bslib::nav_panel(
                title = "Treemap",
                div(
                  id = ns("treemap-container"),
                  class = "treemap-container-style",
                  plotly::plotlyOutput(ns("treemap"), width = "100%", height = "40vh") %>% shinycssloaders::withSpinner(color = "#3C8DBC")
                ),
                div(
                  shiny::selectInput(inputId = ns("variable_to_display"), label = "Variable", choices = choices_variables, selected = "NCBI Phylum", multiple = FALSE, selectize = TRUE, width = "100%")
                )
              ),
              bslib::nav_panel(
                title = "Tree",
                div(
                  id = ns("tree-container"),
                  class = "tree-container-style",
                  plotly::plotlyOutput(ns("tree_plot"), width = "100%", height = "40vh") %>% shinycssloaders::withSpinner(color = "#3C8DBC"),
                ),
                div(
                  shiny::selectInput(inputId = ns("set_tree_layout"), label = "Layout", choices = c("Equal angle", "Daylight", "Rectangular"), selected = "Equal angle", multiple = FALSE, selectize = TRUE, width = "100%")
                ),
                div(
                  "Matching organisms are those that are fully colored. Only organisms with genome sequences are shown."
                )
              ),
              bslib::nav_panel(
                title = "t-SNE",
                div(
                  id = ns("tnse-container"),
                  class = "tsne-container-style",
                  plotly::plotlyOutput(ns("tsne_plot"), width = "100%", height = "40vh") %>% shinycssloaders::withSpinner(color = "#3C8DBC"),
                ),
                div(
                  "Matching organisms are those that are fully colored. Only organisms with genome sequences are shown."
                )
              )
            ),
            
            bslib::card(
              bslib::card_header("Detailed results"),  
              full_screen = TRUE,
              div(
                shinycssloaders::withSpinner(DT::dataTableOutput(ns("table")), color = "#3C8DBC")
              ),
              bslib::layout_column_wrap(
                width = 1/4, 
                div(
                  shiny::checkboxGroupInput(ns("checkboxes_info_organism"), "Organism", choices = choices_info_organism, selected = c(which(names(choices_info_organism) == "Genus"), which(names(choices_info_organism) == "Species"), which(names(choices_info_organism) == "Article link"))),
                  shiny::checkboxGroupInput(ns("checkboxes_info_fermentation"), "Fermentation", choices = choices_info_fermentation)
                ),
                div(
                  shiny::checkboxGroupInput(ns("checkboxes_info_JGI"), "JGI", choices = choices_info_JGI, selected = c(which(names(choices_info_JGI) == "GOLD Organism ID"), which(names(choices_info_JGI) == "IMG Genome ID"))), 
                  shiny::checkboxGroupInput(ns("checkboxes_info_NCBI"), "NCBI", choices = choices_info_NCBI, selected = c(which(names(choices_info_NCBI) == "NCBI Taxonomy ID")))
                ),
                div(   
                  shiny::checkboxGroupInput(ns("checkboxes_info_BacDive"), "BacDive", choices = choices_info_BacDive, selected = c(which(names(choices_info_BacDive) == "BacDive Organism ID")))
                ),
                div(
                  shiny::checkboxGroupInput(ns("checkboxes_info_FAPROTAX"), "FAPROTAX", choices = choices_info_FAPROTAX),
                )
              )
            )
          )
        )
      )
    )
  }

# === Define server ===
  databaseSearchServer <- function(input, output, session) {
    # Set namespace
    ns <- session$ns
    
    # --- Get user input (events) ---
    # No logic
    
    # --- Process input ---
    # Filter data according to query
    filter_data = shiny::eventReactive({input$perform_search}, {
      data = clean_data
    
      # Print status to log
      cat(file = stderr(), paste0("Started search at ", Sys.time(), "\n"))
      
      # Launch modal with progress bar
      launch_modal_with_progress(session, ns("pb"), message = "Performing search")
      
      # Perform filtering
      data =  jqbr::filter_table(data, input$query_builder)
      
      # Remove unnamed columns
      colnames(data)[colnames(data) == ""] <- "Unnamed"
      data = data %>% dplyr::select(-Unnamed)

      return(data)
    })
    
    # Build data table
    build_table <- shiny::reactive({
      # Get data
      data = filter_data()
      
      # Remove redundant columns (those duplicated by columns with links)
      columns_to_remove <- c("NCBI Taxonomy ID", "GOLD Organism ID", "GOLD Project ID", 
                             "IMG Genome ID", "IMG Genome ID max genes", "BacDive Organism ID", 
                             "Article link")
      data <- data %>% dplyr::select(-dplyr::one_of(columns_to_remove))
      
      # Rename columns with links
      if (is.data.frame(data)) {
        data <- data %>% 
          dplyr::rename(
            `NCBI Taxonomy ID` = "NCBI Taxonomy ID link",
            `GOLD Organism ID` = "GOLD Organism ID link",
            `GOLD Project ID` = "GOLD Project ID link",
            `IMG Genome ID` = "IMG Genome ID link",
            `IMG Genome ID max genes` = "IMG Genome ID max genes link",
            `BacDive Organism ID` = "BacDive Organism ID link",
            `Article link` = "Article link button"
          )
      } else {
        print("Data is not a dataframe, rename skipped")
      }
      
      # Get names of columns selected via checkbox
      columns_info_organism = names(choices_info_organism[as.numeric(input$checkboxes_info_organism)])
      columns_info_fermentation = names(choices_info_fermentation[as.numeric(input$checkboxes_info_fermentation)])
      columns_info_JGI = names(choices_info_JGI[as.numeric(input$checkboxes_info_JGI)])
      columns_info_NCBI = names(choices_info_NCBI[as.numeric(input$checkboxes_info_NCBI)])
      columns_info_BacDive = names(choices_info_BacDive[as.numeric(input$checkboxes_info_BacDive)])
      columns_info_FAPROTAX = names(choices_info_FAPROTAX[as.numeric(input$checkboxes_info_FAPROTAX)])
      
      # Keep only selected columns
      columns = c(columns_info_organism, columns_info_fermentation, columns_info_JGI, columns_info_NCBI, columns_info_BacDive, columns_info_FAPROTAX)
      data = data %>% dplyr::select(dplyr::all_of(columns))
      
      # Hide the modal with progress bar
      hide_modal_with_progress(session, ns("pb"))

      # Print status to log
      cat(file = stderr(), paste0("Ended search at ", Sys.time(), "\n"))
      
      return(data)
    })
    
    # Get layout of phylogenetic tree
    get_tree_layout <- shiny::reactive({
      # Update modal with progress bar
      shinyjs::runjs("document.getElementById('modal-text').innerText = 'Building phylogenetic tree';")
      shinyWidgets::updateProgressBar(session = session, id = ns("pb"), value = 0)
      
      #Get layout
      if(input$set_tree_layout=="Daylight")
      {
        layout = load_layout_tree_daylight()
      }else if(input$set_tree_layout=="Equal angle")
      {
        layout = load_layout_tree_equal_angle()
        layout = layout_tree_equal_angle
      }else if(input$set_tree_layout=="Rectangular")
      {
        layout = load_layout_tree_rectangular()
      }

      return(layout)
    })
    
    # Plot tree branches for all organisms
    plot_branches_all = shiny::reactive({
      if(input$set_tree_layout=="Daylight")
      {
        plot = load_plot_branches_all_daylight()
      }else if(input$set_tree_layout=="Equal angle")
      {
        plot = load_plot_branches_all_equal_angle()
      }else if(input$set_tree_layout=="Rectangular")
      {
        plot = load_plot_branches_all_rectangular()
      }

      return(plot)
    })
    
    # Plot tree branches for matching organisms
    plot_branches_matching = shiny::reactive({
      #Get data and layout
      layout = get_tree_layout()
      data = filter_data()
      nodes_to_root = load_nodes_to_root()

      #Format layout
      selected_tips = layout$node[which(layout$label %in% data$`IMG Genome ID max genes`)]
      match = nodes_to_root %>% dplyr::filter(tip_node %in% selected_tips) %>% dplyr::distinct(parent_node, child_node)
      layout <- layout %>%
        dplyr::semi_join(match, by = c("parent" = "parent_node", "node" = "child_node")) %>%
        dplyr::bind_rows(layout %>% dplyr::filter(parent == node))

      #Create plot
      # Plot branches
      if(input$set_tree_layout=="Daylight")
      {
        plot = ggtree_to_plotly(layout = layout, type="daylight", coord_fixed=TRUE, x_to_y_ratio=0.8, color = green_color, linewidth=1)
      }else if(input$set_tree_layout=="Equal angle")
      {
        plot = ggtree_to_plotly(layout = layout, type="equal_angle", coord_fixed=TRUE, x_to_y_ratio=0.8, color = green_color, linewidth=1)
      }else if(input$set_tree_layout=="Rectangular")
      {
        plot = ggtree_to_plotly(layout = layout, type="rectangular", coord_fixed=FALSE, x_to_y_ratio=NULL, color = green_color, linewidth=1)
      }
      
      return(plot)
    })
    
    # Plot tip points for all organisms 
    plot_tips_all = shiny::reactive({
      if(input$set_tree_layout=="Daylight")
      {
        plot = load_plot_tips_all_daylight()
      }else if(input$set_tree_layout=="Equal angle")
      {
        plot = load_plot_tips_all_equal_angle()
      }else if(input$set_tree_layout=="Rectangular")
      {
        plot = load_plot_tips_all_rectangular()
      }

      return(plot)
    })
    
    # Plot tip points for matching organisms 
    plot_tips_matching = shiny::reactive({
      #Get layout and data
      layout = get_tree_layout()
      data = filter_data()
      
      #Format layout
      layout = layout %>% dplyr::filter(isTip==TRUE)
      layout <- add_taxonomy_to_layout(layout = layout, layout_ID = "label", taxonomy = data, taxonomy_ID = "IMG Genome ID max genes")
      layout <- add_fill_to_layout(layout = layout, group = "Phylum", lighten_amount = 0.2)
      layout <- add_color_to_layout(layout = layout, group = "Phylum", lighten_amount = 0)
      
      # Plot branches
      if(input$set_tree_layout=="Daylight")
      {
        plot = plot_scatterplot(df = layout, 
                                color = layout$color, fill =  layout$fill, stroke = 1, size = 5, shape = "circle", alpha=1,
                                label = c("Phylum", "Class", "Order", "Family", "Genus", "Species"), 
                                coord_fixed=TRUE, x_to_y_ratio=0.8)
      }else if(input$set_tree_layout=="Equal angle")
      {
        plot = plot_scatterplot(df = layout, 
                                color = layout$color, fill =  layout$fill, stroke = 1, size = 5, shape = "circle", alpha=1,
                                label = c("Phylum", "Class", "Order", "Family", "Genus", "Species"), 
                                coord_fixed=TRUE, x_to_y_ratio=0.8)
      }else if(input$set_tree_layout=="Rectangular")
      {
        plot = plot_scatterplot(df = layout, 
                                color = layout$color, fill =  layout$fill, stroke = 1, size = 5, shape = "circle", alpha=1,
                                label = c("Phylum", "Class", "Order", "Family", "Genus", "Species"), 
                                coord_fixed=FALSE, x_to_y_ratio=NULL)
      }

      return(plot)
    })
    
    # Combine plots for phylogenetic tree
    combine_tree_plots = shiny::reactive({
      plot1 = plot_branches_all()
      plot2 = plot_tips_all()
      plot3 = plot_branches_matching()
      plot4 = plot_tips_matching()
      
      plotA = overlay_plots(plot1 = plot1, plot2 = plot2) 
      plotB = overlay_plots(plot1 = plot3, plot2 = plot4) 
      
      plotC = overlay_plots(plot1 = plotA, plot2 = plotB)
    
      return(plotC)
    })
    
    
    # Plot t-SNE scatterplot for all organisms 
    plot_scatter_all = shiny::reactive({
      plot = load_plot_tsne_all()
      
      return(plot)
    })
    
    # Plot t-SNE scatterplot for  matching organisms 
    plot_scatter_matching = shiny::reactive({
      #Get layout and data
      layout = load_layout_tsne()
      data = filter_data()
      
      #Format layout
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
    })
    
    #Combine plots for t-SNE
    combine_tsne_plots = shiny::reactive({
      plot1 = plot_scatter_all()
      plot2 = plot_scatter_matching()
      
      plot3 = overlay_plots(plot1 = plot1, plot2 = plot2) 
     
      return(plot3)
    })
    
    # --- Generate outputs ---
    # Output number of matching organisms
    output$n_match <- shiny::renderText(sprintf("Query matched %d organisms", nrow(filter_data())))
    
    # Output plots
    #Treemap
    output$treemap <- plotly::renderPlotly({
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
    
    # Tree
    output$tree_plot = plotly::renderPlotly({
      p = combine_tree_plots()
      p
    })
    
    # t-SNE
    output$tsne_plot <- plotly::renderPlotly({
       p = combine_tsne_plots()
       p
    })
    
    # Output table with matching organisms and columns
    output$table <- DT::renderDataTable({
      table = build_table()
      table
    }, escape = FALSE, options = list(scrollX = TRUE))
    
    # Output downloadable csv with matching results
    output$download_data <- shiny::downloadHandler(
      filename = function() {
        paste("results", "csv", sep = ".")
      },
      content = function(file) {
        sep <- switch("csv", "csv" = ",", "tsv" = "\t")
        data = filter_data()
        # Remove columns with links
        columns_to_remove <- c("NCBI Taxonomy ID link", "GOLD Organism ID", "GOLD Project ID link", 
                               "IMG Genome ID link", "IMG Genome ID max genes link", "BacDive Organism ID link", 
                               "Article link button")
        data <- data %>% dplyr::select(-dplyr::one_of(columns_to_remove))

        # Write to a file specified by the 'file' argument
        utils::write.table(data, file, sep = sep, row.names = FALSE)
      }
    )
  }
