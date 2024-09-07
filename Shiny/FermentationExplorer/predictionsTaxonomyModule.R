# Define the Predictions from Taxonomy Module in Shiny App
# This script defines the user interface (UI) and server for the predictions from taxonomy module.
# It also includes functions and variables specific to this module.  
# Author: Timothy Hackmann
# Date: 5 September 2024

# === Define functions ===
  # --- Functions for predicting traits ---
    #' Convert a Query into a Regular Expression for Matching
    #'
    #' This function converts a query string into a regular expression for matching,
    #' padding strings with "^" and "$" to prevent partial matches. NA values are replaced 
    #' with "^.*$" so they match any string.
    #'
    #' @param x A character string or NA value to be formatted.
    #' @return A formatted regular expression string.
    #' @export
    format_query_element <- function(x) {
      if (!is.na(x)) {
        return(paste0("^", x, "$"))
      } else {
        return("^.*$")
      }
    }
    
    #' Create an Empty Dataframe with Specified Columns
    #'
    #' This function creates an empty dataframe with specified columns, excluding certain columns 
    #' and setting the number of rows.
    #'
    #' @param data A dataframe to extract column names from.
    #' @param exclude_columns A character vector of column names to exclude. Default is NULL.
    #' @param num_rows Number of rows for the new dataframe. If NULL, it defaults to the number of rows in the input data.
    #' @return A new dataframe with specified columns and rows filled with NA values.
    #' @export
    create_empty_dataframe <- function(data, exclude_columns = NULL, num_rows = NULL) {
      # Identify the columns to include by excluding the specified columns
      if (!is.null(exclude_columns)) {
        columns_to_include <- colnames(data)[!colnames(data) %in% exclude_columns]
      } else {
        columns_to_include <- colnames(data)
      }
      
      # Determine the number of rows for the new dataframe
      if (is.null(num_rows)) {
        num_rows <- nrow(data)
      }
      
      # Create the NA dataframe with the specified number of rows and columns
      na_df <- data.frame(matrix(NA, nrow = num_rows, ncol = length(columns_to_include)))
      
      # Set the column names of the new dataframe
      colnames(na_df) <- columns_to_include
      
      # Return the NA dataframe
      return(na_df)
    }
    
    # Expand a dataframe column
    ## This function takes a specified dataframe column, splits any values separated by semi-colons, then puts them in multiple columns
    ## The values should be characters (e.g., value1;value2).  
    ## The new columns will have the names of the values (e.g., value1 and value2)
    ## The rows in the new columns will contain 1 (value originally present) or 0 (value originally absent)
    ## NA originally in the column are preserved
    
    #' Expand a Dataframe Column
    #'
    #' This function expands a specified column in a dataframe by splitting values separated by 
    #' semi-colons into multiple columns.  The values should be characters (e.g., value1;value2).  
    #' The new columns will have the names of the values (e.g., value1 and value2)
    #' The rows in the new columns will contain 1 (value originally present) or 0 (value originally absent)
    #' NA originally in the column are preserved
    #'
    #' @param df A dataframe containing the column to be expanded.
    #' @param col_name The name of the column to be expanded.
    #' @param add A string to append to the expanded column names. Default is an empty string.
    #' @return A dataframe with the specified column expanded into multiple binary columns.
    #' @export
    #' @importFrom dplyr select mutate row_number filter distinct bind_cols arrange
    #' @importFrom rlang sym
    #' @importFrom stringr str_split
    #' @importFrom tidyr unnest_longer pivot_wider
    expand_column <- function(df, col_name, add = "") {
      # Select the column to be expanded and expand it
      col_data  <- df %>%
        dplyr::select(!!rlang::sym(col_name)) %>%                               # Select the column
        dplyr::mutate(row_index = dplyr::row_number()) %>%                      # Add a row index
        dplyr::mutate(!!col_name := stringr::str_split(!!rlang::sym(col_name), ";")) %>%  # Split the column by semi-colons
        tidyr::unnest_longer(!!rlang::sym(col_name)) %>%                        # Expand the list into multiple rows
        dplyr::filter(!is.na(!!rlang::sym(col_name)) & !!rlang::sym(col_name) != "NA") %>% # Remove NAs
        dplyr::mutate(!!col_name := paste0(!!rlang::sym(col_name), add)) %>%      # Add suffix if needed
        dplyr::distinct() %>%                                                     # Ensure unique combinations
        dplyr::mutate(value = 1) %>%                                              # Create a value column for pivoting
        tidyr::pivot_wider(names_from = !!rlang::sym(col_name), values_from = value, values_fill = 0) %>% # Create binary columns
        dplyr::right_join(tidyr::expand(df, row_index = 1:nrow(df)), by = "row_index") %>% # Ensure all row indices are present
        dplyr::arrange(row_index) %>%                                             # Sort by row index
        # dplyr::mutate(dplyr::across(where(is.numeric), ~ tidyr::replace_na(.x, 0))) %>%  # Replace NAs with 0 in numeric columns
        # dplyr::mutate(dplyr::across(where(is.character), 
        #                             ~ dplyr::if_else(.x == "NA" | is.na(.x), "0", .x))) %>% # Replace "NA" and NA with "0" in character columns
        dplyr::select(-row_index)                                                 # Remove the row index
      
      # Replace the unexpanded column with the expanded ones
      df <- df %>%
        dplyr::select(-!!rlang::sym(col_name)) %>%
        dplyr::bind_cols(col_data)
      
      return(df)
    }
    
    #' Expand Delimited Columns in a Dataframe
    #'
    #' This function expands multiple columns in a dataframe that contain delimited values, 
    #' creating new binary columns for each unique value.
    #'
    #' @param df A dataframe containing the columns to be expanded.
    #' @param columns_to_expand A character vector of column names to expand.
    #' @param add A string to append to the expanded column names. Default is an empty string.
    #' @return A dataframe with specified columns expanded into multiple binary columns.
    #' @export
    #' @importFrom dplyr mutate
    expand_delimited_columns <- function(df, columns_to_expand, add = "") {
      for (col_name in columns_to_expand) {
        df[[col_name]][df[[col_name]] == "NA"] <- NA
        # Add a suffix to the column name to show it was expanded
        df <- expand_column(df = df, col_name = col_name, add = paste0("_", col_name))
      }
      
      return(df)
    }
    
    #' Collapse Dataframe Columns into a Single Column
    #'
    #' This function collapses a set of columns in a dataframe into a single column with 
    #' semi-colon separated values.  The rows in the columns should contain 1 (value present) or 0 (value absent).
    #' The names of the columns will be the new values in the new column
    #' These values with be semi-colon seperated (e.g., column1;column2), 
    #' and values absent in the original column will be excluded
    #'
    #' @param df A dataframe containing the columns to be collapsed.
    #' @param cols A character vector of column names to collapse.
    #' @param new_col_name The name of the new column to store collapsed values.
    #' @param delete A string pattern to remove from column names during the collapse. Default is an empty string.
    #' @return A dataframe with the specified columns collapsed into a single column.
    #' @export
    #' @importFrom dplyr rowwise mutate ungroup select
    #' @importFrom stringr str_replace
    collapse_columns <- function(df, cols, new_col_name, delete = "") {
      df[[new_col_name]] <- sapply(1:nrow(df), function(i) {
        col_values <- gsub(delete, "", cols)
        values <- col_values[df[i, cols] == 1]
        values <- values[!is.na(values) & values != "NA"]
        if (length(values) > 0) {
          return(paste(values, collapse = ";"))
        } else {
          return(NA)
        }
      })
      df <- df[, !(colnames(df) %in% cols)]
      return(df)
      
      # df = df %>%
      #   dplyr::rowwise() %>%
      #   dplyr::mutate(
      #     !!new_col_name := paste(
      #       na.omit(
      #         gsub(delete, "", cols[dplyr::across(all_of(cols)) == 1])
      #       ),
      #       collapse = ";"
      #     )
      #   ) %>%
      #   dplyr::ungroup() %>%
      #   dplyr::select(-all_of(cols))
      
      return(df)
    }
    
    #' Collapse Delimited Columns Back into Single Columns
    #'
    #' This function collapses previously expanded delimited columns back into their original 
    #' single columns.
    #'
    #' @param df A dataframe containing the expanded columns.
    #' @param columns_to_expand A character vector of original column names before expansion.
    #' @return A dataframe with the expanded columns collapsed back into single columns.
    #' @export
    collapse_delimited_columns <- function(df, columns_to_expand) {
      for (original_col in columns_to_expand) {
        # Detect the expanded columns that match the original column name with the suffix
        expanded_cols <- colnames(df)[grepl(paste0("_", original_col, "$"), colnames(df))]
        
        # Collapse these expanded columns back into the original column
        df <- collapse_columns(df = df, cols = expanded_cols, new_col_name = original_col, delete = paste0("_", original_col))
      }
      
      return(df)
    }
    
    #' Filter a Table by Taxon
    #'
    #' This function filters a table to return rows matching a specified query taxon.
    #'
    #' @param table A dataframe to be filtered.
    #' @param query_taxon A dataframe containing the taxon query to match.
    #' @return A filtered dataframe containing only rows that match the query taxon.
    #' @export
    #' @importFrom dplyr filter select
    #' @importFrom stringr str_detect
    filter_table_by_taxon <- function(table, query_taxon) {
      # Get the names of the columns that need to be filtered
      filter_columns <- colnames(query_taxon)
      
      # Dynamically filter the table based on the columns in query
      match <- table
      for (col in filter_columns) {
        match <- match %>% dplyr::filter(grepl(pattern = query_taxon[[col]], x = match[[col]]))
      }
      
      # Drop the columns used for filtering
      match <- dplyr::select(as.data.frame(match), -all_of(filter_columns))
      
      return(match)
    }
    
    #' Determine Traits Shared by Matching Organisms
    #'
    #' This function determines traits that are shared by a threshold of matching organisms.
    #' If the threshold is 0.5, then 50% of matching organisms must have the trait for it to be considered shared.
    #'
    #' @param match A dataframe of matching organisms.
    #' @param threshold A numeric value between 0 and 1 indicating the threshold for considering a trait shared. Default is 0.5.
    #' @param ignore_NA Logical. If TRUE, organisms with NA are not counted. Default is TRUE.
    #' @return A character vector of shared traits.
    #' @export
    #' @importFrom dplyr ncol
    determine_shared_traits <- function(match, threshold = 0.5, ignore_NA = TRUE) {
      n_traits <- ncol(match)
      shared_traits <- rep(NA, n_traits)
      
      if (nrow(match) > 0) {
        for (j in seq_len(n_traits)) {
          if (ignore_NA) {
            x <- table(match[, j], exclude = NA)
          } else {
            x <- table(match[, j], useNA = "ifany")
          }
          
          if (any(!is.na(x)) && (max(x) / sum(x)) >= threshold) {
            shared_traits[j] <- names(x)[which.max(x)]
          }
        }
      }
      
      return(shared_traits)
    }
    
    #' Process Query Taxon to Find Shared Traits
    #'
    #' This function processes a query taxon by filtering matching organisms and then finding 
    #' their shared traits based on a specified threshold.
    #'
    #' @param table A dataframe to be filtered by the query taxon.
    #' @param query_taxon A dataframe containing the taxon query to match.
    #' @param threshold A numeric value between 0 and 1 indicating the threshold for considering a trait shared. Default is 0.5.
    #' @param ignore_NA Logical. If TRUE, organisms with NA are not counted. Default is TRUE.
    #' @return A character vector of shared traits.
    #' @export
    #' @importFrom dplyr filter mutate select
    process_query_taxon <- function(table, query_taxon, threshold = 0.5, ignore_NA = TRUE) {
      # Filter the table to find matching taxa
      match <- filter_table_by_taxon(table, query_taxon)

      # Determine shared traits
      shared_traits <- determine_shared_traits(match, threshold, ignore_NA = ignore_NA)
      
      return(shared_traits)
    }

  # --- Other functions ---
    #' Check if Data Follows DADA2 Format
    #'
    #' This function checks if the data contains columns expected in the DADA2 format.
    #'
    #' @param data Dataframe. The data to check.
    #' 
    #' @return Logical. TRUE if the data follows DADA2 format, FALSE otherwise.
    #' @export
    is_dada2_format <- function(data) {
      all(c("Phylum", "Class", "Order", "Family", "Genus", "Species") %in% colnames(data))
    }
    
    #' Check if Data Follows QIIME2 Format
    #'
    #' This function checks if the data contains taxonomy information in the QIIME2 format.
    #'
    #' @param data Dataframe. The data to check.
    #' 
    #' @return Logical. TRUE if the data follows QIIME2 format, FALSE otherwise.
    #' @export
    is_qiime2_format <- function(data) {
      pattern <- "^p__[^;]+(;c__[^;]+)?(;o__[^;]+)?(;f__[^;]+)?(;g__[^;]+)?(;s__[^;]+)?$"
      any(sapply(data, function(x) all(grepl(pattern, x, perl = TRUE))))
    }
    
    #' Process Uploaded Taxonomy File
    #'
    #' This function processes an uploaded taxonomy file, detecting whether it follows the DADA2 or QIIME2 format.
    #'
    #' @param query Dataframe. The uploaded taxonomy data.
    #' 
    #' @return A processed dataframe following either the DADA2 or QIIME2 format, or NULL if the format is unrecognized.
    #' @export
    #' @importFrom dplyr select
    #' @importFrom stringr str_extract
    process_uploaded_taxonomy <- function(query) {
      if (is_dada2_format(query)) {
        query <- query %>% dplyr::select(Phylum, Class, Order, Family, Genus, Species)
        query <- as.data.frame(query)
      } else if (is_qiime2_format(query)) {
        query <- data.frame(matrix(NA, nrow = nrow(query), ncol = 6))
        colnames(query) <- c("Phylum", "Class", "Order", "Family", "Genus", "Species")
        prefix <- c("p__", "c__", "o__", "f__", "g__", "s__")
        
        for (i in seq_len(nrow(query))) {
          for (j in seq_len(ncol(query))) {
            pattern <- paste0(prefix[j], ".*?(;|$)")
            query[i, j] <- stringr::str_extract(query[i, ], pattern = pattern)
            query[i, j] <- gsub(".__", "", query[i, j])
            query[i, j] <- gsub(";", "", query[i, j])
          }
        }
        query <- as.data.frame(query)
      } else {
        query <- NULL
      }
      
      return(query)
    }
    
    #' Filter Organisms by Preferred Taxonomy
    #'
    #' This function filters organisms in a dataset based on the preferred taxonomy type.
    #'
    #' @param data A dataframe containing the organisms to be filtered.
    #' @param taxonomy_type A character string specifying the preferred taxonomy type ("NCBI" or "Bergey").
    #' @return A dataframe filtered by the specified taxonomy type.
    #' @export
    #' @importFrom dplyr filter mutate
    #' @importFrom stringr word
    filter_by_taxonomy = function(data, taxonomy_type) {
      if(taxonomy_type=="NCBI") {
        data = dplyr::filter(data, NCBI_Phylum != "NA")
        data$Phylum = data$NCBI_Phylum
        data$Class = data$NCBI_Class
        data$Order = data$NCBI_Order
        data$Family = data$NCBI_Family
        data$Genus = data$NCBI_Genus
        data$Species = data$NCBI_Species
      } else if(taxonomy_type == "Bergey") {
        data = dplyr::filter(data, Phylum != "NA")
      }
      
      # Get last word in species name (removes genus name if included)
      data$Species = stringr::word(data$Species, -1)
      
      return(data)
    }
    
    #' Create Organism Name from Taxonomy
    #'
    #' This helper function creates an organism name based on its taxonomy, using genus and species 
    #' or other taxonomic levels if genus and species are unavailable.
    #'
    #' @param phylum A character string specifying the phylum.
    #' @param class A character string specifying the class.
    #' @param order A character string specifying the order.
    #' @param family A character string specifying the family.
    #' @param genus A character string specifying the genus.
    #' @param species A character string specifying the species.
    #' @return A character string representing the organism name.
    #' @export
    create_organism_name <- function(phylum, class, order, family, genus, species) {
      if (!is.na(genus) & genus!="NA" & !is.na(species) & species!="NA") {
        return(paste(genus, species, sep = " "))
      } else if (!is.na(genus) & genus!="NA") {
        return(paste(genus, "spp.", sep = " "))
      } else if (!is.na(family) & family!="NA") {
        return(paste(family, "spp.", sep = " "))
      } else if (!is.na(order) & order!="NA") {
        return(paste(order, "spp.", sep = " "))
      } else if (!is.na(class) & class!="NA") {
        return(paste(family, "spp.", sep = " "))
      } else if (!is.na(phylum) & phylum!="NA") {
        return(paste(family, "spp.", sep = " "))
      } else {
        return(NA)
      }
    }
    
    #' Format Taxonomy Results for Plots
    #'
    #' This function formats taxonomy results into a format suitable 
    #' for different types of plots such as summary, heatmap, or treemap.
    #'
    #' @param df A dataframe containing the taxonomy results.
    #' @param plot_type A character string specifying the type of plot ("summary", "heatmap", or "treemap").
    #' @param var_name Optional. A character string specifying the variable name to filter by.
    #' @return A formatted dataframe ready for plotting.
    #' @export
    #' @importFrom dplyr rowwise mutate select group_by n filter ungroup summarize
    #' @importFrom tidyr pivot_longer separate_rows complete pivot_wider
    #' @importFrom stringr str_replace_all
    taxonomy_results_to_plot <- function(df, plot_type, var_name = NULL) {
      # Create organism names
      df <- df %>%
        dplyr::rowwise() %>%
        dplyr::mutate(x = create_organism_name(phylum = Phylum, class = Class, order = Order, family = Family, genus = Genus, species = Species)) %>%
        dplyr::select(x, everything(), -Phylum, -Class, -Order, -Family, -Genus, -Species)
      
      # Ensure unique names
      df <- df %>%
        dplyr::group_by(x) %>%
        dplyr::mutate(Count = dplyr::n(), x = dplyr::if_else(Count > 1, paste0(x, " ", dplyr::row_number()), x)) %>%
        dplyr::select(x, dplyr::everything(), -Count) %>%
        dplyr::ungroup()
      
      # Pivot longer and separate rows by semicolon
      df <- df %>%
        tidyr::pivot_longer(cols = -x, names_to = "var", values_to = "y") %>%
        tidyr::separate_rows(y, sep = ";") %>%
        dplyr::filter(!is.na(y))
      
      if (plot_type == "summary") {
        # Further formatting
        df <- df %>%
          dplyr::group_by(var) %>%
          dplyr::mutate(z = 100) %>%
          tidyr::complete(x, y) %>%
          dplyr::mutate(z = ifelse(!is.na(z), 100, 0)) %>%
          dplyr::ungroup()
        
        #Fill in missing combinations of x and y
        # Ensure complete combinations of x for each y within each var
        df <- df %>%
          dplyr::group_by(y, var) %>%
          tidyr::complete(x = unique(df$x), fill = list(z = 0)) %>%
          dplyr::ungroup()
        
        # Fill missing var values based on the most common var for each y
        df <- df %>%
          dplyr::group_by(y) %>%
          dplyr::mutate(var = ifelse(is.na(var), var[!is.na(var)][1], var)) %>%
          dplyr::ungroup()
        
        # Filter by var
        df = filter_var(df, var_name) 
        
        # Summarize
        df = df %>%
          dplyr::group_by(y) %>%
          dplyr::summarize(z = mean(z), .groups = 'drop') %>%
          dplyr::ungroup()
        
        # Pivot wider
        df <- df %>%
          tidyr::pivot_wider(names_from = y, values_from = z, values_fill = list(z = 0))
        
      }else if (plot_type == "heatmap") {
        # Further formatting
        df <- df %>%
          dplyr::group_by(var) %>%
          dplyr::mutate(z = 100) %>%
          tidyr::complete(x, y) %>%
          dplyr::mutate(z = ifelse(!is.na(z), 100, 0)) %>%
          dplyr::ungroup()
        
        #Factorize by x
        df$x <- factor(df$x, levels = unique(df$x))
        
        #Fill in missing combinations of x and y
        # Ensure complete combinations of x for each y within each var
        df <- df %>%
          dplyr::group_by(y, var) %>%
          tidyr::complete(x = unique(df$x), fill = list(z = 0)) %>%
          dplyr::ungroup()
        
        # Fill missing var values based on the most common var for each y
        df <- df %>%
          dplyr::group_by(y) %>%
          dplyr::mutate(var = ifelse(is.na(var), var[!is.na(var)][1], var)) %>%
          dplyr::ungroup()
        
        # Filter by var
        df = filter_var(df, var_name) 
        
        #Pivot wider
        df <- df %>%
          tidyr::pivot_wider(names_from = y, values_from = z)
        
        # Add asterisk to x labels for clarity
        df$x = paste0(df$x, "*")
        
      }else if (plot_type == "treemap") {
        # Filter by var
        df = filter_var(df, var_name) 
        
        #Summarize by y
        df <- df %>%
          dplyr::group_by(y) %>%
          dplyr::summarise(z = dplyr::n(), .groups = 'drop') %>%
          dplyr::mutate(z = z / sum(z))
        
        #Convert to percentage
        df$z = df$z*100
      }
      
      return(df)
    }

# === Set variables ===
#Choices for traits
choices_traits = c("Type of metabolism", "Substrates for end products", "End products", "Major end products", "Minor end products", 
                   "Cell shape", "Flagellum arrangement", "Growth temperature", "Gram stain", "Indole test", 
                   "Isolation source category 1", "Isolation source category 2", "Isolation source category 3", 
                   "Oxygen tolerance", "Spore formation", "FAPROTAX predicted metabolism")

# === Define user interface (UI) ===
predictionsTaxonomyUI <- function(id) {
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
        shiny::h3("Predict traits from taxonomy")
    ),

    #Sidebar
    bslib::layout_sidebar(
      #Sidebar
      sidebar = bslib::sidebar(
                        width = "30%",
                        fileInput_modal(ns("file_query"), "Upload names of taxa", multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"), modalId = ns("taxonomy_file_modal"), modalLabel = "Download example"),
                        shinyWidgets::pickerInput(inputId = ns("set_traits"), label = "Traits to predict", choices = choices_traits, selected = c("Type of metabolism", "Substrates for end products", "End products"), multiple = TRUE, options = list(`actions-box` = TRUE)),
                        shiny::sliderInput(ns("threshold"), "Prediction threshold", min = 0, max = 1, value = 0.5),
                        shiny::div(class = "vertical-container",
                                   "Simplify names of taxa",
                                   shinyWidgets::switchInput(inputId = ns("simplify_names"), value = TRUE,  size = "small", inline = TRUE)
                        ),
                        shiny::div(class = "vertical-container",
                                   "Ignore missing values in database",
                                   shinyWidgets::switchInput(inputId = ns("ignore_missing"), value = TRUE,  size = "small", inline = TRUE)
                        ),
                        shinyWidgets::radioGroupButtons(inputId = ns("predictionsTaxonomy_filter"), label = "Taxonomy", choices = list("NCBI", "Bergey"), selected = list("NCBI")),
                        shiny::actionButton(ns("make_predictions"), "Make predictions", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
      ),
      
      #Main content area
      div(
                        id = ns("results_page"),
                        
                        shiny::conditionalPanel(
                          condition = "input.make_predictions == 0 | output.check_file_taxonomy",
                          ns = ns,
                          shiny::h4("Please upload files and make selections at left")
                        ),
                        
                        shiny::conditionalPanel(
                          condition = "input.make_predictions > 0 & !output.check_file_taxonomy",
                          ns = ns,

                          bslib::card(
                            bslib::card_header(shiny::textOutput(ns("summary_text"))),
                            shiny::downloadButton(ns('download_data'), 'Download results') %>% shinycssloaders::withSpinner(color = "#3C8DBC"),
                          ),
                          
                          bslib::navset_card_underline(
                            title = "Prediction results",
                            full_screen = TRUE,
                            bslib::nav_panel(
                                title = "Summary",
                                div(
                                  id = ns("summary-container"),
                                  class = "summary-container-style",
                                  plotly::plotlyOutput(ns("summary"), width = "100%", height = "40vh") %>% shinycssloaders::withSpinner(color = "#3C8DBC")
                                ),
                                div(
                                  shiny::conditionalPanel(
                                    condition = "output.flag_multiple_traits",
                                    ns = ns,
                                    div(
                                      shiny::selectInput(inputId = ns("trait_to_display"), label = "Trait", choices = "", selected = "End products", multiple = FALSE, selectize = TRUE, width = "100%")
                                    )
                                  )
                                )
                            ),
                            bslib::nav_panel(
                              title = "Treemap",
                              div(
                                id = ns("treemap-container"),
                                class = "treemap-container-style",
                                plotly::plotlyOutput(ns("treemap"), width = "100%", height = "40vh") %>% shinycssloaders::withSpinner(color = "#3C8DBC")
                              ),
                              div(
                                shiny::conditionalPanel(
                                  condition = "output.flag_multiple_traits",
                                  ns = ns,
                                  div(
                                    shiny::selectInput(inputId = ns("trait_to_display_treemap"), label = "Trait", choices = "", selected = "End products", multiple = FALSE, selectize = TRUE, width = "100%")
                                  )
                                )
                              )
                            ),
                            bslib::nav_panel(
                              title = "Heatmap",
                              div(
                                id = ns("heatmap-container"),
                                class = "heatmap-container-style",
                                plotly::plotlyOutput(ns("heatmap"),  width = "100%", height = "40vh") %>% shinycssloaders::withSpinner(color = "#3C8DBC")
                              ),
                              div(
                                shiny::conditionalPanel(
                                  condition = "output.flag_multiple_traits",
                                  ns = ns,
                                  div(
                                    shiny::selectInput(inputId = ns("trait_to_display_heatmap"), label = "Trait", choices = "", selected = "End products", multiple = FALSE, selectize = TRUE, width = "100%")
                                  )
                                )
                              )
                            )
                          ),
                          
                          bslib::card(
                            bslib::card_header("Detailed results"), 
                            full_screen = TRUE,
                            DT::dataTableOutput(ns("table")) %>% shinycssloaders::withSpinner(color = "#3C8DBC")
                          )
                    )
            )
      )
  )
}

# === Define server ===
predictionsTaxonomyServer <- function(input, output, session, x, selected_section) {
  #Set namespace
  ns <- session$ns

  #***********************
  #Get user input (events)
  #***********************
  # Get query taxa
  get_query <- shiny::reactive({
    # Validate, read, and process the taxonomy file
    query <- validate_and_read_csv(input$file_query$datapath)
    query <- process_uploaded_taxonomy(query)
    
    runValidationModal(need((!is.null(query)) && (nrow(query) > 0), "Please check the format of your uploaded file and try again."))
    
    return(query)
  })

  # Get table of organisms in database
  get_table = shiny::reactive({
    # Get data
    table = raw_data
    
    # Combine end products
    table$Major_end_products[table$Major_end_products == "NA"] <- NA
    table$Minor_end_products[table$Minor_end_products == "NA"] <- NA
    table$End_products <- ifelse(is.na(table$Major_end_products), table$Minor_end_products, ifelse(is.na(table$Minor_end_products), table$Major_end_products, paste(table$Major_end_products, table$Minor_end_products, sep = ";")))
    
    # Filter according to preferred taxonomy
    table = filter_by_taxonomy(data = table, taxonomy_type = input$predictionsTaxonomy_filter)

    return(table)
  })
  
  # Get traits
  get_traits_to_predict <- shiny::eventReactive({input$make_predictions}, {
    traits_to_predict = input$set_traits
    
    runValidationModal(need(traits_to_predict != "", "Please choose a trait"))
    
    return(traits_to_predict)  
  }, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # Get prediction threshold
  get_prediction_threshold = shiny::reactive({
    threshold = input$threshold
    
    return(threshold)
  })
  
  #--- Process input ---
  format_query <- shiny::eventReactive({input$make_predictions}, {
    # Get data
    query = get_query()

    #Launch modal with progress bar
    launch_modal_with_progress(session, ns("pb"), message = "Retrieving data")
    
    # Replace missing values with NA
    query[query == ""] = NA
    query[query == "?"] = NA
    query[query == "unclassified"] = NA
    
    # Remove brackets
    query = data.frame(lapply(query, function(x) gsub("\\[|\\]", "", x)))

    # Simplify names
    if(input$simplify_names == TRUE) {
      query = as.data.frame(t(apply(query, 1, simplify_names, colnames(query))))
    }

    return(query)
  })
  
  # Get predictions
  predict_traits <-  shiny::eventReactive({input$make_predictions}, {
    # Get inputs
    table = get_table()
    traits_to_predict = get_traits_to_predict()
    query = format_query()
    threshold = get_prediction_threshold()
    ignore_NA = input$ignore_missing 

    #Format traits to predict
    traits_to_predict = gsub(pattern=" ", replacement="_", x=traits_to_predict)
    
    # Format table
    table = table %>% dplyr::select(Phylum, Class, Order, Family, Genus, Species, dplyr::all_of(traits_to_predict))
    
    # Expand columns with ";" delimited values into multiple columns
    columns_to_expand <- names(table)[sapply(table, function(col) any(grepl(";", col, fixed = TRUE)))]
    table <- expand_delimited_columns(df = table, columns_to_expand = columns_to_expand)
    
    # Format query taxa as regular expressions (for matching)
    query_regex <- as.data.frame(apply(query, c(1, 2), format_query_element))
    
    # Update modal with progress bar
    shinyWidgets::updateProgressBar(session = session, id = ns("pb"), value = 0)
    shinyjs::runjs("document.getElementById('modal-text').innerText = 'Prediction in progress';")
    
    # Print status to log
    cat(file = stderr(), paste0("Started prediction at ", Sys.time(), "\n"))
    
    # Search table for matches to query taxa
    # Initialize values
    exclude_columns <- c("Phylum", "Class", "Order", "Family", "Genus", "Species")
    traits <- create_empty_dataframe(data = table, exclude_columns = exclude_columns, num_rows = nrow(query))
    
    # Perform matching
    for(i in seq_len(nrow(query))) {
      # Process the current query taxon and store the result in the traits dataframe
      traits[i, ] <- process_query_taxon(table = table, query_taxon = query_regex[i, ], threshold = threshold, ignore_NA = ignore_NA)
      
      # Update the progress bar
      shinyWidgets::updateProgressBar(session = session, id = ns("pb"), value = 1 / nrow(query) * 100 * i)
    }
    
    # Collapse expanded columns back into single columns
    traits <- collapse_delimited_columns(df = traits, columns_to_expand = columns_to_expand)
  
    # Add taxonomy of query organism
    traits = cbind(query, traits)
    
    # Replace all "NA"
    traits[traits == "NA"] <- NA
    
    # Hide the modal with progress bar
    hide_modal_with_progress(session, ns("pb"))
    
    # Print status to log
    cat(file = stderr(), paste0("Ended prediction at ", Sys.time(), "\n"))

    return(traits)
  }
  , ignoreNULL = FALSE,  ignoreInit=FALSE)
  
  # Count taxa with at least one predicted trait
  count_taxa <-  shiny::eventReactive({input$make_predictions}, {
    # Get inputs
    df = predict_traits()
    
    exclude_columns <- c("Phylum", "Class", "Order", "Family", "Genus", "Species")
    
    df = df %>% dplyr::select(-dplyr::all_of(exclude_columns))
    n_taxa = sum(rowSums(!is.na(df)) > 0)
    
    return(n_taxa)
  }
  , ignoreNULL = FALSE,  ignoreInit=FALSE)
  
  # --- Update selections ---
  # Update choices for trait to display
  shiny::observe({
    x = get_traits_to_predict()
    
    if (is.null(x))
      x <- character(0)
    
    shiny::updateSelectInput(session, inputId = "trait_to_display", choices = x, selected = head(x, 1))
    shiny::updateSelectInput(session, inputId = "trait_to_display_treemap", choices = x, selected = head(x, 1))
    shiny::updateSelectInput(session, inputId = "trait_to_display_heatmap", choices = x, selected = head(x, 1))
  })
  
  # Synchronize selections for trait to display
  shiny::observeEvent(input$trait_to_display, {
    selected_value = input$trait_to_display

    shiny::updateSelectInput(session, inputId = "trait_to_display_treemap", selected = selected_value)
    shiny::updateSelectInput(session, inputId = "trait_to_display_heatmap", selected = selected_value)
  })

  shiny::observeEvent(input$trait_to_display_treemap, {
    selected_value = input$trait_to_display_treemap

    shiny::updateSelectInput(session, inputId = "trait_to_display", selected = selected_value)
    shiny::updateSelectInput(session, inputId = "trait_to_display_heatmap", selected = selected_value)
  })

  shiny::observeEvent(input$trait_to_display_heatmap, {
    selected_value = input$trait_to_display_heatmap

    shiny::updateSelectInput(session, inputId = "trait_to_display", selected = selected_value)
    shiny::updateSelectInput(session, inputId = "trait_to_display_treemap", selected = selected_value)
  })
  
  # --- Generate outputs ---
  # Output file upload status
  output$check_file_taxonomy = shiny::reactive({
    is.null(input$file_query$datapath)
  })
  shiny::outputOptions(output, "check_file_taxonomy", suspendWhenHidden = FALSE)
  
  # Output downloadable csv with example taxa
  output$downloadTaxa_1 <- shiny::downloadHandler(
    filename = function() {
      paste("taxa_uncharacterized", "csv", sep = ".")
    },
    content = function(file) {
      table = taxa_uncharacterized
      utils::write.csv(table, file, row.names = FALSE)
    }
  )
  
  output$downloadTaxa_2 <- shiny::downloadHandler(
    filename = function() {
      paste("taxa_rumen_cultured", "csv", sep = ".")
    },
    content = function(file) {
      table = taxa_Hungate
      utils::write.csv(table, file, row.names = FALSE)
    }
  )
  
  output$downloadTaxa_3 <- shiny::downloadHandler(
    filename = function() {
      paste("taxa_rumen_MAGs", "csv", sep = ".")
    },
    content = function(file) {
      table = taxa_RUG
      utils::write.csv(table, file, row.names = FALSE)
    }
  )
  
  output$downloadTaxa_4 <- shiny::downloadHandler(
    filename = function() {
      paste("taxa_infant", "csv", sep = ".")
    },
    content = function(file) {
      table = taxa_infant
      utils::write.csv(table, file, row.names = FALSE)
    }
  )
  
  # Output modal with example files
  # Create modal
  shiny::observeEvent(input$taxonomy_file_modal, ignoreInit = TRUE, {
    shiny::showModal(shiny::modalDialog(
      shiny::h3("Example files"),
      htmltools::tags$ol(class = "circled-list",
                         htmltools::tags$li(shiny::downloadLink(outputId = ns("downloadTaxa_1"), label = "Previously uncharacterized bacteria")),
                         htmltools::tags$li(shiny::downloadLink(outputId = ns("downloadTaxa_2"), label = "Cultured prokaryotes from rumen")),
                         htmltools::tags$li(shiny::downloadLink(outputId = ns("downloadTaxa_3"), label = "MAGs from rumen")),
                         htmltools::tags$li(shiny::downloadLink(outputId = ns("downloadTaxa_4"), label = "OTUs from infant gut"))
      ),
      htmltools::div("Click ",
                     shiny::actionLink(ns("go_to_help"), "here"),
                     " to see detailed guidelines."),
      easyClose = TRUE, footer = NULL
    ))
  })
  
  shiny::observeEvent(input$go_to_help, {
    shiny::updateNavbarPage(session = x, inputId = "tabs", selected = "help")
    shiny::updateNavlistPanel(session = x, inputId = "navlist_panel", selected = "Predict traits from taxonomy")
    shiny::removeModal()
  })
  
  # Output number of matching organisms and traits
  output$summary_text = shiny::renderText(
    if(count_taxa() > 0 & nrow(get_query()) > 1) {
      paste0("Traits predicted for ", count_taxa(), " out of ", nrow(get_query()), " query taxa")
    }else if(count_taxa() > 0 & nrow(get_query()) == 1) {
      paste0("Traits predicted for ", nrow(get_query()), " query taxon")
    }else if(count_taxa() == 0 & nrow(get_query()) > 1) {
      paste0("No traits predicted for ", nrow(get_query()), " query taxa")
    }else{
      paste0("No traits predicted for ", nrow(get_query()), " query taxon")
    }
  )
  
  #Output flag for traits
  output$flag_multiple_traits = shiny::reactive({
    length(get_traits_to_predict()) > 1
  })
  shiny::outputOptions(output, "flag_multiple_traits", suspendWhenHidden = FALSE)
  
  # Output downloadable csv with matching results
  output$download_data <- shiny::downloadHandler(
    filename = function() {
      paste("results", "csv", sep = ".")
    },
    content = function(file) {
      sep <- switch("csv", "csv" = ",", "tsv" = "\t")
      
      # Write to a file specified by the 'file' argument
      utils::write.table(predict_traits(), file, sep = sep, row.names = FALSE)
    }
  )
  
  # Output table with predicted traits
  output$table <- DT::renderDataTable({
    predict_traits() %>% replace(is.na(.), "NA")
  }, 
  escape = FALSE, options = list(scrollX = TRUE))
  
  #Output overview plots
  shiny::observe({
    #Get data
    df = predict_traits()
    
    #Format variable name
    var_name = input$trait_to_display
    var_name_display = var_name
    var_name_display = gsub(pattern="(?<!for end product)s(\\b|$)", replacement="", x=var_name, perl=TRUE)
    var_name = gsub(pattern=" ", replacement="_", x=var_name)
    
    #Summary plot
    output$summary <- plotly::renderPlotly({
      df = taxonomy_results_to_plot(df = df, plot_type="summary", var_name = var_name)
      hovertemplate = paste0("<b>",var_name_display,": %{x}</b><br><b>% organisms positive: %{z:.2f}</b><br><extra></extra>")
      plot = plot_summary(df, 
                          coord_fixed = TRUE, 
                          hovertemplate = hovertemplate,
                          legend_labels = c("0", "25", "50", "75", "100"), 
                          legend_title = "% organisms positive")
      plot
    })
    
    #Treemap
    output$treemap <- plotly::renderPlotly({
      df = taxonomy_results_to_plot(df = df, plot_type="treemap", var_name = var_name)
      hovertemplate = paste0("<b>",var_name_display,": %{label}</b><br><b>% total: %{value:.2f}</b><br><extra></extra>")
      plot = plot_treemap(df, 
                   hovertemplate = hovertemplate)
      plot
    })
    
    #Heatmap
    output$heatmap <- plotly::renderPlotly({
      df = taxonomy_results_to_plot(df = df, plot_type="heatmap", var_name = var_name)
      
      hovertemplate = paste0("<b>",var_name_display,": %{x}</b><br><b>Organism: %{y}</b><br><b>Trait: %{text}</b><br><extra></extra>")
      plot = plot_heatmap(df, 
                          hovertemplate = hovertemplate,
                          data_are_binary = TRUE,
                          legend_labels = c("(-)", "(+)"),
                          legend_title = "Trait")
      plot
    })
  })
}
