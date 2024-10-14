# Define the Predictions from Taxonomy Module in Shiny App
# This script defines the user interface (UI) and server for the predictions from taxonomy module.
# It also includes functions and variables specific to this module.  
# Author: Timothy Hackmann
# Date: 14 October 2024

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
    #' @param traits_to_exclude A character vector of traits that should not be reported as shared traits. Default is "-".
    #' @return A character vector of shared traits.
    #' @export
    #' @importFrom dplyr ncol
    determine_shared_traits <- function(match, threshold = 0.5, traits_to_exclude = "-", ignore_NA = TRUE) {
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
      
      if (!is.null(traits_to_exclude) && length(traits_to_exclude) > 0) {
        shared_traits[shared_traits %in% traits_to_exclude] <- NA
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
    #' @param traits_to_exclude A character vector of traits that should not be reported as shared traits. Default is "-".
    #' @param ignore_NA Logical. If TRUE, organisms with NA are not counted. Default is TRUE.
    #' @return A character vector of shared traits.
    #' @export
    #' @importFrom dplyr filter mutate select
    process_query_taxon <- function(table, query_taxon, threshold = 0.5, traits_to_exclude = "-", ignore_NA = TRUE) {
      # Filter the table to find matching taxa
      match <- filter_table_by_taxon(table, query_taxon)

      # Determine shared traits
      shared_traits <- determine_shared_traits(match, threshold, traits_to_exclude = traits_to_exclude, ignore_NA = ignore_NA)
      
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
      pattern <- "^\\s*k__[^;]+(;\\s*p__[^;]+)?(;\\s*c__[^;]+)?(;\\s*o__[^;]+)?(;\\s*f__[^;]+)?(;\\s*g__[^;]+)?(;\\s*s__[^;]+)?\\s*$"
      all(grepl(pattern, data$Taxonomy, perl = TRUE))
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
        df <- data.frame(matrix(NA, nrow = nrow(query), ncol = 6))
        colnames(df) <- c("Phylum", "Class", "Order", "Family", "Genus", "Species")
        prefix <- c("p__", "c__", "o__", "f__", "g__", "s__")
        
        for (i in seq_len(nrow(df))) {
          for (j in seq_len(ncol(df))) {
            pattern <- paste0(prefix[j], ".*?(;|$)")
            df[i, j] <- stringr::str_extract(query[i, ], pattern = pattern)
            df[i, j] <- gsub(".__", "", df[i, j])
            df[i, j] <- gsub(";", "", df[i, j])
          }
        }
        query <- as.data.frame(df)
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
        data = data %>% dplyr::filter(is.na(`NCBI Phylum`)|(`NCBI Phylum` != "NA"))
        data$Phylum = data$`NCBI Phylum`
        data$Class = data$`NCBI Class`
        data$Order = data$`NCBI Order`
        data$Family = data$`NCBI Family`
        data$Genus = data$`NCBI Genus`
        data$Species = data$`NCBI Species`
      } else if(taxonomy_type == "Bergey") {
        data = data %>% dplyr::filter(is.na(Phylum)|(Phylum != "NA"))
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
    
    #' Add Custom Traits Based on Query
    #'
    #' This function processes a data table by applying a query to identify organisms with 
    #' positive traits, then adds a 'Custom trait' column to indicate which organisms have 
    #' these traits.
    #'
    #' @param data A dataframe containing the organism data.
    #' @param query_string A string representing the query used to filter the organisms.
    #' @param ignore_NA Logical. If TRUE, organisms with NA values are excluded from the final result. Default is FALSE.
    #' @return A dataframe with a 'Custom trait' column added, indicating positive traits.
    #' @export
    #' @importFrom dplyr mutate if_else left_join filter
    #' @importFrom tidyr replace_na
    add_custom_traits <- function(data, query_string, ignore_NA = FALSE) {
      
      # Validate that the query string is not empty
      if(query_string == "") {
        stop("Please build a valid query.")
      }
      
      # Add an index column to the data
      data$index <- seq_len(nrow(data))
      
      # Get data for organisms with positive traits
      data_positive <- filter_data_by_query(data, query_string)
      
      # Add a new column 'Custom trait' to data_positive
      data_positive <- data_positive %>%
        dplyr::mutate(`Custom trait` = dplyr::if_else(dplyr::n() > 0, "+", NA_character_))
      
      # Get data for all organisms (excluding those with NA values if specified)
      data_all <- if (ignore_NA) {
        filter_data_excluding_na(data, query_string)
      } else {
        data
      }
      
      # Merge the positive trait data back using the index column
      data <- dplyr::left_join(data_all, 
                               data_positive %>% dplyr::select(index, `Custom trait`), 
                               by = "index")
      
      # Fill NA values in 'Custom trait' column with "-"
      data$`Custom trait` <- tidyr::replace_na(data$`Custom trait`, "-")
      
      return(data)
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
      
      # Store x values for later use
      x_values <- df$x
      
      # Pivot longer and separate rows by semicolon
      df <- df %>%
        tidyr::pivot_longer(cols = -x, names_to = "var", values_to = "y") %>%
        tidyr::separate_rows(y, sep = ";") %>%
        dplyr::filter(!is.na(y))
      if(nrow(df)==0)
      {
        return(df)
      }else if(plot_type == "summary") {
        # Further formatting
        df <- df %>%
          dplyr::group_by(var) %>%
          dplyr::mutate(z = 100) %>%
          tidyr::complete(x, y) %>%
          dplyr::mutate(z = ifelse(!is.na(z), 100, 0)) %>%
          dplyr::ungroup()
        
        # Fill in missing x
        # Ensure all x are present within each var
        df <- df %>%
          dplyr::group_by(var) %>% 
          tidyr::complete(x = x_values, fill = list(z = 0)) %>%
          dplyr::ungroup()
        
        # Fill in missing combinations of x and y
        # Ensure complete combinations of x for each y within each var
        df <- df %>%
          dplyr::group_by(y, var) %>%
          tidyr::complete(x = unique(df$x), fill = list(z = 0)) %>%
          dplyr::filter(!is.na(y)) %>%
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
        
        # Fill in missing x
        # Ensure all x are present within each var
        df <- df %>%
          dplyr::group_by(var) %>% 
          tidyr::complete(x = x_values, fill = list(z = 0)) %>%
          dplyr::ungroup()
        
        # Fill in missing combinations of x and y
        # Ensure complete combinations of x for each y within each var
        df <- df %>%
          dplyr::group_by(y, var) %>%
          tidyr::complete(x = unique(df$x), fill = list(z = 0)) %>%
          dplyr::filter(!is.na(y)) %>%
          dplyr::ungroup()
        
        # Filter by var
        df = filter_var(df, var_name) 
        
        #Pivot wider
        df <- df %>%
          tidyr::pivot_wider(names_from = y, values_from = z)
        
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
# Choices for traits
choices_traits = c(
                  # Physiology/Function
                  "Type of metabolism", "End products", "Major end products", "Minor end products", 
                  "Substrates for end products", "Oxygen tolerance", "Pathogenicity", 
                  "Temperature category", 
                  "Indole test", 
                  "Antibiotic resistance", "Antibiotic sensitivity", "FAPROTAX predicted metabolism",
                  
                  # Morphology
                  "Cell shape", 
                  "Flagellum arrangement", "Gram stain", "Spore formation",
                  
                  # Isolation traits
                  "Isolation source category 1", "Isolation source category 2", "Isolation source category 3"
                )

# Variables for showing conditional panels
taxonomy_hide_results = "(input.subtabs == 'Standard traits' & (input.make_predictions_standard == 0 | output.check_file_taxonomy_standard))|
                        (input.subtabs == 'Other traits' & (input.make_predictions_other == 0 | output.check_file_taxonomy_other))"

taxonomy_show_results = "(input.subtabs == 'Standard traits' & input.make_predictions_standard > 0 & !output.check_file_taxonomy_standard)|
                        (input.subtabs == 'Other traits' & input.make_predictions_other > 0 & !output.check_file_taxonomy_other)"

# Filters for query builder
query_filters_taxonomy = load_query_filters()
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
query_filters_taxonomy = purrr::keep(query_filters_taxonomy, ~ .x$id %in% traits_to_keep)

# Rules for query builder
query_rules_taxonomy <- list(
  condition = "AND",
  rules = list(
    list(
      id = "Type of metabolism",
      operator = "in"
    )
  )
)

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
        shiny::h3("Predict traits from taxonomy")
    ),

    #Sidebar
    bslib::layout_sidebar(
      #Sidebar
      sidebar = bslib::sidebar(
        width = "30%",
        bslib::navset_tab(id = ns("subtabs"),
                          bslib::nav_panel(title = "Standard traits",
                                           fileInput_modal(ns("file_taxonomy_standard"), "Upload names of taxa", multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"), modalId = ns("taxonomy_file_modal_standard"), modalLabel = "Download example"),
                                           shinyWidgets::pickerInput(inputId = ns("set_traits_standard"), label = "Traits to predict", choices = choices_traits, selected = c("Type of metabolism", "Substrates for end products", "End products"), multiple = TRUE, options = list(`actions-box` = TRUE)),
                                           shiny::sliderInput(ns("threshold_standard"), "Prediction threshold", min = 0, max = 1, value = 0.5),
                                           shiny::div(class = "vertical-container",
                                                      "Simplify names of taxa",
                                                      shinyWidgets::switchInput(inputId = ns("simplify_names_standard"), value = TRUE,  size = "small", inline = TRUE)
                                           ),
                                           shiny::div(class = "vertical-container",
                                                      "Ignore missing values in database",
                                                      shinyWidgets::switchInput(inputId = ns("ignore_missing_standard"), value = TRUE,  size = "small", inline = TRUE)
                                           ),
                                           shinyWidgets::radioGroupButtons(inputId = ns("predictionsTaxonomy_filter_standard"), label = "Taxonomy", choices = list("NCBI", "Bergey"), selected = list("NCBI")),
                                           shiny::actionButton(ns("make_predictions_standard"), "Make predictions", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                          ),
                          bslib::nav_panel(title = "Other traits", 
                                           fileInput_modal(ns("file_taxonomy_other"), "Upload names of taxa", multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"), modalId = ns("taxonomy_file_modal_other"), modalLabel = "Download example"),
                                           # shinyWidgets::pickerInput(inputId = ns("set_traits_other"), label = "Traits to predict", choices = choices_traits, selected = c("Type of metabolism", "Substrates for end products", "End products"), multiple = TRUE, options = list(`actions-box` = TRUE)),
                                           div(
                                             "Choose trait",
                                             jqbr::queryBuilderInput(
                                               inputId = ns("query_builder"),
                                               filters = query_filters_taxonomy,
                                               return_value = "r_rules",
                                               display_errors = TRUE,
                                               rules = query_rules_taxonomy,
                                               add_na_filter = FALSE
                                             )
                                           ),
                                           # shinyWidgets::pickerInput(inputId = ns("set_traits_other"), label = "Traits to predict", choices = choices_traits, selected = c("Type of metabolism", "Substrates for end products", "End products"), multiple = TRUE, options = list(`actions-box` = TRUE)),
                                           shiny::sliderInput(ns("threshold_other"), "Prediction threshold", min = 0, max = 1, value = 0.5),
                                           shiny::div(class = "vertical-container",
                                                      "Simplify names of taxa",
                                                      shinyWidgets::switchInput(inputId = ns("simplify_names_other"), value = TRUE,  size = "small", inline = TRUE)
                                           ),
                                           shiny::div(class = "vertical-container",
                                                      "Ignore missing values in database",
                                                      shinyWidgets::switchInput(inputId = ns("ignore_missing_other"), value = TRUE,  size = "small", inline = TRUE)
                                           ),
                                           shinyWidgets::radioGroupButtons(inputId = ns("predictionsTaxonomy_filter_other"), label = "Taxonomy", choices = list("NCBI", "Bergey"), selected = list("NCBI")),
                                           shiny::actionButton(ns("make_predictions_other"), "Make predictions", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                          )
        ),
      ),
      
      #Main content area
      div(
        id = ns("results_page"),
        
        shiny::conditionalPanel(
          condition = taxonomy_hide_results,
          ns = ns,
          shiny::h4("Please upload files and make selections at left")
        ),
        
        shiny::conditionalPanel(
          condition = taxonomy_show_results,
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
                    shiny::selectInput(inputId = ns("trait_to_display_summary"), label = "Trait", choices = "", selected = "End products", multiple = FALSE, selectize = TRUE, width = "100%")
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
  
  # --- Define triggers for reactive expressions ---
  make_predictions_trigger <- reactive({
    input$make_predictions_standard |
      input$make_predictions_other 
  })

  # --- Get user input (events) ---
  # Get query taxa
  get_query_taxa <- shiny::eventReactive({make_predictions_trigger()},
  {
    # Get file path
    if (input$subtabs == "Standard traits") {
      fp = input$file_taxonomy_standard$datapath
    }else if(input$subtabs == "Other traits"){
      fp = input$file_taxonomy_other$datapath
    }
    
    # Validate, read, and process the taxonomy file
    query <- validate_and_read_csv(fp)
    query <- process_uploaded_taxonomy(query)
    
    runValidationModal(need((!is.null(query)) && (nrow(query) > 0), "Please check the format of your uploaded file and try again."))
    
    return(query)
  },
  ignoreNULL = TRUE,  ignoreInit = FALSE, label="get_query_taxa")
  
  # Get query string (from query builder)
  get_query_string <- shiny::eventReactive({make_predictions_trigger()},
  {
    query_string  = input$query_builder

    runValidationModal(need(query_string != "", "Please build a valid query."))

    return(query_string)
  },
  ignoreNULL = TRUE,  ignoreInit = FALSE, label="get_query_string")
  
  # Get table of organisms in database
  get_table <- shiny::eventReactive({make_predictions_trigger()},
  {
    # Get data
    data = clean_data
    
    # Filter according to preferred taxonomy
    if (input$subtabs == "Standard traits") {
      taxonomy_type = input$predictionsTaxonomy_filter_standard
    }else if(input$subtabs == "Other traits"){
      taxonomy_type = input$predictionsTaxonomy_filter_other
    }
    
    data = filter_by_taxonomy(data = data, taxonomy_type = taxonomy_type)
    
    # Add custom traits
    if(input$subtabs == "Other traits") {
      query_string = get_query_string()
      ignore_NA = input$ignore_missing_other
      
      data <- add_custom_traits(data = data, query_string = query_string, ignore_NA = ignore_NA)
    }

    return(data)
  },
  ignoreNULL = TRUE,  ignoreInit=FALSE, label="get_table")
  
  # Get traits
  get_traits_to_predict <- shiny::eventReactive({make_predictions_trigger()},
  {
    # Get inputs
    if (input$subtabs == "Standard traits") {
      traits_to_predict = input$set_traits_standard
    }else if(input$subtabs == "Other traits"){
      traits_to_predict = "Custom trait"
    }
    
    runValidationModal(need(traits_to_predict != "", "Please choose a trait"))
    
    return(traits_to_predict)
  },
  ignoreNULL = TRUE,  ignoreInit=TRUE, label="get_traits_to_predict")  # Must have ignoreInit = TRUE, or module runs on app start up
  
  # Get prediction threshold
  get_prediction_threshold <- shiny::eventReactive({make_predictions_trigger()},
  {
    # Get inputs
    if (input$subtabs == "Standard traits") {
      threshold = input$threshold_standard
    }else if(input$subtabs == "Other traits"){
      threshold = input$threshold_other
    }
    
    return(threshold)
  },
  ignoreNULL = TRUE,  ignoreInit=FALSE, label="get_prediction_threshold")
  
  #--- Process input ---
  # Format query taxa
  format_query_taxa <- shiny::eventReactive({make_predictions_trigger()},
  {
    # Get data
    query = get_query_taxa()
    
    # Get inputs
    if (input$subtabs == "Standard traits") {
      simplify_names = input$simplify_names_standard
    }else if(input$subtabs == "Other traits"){
      simplify_names = input$simplify_names_other
    }
    
    #Launch modal with progress bar
    display_modal(session, ns("pb"), message = "Retrieving data")
    
    # Replace missing values with NA
    query[query == ""] = NA
    query[query == "?"] = NA
    query[query == "unclassified"] = NA
    
    # Remove brackets
    query = data.frame(lapply(query, function(x) gsub("\\[|\\]", "", x)))
    
    # Simplify names
    if(simplify_names == TRUE) {
      query = as.data.frame(t(apply(query, 1, simplify_names, colnames(query))))
    }
    
    return(query)
  },
  ignoreNULL = TRUE,  ignoreInit=FALSE, label="format_query_taxa")
  
  # Get predictions
  predict_traits <- shiny::eventReactive({make_predictions_trigger()},
  {
    # Get inputs
    if (input$subtabs == "Standard traits") {
      ignore_NA = input$ignore_missing_standard
    }else if(input$subtabs == "Other traits"){
      ignore_NA = input$ignore_missing_other
    }
    
    table = get_table()
    traits_to_predict = get_traits_to_predict()
    
    query = format_query_taxa()

    threshold = get_prediction_threshold()
    
    # Format table
    table = table %>% dplyr::select(Phylum, Class, Order, Family, Genus, Species, dplyr::all_of(traits_to_predict))
    
    # Expand columns with ";" delimited values into multiple columns
    columns_to_expand <- names(table)[sapply(table, function(col) any(grepl(";", col, fixed = TRUE)))]
    table <- expand_delimited_columns(df = table, columns_to_expand = columns_to_expand)
    
    # Format query taxa as regular expressions (for matching)
    query_regex <- as.data.frame(apply(query, c(1, 2), format_query_element))
    
    # Update modal with progress bar
    display_modal(session, ns("pb"), message = "Prediction in progress", value = 0)
    
    # Print status to log
    cat(file = stderr(), paste0("Started prediction at ", Sys.time(), "\n"))
    
    # Search table for matches to query taxa
    # Initialize values
    exclude_columns <- c("Phylum", "Class", "Order", "Family", "Genus", "Species")
    traits <- create_empty_dataframe(data = table, exclude_columns = exclude_columns, num_rows = nrow(query))
    
    # Perform matching
    for(i in seq_len(nrow(query))) {
      # Process the current query taxon and store the result in the traits dataframe
      traits[i, ] <- process_query_taxon(table = table, query_taxon = query_regex[i, ], threshold = threshold, traits_to_exclude = "-", ignore_NA = ignore_NA)
      
      # Update the progress bar
      display_modal(session, ns("pb"), message = "Prediction in progress", value = 1 / nrow(query) * 100 * i)
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
  },
  ignoreNULL = TRUE,  ignoreInit=FALSE, label="predict_traits")
  
  # Count taxa with at least one predicted trait
  count_taxa <- shiny::eventReactive({make_predictions_trigger()},
  {
    # Get inputs
    df = predict_traits()
    
    exclude_columns <- c("Phylum", "Class", "Order", "Family", "Genus", "Species")
    
    df = df %>% dplyr::select(-dplyr::all_of(exclude_columns))
    n_taxa = sum(rowSums(!is.na(df)) > 0)

    return(n_taxa)
  },
  ignoreNULL = TRUE,  ignoreInit=FALSE, label="count_taxa")
  
  # --- Update selections ---
  # Update choices for trait to display
  shiny::observeEvent({make_predictions_trigger()},
  {
    x = get_traits_to_predict()
    
    if (is.null(x))
      x <- character(0)
    
    shiny::updateSelectInput(session, inputId = "trait_to_display_summary", choices = x, selected = head(x, 1))
    shiny::updateSelectInput(session, inputId = "trait_to_display_treemap", choices = x, selected = head(x, 1))
    shiny::updateSelectInput(session, inputId = "trait_to_display_heatmap", choices = x, selected = head(x, 1))
  })
  
  # Synchronize selections for trait to display
  selected_trait <- reactiveVal("End products")

  shiny::observeEvent(input$trait_to_display_summary, {
      selected_trait(input$trait_to_display_summary)
  })

  shiny::observeEvent(input$trait_to_display_treemap, {
      selected_trait(input$trait_to_display_treemap)
  })

  shiny::observeEvent(input$trait_to_display_heatmap, {
      selected_trait(input$trait_to_display_heatmap)
  })

  shiny::observeEvent(selected_trait(), {
      shiny::updateSelectInput(session, "trait_to_display_summary", selected = selected_trait())
      shiny::updateSelectInput(session, "trait_to_display_treemap", selected = selected_trait())
      shiny::updateSelectInput(session, "trait_to_display_heatmap", selected = selected_trait())
  })
  
  # --- Generate outputs ---
  # Output file upload status
  output$check_file_taxonomy_standard = shiny::reactive({
    is.null(input$file_taxonomy_standard$datapath)
  })
  shiny::outputOptions(output, "check_file_taxonomy_standard", suspendWhenHidden = FALSE)
  
  output$check_file_taxonomy_other = shiny::reactive({
    is.null(input$file_taxonomy_other$datapath)
  })
  shiny::outputOptions(output, "check_file_taxonomy_other", suspendWhenHidden = FALSE)
  
  # Output example data for download
  output$downloadTaxa_1 <- create_download_handler("taxa_uncharacterized", function() taxa_uncharacterized)
  output$downloadTaxa_2 <- create_download_handler("taxa_rumen_cultured", function() taxa_rumen_cultured)
  output$downloadTaxa_3 <- create_download_handler("taxa_rumen_MAGs", function() taxa_rumen_MAGs)
  output$downloadTaxa_4 <- create_download_handler("taxa infant", function() taxa_infant)
  
  # Output modal with example files
  shiny::observeEvent(input$taxonomy_file_modal_standard, ignoreInit = TRUE, {
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
  
  shiny::observeEvent(input$taxonomy_file_modal_other, ignoreInit = TRUE, {
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
    if(count_taxa() > 0 & nrow(get_query_taxa()) > 1) {
      paste0("Traits predicted for ", count_taxa(), " out of ", nrow(get_query_taxa()), " query taxa")
    }else if(count_taxa() > 0 & nrow(get_query_taxa()) == 1) {
      paste0("Traits predicted for ", nrow(get_query_taxa()), " query taxon")
    }else if(count_taxa() == 0 & nrow(get_query_taxa()) > 1) {
      paste0("No traits predicted for ", nrow(get_query_taxa()), " query taxa")
    }else{
      paste0("No traits predicted for ", nrow(get_query_taxa()), " query taxon")
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
    var_name = input$trait_to_display_summary
    var_name_display = var_name
    var_name_display = gsub(pattern="(?<!for end product)s(\\b|$)", replacement="", x=var_name, perl=TRUE)
    
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