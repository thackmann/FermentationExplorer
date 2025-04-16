# Functions for Loading Data for Shiny App
# This script contains functions for loading files and objects used by the app.
# These functions are called within modules of the app.  Loading with functions
# on demand speeds up app execution.  
# Author: Timothy Hackmann
# Date: 26 February 2025

# === Define functions ===
# --- Utility functions for loading data ---
  #' Check if an Object Exists and Load if Not Present
  #'
  #' This function checks if an object exists in the environment and loads it from a file if it is not present.
  #' It supports loading from CSV, TXT, RDS, and ZIP files containing these formats. 
  #' Optionally, it can force reload even if the object is already in the environment.
  #'
  #' @param file_path A character string specifying the path to the file.
  #' @param name The name of the object to load. If NULL, the object name is inferred from the file name.
  #' @param load_function The function to use for loading the file. If NULL, it is inferred from the file extension.
  #' @param envir The environment where the object should be loaded. Default is the global environment.
  #' @param force_reload Logical; if TRUE, reloads the object even if it exists in the environment.
  #' @param ... Additional arguments passed to the load function.
  #' @return The loaded object.
  #' @export
  #' @importFrom tools file_ext
  check_and_load <- function(file_path, name = NULL, load_function = NULL, envir = globalenv(), force_reload = FALSE, ...) {
    # Extract the object name if not provided
    if (is.null(name)) {
      file_name <- basename(file_path)  # Get the file name from the path
      name <- sub("\\..*$", "", file_name)  # Remove the extension to get the object name
    }
    
    # Check if the object exists and return it if force_reload is FALSE
    if (!force_reload && exists(name, envir = envir)) {
      return(get(name, envir = envir))
    }
    
    # Determine the file extension
    extension <- tools::file_ext(file_path)
    
    # Handle ZIP files
    if (extension == "zip") {
      temp_dir <- tempfile()
      dir.create(temp_dir)
      
      # Unzip files into the temporary directory
      unzip(file_path, exdir = temp_dir)
      
      # List extracted files and identify supported types
      extracted_files <- list.files(temp_dir, full.names = TRUE)
      valid_files <- extracted_files[grepl("\\.(csv|txt|rds)$", extracted_files)]
      
      if (length(valid_files) == 0) {
        stop("No CSV, TXT, or RDS files found in the ZIP archive.")
      }
      
      # Load the first valid file found
      file_path <- valid_files[1]
      extension <- tools::file_ext(file_path)
    }
    
    # Determine the load function if not provided
    if (is.null(load_function)) {
      load_function <- switch(extension,
                              "rds" = readRDS,
                              "csv" = function(file, ...) readr::read_csv(file, show_col_types = FALSE, ...),
                              "txt" = function(file, ...) readr::read_delim(file, delim = "\t", show_col_types = FALSE, ...),
                              "ko"  = function(file, ...) read.table(file, sep = "\t", header = FALSE, fill = TRUE, ...),
                              stop("Unsupported file extension"))
    }
    
    # Load the object
    obj <- do.call(load_function, c(list(file_path), list(...)))  # Pass additional arguments dynamically
    assign(name, obj, envir = envir)
    
    return(obj)
  }

  # Load Selected Models
  #'
  #' This function loads a list of models based on the selected model names and paths provided.
  #' The function checks and loads each model file from the specified paths.
  #'
  #' @param session The Shiny session object.
  #' @param model_names A character vector of selected model names to be loaded.
  #' @param model_paths A vector of file paths corresponding to the models.
  #' @param file_upload A Boolean describing if files are uploaded. 
  #' @return A named list of loaded model objects.
  #' @export
  #' @importFrom base lapply
  load_models <- function(session = getDefaultReactiveDomain(), 
                          model_names, model_paths, file_upload = FALSE) {
    if(!file_upload) {
      model_list <- lapply(seq_along(model_names), function(i) {
        obj <- check_and_load(file_path = model_paths[[i]])
        return(obj)
      })
    } else if(file_upload) {
      model_list <- lapply(seq_along(model_names), function(i) {
        obj <- validate_and_read_file(session = session, file_path = model_paths[[i]])
        return(obj)
      })
    }
    
    names(model_list) <- model_names
    return(model_list)
  }
  
# --- Functions for loading specific objects or files ---
  #' Load Clean Database
  #' 
  #' This function loads the clean (formatted) database from a CSV file.
  #' The data is loaded and stored in the environment if it is not already present, 
  #' unless `force_reload = TRUE` is specified.
  #' 
  #' @param force_reload Logical; if TRUE, reloads the database even if it exists in the environment.
  #' @return A data frame containing the clean database.
  #' @export
  #' @importFrom readr read_csv
  load_database <- function(force_reload = FALSE) {
    data_fp <- "data/database_clean.zip"
    
    obj <- check_and_load(
      file_path = data_fp, 
      force_reload = force_reload,
      col_types = readr::cols(`IMG Genome ID` = readr::col_character())
    )
    
    return(obj)
  }
  
  #' Load Raw Database
  #'
  #' This function loads the raw (unformated) database from a CSV file.
  #' The data is loaded and stored in the environment if it is not already present.
  #'
  #' @return A data frame containing the raw database
  #' @export
  load_raw_database <- function() {
    data_fp <- "data/database.zip"
    obj <- check_and_load(data_fp)
    
    return(obj)
  }
  
  #' Load Gene Functions
  #' 
  #' This function loads gene functions for all organisms 
  #' from the database with such information available.
  #' 
  #' @return A data frame of gene functions
  #' @export
  #' @importFrom readr read_csv
  load_gene_functions <- function() {
    data_fp <- "data/gene_functions_database.rds" 
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
  
  #' Load Placeholder Filters for Query Builder
  #'
  #' This function loads the query filters for the query builder from an RDS file.
  #' The filters are simple and used as placeholders until the full set of filters
  #' is loaded.
  #' The data is loaded and stored in the environment if it is not already present.
  #'
  #' @return A list containing the query filters
  #' @export
  load_placeholder_filters <- function() {
    data_fp <- "data/query_filters_simple.rds"
    
    obj <- check_and_load(data_fp)
    
    return(obj)
  }
  
  #' Load Taxa for Uncharacterized Bacteria
  #'
  #' This function loads an example dataset from a zip file.
  #' The data is loaded and stored in the environment if it is not already present.
  #'
  #' @return A list containing the query filters
  #' @export
  load_taxa_uncharacterized <- function() {
    data_fp <- "data/taxa_uncharacterized.zip"
    
    obj <- check_and_load(data_fp)
    
    return(obj)
  }
  
  #' Load Taxa for Cultured Rumen Bacteria
  #'
  #' This function loads an example dataset from a zip file.
  #' The data is loaded and stored in the environment if it is not already present.
  #'
  #' @return A list containing the query filters
  #' @export
  load_taxa_rumen_cultured <- function() {
    data_fp <- "data/taxa_rumen_cultured.zip"
    
    obj <- check_and_load(data_fp)
    
    return(obj)
  }
  
  #' Load Taxa for Rumen MAGs
  #'
  #' This function loads an example dataset from a zip file.
  #' The data is loaded and stored in the environment if it is not already present.
  #'
  #' @return A list containing the query filters
  #' @export
  load_taxa_rumen_MAGs <- function() {
    data_fp <- "data/taxa_rumen_MAGs.zip"
    
    obj <- check_and_load(data_fp)
    
    return(obj)
  }
  
  #' Load Taxa from Infants
  #'
  #' This function loads an example dataset from an zip file.
  #' The data is loaded and stored in the environment if it is not already present.
  #'
  #' @return A list containing the query filters
  #' @export
  load_taxa_infant <- function() {
    data_fp <- "data/taxa_infant.zip"
    
    obj <- check_and_load(data_fp)
    
    return(obj)
  }
  
  #' Load Gene Functions for E. coli
  #'
  #' This function loads an example dataset from a zip file.
  #' The data is loaded and stored in the environment if it is not already present.
  #'
  #' @return A list containing the query filters
  #' @export
  load_gene_functions_e_coli <- function() {
    data_fp <- "data/gene_functions_e_coli.zip"
    
    obj <- check_and_load(data_fp)
    
    return(obj)
  }
  
  #' Load Gene Functions for Uncharacterized Bacteria
  #'
  #' This function loads an example dataset from an zip file.
  #' The data is loaded and stored in the environment if it is not already present.
  #'
  #' @return A list containing the query filters
  #' @export
  load_gene_functions_uncharacterized <- function() {
    data_fp <- "data/gene_functions_uncharacterized.zip"
    
    obj <- check_and_load(data_fp)
    
    return(obj)
  }
  
  #' Load Gene Functions for Rumen Cultured Bacteria
  #'
  #' This function loads an example dataset from an zip file.
  #' The data is loaded and stored in the environment if it is not already present.
  #'
  #' @return A list containing the query filters
  #' @export
  load_gene_functions_rumen_cultured <- function() {
    data_fp <- "data/gene_functions_rumen_cultured.zip"
    
    obj <- check_and_load(data_fp)
    
    return(obj)
  }
  
  #' Load Gene Functions for Rumen MAGs
  #'
  #' This function loads an example dataset from an zip file.
  #' The data is loaded and stored in the environment if it is not already present.
  #'
  #' @return A list containing the query filters
  #' @export
  load_gene_functions_rumen_MAGs <- function() {
    data_fp <- "data/gene_functions_rumen_MAGs.zip"
    
    obj <- check_and_load(data_fp)
    
    return(obj)
  }
  
  #' Load Predictor Variables for Fermentation
  #'
  #' This function loads an example dataset from an zip file.
  #' The data is loaded and stored in the environment if it is not already present.
  #'
  #' @return A list containing the query filters
  #' @export
  load_predictors_fermentation <- function() {
    data_fp <- "data/predictors_fermentation.zip"
    
    obj <- check_and_load(data_fp)
    
    return(obj)
  }
  
  #' Load Predictor Variables for Methanogenesis
  #'
  #' This function loads an example dataset from an zip file.
  #' The data is loaded and stored in the environment if it is not already present.
  #'
  #' @return A list containing the query filters
  #' @export
  load_predictors_methanogenesis <- function() {
    data_fp <- "data/predictors_methanogenesis.zip"
    
    obj <- check_and_load(data_fp)
    
    return(obj)
  }
  
  #' Load Response Variable for Fermentation
  #'
  #' This function loads an example dataset from an zip file.
  #' The data is loaded and stored in the environment if it is not already present.
  #'
  #' @return A list containing the query filters
  #' @export
  load_response_fermentation <- function() {
    data_fp <- "data/response_fermentation.zip"

    obj <- check_and_load(data_fp)
    
    return(obj)
  }
  
  #' Load Response Variable for Methanogenesis
  #'
  #' This function loads an example dataset from an zip file.
  #' The data is loaded and stored in the environment if it is not already present.
  #'
  #' @return A list containing the query filters
  #' @export
  load_response_methanogenesis <- function() {
    data_fp <- "data/response_methanogenesis.zip"

    obj <- check_and_load(data_fp)
    
    return(obj)
  }
  
  #' Load Reference Reactions for Glucose Fermentation
  #'
  #' This function loads an example dataset from an zip file.
  #' The data is loaded and stored in the environment if it is not already present.
  #'
  #' @return A list containing the query filters
  #' @export
  load_reference_reactions_glucose_fermentation <- function() {
    data_fp <- "data/reference_reactions_glucose_fermentation.zip"
    
    obj <- check_and_load(data_fp)
    
    return(obj)
  }
  
  #' Load Reference Reactions for Fructose Fermentation
  #'
  #' This function loads an example dataset from an zip file.
  #' The data is loaded and stored in the environment if it is not already present.
  #'
  #' @return A list containing the query filters
  #' @export
  load_reference_reactions_fructose_fermentation <- function() {
    data_fp <- "data/reference_reactions_fructose_fermentation.zip"
    
    obj <- check_and_load(data_fp)
    
    return(obj)
  }
  
  #' Load Reference Reactions for Methanogenesis
  #'
  #' This function loads an example dataset from an zip file.
  #' The data is loaded and stored in the environment if it is not already present.
  #'
  #' @return A list containing the query filters
  #' @export
  load_reference_reactions_methanogenesis <- function() {
    data_fp <- "data/reference_reactions_methanogenesis.zip"
    
    obj <- check_and_load(data_fp)
    
    return(obj)
  }
  
  #' Load Reference Reactions for Glycolysis
  #'
  #' This function loads an example dataset from an zip file.
  #' The data is loaded and stored in the environment if it is not already present.
  #'
  #' @return A list containing the query filters
  #' @export
  load_reference_reactions_glycolysis <- function() {
    data_fp <- "data/reference_reactions_glycolysis.zip"
    
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
  
  #' Load Layout Tree Data in Daylight Format
  #'
  #' This function loads the layout tree data in daylight format from a rds file.
  #' The data is loaded and stored in the environment if it is not already present.
  #'
  #' @return A data frame containing the layout tree data in daylight format.
  #' @export
  load_layout_tree_daylight <- function() {
    data_fp <- "data/layout_tree_daylight.rds"
    obj <- check_and_load(data_fp)
    
    return(obj)
  }
  
  #' Load Layout Tree Data in Equal Angle Format
  #'
  #' This function loads the layout tree data in equal angle format from a rds file.
  #' The data is loaded and stored in the environment if it is not already present.
  #'
  #' @return A data frame containing the layout tree data in equal angle format.
  #' @export
  load_layout_tree_equal_angle <- function() {
    data_fp <- "data/layout_tree_equal_angle.rds"
    obj <- check_and_load(data_fp)
    
    return(obj)
  }
  
  #' Load Layout Tree Data in Rectangular Format
  #'
  #' This function loads the layout tree data in rectangular format from a rds file.
  #' The data is loaded and stored in the environment if it is not already present.
  #'
  #' @return A data frame containing the layout tree data in rectangular format.
  #' @export
  load_layout_tree_rectangular <- function() {
    data_fp <- "data/layout_tree_rectangular.rds"
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
    data_fp <- "data/layout_tsne.rds"
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
  #' This function loads the nodes to root data from a rds file.
  #' The data is loaded and stored in the environment if it is not already present.
  #'
  #' @return A data frame containing the nodes to root data.
  #' @export
  load_nodes_to_root <- function() {
    data_fp <- "data/nodes_to_root.rds"
    obj <- check_and_load(data_fp)
    
    return(obj)
  }

  
  
  
  #' Load Placeholder Filters for Query Builder
  #'
  #' This function loads the query filters for the query builder from an RDS file.
  #' The filters are simple and used as placeholders until the full set of filters
  #' is loaded.
  #' The data is loaded and stored in the environment if it is not already present.
  #'
  #' @return A list containing the query filters
  #' @export
  load_query_filters_simple <- function() {
    data_fp <- "data/query_filters_simple.rds"
    
    obj <- check_and_load(data_fp)
    
    return(obj)
  }