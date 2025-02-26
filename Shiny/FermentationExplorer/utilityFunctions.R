# Utility Functions for App
# This script contains various utility functions for use in the Shiny app,
# including custom operators, file input handling, validation, and data cleaning.
# Author: Timothy Hackmann
# Date: 18 February 2025

# === Define functions ===
  #' Pipe Operator
  #'
  #' This operator is imported from the magrittr package and is used to chain operations together.
  #'
  import::from(magrittr, "%>%")

  #' Null Defaults Operator
  #'
  #' This custom operator returns the first argument if it is not NULL, otherwise returns the second argument.
  #'
  #' @param x The value to check for NULL.
  #' @param y The default value to return if `x` is NULL.
  #' @return Returns `x` if it is not NULL; otherwise, returns `y`.
  #' @export
  `%||%` <- function(x, y) {
    if (!is.null(x)) x else y
  }
  
  #' Negation of In Operator
  #'
  #' This custom operator returns the negation of the `%in%` operator, 
  #' checking if elements are not present in a vector or list.
  #'
  #' @param x The values to check.
  #' @param table The vector or list to check against.
  #' @return A logical vector indicating if elements of `x` are not in `table`.
  #' @export
  #' @importFrom base Negate
  `%nin%` = Negate(`%in%`)
  
  #' Round Values to Binary
  #'
  #' This function rounds numeric values to 0 or 1 based on a threshold of 0.5.
  #'
  #' @param x A numeric vector of values to be rounded.
  #' @return A numeric vector with values rounded to 0 or 1.
  #' @export
  make_binary <- function(x) {
    ifelse(x >= 0.5, 1, 0)
  }
  
  #' Assign a Default Value if Input is Invalid
  #'
  #' This function checks whether an input value (`x`) is `NULL`, `NA`, or an empty string (`""`). 
  #' If so, it assigns and returns the specified `default` value. Otherwise, it returns `x` unchanged.
  #'
  #' @param x The input value to check.
  #' @param default The default value to return if `x` is invalid (`NULL`, `NA`, or `""`).
  #'
  #' @return The original `x` if valid, or `default` if `x` is `NULL`, `NA`, or an empty string.
  #' 
  #' @examples
  #' assign_if_invalid(NULL, "LPSN")  # Returns "LPSN"
  #' assign_if_invalid(NA, "LPSN")    # Returns "LPSN"
  #' assign_if_invalid("", "LPSN")    # Returns "LPSN"
  #' assign_if_invalid("Valid", "LPSN") # Returns "Valid"
  #'
  #' @export
  assign_if_invalid <- function(x, default) {
    if (is.null(x) || is.na(x) || !nzchar(x)) {
      return(default)
    }
    return(x)
  }
  
  #' Detect Columns Matching a Pattern
  #'
  #' This function detects which columns in a dataframe contain values matching a specified pattern.
  #'
  #' @param data A dataframe to search for matching patterns.
  #' @param pattern A character string specifying the regular expression pattern to match.
  #' @return A character vector of column names that match the pattern, or NA if none are found.
  #' @export
  #' @importFrom base grepl
  detect_pattern_column <- function(data, pattern) {
    matching_columns <- vector("character")
  
    for (col_name in colnames(data)) {
      if (any(grepl(pattern, data[[col_name]], perl = TRUE))) {
        matching_columns <- c(matching_columns, col_name)
      }
    }
    
    if (length(matching_columns) == 0) {
      return(NA)
    } else {
      return(matching_columns)
    }
  }
  
  #' Detect Delimiter and Header Presence in File
  #'
  #' This function detects the delimiter used in a file (comma or tab) and checks if the file contains a header row.
  #'
  #' @param filepath A character string specifying the path to the file.
  #' @return A list containing the detected delimiter and a logical value indicating if the file has a header.
  #' @export
  #' @importFrom utils readLines
  #' @importFrom stringr str_detect
  detect_delim_and_header <- function(filepath) {
    # Read first few lines
    lines <- readLines(filepath, n = 10)
    
    # Detect delimiter
    delim <- ifelse(lengths(strsplit(lines, ",")) >= lengths(strsplit(lines, "\t")), ",", "\t")
    delim = as.character(names(table(delim))[which.max(table(delim))])
    
    # Detect presence of a header
    pattern <- "^K[0-9]{5}$"
    has_header <- TRUE
    first_line <- strsplit(lines[1], delim)[[1]]
    
    for (value in first_line) {
      if (stringr::str_detect(value, pattern) || value == "") {
        has_header <- FALSE
        break
      }
    }
    
    return(list(delim = delim, has_header = has_header))
  }
  
  #' Read File with Custom Delimiter and Header Settings
  #'
  #' This function reads a file using the appropriate delimiter and header settings as detected by `detect_delim_and_header`.
  #'
  #' @param filepath A character string specifying the path to the file.
  #' @return A dataframe containing the data read from the file.
  #' @export
  read_custom_file <- function(filepath) {
    info <- detect_delim_and_header(filepath)
    delim <- info$delim
    has_header <- info$has_header
    
    data <- read.table(filepath, sep = delim, header = has_header, stringsAsFactors = FALSE, fill = TRUE)
    return(data)
  }
  
  #' Create a Label for Shiny Input
  #'
  #' This helper function creates a label for a Shiny input element.
  #'
  #' @param inputId The input ID for the label.
  #' @param label A character string specifying the label text.
  #' @return A Shiny UI element for the label.
  #' @export
  #' @importFrom shiny tags
  shinyInputLabel <- function(inputId, label = NULL) {
    shiny::tags$label(
      label,
      class = "control-label",
      class = if (is.null(label)) "shiny-label-null",
      `for` = inputId
    )
  }
  
  #' Create a Shiny Download Handler for Multiple File Types
  #'
  #' This function generates a Shiny download handler for downloading either `.csv` or `.rds` files.
  #' The handler dynamically creates filenames based on a provided prefix, loads the data using a function or object, 
  #' and saves the data in the specified file format.
  #'
  #' @param filename_prefix A character string or reactive used as the prefix for the downloaded file's name.
  #' @param data_source An object or function returns the data to be downloaded. 
  #' @param file_type A character string indicating the file type for download. 
  #'        Can be either "csv" or "rds". Default is "csv".
  #' @return A Shiny download handler, ready to be assigned to an output object in a Shiny server function.
  #' @export
  #' @examples
  #' \dontrun{
  #' # In a Shiny server function:
  #' output$downloadData <- create_download_handler("gene_functions", function() gene_functions_data)
  #'
  #' # For downloading an RDS file:
  #' output$downloadModel <- create_download_handler("model_methanogenesis", load_model_methanogenesis, file_type = "rds")
  #' }
  create_download_handler <- function(filename_prefix, data_source, file_type = "csv") {
    shiny::downloadHandler(
      filename = function() {
        # Dynamically evaluate reactive expressions inside the function
        prefix <- if (is.reactive(filename_prefix)) filename_prefix() else filename_prefix
        paste0(prefix, ".", file_type)
      },
      content = function(file) {
        # Ensure data_source is evaluated dynamically
        data <- if (is.function(data_source)) data_source() else data_source
        
        if (file_type == "csv") {
          write.csv(data, file, row.names = FALSE)
        } else if (file_type == "rds") {
          saveRDS(data, file)
        } else if (file_type == "txt") {
          writeLines(data, file)
        } else {
          stop("Unsupported file type")
        }
      }
    )
  }
  
  
  #' Show a Shiny Modal with Download Links
  #'
  #' @param ns A namespace function (for module compatibility, default is identity).
  #' @param title The title of the modal.
  #' @param downloads A named list where names are output IDs and values are labels for the download links.
  #' @param help_link_id The input ID for the help action link.
  #' @export
  showDownloadModal <- function(ns = identity, title = "Example files", downloads, help_link_id = "go_to_help") {
    shiny::showModal(shiny::modalDialog(
      shiny::h3(title),
      htmltools::tags$ol(class = "circled-list",
                         lapply(names(downloads), function(output_id) {
                           htmltools::tags$li(shiny::downloadLink(outputId = ns(output_id), label = downloads[[output_id]]))
                         })
      ),
      htmltools::div("Click ",
                     shiny::actionLink(ns(help_link_id), "here"),
                     " to see detailed guidelines."
      ),
      easyClose = TRUE, footer = NULL
    ))
  }
  
  #' Validate Input and Launch Modal on Error
  #'
  #' This function validates Shiny input and launches a modal dialog with an error message if validation fails.
  #' It is modified from shiny::validate() to launch modal rather than returning a simple text output. 
  #' 
  #' @param session The Shiny session object.
  #' @param ... Validation conditions to check.
  #' @param errorClass A character vector of error classes to assign to the error. Default is an empty character vector.
  #' @param delay_time The delay in milliseconds before launching modal. 
  #' @return Invisible if validation passes; otherwise, stops the reactive process with a modal error message.
  #' @export
  #' @importFrom shiny removeModal showModal modalDialog h4 p
  #' @importFrom rlang list2
  #' @importFrom stats na.omit
  runValidationModal <- function(session, ..., errorClass = character(0), delay_time = 250) {
    # Test validation conditions
    results <- sapply(rlang::list2(...), function(x) {
      if (is.null(x)) 
        return(NA_character_)
      else if (identical(x, FALSE)) 
        return("")
      else if (is.character(x)) 
        return(paste(as.character(x), collapse = "\n"))
      else stop("Unexpected validation result: ", as.character(x))
    })
    results <- stats::na.omit(results)
    if (length(results) == 0) 
      return(invisible())
    results <- results[nzchar(results)]
    
    # Show error message
    shinyjs::delay(delay_time, { 
        # Remove existing modals
        shiny::removeModal()
      
        # Set the modal open state to FALSE
        session$userData$modal_open(FALSE)
        
        # Launch modal
        shiny::showModal(shiny::modalDialog(
          shiny::h4("Error in input"),
          shiny::p(paste(results)),
          easyClose = TRUE, footer = NULL
        ))
    })
    
    # Stop reactive process
    reactiveStop(paste("", collapse = "\n"), c(errorClass,
                                               "validation"))
  }
  
  #' Stop Reactive Process with Condition
  #'
  #' This helper function stops a Shiny reactive process with a specified condition.
  #'
  #' @param message A character string specifying the error message.
  #' @param class A character vector of classes to assign to the error.
  #' @return Stops the reactive process with the specified condition.
  #' @export
  reactiveStop <- function(message = "", class = NULL) {
    stopWithCondition(c("shiny.silent.error", class), message)
  }
  
  #' Stop Process with Condition
  #'
  #' This helper function stops the execution of code with a specified condition and error message.
  #'
  #' @param class A character vector of classes to assign to the error.
  #' @param message A character string specifying the error message.
  #' @return Stops the execution with the specified condition.
  #' @export
  stopWithCondition <- function(class, message) {
    cond <- structure(
      list(message = message),
      class = c(class, 'error', 'condition')
    )
    stop(cond)
  }
  
  #' Display Modal with Progress Bar
  #'
  #' This function displays a modal with a progress bar.  If a similar modal
  #' is already open, it will update the progress bar of that modal.  If no modal 
  #' is open, it will launch a new modal.  
  #' 
  #' @param session The Shiny session object.
  #' @param id The id of the progress bar.
  #' @param message Optional message to display above the progress bar (default: "Initializing").
  #' @param value Optional value to update the progress bar (default: 0).
  #'
  #' @importFrom shinyjs runjs
  #' @importFrom shinyWidgets updateProgressBar
  #' @importFrom shiny showModal modalDialog removeModal tags
  #'
  #' @export
  display_modal <- function(session, id, message = "Initializing", value = 0, delay_time=0) {
    # Launch modal (if not alreay open)
    if (isFALSE(session$userData$modal_open())) {
      shiny::showModal(shiny::modalDialog(
        shiny::tags$h4(id = "modal-text", message),
        shinyWidgets::progressBar(id = id, value = 0, display_pct = TRUE),
        easyClose = FALSE, footer = NULL
      ))
      
      # Set the modal open state to TRUE
      session$userData$modal_open(TRUE)
    } else {
      # Update the modal message using JavaScript
      shinyjs::runjs(sprintf("document.getElementById('modal-text').innerText = '%s';", message))
  
      # Set the progress bar to the specified value
      shinyWidgets::updateProgressBar(session = session, id = id, value = value)
    }
  }
  
  #' Hide Modal with Progress Bar
  #'
  #' This function hides a modal with a progress bar.  It includes a brief delay 
  #' to ensure UI elements are fully updated before the modal is removed (otherwise
  #' the modal may not close).
  #' 
  #' @param session The Shiny session object.
  #' @param id The id of the progress bar to be updated.
  #' @param delay_time The delay in milliseconds before hiding the modal (default: 250).
  #' @export 
  #' @importFrom shinyjs delay
  #' @importFrom shinyWidgets updateProgressBar
  #' @importFrom shiny removeModal
  hide_modal_with_progress <- function(session, id, delay_time = 250) {
    shinyjs::delay(delay_time, {
      shinyWidgets::updateProgressBar(session = session, id = id, value = 100)
      shiny::removeModal()
    })
    
    # Set the modal open state to FALSE
    session$userData$modal_open(FALSE)
  }
  
  #' Filter and Select Columns Based on Variable Name
  #'
  #' This helper function filters a dataframe by a specified variable name and selects relevant columns.
  #'
  #' @param df A dataframe to filter and select from.
  #' @param var_name A character string specifying the variable name to filter by.
  #' @return A filtered dataframe with relevant columns selected.
  #' @export
  #' @importFrom dplyr filter select
  filter_var <- function(df, var_name) {
    if (!is.null(var_name)) {
      df <- df %>%
        dplyr::filter(var == var_name) %>%
        dplyr::select(-var)
    }
    return(df)
  }
  
  #' Validate and Read a CSV File
  #'
  #' This function validates that the file is a CSV, ensures it exists, and reads it into a dataframe.
  #'
  #' @param session The Shiny session object.
  #' @param file_path Character. The path to the CSV file.
  #' 
  #' @return A dataframe containing the contents of the CSV file, or NULL if an error occurs.
  #' @export
  #' @importFrom tools file_ext
  #' @importFrom shiny req
  #' @importFrom utils readr::read_csv
  validate_and_read_csv <- function(session, file_path) {
    file_ext <- tools::file_ext(file_path)
    runValidationModal(session = session, need(file_ext == "csv", "Please upload a CSV file."))
    shiny::req(file_path)
    
    data <- tryCatch({
      readr::read_csv(file_path)
    }, error = function(e) {
      runValidationModal(session = session, "Error reading the file. Please check the file format.")
      return(NULL)
    })
    
    return(data)
  }
  
  #' Validate and Read an RDS File
  #'
  #' This function validates that the file is an RDS, ensures it exists, and reads it into an R object.
  #' 
  #' @param session The Shiny session object.
  #' @param file_path Character. The path to the RDS file.
  #' 
  #' @return The R object contained in the RDS file, or NULL if an error occurs.
  #' @export
  #' @importFrom tools file_ext
  #' @importFrom shiny req
  #' @importFrom base readRDS
  validate_and_read_rds <- function(session, file_path) {
    file_ext <- tools::file_ext(file_path)
    runValidationModal(session = session, need(file_ext == "rds", "Please upload an RDS file."))
    shiny::req(file_path)
    
    data <- tryCatch({
      readRDS(file_path)
    }, error = function(e) {
      runValidationModal(session = session, "Error reading the file. Please check the file format.")
      return(NULL)
    })
    
    return(data)
  }
  
  #' Check for Genome and KO Columns
  #'
  #' This function checks if a dataframe has one column for Genome IDs and another for KO IDs.
  #'
  #' @param data Dataframe. The data to check.
  #' 
  #' @return Logical. TRUE if both Genome and KO ID columns are found, FALSE otherwise.
  #' @export
  #' @importFrom dplyr select
  has_genome_and_ko_columns <- function(data) {
    ko_pattern <- "^K[0-5]{5}$"
    ko_column <- detect_pattern_column(data, ko_pattern)
    has_genome_column <- "Genome" %in% colnames(data)
    
    return(!is.na(ko_column)[1] && has_genome_column)
  }
  
  #' Check for KO Columns Only
  #'
  #' This function checks if a dataframe has KO IDs in one or more columns.
  #'
  #' @param data Dataframe. The data to check.
  #' 
  #' @return Logical. TRUE if there is at least one KO ID column, FALSE otherwise.
  #' @export
  #' @importFrom dplyr select
  has_ko_only_columns <- function(data) {
    ko_pattern <- "^K[0-5]{5}$"
    ko_columns <- detect_pattern_column(data, ko_pattern)
    
    return(!is.na(ko_columns)[1])
  }
  
  #' Process Uploaded Gene Functions File
  #'
  #' This function processes an uploaded gene functions file, detecting whether it has separate Genome and KO columns, or KO columns only.
  #'
  #' @param gene_functions Dataframe. The uploaded gene functions data.
  #' 
  #' @return A processed dataframe or NULL if the file structure is unrecognized.
  #' @export
  #' @importFrom dplyr select group_by mutate
  #' @importFrom tidyr spread
  process_uploaded_gene_functions <- function(gene_functions) {
    if (has_genome_and_ko_columns(gene_functions)) {
      ko_column <- detect_pattern_column(gene_functions, "^K[0-5]{5}$")[1]
      gene_functions$Database_ID <- gene_functions[[ko_column]]
      
      gene_functions <- gene_functions %>%
        dplyr::group_by(Genome) %>%
        dplyr::mutate(row_id = dplyr::row_number()) %>%
        tidyr::spread(Genome, Database_ID) %>%
        dplyr::select(-row_id)
    } else if (has_ko_only_columns(gene_functions)) {
      ko_columns <- detect_pattern_column(gene_functions, "^K[0-5]{5}$")
      gene_functions <- gene_functions[, ko_columns, drop = FALSE]
    } else {
      gene_functions <- NULL
    }
    
    return(gene_functions)
  }
  
  #' Process Gene Functions from the Database
  #'
  #' This function processes gene functions for selected organisms, filtering and reshaping the data accordingly.
  #'
  #' @param gene_functions Dataframe. The gene functions data.
  #' @param organism_by_genome Dataframe. The dataframe mapping organisms to genomes.
  #' @param selected_organisms Character vector. The organisms selected by the user.
  #'
  #' @return A dataframe with gene functions for the selected organisms, with renamed columns to use organism names.
  #' @export
  #' @importFrom dplyr filter group_by mutate select
  #' @importFrom tidyr pivot_wider
  process_database_gene_functions <- function(gene_functions, organism_by_genome, selected_organisms) {
    filtered_data <- organism_by_genome %>%
      dplyr::filter(Organism %in% selected_organisms)

    filtered_data$Genome <- as.character(filtered_data$Genome)

    gene_functions <- gene_functions %>%
      dplyr::filter(Genome %in% filtered_data$Genome) %>%
      dplyr::group_by(Genome) %>%
      dplyr::mutate(row_id = dplyr::row_number()) %>%
      tidyr::pivot_wider(names_from = Genome, values_from = Database_ID) %>%
      dplyr::select(-row_id) %>%
      dplyr::select(all_of(filtered_data$Genome))

    colnames(gene_functions) <- filtered_data$Organism

    return(gene_functions)
  }
  
  #' Validate Reference Reactions Data
  #'
  #' This function checks whether the uploaded reference reactions data contains all required columns.
  #' If any required columns are missing, it returns an empty string.
  #'
  #' @param reference_reactions Dataframe. The uploaded reference reactions data.
  #' 
  #' @return The validated dataframe or an empty string if required columns are missing.
  #' @export
  #' @importFrom dplyr select
  validate_reference_reactions <- function(reference_reactions) {
    required_columns <- c("abbreviation", "equation", "direction", "officialName", 
                          "geneAssociation", "subsystem", "reaction_ID", "Enzyme", "Genes")
    
    if (!all(required_columns %in% colnames(reference_reactions))) {
      return("")
    }
    
    return(reference_reactions)
  }
  
  #' Extract Variables from Query Builder Output
  #'
  #' This function extracts all variables enclosed in backticks from a query builder output string.
  #'
  #' @param query_string A string containing a query with variables enclosed in backticks.
  #' @return A character vector of the extracted variable names.
  #' @export
  extract_query_var <- function(query_string) {
    # Extract variables between backticks
    variables <- stringr::str_extract_all(query_string, "`[^`]+`")[[1]]
    
    # Remove the backticks
    variables <- stringr::str_replace_all(variables, "`", "")
    
    return(variables)
  }
  
  #' Filter Data Based on Query String
  #'
  #' This function dynamically filters data based on a query string from jqbr::queryBuilderInput()
  #' The query string is evaluated and used to filter the data.
  #'
  #' @param data A data frame containing the data to be filtered.
  #' @param query_string A string representing the filtering condition (e.g., "`Gram_stain` == \"positive\"").
  #' @return A filtered data frame based on the query string condition.
  #' @export
  #' @importFrom dplyr filter
  #' @importFrom rlang parse_expr
  filter_data_by_query <- function(data, query_string) {
    data_filtered <- data %>% dplyr::filter(!!rlang::parse_expr(query_string))
    return(data_filtered)
  }
  
  #' Filter Data Excluding NA Values
  #'
  #' This function filters data, removing rows that have NA or "NA" values in the variables 
  #' extracted from the query string.  The query string is from jqr::queryBuilderInput().
  #'
  #' @param data A data frame containing the data to be filtered.
  #' @param query_string A string representing the query filter (e.g., "`Gram_stain` == \"positive\"").
  #' @return A filtered data frame that excludes rows where any extracted variable has NA or "NA" values.
  #' @export
  #' @importFrom dplyr filter
  #' @importFrom rlang sym
  filter_data_excluding_na <- function(data, query_string) {
    vars <- extract_query_var(query_string)
    data_all <- data
    for (var in vars) {
      data_all <- data_all %>% dplyr::filter(!is.na(!!rlang::sym(var)) | !!rlang::sym(var) != "NA")
    }
    return(data_all)
  }
  
  #' Mutate Data Based on Query String 
  #'
  #' This function dynamically mutates data based on a query string from jqr::queryBuilderInput().
  #' It adds a new column indicating whether the condition is met (1) or not (0), and the name
  #' of this new column can be specified.
  #'
  #' @param data A data frame containing the data to be filtered.
  #' @param query_string A string representing the filtering condition (e.g., "`Gram_stain` == \"positive\"").
  #' @param new_col A string specifying the name of the new column to be added.
  #' @return A mutated data frame based on the query string condition.
  #' @export
  #' @importFrom dplyr mutate
  #' @importFrom rlang parse_expr sym
  mutate_data_by_query <- function(data, query_string, new_col="X") {
    condition <- rlang::parse_expr(query_string)
    
    data <- data %>%
      dplyr::mutate(!!rlang::sym(new_col) := ifelse(!!condition, 1, 0))
    
    return(data)
  }
  
  #' Extract Genomes from Dataframe
  #'
  #' This function extracts unique genomes from a given dataframe, removing NA and "NA" values.
  #'
  #' @param data A data frame containing genome information.
  #' @param genome_column The column containing genome IDs.
  #' @return A vector of unique genome IDs.
  #' @export
  extract_genomes <- function(data, genome_column = "IMG Genome ID max genes") {
    # Select the genome column, filter out NA and "NA", and get unique values
    genomes <- data %>%
      dplyr::select(!!rlang::sym(genome_column)) %>%
      dplyr::filter(!is.na(!!rlang::sym(genome_column)) & !!rlang::sym(genome_column) != "NA") %>%
      dplyr::pull(!!rlang::sym(genome_column)) %>%
      # unique() %>%
      as.character() 
    
    return(genomes)
  }
  
  #' Format Response Variable for Random Forest Model
  #'
  #' This function formats the response variable for a random forest model.  The input
  #' is the app's database and a query string.  The query string specifies which organisms
  #' in the database are positive for the trait.  The function returns a dataframe with
  #' responses (0 = negative for trait, 1 = positive for trait) and genome ID for each organism.
  #' By default, organisms with NA for any variables in the query are excluded.
  #'
  #' @param data The app's database (a data frame)
  #' @param query_string A string representing the query filter (e.g., "`Gram_stain` == \"positive\"").
  #' @return A response data frame with genome IDs and a binary response (1 for positive trait, 0 otherwise).
  #' @export
  #' @importFrom dplyr filter
  #' @importFrom rlang parse_expr sym
  format_response <- function(data, query_string, ignore_NA = TRUE) {
    # Get data for organisms with positive traits
    data_positive <- filter_data_by_query(data, query_string)
    
    # Get data for all organisms (excluding those with NA values if specified)
    if(ignore_NA)
    {
      data_all <- filter_data_excluding_na(data, query_string)
    }else
    {
      data_all = data
    }
    
    # Get genomes for organisms
    positive_genomes <- extract_genomes(data_positive)
    all_genomes <- extract_genomes(data_all)
    
    # Ensure both vectors are character
    positive_genomes <- as.character(positive_genomes)
    all_genomes <- as.character(all_genomes)
    
    # Create a response dataframe with binary values: 1 for positive genomes, 0 for others
    response <- data.frame(
      Genome = all_genomes,
      Response = ifelse(all_genomes %in% positive_genomes, 1, 0)
    )
    
    return(response)
  }
  
  #' Format Predictor Variables for Random Forest Model
  #'
  #' This function gets predictors for a random forest model from gene functions 
  #' of a given set of genomes. It puts genomes in rows and gene functions in columns, 
  #' converting the latter to binary values (0 = absent in genome, 1 = present in genome).  
  #' It optionally subsamples a proportion of the rows and columns to reduce the number of predictors.
  #'
  #' @param gene_functions A data frame containing gene functions with KO IDs and Genome IDs.
  #' @param seed An optional seed value for reproducibility. Default is NULL.
  #' @param responses_to_keep An optional proportion of rows to keep when subsampling. Must be between 0 and 1. Default is NULL.
  #' @param predictors_to_keep An optional proportion of columns to keep when subsampling. Must be between 0 and 1. Default is NULL.
  #' @param only_keep_genome_ko A logical indicating whether to retain only the Genome column and KO columns. Default is TRUE
  #' @return A data frame of formatted predictors.
  #' @export
  #' @importFrom dplyr select mutate distinct sample_n
  #' @importFrom tidyr pivot_wider pivot_longer
  #' @importFrom rlang sym
  format_predictors = function(gene_functions, seed = NULL, responses_to_keep = NULL, predictors_to_keep = NULL, only_keep_genome_ko = TRUE) {
    predictors = gene_functions
    
    if (has_genome_and_ko_columns(predictors)) {
      ko_column <- detect_pattern_column(data = predictors, pattern = "^K[0-9]{5}$")
      predictors <- predictors %>% 
        dplyr::select(Genome, !!rlang::sym(ko_column)) %>% 
        dplyr::distinct() %>% 
        dplyr::mutate(value = 1) %>% 
        tidyr::pivot_wider(names_from = !!rlang::sym(ko_column), values_from = value, values_fill = list(value = 0))
    } else if (has_ko_only_columns(predictors)) {
      ko_column <- detect_pattern_column(data = predictors, pattern = "^K[0-5]{5}$")
      predictors = predictors[[ko_column]]
      predictors = predictors %>% tidyr::pivot_longer(cols = everything(), names_to = "Genome", values_to = "Database_ID")
      predictors <- predictors %>% 
        dplyr::select(Genome, !!rlang::sym(ko_column)) %>% 
        dplyr::distinct() %>% 
        dplyr::mutate(value = 1) %>% 
        tidyr::pivot_wider(names_from = !!rlang::sym(ko_column), values_from = value, values_fill = list(value = 0))
    } else {
      predictors <- NULL
    }
    
    # Ensure Genome is character
    predictors$Genome <- as.character(predictors$Genome) 
    predictors <- predictors %>%
      dplyr::select(Genome, everything())
    
    # Remove all non-KO columns if the option is enabled
    if (only_keep_genome_ko) {
      ko_columns <- grep("^K[0-9]{5}$", colnames(predictors), value = TRUE)
      predictors <- predictors %>% dplyr::select(Genome, all_of(ko_columns))
    }
    
    # Set seed if provided
    if (!is.null(seed)) {
      set.seed(seed)
    }
    
    # Subsample rows
    if (!is.null(responses_to_keep)) {
      if (responses_to_keep > 1 || responses_to_keep < 0) {
        stop("Row proportion must be between 0 and 1.")
      }
      total_rows <- nrow(predictors)
      n_rows <- round(total_rows * responses_to_keep)
      predictors <- predictors %>% dplyr::sample_n(n_rows)
    }
    
    # Subsample columns (excluding Genome column)
    if (!is.null(predictors_to_keep) && !only_keep_genome_ko) {
      if (predictors_to_keep > 1 || predictors_to_keep < 0) {
        stop("Column proportion must be between 0 and 1.")
      }
      
      non_genome_columns <- setdiff(colnames(predictors), "Genome")
      total_cols <- length(non_genome_columns)
      n_cols <- round(total_cols * predictors_to_keep)
      selected_cols <- sample(non_genome_columns, n_cols)
      
      predictors <- predictors %>%
        dplyr::select(Genome, all_of(selected_cols))
    }
    
    return(predictors)
  }
  
  
  #' Format Data for Random Forest Model
  #'
  #' This function formats the data by joining predictors and response if the "Genome" column is present,
  #' or combines them using cbind if "Genome" is not present. It ensures that the response column is named "Response".
  #'
  #' @param predictors A data frame of formatted predictors.
  #' @param response A data frame of formatted response variables.
  #' @return A data frame where the first column is the response and the remaining columns are predictors.
  #' @export
  #' @importFrom dplyr inner_join select rename
  format_rf_data <- function(predictors, response) {
    # Check for "Genome" column in both predictors and response
    if ("Genome" %in% colnames(predictors) && "Genome" %in% colnames(response)) {
      # Join predictors and response by Genome
      data <- predictors %>%
        dplyr::inner_join(response, by = "Genome") %>%
        dplyr::select(-Genome)
      
      # Ensure the response column is named "Response"
      data <- data %>% dplyr::rename(Response = last_col())
      
    } else {
      # Check if number of rows in predictors and response match
      if (nrow(predictors) != nrow(response)) {
        stop("The number of rows in 'predictors' and 'response' must be the same.")
      }
      
      # Combine predictors and response using cbind
      data <- cbind(predictors, Response = response)
    }
    
    # Do further formatting
    data$Response <- as.factor(data$Response)
    data <- data %>% dplyr::select(Response, everything())
    
    return(data)
  }
  
  #' Split Train and Test Data
  #'
  #' This function splits the dataset into training and test sets.
  #' The response variable should be the first column.
  #'
  #' @param data A data frame where the first column is the response and the remaining columns are predictors.
  #' @param seed An optional seed value for reproducibility. Default is 123.
  #' @param training_split The proportion of data to use for training. Default is 0.7.
  #' @return A list containing training and test datasets.
  #' @export
  split_data <- function(data, seed = 123, training_split = 0.7) {
    set.seed(seed)
    ind <- sample(2, nrow(data), replace = TRUE, prob = c(training_split, 1-training_split))
    train <- data[ind == 1, ]
    test <- data[ind == 2, ]
    
    split = list(train = train, test = test)
    
    return(split)
  }
  
  #' Train Random Forest Model
  #'
  #' This function trains a random forest model using the formatted data.
  #' The model assumes that the first column is the response variable and the remaining columns are predictors.
  #' The model can be configured with a specified number of trees and maximum nodes.
  #'
  #' @param data A data frame where the first column is the response and the remaining columns are predictors.
  #' @param seed An optional seed value for reproducibility. Default is 123.
  #' @param ntree The number of trees to grow in the random forest. Default is 500.
  #' @param maxnodes The maximum number of terminal nodes trees in the forest can have. Default is NULL.
  #' @param positive_class_weight The weight given to the positive class of responses. Default is 0.5 (equal weight for negative and positive classes).
  #' @return A random forest model object.
  #' @export
  #' @importFrom randomForest randomForest
  train_rf <- function(data, seed = 123, training_split = 0.7, ntree = 500, maxnodes = NULL, positive_class_weight=0.5) {
    # Get response column
    response_column <- data[[1]]
    if (!is.factor(response_column)) {
      response_column <- as.factor(response_column)
    }
    
    # The remaining columns are predictors
    predictors <- data[, -1]
    
    # Combine response and predictors into a new data frame for modeling
    modeling_data <- data.frame(Response = response_column, predictors)
    
    # Fit the random forest model
    rf <- randomForest::randomForest(Response ~ ., data = data, ntree = ntree, maxnodes = maxnodes, classwt = c("0" = 1-positive_class_weight, "1" = positive_class_weight)) 
    
    return(rf)
  }
  
  #' Evaluate Random Forest Model
  #'
  #' This function evaluates the random forest model using the test data.
  #' It returns a confusion matrix
  #'
  #' @param rf A random forest model object.
  #' @param data A data frame containing the test data. The first column must be the response.
  #' @return A confusion matrix 
  #' @export
  #' @importFrom caret confusionMatrix
  evaluate_rf <- function(rf, data) {
    # Get model
    model = rf
    
    # Get response column
    response_column <- data[[1]]
    if (!is.factor(response_column)) {
      response_column <- as.factor(response_column)
    }
    
    # The remaining columns are predictors
    predictors <- data[, -1]
    
    # Get predictions
    predictions = randomForest:::predict.randomForest(object = model, newdata = predictors, type = "prob")[, 2] # Assuming the second column is the probability of the positive class
    
    # Convert predicted probabilities to classes based on a threshold of 0.5
    predicted_classes <- as.factor(ifelse(predictions >= 0.5, levels(response_column)[2], levels(response_column)[1]))
    
    # Create a confusion matrix
    confusion_matrix <- caret::confusionMatrix(predicted_classes, response_column)
    
    return(confusion_matrix)
  }
  
  #' Build Random Forest Model
  #'
  #' This function trains and evaluates a random forest model.
  #'
  #' @param data A data frame where the first column is the response and the remaining columns are predictors.
  #' @param seed An optional seed value for reproducibility. Default is 123.
  #' @param training_split The proportion of data to use for training. Default is 0.7.
  #' @param ntree The number of trees to grow in the random forest. Default is 500.
  #' @param maxnodes The maximum number of terminal nodes trees in the forest can have. Default is NULL.
  #' @param positive_class_weight The weight given to the positive class of responses. Default is 0.5 (equal weight for negative and positive classes).
  #' @return A random forest model object with evaluation results added.
  #' @export
  #' @importFrom randomForest randomForest
  #' @importFrom caret confusionMatrix
  build_rf <- function(data, seed = 123, training_split = 0.7, ntree = 500, maxnodes = NULL, positive_class_weight = 0.5) {
    # Split data into training and test sets
    data_split <- split_data(data = data, seed = seed, training_split = training_split)
    train <- data_split$train
    test <- data_split$test
    
    # Check if training or test data is empty
    if (nrow(train) == 0|nrow(test) == 0) {
      return(NULL)
    }
    
    # Train the model
    rf <- train_rf(data = train, seed = seed, ntree = ntree, maxnodes = maxnodes, positive_class_weight = positive_class_weight)
  
    # Evaluate the model
    confusion_matrix <- evaluate_rf(rf = rf, data = test)
    
    # Add evaluation results to the random forest model
    rf$evaluation_results <- confusion_matrix
    
    return(rf)
  }
  
  #' Save Random Forest Model
  #'
  #' This function saves a random forest model to an RDS file with optional compression and environment cleaning to reduce file size.
  #'
  #' @param rf A random forest model object to save.
  #' @param data_fp The file path where the model should be saved.
  #' @param remove_proximity Logical. If TRUE, removes proximity data from the model to reduce file size. Default is TRUE.
  #' @param clean_environment Logical. If TRUE, removes non-essential objects from the environment to reduce file size. Default is TRUE.
  #' @param compress The compression method to use when saving the RDS file. Default is "xz".
  #' @return Saves the random forest model to the specified file path.
  #' @export
  save_rf = function(rf, data_fp, remove_proximity=TRUE, clean_environment = TRUE, compress="xz")
  {
    #Remove data for proximity (reduces file size)
    if(remove_proximity==TRUE)
    {
      rf$proximity <- NULL 
    }
    
    # Remove non-essential objects from the environment (reduces file size)
    if(clean_environment==TRUE)
    {
      rm(list = setdiff(ls(envir = attr(rf$terms, ".Environment")), c("train", "test", "rf", "var")), envir = attr(rf$terms, ".Environment"))
    }
    
    saveRDS(object=rf, file = data_fp, compress = compress)
  }
  
  #' Collapse Dataframe Columns into a Single Column
  #'
  #' This function collapses a set of columns in a dataframe into a single column with 
  #' semi-colon separated values. The rows in the columns can contain user-defined 
  #' positive and negative values (e.g., "1" for presence and "0" for absence).
  #' The names of the columns will be the new values in the new column.
  #' These values will be semi-colon separated (e.g., column1;column2), 
  #' and values absent in the original column will be excluded.
  #'
  #' @param df A dataframe containing the columns to be collapsed.
  #' @param cols A character vector of column names to collapse.
  #' @param new_col_name The name of the new column to store collapsed values.
  #' @param delete A string pattern to remove from column names during the collapse. Default is an empty string.
  #' @param positive_value The value that represents a "positive" presence (default is "1").
  #' @param negative_value The value that represents a "negative" absence (default is "0").
  #' @return A dataframe with the specified columns collapsed into a single column.
  #' @export
  #' @importFrom dplyr rowwise mutate ungroup select
  #' @importFrom stringr str_replace
  collapse_columns <- function(df, cols, new_col_name, delete = "", positive_value = "1", negative_value = "0") {
    df[[new_col_name]] <- sapply(1:nrow(df), function(i) {
      col_values <- gsub(delete, "", cols)
      # Identify positive matches based on the positive value
      values <- col_values[df[i, cols] == positive_value]
      # Exclude NA and empty values
      values <- values[!is.na(values) & values != "NA"]
      if (length(values) > 0) {
        return(paste(values, collapse = ";"))
      } else {
        return(NA)
      }
    })
    df <- df[, !(colnames(df) %in% cols)]
    
    return(df)
  }
  
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
  
  #' Expand Taxonomy String into Vector
  #'
  #' Converts a Greengenes-compatible taxonomy string into a vector of taxonomic levels.
  #'
  #' @param tax_string A character string formatted as a Greengenes taxonomy, 
  #'   with levels separated by semicolons and prefixed with rank indicators (e.g., `d__`, `p__`).
  #' @param simplify_species Logical. If TRUE, only the species epithet (e.g., "coli" from "Escherichia coli") is returned
  #'   in the species position. Default is TRUE.
  #' @return A character vector containing the taxonomic levels without prefixes. If `simplify_species` is TRUE, the species 
  #'   level contains only the epithet.
  #' @examples
  #' tax_string <- "d__Bacteria;p__Abditibacteriota;c__Abditibacteriia;o__Abditibacteriales;f__Abditibacteriaceae;g__Abditibacterium;s__Abditibacterium utsteinense"
  #' expand_taxonomy(tax_string, simplify_species = TRUE)
  #' @export
  expand_taxonomy <- function(tax_string, simplify_species = TRUE) {
    # Split the taxonomy string by semicolons
    tax_vector <- unlist(strsplit(tax_string, ";"))
    
    # Remove the rank prefixes (d__, p__, etc.) and extract only the names
    tax_vector <- sub("^[a-z]__*", "", tax_vector)
    
    # If simplify_species is TRUE, extract only the species epithet
    if (simplify_species && length(tax_vector) >= 7) {
      tax_vector[7] <- sub(".* ", "", tax_vector[7]) # Remove genus part, leaving only the species epithet
    }
    
    return(tax_vector)
  }
  
  #' Expand and Merge Taxonomy into Dataframe
  #'
  #' Expands taxonomic strings in a specified column of a given dataframe and merges the extracted 
  #' taxonomic levels into that dataframe. If the required taxonomy columns ("Domain", "Phylum", "Class", 
  #' "Order", "Family") already exist, the function skips processing.
  #'
  #' @param data A dataframe containing a column with taxonomic strings 
  #'   formatted as Greengenes-style taxonomy (i.e., levels separated by semicolons and prefixed with rank indicators).
  #' @param col_name A string specifying the name of the column containing the taxonomic strings.
  #' @return A dataframe with additional columns for expanded taxonomy, specifically "Domain", "Phylum", 
  #'   "Class", "Order", and "Family". The "Genus" and "Species" columns are removed before merging.
  #' @export
  expand_and_merge_taxonomy <- function(data, col_name) {
    required_cols <- c("Domain", "Phylum", "Class", "Order", "Family")
    
    if (!all(required_cols %in% colnames(data))) {
      tax <- data[[col_name]] %>%
        purrr::map(expand_taxonomy) %>%
        purrr::map_dfr(~purrr::set_names(as.list(.), c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species")))
  
      tax <- dplyr::select(tax, -Genus, -Species)
      data <- cbind(tax, data)
    }    
    
    return(data)
  }
  
  #' Get Organism and Genome Information
  #'
  #' This function extracts organism names and their corresponding genome IDs
  #' from a cleaned database. The organism names are formatted as 
  #' "Genus Species Subspecies", with missing subspecies values properly handled.
  #'
  #' @param database A dataframe containing organism data, including columns for genome IDs, genus, species, and subspecies.
  #' @param genome_id_col A character string specifying the column name that contains genome IDs. Defaults to `"IMG Genome ID max genes"`.
  #' @param genus_col A character string specifying the column name that contains genus information. Defaults to `"Genus"`.
  #' @param species_col A character string specifying the column name that contains species information. Defaults to `"Species"`.
  #' @param subspecies_col A character string specifying the column name that contains subspecies information. Defaults to `"Subspecies"`.
  #' @param remove_na_genome A logical value indicating whether to remove organisms with NA genome IDs. Defaults to TRUE.
  #'
  #' @return A dataframe with two columns: `Organism` and `Genome`.
  #'   
  #' @examples
  #' # Example usage
  #' get_organism_by_genome(database_clean, 
  #'                        genome_id_col = "Genome_ID", 
  #'                        genus_col = "Genus_Name",
  #'                        species_col = "Species_Name",
  #'                        subspecies_col = "Subspecies_Name",
  #'                        remove_na_genome = TRUE)
  #'
  #' @importFrom dplyr filter mutate rename select
  #' @export
  get_organism_by_genome <- function(database, 
                                     genome_id_col = "IMG Genome ID max genes", 
                                     genus_col = "Genus", 
                                     species_col = "Species", 
                                     subspecies_col = "Subspecies",
                                     remove_na_genome = TRUE) {
    
    result <- database %>%
      dplyr::mutate(Organism = paste(.data[[genus_col]], .data[[species_col]], .data[[subspecies_col]], sep = " ")) %>%
      dplyr::mutate(Organism = gsub(" NA$", "", Organism)) %>%
      dplyr::rename(Genome = .data[[genome_id_col]]) %>%
      dplyr::select(Organism, Genome) 
    
    if (remove_na_genome) {
      result <- result %>% dplyr::filter(!is.na(Genome) & Genome != "NA")
    }
    
    return(result)
  }
  
  #' Get Choices for Organisms
  #'
  #' This function gets names of organisms that have gene functions in
  #' the database. It can optionally filter organisms based on their 
  #' metabolism type and genome ID column.
  #'
  #' @param database A dataframe containing organism data, including columns 
  #'   for genome IDs, genus, species, and subspecies.
  #' @param genome_id_col A character string specifying the column name that 
  #'   contains genome IDs. Defaults to `"IMG Genome ID max genes"`.
  #' @param metabolism_type_col A character string specifying the column name that 
  #'   contains metabolism type information. Defaults to `"Type of metabolism"`.
  #' @param genus_col A character string specifying the column name that contains 
  #'   genus information. Defaults to `"Genus"`.
  #' @param species_col A character string specifying the column name that contains 
  #'   species information. Defaults to `"Species"`.
  #' @param subspecies_col A character string specifying the column name that contains 
  #'   subspecies information. Defaults to `"Subspecies"`.
  #' @param metabolism_type A character string specifying the metabolism type to filter 
  #'   the organisms by (e.g., "Fermentation", "Methanogenesis"). Defaults to `NULL`, 
  #'   meaning no metabolism filtering is applied.
  #' @param filter_by_genome_id A logical indicating whether to filter out rows with 
  #'   missing or "NA" values in the `genome_id_col`. Defaults to `TRUE`.
  #'
  #' @return A character vector of organism names formatted as "Genus Species Subspecies".
  #'   Entries with missing values in `Subspecies` are cleaned to remove trailing "NA".
  #'   
  #' @examples
  #' # Example usage
  #' get_organism_choices(database_clean, 
  #'                      genome_id_col = "Genome_ID", 
  #'                      metabolism_type_col = "Metabolism",
  #'                      genus_col = "Genus_Name",
  #'                      species_col = "Species_Name",
  #'                      subspecies_col = "Subspecies_Name",
  #'                      metabolism_type = "Fermentation",
  #'                      filter_by_genome_id = FALSE)
  #'
  #' @importFrom dplyr filter mutate pull
  #' @export
  get_organism_choices <- function(database, 
                                   genome_id_col = "IMG Genome ID max genes", 
                                   metabolism_type_col = "Type of metabolism", 
                                   genus_col = "Genus", 
                                   species_col = "Species", 
                                   subspecies_col = "Subspecies", 
                                   metabolism_type = NULL,
                                   filter_by_genome_id = TRUE) {
    
    if (filter_by_genome_id) {
      database <- database %>%
        dplyr::filter(.data[[genome_id_col]] != "NA" & !is.na(.data[[genome_id_col]]))
    }
    
    if (!is.null(metabolism_type)) {
      database <- database %>%
        dplyr::filter(.data[[metabolism_type_col]] == metabolism_type)
    }
    
    database %>%
      dplyr::mutate(choices = paste(.data[[genus_col]], 
                                    .data[[species_col]], 
                                    .data[[subspecies_col]], sep = " ")) %>%
      dplyr::mutate(choices = gsub(" NA$", "", choices)) %>%
      dplyr::pull(choices)
  }
  
  #' Get Choices for Taxa
  #'
  #' This function extracts unique taxonomic names from a dataframe and appends 
  #' the corresponding rank in parentheses.
  #'
  #' @param df A dataframe containing taxonomic ranks as columns (e.g., "Phylum", "Class", 
  #'   "Order", "Family", "Genus", "Species"), with taxon names as values.
  #'
  #' @return A character vector of unique taxon names formatted as "Taxon_Name (Rank)".
  #'
  #' @examples
  #' taxon_df <- data.frame(
  #'   Phylum = c("Abditibacteriota", "Bacillota", "Bacillota"),
  #'   Class = c("Abditibacteriia", "Bacilli", "Erysipelotrichia"),
  #'   Order = c("Abditibacteriales", "Lactobacillales", "Erysipelotrichales")
  #' )
  #' 
  #' get_taxon_choices(taxon_df)
  #'
  #' @export
  get_taxon_choices <- function(data) {
    # Get column names as taxonomic ranks
    ranks <- c("Phylum", "Class", "Order", "Family", "Genus")
    
    data <- data %>% dplyr::select(dplyr::all_of(ranks))
    
    # Convert the dataframe into a vector with the required format
    choices <- unlist(mapply(function(col, rank) {
      paste(data[[col]], sprintf("(%s)", rank))
    }, colnames(data), ranks, SIMPLIFY = FALSE))
    
    # Ensure unique values and remove names
    choices <- unique(unname(choices))
    
    return(choices)
  }
  
  #' Update Query Builder Filters
  #' 
  #' This function loads query filters, filters them based on the selected variable list, 
  #' and updates the query builder input. It includes a brief delay 
  #' to ensure UI elements are fully updated before the builder is updated (otherwise
  #' the update may not be successful).
  #' 
  #' @param inputId A character string specifying the ID of the query builder input.
  #' @param choices A character vector of variable names to filter the query builder filters.
  #' @param delay_time An optional delay in milliseconds before applying the update (default: 250 ms).
  #' 
  #' @return Updates the query builder input dynamically.
  #' 
  #' @examples
  #' update_query_builder("query_builder", choices_traits_taxonomy)
  update_query_builder <- function(inputId, choices, delay_time = 250) {
    filters <- load_query_filters()
    filters <- purrr::keep(filters, ~ .x$id %in% choices)
    
      shinyjs::delay(delay_time, {
        jqbr::updateQueryBuilder(
          inputId = inputId,
          setFilters = filters
        )
      })
  }
  
  #' Update Selectize Input
  #'
  #' This function updates a Selectize input in a Shiny application. It dynamically sets
  #' the available choices and selects a default value.
  #'
  #' @param session The Shiny session object.
  #' @param inputId A character string specifying the ID of the Selectize input to update.
  #' @param choices A character vector of choices to populate the Selectize input.
  #' @param selected A character vector specifying the default selected choice(s).
  #' @param server A logical indicating whether to use server-side processing (default: TRUE).
  #'
  #' @return Updates the specified Selectize input dynamically.
  #'
  #' @examples
  #' update_select_input(session, "gene_functions_database", choices = c("Option 1", "Option 2"))
  update_select_input <- function(session, inputId, choices = NULL, selected = NULL, server = TRUE) {
    if (is.null(choices)) choices <- character(0)
    if (is.null(selected)) selected <- head(choices, 1)

    shiny::updateSelectizeInput(session, inputId = inputId, choices = choices, selected = selected, server = server)
  }
  
  #' Update Picker Input
  #'
  #' This function updates a Picker input (shinyWidgets) in a Shiny application. It sets
  #' the available choices and selects a default value.
  #'
  #' @param session The Shiny session object.
  #' @param inputId A character string specifying the ID of the Picker input to update.
  #' @param choices A character vector of choices to populate the Picker input.
  #' @param selected A character vector specifying the default selected choice(s). 
  #'
  #' @return Updates the specified Picker input dynamically.
  #'
  #' @examples
  #' update_picker_input(session, "variable_to_display", choices = c("Phylum", "Genus"))
  update_picker_input <- function(session, inputId, choices = NULL, selected = NULL) {
    if (is.null(choices)) choices <- character(0)
    if (is.null(selected)) selected <- head(choices, 1)
    
    shinyWidgets::updatePickerInput(session, inputId = inputId, choices = choices, selected = selected)
  }
  
  #' Update Checkbox Group Input
  #'
  #' This function updates a checkbox group input in a Shiny application. It dynamically sets
  #' the available choices and selects default values.
  #'
  #' @param session The Shiny session object.
  #' @param inputId A character string specifying the ID of the checkbox group input to update.
  #' @param choices A named list or character vector of choices to populate the checkbox group.
  #' @param selected A character vector specifying the default selected choices. Defaults to NULL.
  #'
  #' @return Updates the specified checkbox group input dynamically.
  #'
  #' @examples
  #' update_checkbox_group(session, "checkboxes_info_organism", choices = c("Genus", "Species"))
  update_checkbox_group <- function(session, inputId, choices = NULL, selected = NULL) {
    if (is.null(choices)) choices <- character(0)
    
    shiny::updateCheckboxGroupInput(session, inputId = inputId, choices = choices, selected = selected)
  }
  
  #' Count Traits and Organisms with Predictions
  #'
  #' This function calculates the total number of traits and organisms while considering:
  #' - Numeric traits: Counted if at least one value is greater than `min_value`.
  #' - Categorical traits: Counted if at least one value is non-NA and non-empty.
  #'
  #' @param df Dataframe. A dataset containing both numeric and categorical trait values, with organisms as rows.
  #' @param exclude_columns Character vector (optional). A vector of column names to exclude from the count, 
  #'   typically taxonomic columns such as "Phylum", "Class", "Order", etc.
  #' @param min_value Numeric. The minimum value required for a numeric trait to be counted as predicted. Default is 0.5.
  #'
  #' @return A list containing:
  #' \itemize{
  #'   \item \code{traits_total} - The total number of traits (columns).
  #'   \item \code{traits_predictions} - The number of traits where at least one organism has a value > `min_value` (numeric) or non-NA (categorical).
  #'   \item \code{organisms_total} - The total number of organisms (rows).
  #'   \item \code{organisms_predictions} - The number of organisms with at least one predicted trait.
  #' }
  #'
  #' @examples
  #' df <- data.frame(
  #'   Organism = c("A", "B", "C"),
  #'   Trait1 = c(0.2, 0.6, 0.8),  # Numeric
  #'   Trait2 = c(NA, "yes", "")   # Categorical
  #' )
  #' count_traits(df, exclude_columns = "Organism", min_value = 0.5)
  #' # Returns: List with traits_total = 2, traits_predictions = 2, organisms_total = 3, organisms_predictions = 2
  #'
  #' @export
  count_traits <- function(df, exclude_columns = NULL, min_value = 0.5) {
    if (!is.null(exclude_columns)) {
      df <- df %>% dplyr::select(-dplyr::all_of(exclude_columns))
    }
    
    # Identify numeric and character columns
    numeric_cols <- dplyr::select_if(df, is.numeric)
    char_cols <- dplyr::select_if(df, is.character)
    
    # Convert to logical matrices with drop = FALSE to preserve dimensions
    numeric_matrix <- if (ncol(numeric_cols) > 0) as.matrix(numeric_cols > min_value, drop = FALSE) else NULL 
    char_matrix <- if (ncol(char_cols) > 0) as.matrix(!is.na(char_cols) & char_cols != "", drop = FALSE) else NULL
    
    # Ensure cbind() always outputs a matrix, even when there's only one row
    combined_matrix <- cbind(
      if (!is.null(numeric_matrix)) numeric_matrix else matrix(, nrow(df), 0),
      if (!is.null(char_matrix)) char_matrix else matrix(, nrow(df), 0)
    )
    
    # Count traits (numeric: values > min_value, character: non-NA & non-empty)
    numeric_traits_predicted <- if (!is.null(numeric_matrix)) colSums(numeric_matrix, na.rm = TRUE) > 0 else logical()
    char_traits_predicted <- if (!is.null(char_matrix)) colSums(char_matrix, na.rm = TRUE) > 0 else logical()

    # Count organisms with predictions (row-wise OR operation)
    organisms_predictions <- sum(rowSums(combined_matrix, na.rm = TRUE) > 0)
    
    list(
      traits_total = ncol(df),
      traits_predictions = sum(as.numeric(numeric_traits_predicted)) + sum(as.numeric(char_traits_predicted)),
      organisms_total = nrow(df),
      organisms_predictions = organisms_predictions
    )
  }
  
  
  #' Count End products and Organisms with Flux Predictions
  #'
  #' This function calculates the total number of end products and organisms, as well as the 
  #' number of organisms for which at least one flux value exceeds a specified threshold.
  #'
  #' @param df Dataframe. A dataset containing flux data, including columns for organisms, end products, and flux values.
  #' @param min_value Numeric. The minimum flux value required for an organism to be counted as having a prediction. Default is 1.
  #'
  #' @return A list containing:
  #' \itemize{
  #'   \item \code{endproducts_total} - The total number of unique end products.
  #'   \item \code{endproducts_predictions} - The number of end products with at least one organism showing a flux > min_value.
  #'   \item \code{organisms_total} - The total number of unique organisms.
  #'   \item \code{organisms_predictions} - The number of organisms with at least one flux value > min_value.
  #' }
  #'
  #' @examples
  #' df <- data.frame(
  #'   Organism = c("A", "A", "B", "B", "C"),
  #'   End_product = c("Acetate", "Propionate", "Acetate", "Butyrate", "Lactate"),
  #'   Flux = c(2, 0.5, 3, 0, 0)
  #' )
  #' count_fluxes(df, min_value = 1)
  #' # Returns: List with endproducts_total = 4, endproducts_predictions = 3, organisms_total = 3, organisms_predictions = 2
  #'
  #' @export
  count_fluxes <- function(df, min_value = 1) {
    list(
      endproducts_total = df %>% dplyr::pull(End_product) %>% unique() %>% length(),
      endproducts_predictions = df %>% dplyr::filter(Flux > min_value) %>% dplyr::pull(End_product) %>% unique() %>% length(),
      organisms_total = df %>% dplyr::pull(Organism) %>% unique() %>% length(),
      organisms_predictions = df %>% dplyr::filter(Flux > min_value) %>% dplyr::pull(Organism) %>% unique() %>% length()
    )
  }
  
  #' Format Summary Text for Predictions
  #'
  #' Generates a summary statement about the number of traits or end products predicted 
  #' for a given number of organisms.
  #'
  #' @param count1 Integer. The number of predicted traits or end products.
  #' @param count2 Integer. The number of organisms with predictions.
  #' @param label1 Character. The label for the predicted items (e.g., "traits", "end products").
  #' @param label2 Character. The label for the entities being predicted for (e.g., "organisms").
  #' @param total2 Integer (optional). The total number of organisms in the dataset.
  #' 
  #' @return A character string summarizing the predictions.
  #' @examples
  #' format_summary_text(5, 2, "traits", "organisms", 3)
  #' # Returns: "5 traits predicted for 2 of 3 organisms"
  #'
  #' format_summary_text(0, 3, "traits", "organisms", 3)
  #' # Returns: "No traits predicted for 3 organisms"
  #'
  #' @export
  format_summary_text <- function(count1, count2, label1, label2, total2 = NULL) {
    label1_singular <- sub("s$", "", label1)  # Convert to singular form if needed
    label2_singular <- sub("s$", "", label2)  # Convert to singular form if needed
    
    string <- NULL
    
    # First part (e.g., "5 traits predicted for ")
    if(count1 > 1){
      string <- paste0(count1, " ", label1, " predicted for ")
    }else if(count1 == 1){
      string <- paste0(count1, " ", label1_singular, " predicted for ")
    }else if(count1 == 0){
      string <- paste0("No ", label1, " predicted for ")
    }
 
    # Second part (e.g., "2 organisms")
    if(count2 > 1){
      string <- paste0(string, count2, " ", label2)
    } else if(count2 == 1){
      string <- paste0(string, count2, " ", label2_singular)
    } else if(count2 == 0 & total2 > 1){
      string <- paste0(string, total2, " ", label2)
    } else if(count2 == 0 & total2 == 1){
      string <- paste0(string, total2, " ", label2_singular)
    } 
    
    # Third part (e.g., "(No predictions for 1 more organism"))
    if(count1 > 0 & total2 > 1 & (total2 - count2)>1){
      string <- paste0(string, " (", total2 - count2, " more ", label2, " had no predictions)")
    }
    
    if(count1 > 0 & total2 > 1 & (total2 - count2)==1){
      string <- paste0(string, " (", total2 - count2, " more ", label2_singular, " had no predictions)")
    }
    
    return(string)
  }