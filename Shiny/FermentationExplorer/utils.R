# Utility Functions for App
# This script contains various utility functions for use in the Shiny app,
# including custom operators, file input handling, validation, and data cleaning.
# Author: Timothy Hackmann
# Date: 14 October 2024

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

#' Define a File Input Box with a Link to a Modal
#'
#' This function defines a custom file input box in a Shiny app, which includes a link to a modal for additional actions or information.
#'
#' @param inputId The input ID for the file input.
#' @param label A label for the file input.
#' @param multiple Logical. If TRUE, allows multiple file selection. Default is FALSE.
#' @param accept A character vector of accepted file types. Default is NULL.
#' @param width A character string specifying the width of the input box. Default is NULL.
#' @param buttonLabel The label for the file browse button. Default is "Browse...".
#' @param placeholder Placeholder text for when no file is selected. Default is "No file selected".
#' @param modalId The input ID for the modal link.
#' @param modalLabel The label for the modal link.
#' @return A Shiny UI element for the custom file input box.
#' @export
#' @importFrom shiny tags div actionLink
fileInput_modal <- function(inputId, label, multiple = FALSE, accept = NULL, width = NULL, buttonLabel = "Browse...", placeholder = "No file selected", modalId, modalLabel) {
  restoredValue <- shiny::restoreInput(id = inputId, default = NULL)
  if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
    warning("Restored value for ", inputId, " has incorrect format.")
    restoredValue <- NULL
  }
  if (!is.null(restoredValue)) {
    restoredValue <- jsonlite::toJSON(restoredValue, strict_atomic = FALSE)
  }
  inputTag <- shiny::tags$input(id = inputId, name = inputId, type = "file", 
                                style = "display: none;", `data-restore` = restoredValue)
  if (multiple) 
    inputTag$attribs$multiple <- "multiple"
  if (length(accept) > 0) 
    inputTag$attribs$accept <- paste(accept, collapse = ",")
  shiny::div(
    class = "form-group shiny-input-container", style = if (!is.null(width)) 
      paste0("width: ", shiny::validateCssUnit(width), ";"),
    shinyInputLabel(inputId, label), 
    shiny::div(
      class = "input-group", 
      shiny::tags$label(class = "input-group-btn input-group-prepend", 
                        shiny::span(class = "btn btn-default btn-file", 
                                    buttonLabel, inputTag)), 
      shiny::tags$input(type = "text", 
                        class = "form-control", placeholder = placeholder, 
                        readonly = "readonly")),
    shiny::actionLink(inputId = modalId, label = modalLabel),
    shiny::tags$div(
      id = paste(inputId, "_progress", sep = ""), class = "progress active shiny-file-input-progress", 
      shiny::tags$div(class = "progress-bar"))
  )
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
#' @param filename_prefix A character string used as the prefix for the downloaded file's name.
#' @param load_function A function or object that returns the data to be downloaded. 
#'        If it is a function, it will be called within the handler to retrieve the data.
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
create_download_handler <- function(filename_prefix, load_function, file_type = "csv") {
  shiny::downloadHandler(
    filename = function() {
      paste(filename_prefix, file_type, sep = ".")
    },
    content = function(file) {
      obj <- load_function()
      print(paste("Saving to file:", file))  # Log the file path
      print(str(obj))  # Log the structure of the object
      
      if (file_type == "csv") {
        write.csv(obj, file, row.names = FALSE)
      } else if (file_type == "rds") {
        saveRDS(obj, file)  # Try saving the object
      } else {
        stop("Unsupported file type")
      }
    }
  )
}

#' Validate Input and Launch Modal on Error
#'
#' This function validates Shiny input and launches a modal dialog with an error message if validation fails.
#' It is modified from shiny::validate() to launch modal rather than returning a simple text output. 
#' 
#' @param ... Validation conditions to check.
#' @param errorClass A character vector of error classes to assign to the error. Default is an empty character vector.
#' @param delay_time The delay in milliseconds before launching modal. 
#' @param session The Shiny session object.
#' @return Invisible if validation passes; otherwise, stops the reactive process with a modal error message.
#' @export
#' @importFrom shiny removeModal showModal modalDialog h4 p
#' @importFrom rlang list2
#' @importFrom stats na.omit
runValidationModal <- function(..., errorClass = character(0), delay_time = 250) {
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
  
  # Launch modal
  shinyjs::delay(delay_time, { 
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
  # Track if the modal is open
  if (!isTRUE(session$userData$modal_open())) {
      shiny::showModal(shiny::modalDialog(
        shiny::tags$h4(id = "modal-text", message),
        shinyWidgets::progressBar(id = id, value = 0, display_pct = TRUE),
        easyClose = TRUE, footer = NULL
      ))
    
    # Set the modal open state to TRUE
    session$userData$modal_open(TRUE)

    # # Add JavaScript to detect when the modal is closed
    # shinyjs::runjs("
    #   $('#shiny-modal').on('hidden.bs.modal', function () {
    #     Shiny.setInputValue('modal_closed', Math.random());
    #   });
    # ")
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

#' Check if an Object Exists and Load if Not Present
#'
#' This function checks if an object exists in the environment and loads it from a file if it is not present.
#' The function supports loading from different file formats.
#'
#' @param file_path A character string specifying the path to the file.
#' @param name The name of the object to load. If NULL, the object name is inferred from the file name.
#' @param load_function The function to use for loading the file. If NULL, the function is inferred from the file extension.
#' @param envir The environment where the object should be loaded. Default is the global environment.
#' @param ... Additional arguments passed to the load function.
#' @return The loaded object.
#' @export
#' @importFrom tools file_ext
check_and_load <- function(file_path, name = NULL, load_function = NULL, envir = globalenv(), ...) {
  # Extract the object name if not provided
  if (is.null(name)) {
    file_name <- basename(file_path)  # Get the file name from the path
    name <- sub("\\..*$", "", file_name)  # Remove the extension to get the object name
  }

  # Determine the load function if not provided
  if (is.null(load_function)) {
    extension <- tools::file_ext(file_path)
    load_function <- switch(extension,
                            "rds" = readRDS,
                            "csv" = read.csv,
                            stop("Unsupported file extension"))
  }
  
  # Load the object if it doesn't exist in the environment
  if (!exists(name, envir = envir)) {
    obj <- load_function(file_path, ...)
    assign(name, obj, envir = envir)
  } else {
    obj <- get(name, envir = envir)
  }
  return(obj)
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
#' @param file_path Character. The path to the CSV file.
#' 
#' @return A dataframe containing the contents of the CSV file, or NULL if an error occurs.
#' @export
#' @importFrom tools file_ext
#' @importFrom shiny req
#' @importFrom utils read.csv
validate_and_read_csv <- function(file_path) {
  file_ext <- tools::file_ext(file_path)
  runValidationModal(need(file_ext == "csv", "Please upload a CSV file."))
  shiny::req(file_path)
  
  data <- tryCatch({
    utils::read.csv(file_path, stringsAsFactors = FALSE)
  }, error = function(e) {
    runValidationModal("Error reading the file. Please check the file format.")
    return(NULL)
  })
  
  return(data)
}


#' Validate and Read an RDS File
#'
#' This function validates that the file is an RDS, ensures it exists, and reads it into an R object.
#'
#' @param file_path Character. The path to the RDS file.
#' 
#' @return The R object contained in the RDS file, or NULL if an error occurs.
#' @export
#' @importFrom tools file_ext
#' @importFrom shiny req
#' @importFrom base readRDS
validate_and_read_rds <- function(file_path) {
  file_ext <- tools::file_ext(file_path)
  runValidationModal(need(file_ext == "rds", "Please upload an RDS file."))
  shiny::req(file_path)
  
  data <- tryCatch({
    base::readRDS(file_path)
  }, error = function(e) {
    runValidationModal("Error reading the file. Please check the file format.")
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
#' This function dynamically filters data based on a query string from jqr::queryBuilderInput()
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
#' It optionally subsamples a proportion of the columns to reduce the number of predictors.
#'
#' @param gene_functions A data frame containing gene functions with KO IDs and Genome IDs.
#' @param seed An optional seed value for reproducibility. Default is NULL.
#' @param proportion_to_keep An optional proportion of columns to keep when subsampling. Must be between 0 and 1. Default is NULL.
#' @return A data frame of formatted predictors.
#' @export
#' @importFrom dplyr select mutate distinct
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom rlang sym
format_predictors = function(gene_functions, seed = NULL, proportion_to_keep = NULL)
{
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
  
  # Do further formatting
  predictors$Genome <- as.character(predictors$Genome) 
  predictors <- predictors %>%
    dplyr::select(Genome, everything())
  
  # Randomly subsample columns (reduces number of predictors)
  # Set seed
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Subsample columns
  if (!is.null(proportion_to_keep)) {
    if (proportion_to_keep > 1 || proportion_to_keep < 0) {
      stop("Proportion must be between 0 and 1.")
    }
    
    # Exclude the Genome column from subsampling
    non_genome_columns <- setdiff(colnames(predictors), "Genome")
    total_cols <- length(non_genome_columns)
    n_cols <- round(total_cols * proportion_to_keep)
    
    # Sample from non-Genome columns only
    selected_cols <- sample(non_genome_columns, n_cols)
    
    # Rebuild predictors by adding Genome back
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
}
