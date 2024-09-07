# Utility Functions for App
# This script contains various utility functions for use in the Shiny app,
# including custom operators, file input handling, validation, and data cleaning.
# Author: Timothy Hackmann
# Date: 6 September 2024

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
#' @importFrom utils read.table
read_custom_file <- function(filepath) {
  info <- detect_delim_and_header(filepath)
  delim <- info$delim
  has_header <- info$has_header
  
  data <- utils::read.table(filepath, sep = delim, header = has_header, stringsAsFactors = FALSE, fill = TRUE)
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

#' Validate Input and Launch Modal on Error
#'
#' This function validates Shiny input and launches a modal dialog with an error message if validation fails.
#' It is modified from shiny::validate() to launch modal rather than returning a simple text output
#'
#' @param ... Validation conditions to check.
#' @param errorClass A character vector of error classes to assign to the error. Default is an empty character vector.
#' @return Invisible if validation passes; otherwise, stops the reactive process with a modal error message.
#' @export
#' @importFrom shiny removeModal showModal modalDialog h4 p
#' @importFrom rlang list2
#' @importFrom stats na.omit
runValidationModal <- function(..., errorClass = character(0)) {
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
  
  #Close existing modals
  shiny::removeModal()
  
  #Launch modal
  shiny::showModal(shiny::modalDialog(
    shiny::h4("Error in input"),
    shiny::p(paste(results)),
    easyClose = TRUE, footer = NULL
  ))
  
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

#' Launch Modal with Progress Bar
#'
#' This function launches a modal with a progress bar and an optional message. 
#' The progress bar starts at 0% and can be updated as needed.
#'
#' @param session The Shiny session object.
#' @param id The id of the progress bar.
#' @param message Optional message to display above the progress bar (default: "Initializing").
#'
#' @importFrom shinyWidgets updateProgressBar
#' @importFrom shiny showModal modalDialog
#' @importFrom shiny tags
#' 
#' @export
launch_modal_with_progress <- function(session, id, message = "Initializing") {
  # Initialize progress bar at 0%
  shinyWidgets::updateProgressBar(session = session, id = id, value = 0)
  
  # Show modal with progress bar and message
  shiny::showModal(shiny::modalDialog(
    shiny::tags$h4(id = "modal-text", message),
    shinyWidgets::progressBar(id = id, value = 0, display_pct = TRUE),
    easyClose = TRUE, footer = NULL
  ))
}

#' Hide Modal with Progress Bar
#'
#' This function hides a modal with a progress bar.  It includes a brief delay 
#' to ensure UI elements are fully updated before the modal is removed.
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

#' Process Gene Functions from a Database
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