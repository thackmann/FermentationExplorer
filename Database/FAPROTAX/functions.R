# Define Functions for Obtaining Data from FAPROTAX
# These are functions specific to this data source
# Author: Timothy Hackmann
# Date: 4 April 2025

# === Define functions ===
#' Remove Header from FAPROTAX Report
#'
#' @description Removes the header section from the report, starting after the line
#' that reads "# Detailed group assignments are listed below".
#' @param lines A character vector of lines from the FAPROTAX report.
#' @return A character vector with the header removed.
#' @export
remove_header <- function(lines) {
  start_index <- which(grepl("^# Detailed group assignments are listed below", lines))
  
  if (length(start_index) > 0) {
    lines <- lines[(start_index + 1):length(lines)]
  } else {
    stop("The line '# Detailed group assignments are listed below' was not found.")
  }
  
  return(lines)
}


#' Split Lines into Groups
#'
#' @description Splits lines into groups based on headers (lines that start with "# ")
#' and ending with a blank line.
#' @param lines A character vector of lines with header removed.
#' @return A list where each name is a group and each element is a vector of taxonomy entries.
#' @importFrom stringr word
#' @export
split_into_groups <- function(lines) {
  groups <- list()
  current_group <- NULL
  current_entries <- c()
  
  for (line in lines) {
    if (grepl("^# ", line)) {
      if (!is.null(current_group)) {
        groups[[current_group]] <- current_entries
      }
      
      cleaned_name <- gsub("^#\\s*", "", line)
      current_group <- stringr::word(cleaned_name, 1)
      current_entries <- c()
      
    } else if (line == "") {
      if (!is.null(current_group)) {
        groups[[current_group]] <- current_entries
        current_group <- NULL
        current_entries <- c()
      }
      
    } else if (!is.null(current_group)) {
      current_entries <- c(current_entries, trimws(line))
    }
  }
  
  if (!is.null(current_group)) {
    groups[[current_group]] <- current_entries
  }
  
  return(groups)
}

#' Flatten Groups into Dataframe
#'
#' @description Flattens a list of groups into a dataframe with two columns: `group` and `taxonomy`.
#' @param groups A list where each name is a group and each element is a vector of taxonomy entries.
#' @return A dataframe with `group` and `taxonomy` columns.
#' @export
flatten_groups <- function(groups) {
  do.call(rbind, lapply(names(groups), function(group) {
    data.frame(
      group = group,
      taxonomy = groups[[group]]
    )
  }))
}

#' Concatenate Groups by Taxonomy
#'
#' @description For rows with the same taxonomy, concatenates group names in alphabetical order, separated by a semicolon.
#' @param df A dataframe with `group` and `taxonomy` columns.
#' @return A dataframe with unique `taxonomy` values and concatenated `group` names.
#' @importFrom dplyr group_by summarize select
#' @export
concatenate_groups_by_taxonomy <- function(df) {
  df %>%
    dplyr::group_by(taxonomy) %>%
    dplyr::summarize(
      group = paste(sort(unique(group)), collapse = ";"),
      .groups = "drop"
    ) %>%
    dplyr::select(group, taxonomy)
}


#' Read FAPROTAX Report File
#'
#' @description Reads a FAPROTAX report file and processes it into a structured dataframe.
#' @param file_path The path to the FAPROTAX report file.
#' @return A dataframe with two columns: `group` and `taxonomy`.
#' @importFrom readr read_lines
#' @importFrom dplyr group_by summarize select
#' @importFrom stringr word
#' @export
read_faprotax_report <- function(file_path) {
  # Step 1: Read the file
  lines <- readLines(file_path)
  
  # Step 2: Remove header
  lines <- remove_header(lines)
  
  # Step 3: Split into groups
  groups <- split_into_groups(lines)
  
  # Step 4: Flatten groups to dataframe
  flat_df <- flatten_groups(groups)
  
  # Step 5: Concatenate groups by taxonomy
  concatenated_df <- concatenate_groups_by_taxonomy(flat_df)
  
  return(concatenated_df)
}
