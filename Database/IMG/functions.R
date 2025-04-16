# Define Functions for Obtaining Data from IMG
# These are functions specific to this data source
# Author: Timothy Hackmann
# Date: 4 April 2025

#' Read KO Tab File
#'
#' Reads a `.ko.tab.txt` file from a specified folder path. The function looks
#' for a file named according to the folder name (e.g., `folder_name.ko.tab.txt`)
#' and reads it into a dataframe if it exists.
#'
#' @param folder_path A string representing the path to the folder containing the `.ko.tab.txt` file.
#'
#' @return A dataframe with the contents of the `.ko.tab.txt` file.
#' Returns `NULL` if the file does not exist.
#'
#' @details This function expects a specific file naming convention where the
#' file name is derived from the folder name (i.e., `folder_name.ko.tab.txt`).
#' It reads the file as a tab-delimited text file, handling NA values, trimming whitespace,
#' and ignoring column type messages.
#'
#' @importFrom readr read_delim
#'
#' @examples
#' \dontrun{
#' # Example usage
#' data <- read_ko_tab_file("path/to/folder")
#' }
#'
#' @export
read_ko_tab_file <- function(folder_path) {
  # Extract the folder name from the given path
  folder_name <- basename(folder_path)
  
  # Define the expected file name format
  file_name <- paste0(folder_name, ".ko.tab.txt")
  
  # Construct the full file path
  file_path <- file.path(folder_path, file_name)
  
  # Check if the file exists
  if (!file.exists(file_path)) {
    return(NULL)
  }
  
  # Read in the contents as a dataframe
  data <- readr::read_delim(file_path, delim = "\t", col_names = TRUE, quote = "\"",
                            escape_double = TRUE, na = c("", "NA"),
                            trim_ws = TRUE, show_col_types = FALSE)
  
  return(data)
}

