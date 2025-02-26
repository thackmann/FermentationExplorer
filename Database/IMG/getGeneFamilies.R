# Get Gene Families
# This script gets gene families (KO IDs) for organisms in the database
# It exports them as an *rds object for use in the app
# Requirements:
# - Packages in install/installPackages.R
# - List of IMG genome IDs from database.csv
# - Access to IMG (https://img.jgi.doe.gov/m/)
# Author: Timothy Hackmann
# Date: 19 February 2025

# === Define functions ===
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

  
# === Get database directory ===
  database_directory <- FileLocator::getCurrentFileLocation()
  subdirectory <- "/IMG"
  database_directory <- gsub(paste0(subdirectory, "$"), "", database_directory)

# === Load external R files ===
  setwd(database_directory)
  source("utils\\databaseUtils.R", local = TRUE)

# ===  Use files with IMG genome IDs to download genomes from IMG ===
  ## Navigate to IMG, log on, and then navigate to Find Genomes (https://img.jgi.doe.gov/cgi-bin/mer/main.cgi?section=GenomeSearch&page=searchForm)
  ## Get values of IMG genomes_ID_max_genes from database.csv
  ## Paste these values into search bar.  In "Search by ID (list)" field, choose "IMG Genome ID (IMG Taxon ID)".  Click "Search".
  ## In the screen that appears, click "Select All" and "Add Selected to Genome Cart".  Repeat for remaining files.
  ## In the Genome Cart screen that appears, click the check box in the left corner (to select all genomes).
  ## Click the "Upload & Export & Save" tab, scroll to "Export Genomes", then click "Download Genomes"
  ## Wait for download link to be emailed, then proceed with downloading
  ## Unzip download (large file) locally

#=== Process genome files ===
  # Unzip individual genome files
    genome_directory <- "C:\\My Directory" # Set to directory where files above were downloaded
    genome_files = get_matching_files(directory = genome_directory, pattern = "\\.tar.gz$")

    for(i in 1:length(genome_files))
    {
      untar(genome_files[i], exdir = genome_directory)

      # Show progress
      svMisc::progress(value = i, max = length(genome_files))
    }

  # Extract KO IDs from genomes files
    genome_folders = list.dirs(genome_directory, full.names = TRUE, recursive = FALSE)

  # Initialize an empty list to store each dataframe
    KO_data <- list()

  # Initialize a vector to store missing IMG_genome_IDs
    missing_files <- c()

  # Loop over genome folders
    for (i in seq_along(genome_folders)) {
      # Extract the folder name as IMG_genome_ID
      IMG_genome_ID <- basename(genome_folders[i])

      # Try to read the file
      data <- read_ko_tab_file(genome_folders[i])

      # Check if data is NULL, indicating the file was missing
      if (is.null(data)) {
        # Add the IMG_genome_ID to missing_files and continue to the next iteration
        missing_files <- c(missing_files, IMG_genome_ID)
        next
      }

      # Add the IMG_genome_ID column to the dataframe
      data <- data %>%
        dplyr::mutate(IMG_genome_ID = IMG_genome_ID)

      # Store the dataframe in the list
      KO_data[[i]] <- data

      svMisc::progress(value = i, max = length(genome_folders))
    }

  # Combine all dataframes into one
    KO_data <- dplyr::bind_rows(KO_data)

  # Reduce size of dataframe
    KO_data_compressed <- KO_data %>%
      dplyr::select(IMG_genome_ID, ko_id) %>%        # Select relevant columns
      dplyr::distinct() %>%                          # Remove duplicates
      dplyr::mutate(ko_id = gsub(pattern = "KO:", "", x = ko_id)) %>%  # Remove 'KO:' prefix
      dplyr::dplyr::rename(Genome = IMG_genome_ID, Database_ID = ko_id) %>%  # Rename columns
      dplyr::mutate(across(everything(), as.factor)) %>%  # Convert to factors
      droplevels()  # Drop unused factor levels

#===  Export ===
  setwd(database_directory)
  saveRDS(KO_data_compressed, file = paste0(database_directory, "gene_functions_database.rds"))
