# Utility Functions for Database
# This script contains various utility functions for use in the constructing the 
# database, including operators, dataframe manipulation, and web scraping.
# Author: Timothy Hackmann
# Date: 14 February 2025

# === Define functions ===
  #' Pipe Operator
  #'
  #' This operator is imported from the magrittr package and is used to chain operations together.
  #'
  import::from(magrittr, "%>%")

  #' Install Missing CRAN Packages
  #'
  #' This function checks for missing CRAN packages and installs them if they are not already installed.
  #'
  #' @param packages A character vector of CRAN package names to check and install if missing.
  #' @return None. The function installs missing packages and provides a message if installation occurs.
  #' @examples
  #' cran_packages <- c("dplyr", "ggplot2")
  #' install_missing_cran_packages(cran_packages)
  install_missing_cran_packages <- function(packages) {
    # Identify missing packages
    missing_cran <- packages[!(packages %in% installed.packages()[, "Package"])]
    
    # Check if there are any missing packages
    if (length(missing_cran) > 0) {
      message("Installing missing CRAN packages: ", paste(missing_cran, collapse = ", "))
      install.packages(missing_cran)
    } else {
      message("All packages are already installed.")
    }
  }
  
  #' Install Missing Bioconductor Packages
  #'
  #' This function checks for missing Bioconductor packages and installs them using BiocManager if they are not already installed.
  #'
  #' @param packages A character vector of Bioconductor package names to check and install if missing.
  #' @return None. The function installs missing packages and provides a message if installation occurs.
  #' @examples
  #' bioc_packages <- c("Biostrings", "GenomicRanges")
  #' install_missing_bioc_packages(bioc_packages)
  install_missing_bioc_packages <- function(packages) {
    # Ensure BiocManager is installed
    if (!requireNamespace("BiocManager", quietly = TRUE)) {
      install.packages("BiocManager")
    }
    
    # Identify missing packages
    missing_bioc <- packages[!(packages %in% installed.packages()[, "Package"])]
    
    # Check if there are any missing packages
    if (length(missing_bioc) > 0) {
      message("Installing missing Bioconductor packages: ", paste(missing_bioc, collapse = ", "))
      BiocManager::install(missing_bioc)
    } else {
      message("All Bioconductor packages are already installed.")
    }
  }
  
  #' Install Missing GitHub Packages
  #'
  #' This function checks for missing GitHub packages and installs them if they are not already installed.
  #'
  #' @param packages A named character vector of GitHub repository names in the format "username/repository".
  #'        If a package requires installation from a subdirectory, specify it as a named element where
  #'        the name is the repo and the value is the subdirectory.
  #' @return None. The function installs missing GitHub packages and provides a message if installation occurs.
  #' @examples
  #' github_packages <- c("r-lib/remotes", "thackmann/FileLocator" = "FileLocator")
  #' install_missing_github_packages(github_packages)
  install_missing_github_packages <- function(packages) {
    # Ensure remotes package is installed
    if (!requireNamespace("remotes", quietly = TRUE)) {
      install.packages("remotes")
    }
    
    # Extract package names from repo paths
    repo_names <- ifelse(names(packages) != "", names(packages), sub(".*/", "", packages))
    
    # Identify missing packages
    missing_github <- packages[!(repo_names %in% installed.packages()[, "Package"])]
    
    # Check if there are any missing packages
    if (length(missing_github) > 0) {
      message("Installing missing GitHub packages: ", paste(names(missing_github), collapse = ", "))
      
      for (repo in names(missing_github)) {
        subdir <- missing_github[repo]
        if (subdir == "") {
          remotes::install_github(repo)
        } else {
          remotes::install_github(repo, subdir = subdir)
        }
      }
    } else {
      message("All GitHub packages are already installed.")
    }
  }

  #' Add columns to a target dataframe based on indices from vectors.
  #'
  #' This function adds columns from a source dataframe to a target dataframe
  #' based on indices from two vectors. It allows adding values from multiple
  #' columns in the source dataframe to specified columns in the target dataframe.
  #'
  #' @param target_df Dataframe. The target dataframe to which the new columns will be added.
  #' @param source_df Dataframe. The source dataframe containing the values to add.
  #' @param target_index Integer vector. The indices of the target dataframe where the new values will be placed.
  #' @param source_index Integer vector. The indices of the source dataframe corresponding to the values to be added.
  #' @param source_col_names Character vector. A vector of column names in the source dataframe to be added.
  #' @param target_col_names Character vector. A vector of column names in the target dataframe where values will be placed.
  #'   This should be the same length as `source_col_names`.
  #' @param sep Character. A separator to use when concatenating values from the source dataframe. Default is ','.
  #'
  #' @return The target dataframe with the new columns added.
  #' @export
  #' @examples
  add_columns_based_on_indices <- function(target_df, source_df, target_index, source_index, source_col_names, target_col_names = source_col_names, sep = ",") {
    # Check if the lengths of target_index and source_index match
    if (length(target_index) != length(source_index)) {
      stop("Length of target_index and source_index must be the same.")
    }
    
    # Check if the lengths of source_col_names and target_col_names match
    if (length(source_col_names) != length(target_col_names)) {
      stop("Length of source_col_names and target_col_names must be the same.")
    }
    
    # Convert target_index and source_index into a dataframe for easy grouping
    index_df <- data.frame(target_index = target_index, source_index = source_index)
    
    # Loop over each column name pair and add the corresponding values from the source dataframe to the target dataframe
    for (i in seq_along(source_col_names)) {
      source_col <- source_col_names[i]
      target_col <- target_col_names[i]
      
      # Ensure the target dataframe has the new column initialized with NA
      target_df[[target_col]] <- NA
      
      # Loop over each unique target index
      for (t_index in unique(index_df$target_index)) {
        # Get the source indices that correspond to this target index
        matching_source_indices <- index_df$source_index[index_df$target_index == t_index]
        
        # Extract unique values from the source dataframe and concatenate them
        unique_values <- unique(source_df[[source_col]][matching_source_indices])
        
        # Remove NA values from unique values (unless all values are NA)
        if (!all(is.na(unique_values))) {
          unique_values <- unique_values[!is.na(unique_values)]
        }
        
        # Concatenate the unique values with the specified separator
        concatenated_values <- paste(unique_values, collapse = sep)
        
        # Assign the concatenated values to the corresponding target index in the target column
        target_df[[target_col]][t_index] <- concatenated_values
      }
    }
    
    return(target_df)
  }
  
  #' Retrieve the Body of a Web Page
  #'
  #' This function retrieves the body content of a web page using the `polite` package.
  #'
  #' @param url A character string representing the URL of the web page to scrape.
  #' @param user_agent A character string specifying the user agent to use for the request. 
  #'        Default is `"me"`.
  #'
  #' @return An object containing the HTML body of the web page.
  #' @export
  #'
  #' @examples
  #' # Example usage:
  #' body <- get_web_page_body("https://example.com", user_agent = "me")
  #'
  get_web_page_body <- function(url, user_agent = "me") {
    # Initiate a session with the specified URL
    session <- polite::bow(url, user_agent = user_agent)
    
    # Scrape the body of the page
    body <- polite::scrape(bow = session)
    
    return(body)
  }
  
  # --- Clean database file and add links ---
  #' Save Data in Original Format and Zip It
  #'
  #' This function saves a dataframe in its original format (e.g., CSV, RDS), 
  #' compresses it into a ZIP archive, and optionally removes the original file.
  #'
  #' @param data A dataframe to be saved.
  #' @param fp A character string specifying the full file path, including the original extension (e.g., "data/file.csv").
  #' @param remove_original A logical value indicating whether to delete the original file after zipping. Default is `TRUE`.
  #' @param overwrite A logical value indicating whether to overwrite an existing ZIP file. Default is `TRUE`.
  #' 
  #' @return The function does not return anything. It writes the file to disk.
  #' 
  #' @export
  save_as_zip <- function(data, fp, remove_original = TRUE, overwrite = TRUE) {
    # Ensure the zip package is available
    if (!requireNamespace("zip", quietly = TRUE)) stop("Package 'zip' is required. Install it with install.packages('zip')")
    
    # Normalize the file path
    fp <- normalizePath(fp, winslash = "/", mustWork = FALSE)
    
    # Extract file name, extension, and directory
    ext <- tools::file_ext(fp)
    if (ext == "") stop("File path must include an extension (e.g., .csv, .rds).")
    
    file_name <- basename(fp)  # e.g., "database_clean.csv"
    dir_name <- dirname(fp)    # e.g., "data"
    
    # Create a folder with the same name as the object
    object_name <- tools::file_path_sans_ext(file_name)  # e.g., "database_clean"
    zip_folder <- file.path(dir_name, object_name)
    
    # Ensure the folder is clean
    if (dir.exists(zip_folder)) unlink(zip_folder, recursive = TRUE)
    dir.create(zip_folder)
    
    # Define paths
    save_fp <- file.path(zip_folder, file_name)  # Save inside the new folder
    zip_fp <- file.path(dir_name, paste0(object_name, ".zip"))  # ZIP file in same directory
    
    # Save file based on extension
    switch(ext,
           "csv" = write.csv(data, save_fp, row.names = FALSE),
           "rds" = saveRDS(data, save_fp),
           stop("Unsupported file format: ", ext)
    )
    
    # Overwrite existing ZIP file if specified
    if (overwrite && file.exists(zip_fp)) file.remove(zip_fp)
    
    # Create ZIP file using zip::zipr()
    zip::zipr(zip_fp, files = save_fp, recurse = FALSE, compression_level = 9)
    
    # Ensure the ZIP file was created before deleting the original
    if (file.exists(zip_fp) && remove_original) unlink(zip_folder, recursive = TRUE)
  }
  