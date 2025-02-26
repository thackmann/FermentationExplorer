# Install Packages for App
# This script checks for packages required by the app and then installs if
# needed.  It also imports functions.
# Author: Timothy Hackmann
# Date: 14 February 2025

# === Define functions ===
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

# === Install packages ===
  # Define required packages
  cran_packages <- unique(c(
    "ape", "dplyr", "httr", "import", "jqbr", "KeyboardSimulator", "magrittr", 
    "polite", "purrr", "readr", "remotes", "rvest", "stringr", "svMisc", "utils"
  ))
  bioc_packages <- c("Biostrings", "ShortRead")
  github_packages <- c("thackmann/FileLocator" = "FileLocator")
  
  # Install missing packages
  install_missing_cran_packages(cran_packages)
  install_missing_bioc_packages(bioc_packages)
  install_missing_github_packages(github_packages)