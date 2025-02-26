# Get Predictions from FAPROTAX
# This script gets predicted traits from FAPROTAX for organisms in the database
# It formats the database for FAPROTAX, takes results from FAPROTAX, and formats
# them back in a form the database can use
# It is not called during app execution
# Requirements:
# - Packages in install/installPackages.R
# - Data from getLpsnPhylogeny.R script
# - Access to FAPROTAX (http://www.loucalab.com/archive/FAPROTAX/)
# Author: Timothy Hackmann
# Date: 13 February 2025

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

# === Get database directory ===
  database_directory <- FileLocator::getCurrentFileLocation()
  subdirectory <- "/FAPROTAX"
  database_directory <- gsub(paste0(subdirectory, "$"), "", database_directory)
  
# === Load external R files ===
  setwd(database_directory)
  source("utils\\databaseUtils.R", local = TRUE)

# === Load database ===
  # Read in data from getLpsnPhylogeny.R script
  setwd(database_directory)
  lpsn_phylogeny <- utils::read.csv("LPSN\\data\\lpsn_phylogeny.csv")

# === Format data for FAPROTAX ===
  # Format taxonomy
    taxonomy = lpsn_phylogeny %>% dplyr::select(Phylum, Class, Order, Family, Genus, Species)
    taxonomy = apply(taxonomy, 1, paste, collapse = ";")

  # Get OTU table
    otu_table <- data.frame(`#OTU` = seq_along(taxonomy), ID = "1.0", taxonomy = taxonomy, check.names = FALSE)

  # Export
    FAPROTAX_directory <- "C:\\My Directory" # Set to directory for FAPROTAX
    setwd(FAPROTAX_directory)
    write.table(otu_table, "otu_table.tsv", sep = "\t", row.names = FALSE, quote = FALSE)

# === Install and run FAPROTAX ===
  # These steps are done outside R
  # In Firefox, download Anaconda from
  # https://www.anaconda.com/
  #
  # In Windows Explorer, install Anaconda
  #
  # In Windows Explorer, set environmental variable PATH with
  # C:\Users\UserName\AppData\Local\Programs\Python\Python37
  # C:\Users\UserName\Local\Programs\Python\Python38-32
  # C:\Users\UserName\Local\Programs\Python\Python38-32\Scripts
  # C:\Users\UserName\Local\continuum\anaconda3\lib\site-packages
  #
  # In Anaconda prompt, run
  # python -m pip install numpy
  # conda config --add channels defaults
  # conda config --add channels bioconda
  # conda config --add channels conda-forge
  # conda install -c bioconda biom-format
  # conda install -c bioconda h5py
  #
  # In Firefox, download FAPROTAX from
  # http://www.loucalab.com/archive/FAPROTAX/lib/php/index.php?section=Download
  # In Windows Explorer, unzip to
  # C:\Users\UserName\Downloads
  #
  # In Anaconda prompt, run
  # cd C:\Users\UserName\Downloads\FAPROTAX_1.2.10\FAPROTAX_1.2.10
  # python collapse_table.py -h
  # If help file is displayed, installation was successful.
  #
  # To make predictions, use otu_table.tsv exported above and run
  # cd C:\Users\UserName\Downloads\FAPROTAX_1.2.10\FAPROTAX_1.2.10
  # python collapse_table.py -i otu_table.tsv -o functional_table.tsv -g FAPROTAX.txt -c "#" -d "taxonomy" --omit_columns 0 --column_names_are_in last_comment_line -r report.txt -n columns_after_collapsing -v
  #
  # If an error occurs, compare structure of otu_table.tsv against expample table from
  # https://pages.uoregon.edu/slouca/LoucaLab/archive/FAPROTAX/SECTION_Instructions/files/example_01/otu_table.tsv

# === Read in predictions from FAPROTAX ===
  setwd(FAPROTAX_directory)
  FAPROTAX_data = read_faprotax_report("report.txt")

# Export
  setwd(database_directory)
  write.csv(FAPROTAX_data, file = "FAPROTAX\\data\\FAPROTAX_data.csv")
