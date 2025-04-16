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

# === Get database directory ===
  database_directory <- FileLocator::getCurrentFileLocation()
  subdirectory <- "/FAPROTAX"
  database_directory <- gsub(paste0(subdirectory, "$"), "", database_directory)
  
# === Load external R files ===
  setwd(database_directory)
  source("functions\\helperFunctions.R", local = TRUE)
  source("FAPROTAX\\functions.R", local = TRUE)

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
