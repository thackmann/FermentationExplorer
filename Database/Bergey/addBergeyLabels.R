# Add Labels to Data from Bergey's Manual
# This script adds labels for metabolism or other traits to metadata from Bergey's Manual
# Requirements:
# - Packages in install/installPackages.R
# - Metadata for Bergey's Manual from extractBergeyData.R script
# - Labels for metabolism or other traits from Bergey's Manual
# Author: Timothy Hackmann
# Date: 13 February 2025

# === Get database directory ===
  database_directory <- FileLocator::getCurrentFileLocation()
  subdirectory <- "/Bergey"
  database_directory <- gsub(paste0(subdirectory, "$"), "", database_directory)

# === Load external R files ===
  setwd(database_directory)
  source("functions\\helperFunctions.R", local = TRUE)
  source("Bergey\\functions.R", local = TRUE)

# === Read in data ===
  setwd(database_directory)

  # Read in data from extractBergeyData.R script
  data <- utils::read.csv("Bergey\\data\\Bergey_data.csv")

  # Read in labels
  labels <- utils::read.csv("Bergey\\data\\labels.csv")

# === Add labels  ===
  # Add phylogeny
  matches = match(
    x = paste0(labels$Genus, labels$Species, labels$Subspecies, labels$Strain),
    table = paste0(data$Genus, data$Species, labels$Subspecies, data$Strain)
    )

  # Use indices to add phylogeny to database
  data <- add_columns_based_on_indices(
    target_df =  data,
    source_df = labels,
    target_index = matches,
    source_index = seq_along(matches),
    source_col_names = c(
      "Type_of_metabolism",
      "Text_for_end_products",
      "Major_end_products",
      "Minor_end_products",
      "Text_for_substrates",
      "Substrates_for_end_products"
    ),
    target_col_names = c(
      "Type_of_metabolism",
      "Text_for_end_products",
      "Major_end_products",
      "Minor_end_products",
      "Text_for_substrates",
      "Substrates_for_end_products"
    )
  )

# === Export  ===
  write.csv(data, file = "Bergey\\data\\Bergey_data_with_labels.csv")
