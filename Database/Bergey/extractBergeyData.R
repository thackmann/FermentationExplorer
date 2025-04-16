# Extract Data from Bergey's Manual
# This script processes HTML files from Bergey's Manual to extract structured data.
# It performs several functions:
# 1. Extracts taxonomy, tables, and various sections of each HTML document.
# 2. Converts text encoding to ASCII to remove non-ASCII characters that may interfere with parsing.
# 3. Extracts genus, species, and subspecies names along with strain IDs, using patterns designed
#    for specific naming conventions and formats found in biological publications.
# 4. Combines extracted data into a structured dataframe, cleans formatting, removes duplicates,
#    and exports the final dataset to a CSV file.
# Requirements:
# - Packages in install/installPackages.R
# - Article files downloaded using downloadBergeyArticles.R script
# Author: Timothy Hackmann
# Date: 13 February 2025

# === Define functions ===

# === Get database directory ===
  database_directory <- FileLocator::getCurrentFileLocation()
  subdirectory <- "/Bergey"
  database_directory <- gsub(paste0(subdirectory, "$"), "", database_directory)

# === Load external R files ===
  setwd(database_directory)
  source("functions\\helperFunctions.R", local = TRUE)
  source("Bergey\\functions.R", local = TRUE)
  
# === Extract data ===
  # Get file names for articles
  article_directory <- paste0(database_directory, "/articles")
  setwd(article_directory)

  article_files = get_matching_files(pattern = "gbm[0-9]{5}(\\.pub[0-9]+[a-z]*)?\\.html$")

  # Initialize a list to store results for each file
  data_list <- list()

  # Loop over files
  for(i in 1:length(article_files)) {
    # Get article link
    article_link <- paste0("https://doi.org/10.1002/9781118960608.",
                         stringr::str_extract(article_files[i], "gbm[0-9]{5}(\\.pub[0-9]+)?"))

    # Extract information from the XML document
    xml_document <- get_xml(article_files[i])
    taxonomy <- get_taxonomy(xml_document)
    tables <- get_tables(xml_document)
    abstract <- get_abstract(xml_document)
    full_text <- get_full_text(xml_document)
    main_text <- get_main_text(full_text, special_genus = taxonomy[5])
    tableless_text <- get_tableless_text(full_text, tables)
    genus_text <- get_genus_text(tableless_text)
    species_text <- get_species_text(tableless_text, special_genus = taxonomy[5])

    # Extract organism names and descriptions
    names <- get_organism_names(genus = taxonomy[5], species_text, special_genus = taxonomy[5])
    species_descriptions <- get_species_descriptions(names, species_text)

    # Flatten species info for each file
    for(j in 1:length(names)) {
      species_name <- get_species_name(genus = taxonomy[5], name = names[j])
      subspecies_name <- get_subspecies_name(genus = taxonomy[5], name = names[j])
      strain_ID <- get_strain_ID(name = names[j], species_description = species_descriptions[j],
                                 special_genus = taxonomy[5])

      # Append a dataframe row for each species
      data_list[[length(data_list) + 1]] <- data.frame(
        Article_link = article_link,
        Taxonomy = paste(taxonomy, collapse = ";"),  # Flatten taxonomy to a single string
        Phylum = taxonomy[1],
        Class = taxonomy[2],
        Order = taxonomy[3],
        Family = taxonomy[4],
        Genus = taxonomy[5],
        Species = species_name,
        Subspecies = subspecies_name,
        Strain = paste(strain_ID, collapse = ";")
      )
    }

    # Show progress
    svMisc::progress(value = i, max = length(article_files))
  }

  # Combine all results into a dataframe
  data <- do.call(rbind, data_list)

  # Clean up formatting
    # Remove extra species
    data = remove_extra_species(data)

    # Remove organisms with no species name
    data = data %>% dplyr::filter(!is.na(Species))

    # Remove duplicate values
    data = data %>% dplyr::distinct(Genus, Species, Subspecies, .keep_all = TRUE)

    # Convert to factor
    data <- data.frame(lapply(data, factor))

  # Export
    setwd(database_directory)
    write.csv(data, file = "Bergey\\data\\Bergey_data.csv")
