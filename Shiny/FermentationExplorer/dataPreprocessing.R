# Preprocess Data for Shiny App
# This script generates data files for the app
# It is not called during app execution
# Requirements
# - Packages in installPackages.R
# Author: Timothy Hackmann
# Date: 26 February 2025

# === Set system locale ===
Sys.setlocale("LC_ALL", "C")

# === Get app directory ===
  app_directory <- FileLocator::getCurrentFileLocation()
  
# === Load external R files ===
  # Load external R files
  setwd(app_directory)
  source("utilityFunctions.R", local = TRUE)
  source("loadDataFunctions.R", local = TRUE)
  source("plotFunctions.R", local = TRUE) 
  
# === Preprocess data ===
    # --- Clean database file and add links ---
    #' Save Data in Original Format and Zip It
    #'
    #' This function saves a dataframe in its original format (e.g., CSV, RDS), 
    #' compresses it into a ZIP archive, and optionally removes the original file.
    #'
    #' @param data A dataframe to be saved.
    #' @param fp A character string specifying the full file path, including the original extension (e.g., "data/file.csv").
    #' @param remove_original A logical value indicating whether to delete the original file after zipping. Default is `TRUE`.
    #' 
    #' @return The function does not return anything. It writes the file to disk.
    #' 
    #' @export
    save_as_zip <- function(data, fp, remove_original = TRUE) {
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
      
      if (!dir.exists(zip_folder)) dir.create(zip_folder)  # Ensure the folder exists
      
      # Define paths
      save_fp <- file.path(zip_folder, file_name)  # Save inside the new folder
      zip_fp <- file.path(dir_name, paste0(object_name, ".zip"))  # ZIP file in same directory
      
      # Save file based on extension
      switch(ext,
             "csv" = write.csv(data, save_fp, row.names = FALSE),
             "rds" = saveRDS(data, save_fp),
             stop("Unsupported file format: ", ext)
      )
      
      # Create ZIP file using zip::zipr() - NO unnecessary folder structure!
      zip::zipr(zip_fp, files = save_fp, recurse = FALSE, compression_level = 9)
      
      # Remove the original folder if specified
      if (remove_original) unlink(zip_folder, recursive = TRUE)
    }
    
    #' Collapse Taxonomy Vector into String
    #'
    #' Converts a vector of taxonomic levels into a Greengenes-compatible taxonomy string.
    #'
    #' @param tax_vector A character vector containing taxonomic levels in order:
    #'   domain, phylum, class, order, family, genus, species.
    #' @return A single character string formatted as a Greengenes taxonomy string.
    #' @examples
    #' tax_vector <- c("Bacteria", "Abditibacteriota", "Abditibacteriia", "Abditibacteriales", "Abditibacteriaceae", "Abditibacterium", "tutsteinense")
    #' format_Taxonomy(tax_vector)
    #' @export
    collapse_taxonomy <- function(tax_vector) {
      # Replace NA values with empty strings
      tax_vector <- ifelse(is.na(tax_vector), "NA", tax_vector)

      # Define prefixes for each taxonomic rank
      prefixes <- c("d__", "p__", "c__", "o__", "f__", "g__", "s__")

      # Ensure there are as many prefixes as taxonomic levels
      if (length(tax_vector) != length(prefixes)) {
        stop("The input vector does not have the correct number of taxonomic levels.")
      }

      # Format the taxonomy string with prefixes
      tax_string <- paste0(prefixes, tax_vector, collapse = ";")

      # Handle the species name (combine genus and species)
      tax_string <- sub("s__.*", paste0("s__", tax_vector[6], " ", tax_vector[7]), tax_string)

      return(tax_string)
    }

    #' Create HTML Link to External Website
    #'
    #' This function creates an HTML link to an external website using a vector of comma-separated IDs.
    #' Each ID is converted into an individual clickable link.
    #'
    #' @param IDs A character vector of comma-separated IDs.
    #' @param url A character string specifying the base URL for the links.
    #' @return A character vector of HTML links corresponding to the IDs.
    #' @export
    createLink <- function(IDs, url) {
      IDs <- as.character(IDs)
      IDs[IDs == "NA"] <- NA

      links <- lapply(IDs, function(ID) {
        if (!is.na(ID)) {
          ID_split <- strsplit(x = ID, split = ",")[[1]]
          individual_links <- sprintf('<a href="%s" target="_blank">%s</a>', URLdecode(paste0(url, ID_split)), ID_split)
          paste(individual_links, collapse = ",")
        } else {
          "NA"
        }
      })

      return(unlist(links))
    }

    #' Create HTML Link Buttons
    #'
    #' This function creates HTML link buttons for a given set of URLs.
    #' Each URL is converted into a clickable button. If a URL is NA, the output will be NA.
    #'
    #' @param urls A character vector of URLs.
    #' @return A character vector of HTML link buttons corresponding to the URLs, or NA if the input URL is NA.
    #' @export
    createLinkButton <- function(urls) {
      urls <- as.character(urls)
      # Explicitly handle NAs
      links <- ifelse(is.na(urls), NA_character_,
                      sprintf('<a href="%s" target="_blank" class="btn btn-primary">Link</a>', URLdecode(urls)))
      return(links)
    }

    #' Clean Data from External Sources
    #'
    #' This function cleans data imported from external sources (BacDive and FAPROTAX)
    #' It replaces patterns, converts values to numeric,
    #' and optionally converts text to lowercase.
    #'
    #' @param x A character vector containing the data to be cleaned.
    #' @param is_numeric Logical. If TRUE, numeric cleaning is applied. Default is FALSE.
    #' @param to_lower Logical. If TRUE, text is converted to lowercase. Default is FALSE.
    #' @return A cleaned character vector.
    #' @export
    clean_external_data = function(x, is_numeric = FALSE, to_lower = FALSE) {
      if(is_numeric == TRUE) {
        x = as.character(x)

        # Replace all patterns at once
        x = gsub(pattern = "<|>|day[s]*|_| |\\.$|\\.-", replacement = "", x = x)

        # Calculate means of split values
        ## Applying this to 20-30 would give 25
        ## Applying this to 20-30;40-50 would give 35
        z <- strsplit(x, ";")
        z <- lapply(z, function(part) {
          part_means <- sapply(part, function(subpart) {
            sub_values <- strsplit(subpart, "-")[[1]]
            num_values <- as.numeric(sub_values[!grepl("\\D", sub_values)])  # Filter out non-numeric values
            if (length(num_values) > 0) mean(num_values) else NA
          })
          mean(part_means, na.rm = TRUE)
        })

        x = sapply(z, mean, na.rm = TRUE)

        x = as.numeric(x)

      } else if(is_numeric == FALSE) {
        x = gsub(pattern = "^0$", replacement = "-", x = x)
        x = gsub(pattern = "^1$", replacement = "+", x = x)
        x = gsub(pattern = "^0;1$", replacement = "-", x = x)
        x = gsub(pattern = "^1;0$", replacement = "+", x = x)
        x = gsub(pattern = "#", replacement = "", x = x)
        x = gsub(pattern = "_", replacement = " ", x = x)
      }

      if(to_lower == TRUE) {
        x = tolower(x)
      }

      return(x)
    }

    #' Convert Salt Concentration to mol/L
    #'
    #' This function takes two vectors: one for salt concentration values and one for units.
    #' It converts all concentration values to mol/L, assuming NaCl as the standard salt for conversion.
    #'
    #' @param concentration A numeric vector of salt concentration values.
    #' @param unit A character vector of corresponding units for each concentration.
    #' @param molecular_weight A numeric value specifying the molecular weight of the salt (default: NaCl, 58.44 g/mol).
    #'
    #' @return A numeric vector of concentrations in mol/L.
    #'
    #' @examples
    #' # Example usage with concentration values and units:
    #' transform_salt_concentration(c(1, 2, NaN, 4), c("g/L", "%", "M", "g/L"))
    #'
    convert_salt_concentration <- function(concentration, unit, molecular_weight = 58.44) {

      # Initialize a vector to store the results
      converted_concentration <- numeric(length(concentration))

      # Loop over each concentration and unit
      for (i in seq_along(concentration)) {
        # Handle NaN, NA, or NA unit
        if (is.nan(concentration[i]) || is.na(concentration[i]) || is.na(unit[i])) {
          converted_concentration[i] <- NA
        } else {
          # Convert based on the unit
          if (unit[i] == "g/L") {
            # g/L to mol/L: divide by molecular weight
            converted_concentration[i] <- concentration[i] / molecular_weight
          } else if (unit[i] == "%" || unit[i] == "%(w/v)") {
            # % (w/v): convert to g/L, then to mol/L (1% = 10 g/L)
            g_per_l <- concentration[i] * 10
            converted_concentration[i] <- g_per_l / molecular_weight
          } else if (unit[i] == "M") {
            # M (mol/L): no conversion needed
            converted_concentration[i] <- concentration[i]
          } else {
            # If the unit is not recognized, set it to NA
            converted_concentration[i] <- NA
          }
        }
      }

      return(converted_concentration)
    }

    # Get data
      data <- load_raw_database()

    # Clean data
      data[] <- lapply(data, as.character)

      data_vars <- c("BacDive_Antibiotic_resistance", "BacDive_Antibiotic_sensitivity", "BacDive_Cell_length",
                     "BacDive_Cell_shape", "BacDive_Cell_width", "BacDive_Colony_size",
                     "BacDive_Flagellum_arrangement", "BacDive_Gram_stain", "BacDive_Incubation_period",
                     "BacDive_Indole_test", "BacDive_Motility", "BacDive_Oxygen_tolerance",
                     "BacDive_pH_for_growth", "BacDive_Pathogenicity_animal", "BacDive_Pathogenicity_human",
                     "BacDive_Pathogenicity_plant", "BacDive_Salt_concentration", "BacDive_Salt_concentration_unit",
                     "BacDive_Spore_formation", "BacDive_Temperature_for_growth", "BacDive_Voges_proskauer",
                     "BacDive_Isolation_category_1", "BacDive_Isolation_category_2", "BacDive_Isolation_category_3",
                     "FAPROTAX_traits")

      is_numeric_vars <- c(FALSE, FALSE, TRUE,
                           FALSE, TRUE, TRUE,
                           FALSE, FALSE, TRUE,
                           FALSE, FALSE, FALSE,
                           TRUE, FALSE, FALSE,
                           FALSE, TRUE, FALSE,
                           FALSE, TRUE, FALSE,
                           FALSE, FALSE, FALSE,
                           FALSE)

      data[data_vars] <- mapply(clean_external_data, x = data[data_vars], is_numeric = is_numeric_vars, SIMPLIFY = FALSE)

    # Convert salt concentration to uniform units (mol L-1)
      data$BacDive_Salt_concentration <- convert_salt_concentration(concentration = data$BacDive_Salt_concentration, unit = data$BacDive_Salt_concentration_unit)

    # Replace a complex antibiotic name with a simpler one (makes easier to match with regex)
      data$BacDive_Antibiotic_resistance = gsub(pattern="0129 \\(2,4-Diamino-6,7-di-iso-propylpteridine phosphate\\)", replacement = "2,4-Diamino-6,7-diisopropylpteridine", x = data$BacDive_Antibiotic_resistance)
      data$BacDive_Antibiotic_sensitivity = gsub(pattern="0129 \\(2,4-Diamino-6,7-di-iso-propylpteridine phosphate\\)", replacement = "2,4-Diamino-6,7-diisopropylpteridine", x = data$BacDive_Antibiotic_sensitivity)

    # Combine columns
      # Pathogenicity
      data = collapse_columns(df = data,
                              cols=c("BacDive_Pathogenicity_animal", "BacDive_Pathogenicity_human", "BacDive_Pathogenicity_plant"),
                              new_col_name = "BacDive_Pathogenicity",
                              delete = "BacDive_Pathogenicity_",
                              positive_value ="+",
                              negative_value="-")

      # Type of metabolism
      data <- data %>%
        dplyr::mutate(Type_of_metabolism = dplyr::coalesce(
          Literature_Type_of_metabolism,
          VPI_Type_of_metabolism,
          Bergey_Type_of_metabolism
        ))

      # Major end products
      data <- data %>%
        dplyr::mutate(Major_end_products = dplyr::coalesce(
          Literature_Major_end_products,
          VPI_Major_end_products,
          Bergey_Major_end_products
        ))

      # Minor end products
      data <- data %>%
        dplyr::mutate(Minor_end_products = dplyr::coalesce(
          Literature_Minor_end_products,
          VPI_Minor_end_products,
          Bergey_Minor_end_products
        ))

      # End products
      data$Major_end_products[data$Major_end_products == "NA"] <- NA
      data$Minor_end_products[data$Minor_end_products == "NA"] <- NA
      data$End_products <- ifelse(is.na(data$Major_end_products), data$Minor_end_products, ifelse(is.na(data$Minor_end_products), data$Major_end_products, paste(data$Major_end_products, data$Minor_end_products, sep = ";")))

      # Substrates for end products
      data <- data %>%
        dplyr::mutate(Substrates_for_end_products = dplyr::coalesce(
          Literature_Substrates_for_end_products,
          Bergey_Substrates_for_end_products
        ))

      # LPSN taxonomy
      data$LPSN_Taxonomy <- apply(
        data[c("LPSN_Domain", "LPSN_Phylum", "LPSN_Class", "LPSN_Order", "LPSN_Family", "LPSN_Genus", "LPSN_Species")],
        1,
        collapse_taxonomy
      )

      # GTDB taxonomy
      data$GTDB_Taxonomy <- apply(
        data[c("GTDB_Domain", "GTDB_Phylum", "GTDB_Class", "GTDB_Order", "GTDB_Family", "GTDB_Genus", "GTDB_Species")],
        1,
        collapse_taxonomy
      )

      # NCBI taxonomy
      data$NCBI_Domain = NA
      data$NCBI_Taxonomy <- apply(
        data[c("NCBI_Domain", "NCBI_Phylum", "NCBI_Class", "NCBI_Order", "NCBI_Family", "NCBI_Genus", "NCBI_Species")],
        1,
        collapse_taxonomy
      )

      # Bergey taxonomy
      data$Bergey_Domain = NA
      data$Bergey_Taxonomy <- apply(
        data[c("Bergey_Domain", "Bergey_Phylum", "Bergey_Class", "Bergey_Order", "Bergey_Family", "Bergey_Genus", "Bergey_Species")],
        1,
        collapse_taxonomy
      )

    # Add links
      data$LPSN_Page_link <- createLinkButton(data$LPSN_Page_link)
      data$NCBI_Taxonomy_ID_link <- createLink(data$NCBI_Taxonomy_ID, "https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?mode=Info&id=")
      data$GOLD_Organism_ID_link <- createLink(data$GOLD_Organism_ID, "https://gold.jgi.doe.gov/organism?id=")
      data$GOLD_Project_ID_link <- createLink(data$GOLD_Project_ID, "https://gold.jgi.doe.gov/project?id=")
      data$IMG_Genome_ID_link <- createLink(data$IMG_Genome_ID, "https://img.jgi.doe.gov/cgi-bin/m/main.cgi?section=TaxonDetail&page=taxonDetail&taxon_oid=")
      data$BacDive_ID_link <- createLink(data$BacDive_ID, "https://bacdive.dsmz.de/strain/")
      data$GTDB_ID_link <- createLink(data$GTDB_ID, "https://gtdb.ecogenomic.org/searches?s=al&q=")
      data$Bergey_Article_link <- createLinkButton(data$Bergey_Article_link)

    # Keep only columns used in app
      data <- data %>% dplyr::select(
        "Genus", "Species", "Subspecies", "Strain",
        "LPSN_ID", "LPSN_Page_link", "LPSN_Taxonomy",
        "GTDB_ID", "GTDB_ID_link", "GTDB_Taxonomy",
        "GOLD_Organism_ID", "GOLD_Organism_ID_link", "GOLD_Project_ID", "GOLD_Project_ID_link",
        "NCBI_Taxonomy_ID", "NCBI_Taxonomy_ID_link", "NCBI_Taxonomy",
        "IMG_Genome_ID", "IMG_Genome_ID_link", "IMG_Genome_ID_max_genes",
        "BacDive_ID", "BacDive_ID_link",
        "BacDive_Antibiotic_resistance", "BacDive_Antibiotic_sensitivity", "BacDive_Cell_length", "BacDive_Cell_shape",
        "BacDive_Cell_width", "BacDive_Colony_size", "BacDive_Flagellum_arrangement", "BacDive_Gram_stain", "BacDive_Incubation_period",
        "BacDive_Indole_test", "BacDive_Motility", "BacDive_Oxygen_tolerance", "BacDive_pH_for_growth", "BacDive_Pathogenicity",
        "BacDive_Salt_concentration", "BacDive_Spore_formation", "BacDive_Temperature_for_growth", "BacDive_Voges_proskauer",
        "BacDive_Isolation_category_1", "BacDive_Isolation_category_2", "BacDive_Isolation_category_3",
        "Bergey_Article_link", "Bergey_Taxonomy",
        "Type_of_metabolism", "Major_end_products", "Minor_end_products", "End_products", "Substrates_for_end_products",
        "FAPROTAX_traits"
      )

      # Rename columns
      data <- data %>% dplyr::rename(
        Antibiotic_resistance = "BacDive_Antibiotic_resistance",
        Antibiotic_sensitivity = "BacDive_Antibiotic_sensitivity",
        Cell_length = "BacDive_Cell_length",
        Cell_shape = "BacDive_Cell_shape",
        Cell_width = "BacDive_Cell_width",
        Colony_size = "BacDive_Colony_size",
        Flagellum_arrangement = "BacDive_Flagellum_arrangement",
        Gram_stain = "BacDive_Gram_stain",
        Incubation_period = "BacDive_Incubation_period",
        Indole_test = "BacDive_Indole_test",
        Motility = "BacDive_Motility",
        Oxygen_tolerance = "BacDive_Oxygen_tolerance",
        pH_for_growth = "BacDive_pH_for_growth",
        Pathogenicity = "BacDive_Pathogenicity",
        Salt_concentration = "BacDive_Salt_concentration",
        Spore_formation = "BacDive_Spore_formation",
        Temperature_for_growth = "BacDive_Temperature_for_growth",
        Voges_proskauer = "BacDive_Voges_proskauer",
        Isolation_category_1 = "BacDive_Isolation_category_1",
        Isolation_category_2 = "BacDive_Isolation_category_2",
        Isolation_category_3 = "BacDive_Isolation_category_3"
      )

      data <- dplyr::rename(data,
                            Cell_length_in_microns = "Cell_length",
                            Cell_width_in_microns = "Cell_width",
                            Incubation_period_in_days = "Incubation_period",
                            Temperature_for_growth_in_degrees = "Temperature_for_growth",
                            Salt_for_growth_in_moles_per_liter = "Salt_concentration",
                            Voges_Proskauer = "Voges_proskauer"
      )

    # Rearrange columns
      data = data %>%
        dplyr::select(-FAPROTAX_traits, everything(), FAPROTAX_traits)
      data <- data %>%
        dplyr::select(1:which(colnames(data) == "Major_end_products") - 1,
                      End_products,
                      Major_end_products,
                      everything()
        )

    # Convert character columns to factor
      data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)

    # Remove underscores in column names
      colnames(data) <- gsub(pattern = "_", replacement = " ", x = colnames(data))

    # Save file
      fp <- "data//database_clean.csv"
      save_as_zip(data = data, fp = fp)

  # --- Generate query filters ---
    # Define functions
    #' Get Unique Delimited Values from a Vector
    #'
    #' This function takes a character vector where elements are delimited, and finds all unique values across the vector.
    #' It includes options to sort the values and remove `NA` or the string `"NA"`.
    #'
    #' @param vec A character vector where some elements are delimited by a specified delimiter.
    #' @param delimiter A string specifying the delimiter used to separate elements within each entry of the vector. Defaults to `";"`.
    #' @param sort Should the unique values be sorted? Defaults to `TRUE`.
    #' @param remove_na Should `NA` and the string `"NA"` be removed from the result? Defaults to `TRUE`.
    #'
    #' @return A character vector of unique values found across all elements in the input vector.
    #'
    #' @examples
    #' vec <- c("apple;orange", "apple", "strawberry;NA", NA)
    #' unique_from_delimited(vec, sort = TRUE, remove_na = TRUE)
    #'
    #' @export
    unique_from_delimited <- function(vec, delimiter = ";", sort = TRUE, remove_na = TRUE) {
      # Split and unlist the vector
      values <- unlist(strsplit(vec, delimiter))

      # Remove NA and "NA" if requested
      if (remove_na) {
        values <- values[!is.na(values) & values != "NA"]
      }

      # Get unique values
      unique_values <- unique(values)

      # Sort values if requested
      if (sort) {
        unique_values <- sort(unique_values)
      }

      return(unique_values)
    }

    #' Create Plugin Config for Selectize
    #'
    #' This function takes a vector as input and returns the full `plugin_config` list
    #' for use with the selectize plugin for jqbr::queryBuilderInput. It can handle
    #' delimited vectors if specified.
    #'
    #' @param vec A vector containing the values from which unique options will be created.
    #'   It can be a numeric, character, or factor vector.
    #' @param delimited Logical, whether the vector contains delimited values. Defaults to `FALSE`.
    #' @param delimiter A string specifying the delimiter used to separate elements within each entry of the vector if `delimited = TRUE`. Defaults to `";"`.
    #' @param sort Logical, whether to sort the unique values. Defaults to `TRUE`.
    #' @param remove_na Logical, whether to remove `NA` and the string `"NA"`. Defaults to `TRUE`.
    #'
    #' @return A list representing the `plugin_config` for the selectize plugin, including:
    #'   \item{valueField}{The field used as the unique identifier (`id`).}
    #'   \item{labelField}{The field used for display names (`name`).}
    #'   \item{searchField}{The field used for searching (`name`).}
    #'   \item{sortField}{The field used for sorting (`name`).}
    #'   \item{options}{A list of named lists representing unique values from the input vector.}
    #'
    #' @examples
    #' # Example usage with a character vector:
    #' plugin_config <- create_selectize_plugin_config(data$Phylum)
    #'
    #' # Example usage with a delimited vector:
    #' plugin_config <- create_selectize_plugin_config(data$Phylum, delimited = TRUE)
    #'
    #' @export
    create_selectize_plugin_config <- function(vec, delimited = FALSE, delimiter = ";", sort = TRUE, remove_na = TRUE) {

      # Determine unique values
      if (delimited) {
        unique_values <- unique_from_delimited(vec, delimiter = delimiter, sort = sort, remove_na = remove_na)
      } else {
        unique_values <- unique(vec)
        if (remove_na) {
          unique_values <- unique_values[!is.na(unique_values) & unique_values != "NA"]
        }
        if (sort) {
          unique_values <- sort(unique_values)
        }
      }

      # Create the plugin config
      list(
        valueField = "id",
        labelField = "name",
        searchField = "name",
        sortField = "name",
        options = lapply(unique_values, function(value) {
          list(id = value, name = value)
        })
      )
    }

    #' Create Plugin Config for Slider
    #'
    #' This function takes a numeric vector as input and returns the `plugin_config` list
    #' for use with the slider plugin for jqbr::queryBuilderInput.
    #'
    #' @param vec A numeric vector containing the values from which min, max, and default values will be created.
    #' @param remove_na Logical, whether to remove `NA` and the string `"NA"`. Defaults to `TRUE`.
    #'
    #' @return A list representing the `plugin_config` for the slider plugin, including:
    #'   \item{min}{The minimum value of the vector.}
    #'   \item{max}{The maximum value of the vector.}
    #'   \item{value}{The midpoint value between the min and max.}
    #'
    #' @examples
    #' # Example usage with a numeric vector:
    #' plugin_config <- create_slider_plugin_config(c(1, 2, 3, 4, 5))
    #'
    #' @export
    create_slider_plugin_config <- function(vec, remove_na = TRUE) {

      # Remove NAs if necessary
      if (remove_na) {
        vec <- vec[!is.na(vec)]
      }

      # Calculate min, max, and midpoint
      min_value <- min(vec)
      max_value <- max(vec)
      mid_value <- (min_value + max_value) / 2

      # Create the plugin config
      list(
        min = min_value,
        max = max_value,
        value = mid_value
      )
    }

    # Get data
      # Load layout
      data <- load_database(force_reload = TRUE)

      # Add taxonomy (LPSN)
      col_name <- "LPSN Taxonomy"
      data <- expand_and_merge_taxonomy(data = data, col_name = col_name)

    # Generate main filters
    query_filters <- list(
        # Taxonomy
        list(id = "Phylum", label = "Phylum", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$Phylum)),
        list(id = "Class", label = "Class", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$Class)),
        list(id = "Order", label = "Order", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$Order)),
        list(id = "Family", label = "Family", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$Family)),
        list(id = "Genus", label = "Genus", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$Genus)),
        list(id = "Species", label = "Species", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$Species)),
        list(id = "Subspecies", label = "Subspecies", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$Subspecies)),
        list(id = "Strain", label = "Strain", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Strain`)),
        list(id = "NCBI Taxonomy ID", label = "NCBI Taxonomy ID", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`NCBI Taxonomy ID`)),
        list(id = "GOLD Organism ID", label = "GOLD Organism ID", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`GOLD Organism ID`, delimited = TRUE, delimiter = ",")),
        list(id = "GOLD Project ID", label = "GOLD Project ID", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`GOLD Project ID`, delimited = TRUE, delimiter = ",")),
        list(id = "IMG Genome ID", label = "IMG Genome ID", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`IMG Genome ID`, delimited = TRUE, delimiter = ",")),
        list(id = "IMG Genome ID max genes", label = "IMG Genome ID max genes", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`IMG Genome ID max genes`)),
        list(id = "BacDive ID", label = "BacDive ID", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`BacDive ID`)),

        # Physiology/Function
        list(id = "Type of metabolism", label = "Type of metabolism", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Type of metabolism`)),
        list(id = "End products", label = "End products", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`End products`, delimited = TRUE, delimiter = ";")),
        list(id = "Major end products", label = "Major end products", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Major end products`, delimited = TRUE, delimiter = ";")),
        list(id = "Minor end products", label = "Minor end products", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Minor end products`, delimited = TRUE, delimiter = ";")),
        list(id = "Substrates for end products", label = "Substrates for end products", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Substrates for end products`, delimited = TRUE, delimiter = ";")),
        list(id = "Oxygen tolerance", label = "Oxygen tolerance", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Oxygen tolerance`)),
        list(id = "Pathogenicity", label = "Pathogenicity", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Pathogenicity`, delimited = TRUE, delimiter = ";")),
        list(id = "Temperature for growth in degrees", label = "Temperature for growth in degrees", type = "double", plugin = "slider", plugin_config = create_slider_plugin_config(data$`Temperature for growth in degrees`)),
        list(id = "Salt for growth in moles per liter", label = "Salt for growth in moles per liter", type = "double", plugin = "slider", plugin_config = create_slider_plugin_config(data$`Salt for growth in moles per liter`)),
        list(id = "pH for growth", label = "pH for growth", type = "double", plugin = "slider", plugin_config = create_slider_plugin_config(data$`pH for growth`)),
        list(id = "Indole test", label = "Indole test", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Indole test`)),
        list(id = "Voges Proskauer", label = "Voges Proskauer", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Voges Proskauer`)),
        list(id = "Motility", label = "Motility", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Motility`)),
        list(id = "Antibiotic resistance", label = "Antibiotic resistance", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Antibiotic resistance`, delimited = TRUE, delimiter = ";")),
        list(id = "Antibiotic sensitivity", label = "Antibiotic sensitivity", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Antibiotic sensitivity`, delimited = TRUE, delimiter = ";")),
        list(id = "FAPROTAX traits", label = "FAPROTAX traits", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`FAPROTAX traits`, delimited = TRUE, delimiter = ";")),

        # Morphology
        list(id = "Cell length in microns", label = "Cell length in microns", type = "double", plugin = "slider", plugin_config = create_slider_plugin_config(data$`Cell length in microns`)),
        list(id = "Cell width in microns", label = "Cell width in microns", type = "double", plugin = "slider", plugin_config = create_slider_plugin_config(data$`Cell width in microns`)),
        list(id = "Cell shape", label = "Cell shape", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Cell shape`)),
        list(id = "Colony size", label = "Colony size", type = "double", plugin = "slider", plugin_config = create_slider_plugin_config(data$`Colony size`)),
        list(id = "Flagellum arrangement", label = "Flagellum arrangement", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Flagellum arrangement`)),
        list(id = "Gram stain", label = "Gram stain", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Gram stain`)),
        list(id = "Spore formation", label = "Spore formation", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Spore formation`)),

        # Isolation traits
        list(id = "Isolation category 1", label = "Isolation category 1", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Isolation category 1`)),
        list(id = "Isolation category 2", label = "Isolation category 2", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Isolation category 2`)),
        list(id = "Isolation category 3", label = "Isolation category 3", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Isolation category 3`))
      )

    # Save object to file
    data_fp = paste0("data/query_filters.rds")
    saveRDS(query_filters, file = data_fp)

    # Generate placeholder filters (simple filters to load at app startup)
    query_filters_simple <- list(
      list(id = "Type of metabolism", label = "Type of metabolism", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Type of metabolism`))
      )
    
    # Save object to file
    data_fp = paste0("data/query_filters_simple.rds")
    saveRDS(query_filters_simple, file = data_fp)
    
  # --- Generate files for phylogenetic tree ---
    # Load tree
    data_fp <- "data/tree.tre"
    tree <- ape::read.tree(data_fp)

    # Get layouts for phylogenetic tree
      layout_types <- c("rectangular", "daylight", "equal_angle")

      for (layout_type in layout_types) {
        layout <- get_tree_layout(tree, layout_type = layout_type)
        data_fp <- paste0("data/layout_tree_", layout_type, ".rds")
        saveRDS(layout, data_fp)
      }

    # Get nodes from tips to root
        nodes_to_root = get_nodes_to_root(tree = tree)
        data_fp = "data/nodes_to_root.rds"
        saveRDS(nodes_to_root, file = data_fp)

    # Plot branches for all organisms
      for (layout_type in layout_types) {
        # Load layout
        data_fp = paste0("data/layout_tree_", layout_type, ".rds")
        layout <- readRDS(data_fp)

        # Plot branches
        if(layout_type=="rectangular")
        {
          plot <- ggtree_to_plotly(layout = layout, type = layout_type, color = "#e5e5e5", coord_fixed = FALSE)
        }else{
          plot <- ggtree_to_plotly(layout = layout, type = layout_type, color = "#e5e5e5", coord_fixed = TRUE, x_to_y_ratio = 0.8)
        }

        # Save plot to file
        data_fp = paste0("data/plot_branches_all_", layout_type, ".rds")
        saveRDS(plot, file = data_fp)
      }

    # Plot tip points for all organisms
      for (layout_type in layout_types) {
        # Load layout and data
        data_fp = paste0("data/layout_tree_", layout_type, ".rds")
        layout <- readRDS(data_fp)
        data <- load_database()

        # Format layout
        layout_tips <- layout %>% dplyr::filter(isTip == TRUE)
        layout_tips <- add_taxonomy_to_layout(layout = layout_tips, layout_ID = "label", taxonomy = data, taxonomy_ID = "IMG_Genome_ID_max_genes")
        layout_tips <- add_fill_to_layout(layout = layout_tips, group = "Phylum", lighten_amount = 0.95)
        layout_tips <- add_color_to_layout(layout = layout_tips, group = "Phylum", lighten_amount = 0.9)

        # Create scatter plot
        # Plot branches
        if(layout_type=="rectangular")
        {
          plot <- plot_scatterplot(df = layout_tips,
                                   color = layout_tips$color, fill =  layout_tips$fill, stroke = 1, size = 5, shape = "circle", alpha=1,
                                   label = c("Phylum", "Class", "Order", "Family", "Genus", "Species"),
                                   coord_fixed=FALSE, x_to_y_ratio=NULL)
        }else{
          plot <- plot_scatterplot(df = layout_tips,
                                   color = layout_tips$color, fill =  layout_tips$fill, stroke = 1, size = 5, shape = "circle", alpha=1,
                                   label = c("Phylum", "Class", "Order", "Family", "Genus", "Species"),
                                   coord_fixed=TRUE, x_to_y_ratio=0.8)
        }

        # Save plot to file
        data_fp = paste0("data/plot_tips_all_", layout_type, ".rds")
        saveRDS(plot, file = data_fp)
      }

  # --- Generate files for t-SNE plot ---
      # Plot scatter plot for all organisms
        # Load layout and data
        layout <- load_layout_tsne()
        data <- load_database()

        #Format layout
        layout <- add_taxonomy_to_layout(layout = layout, layout_ID = "IMG_Genome_ID_max_genes", taxonomy = data, taxonomy_ID = "IMG_Genome_ID_max_genes")
        layout <- add_fill_to_layout(layout = layout, group = "Phylum", lighten_amount = 0.95)
        layout <- add_color_to_layout(layout = layout, group = "Phylum", lighten_amount = 0.9)

        #Create scatter plot
        plot = plot_scatterplot(df = layout,
                         color = layout$color, fill =  layout$fill, stroke = 1, size = 5, shape = "circle", alpha=1,
                         label = c("Phylum", "Class", "Order", "Family", "Genus", "Species"),
                         ticklen.x = 4, ticklen.y = 4, showticklabels.x = TRUE, showticklabels.y = TRUE, title.x = "Dimension 1", title.y = "Dimension 2",
                         coord_fixed=TRUE, x_to_y_ratio=1)

        # Save plot to file
        data_fp = paste0("data/plot_tsne_all.rds")
        saveRDS(plot, file = data_fp)

  # --- Generate random forest models ---
      # Generate models
        #Load data
        gene_functions_database <- load_gene_functions()
        gene_functions = gene_functions_database %>% dplyr::filter(grepl("^K", Database_ID)) # Keep only KO IDs

        data <- load_database()
        colnames(data) = gsub(pattern="\\.", replacement=" ", x = colnames(clean_data))

        # Get inputs
        data = data
        proportion_to_keep = 0.1
        seed = 123

        ignore_NA = FALSE
        ntree = 50
        maxnodes = 30
        training_split = 0.7

        # Perform model fitting
        vars <- c("Fermentation", "Methanogenesis")
        for (var in vars) {
          query_string = paste0("`Type of metabolism` %in% c(\"", var, "\")")

          # Format predictors
          predictors <- format_predictors(gene_functions = gene_functions, proportion_to_keep = proportion_to_keep, seed = seed)

          # Format response
          response <- format_response(data = data, query_string = query_string, ignore_NA=ignore_NA)

          #Format data
          rf_data <- format_rf_data(predictors = predictors, response = response)

          # Build the random forest model
          rf <- build_rf(data = rf_data, seed = seed, training_split = training_split, ntree = ntree, maxnodes = maxnodes)

          #Save response variable
          fp <- paste0("data/response_", tolower(var), ".csv")
          save_as_zip(data = response, fp = fp)
          
          #Save predictor variables
          fp <- paste0("data/predictors_", tolower(var), ".csv")
          save_as_zip(data = response, fp = fp)

          # Save random forest model
          save_fp <- paste0("data/random_forest_", tolower(var), ".rds")
          save_rf(rf = rf, data_fp = save_fp)
        }