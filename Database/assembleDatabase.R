# Assemble Database
# This script assembles the database for the app
# It draws on data from LPSN, Bergey's Manual, BacDive, NCBI, GOLD, and IMG.
# It is not called during app execution.
# Requirements:
# - Packages in install/installPackages.R
# - Data from LPSN from LPSN/getLpsnOrganisms.R script
# - Additional data from LPSN from LPSN/getLpsnPhylogeny.R script
# - Additional data from LPSN from LPSN/getLpsnRibosomalSequences.R script
# - Data downloaded from BacDive at links below
# - Data from Bergey's Manual from Bergey/addBergeyLabels.R script
# - Data downloaded from NCBI at links below
# - Data downloaded from GOLD at links below
# - Data downloaded from IMG following instructions below
# - Data from FAPROTAX from FAPROTAX/getFAPROTAXpredictions.R script
# - Data from primary literature
# - Data from VPI Anaerobe Manual
# Author: Timothy Hackmann
# Date: 23 Mar 2025

# === Define functions ===
  #' Extract Culture Collection Names
  #'
  #' This function extracts names of culture collections from a vector of strain names.
  #' The names are prefixes in strain names (e.g., "DSM" in "DSM 1").
  #'
  #' @param strains A character vector where each element contains strain information,
  #'                with strain names separated by semicolons.
  #' @param pattern A regular expression pattern used to identify culture collection prefixes.
  #' @param n An integer specifying the number of top collections to return (default is 32).
  #'
  #' @return A character vector of the top `n` culture collection names sorted by frequency.
  #' @examples
  #' strains <- c("DSM 1234; ATCC 5678", "JCM 9101; DSM 2234")
  #' get_collection_names(strains, pattern = "^([A-Za-z]+)\\s\\d+", n = 5)
  #' @export
  get_collection_names <- function(strains, pattern = "^([A-Za-z]+)\\s\\d+", n = 32) {
    # Ensure the input is a character vector
    if (!is.character(strains)) {
      stop("The input must be a character vector.")
    }

    # Initialize an empty vector to store collection_names
    collection_names <- c()

    # Loop over each strain entry in the vector
    for (line in strains) {
      # Split each line by semicolons and trim whitespace
      strain_list <- strsplit(line, ";")[[1]]
      strain_list <- trimws(strain_list)

      # Extract the prefix if it matches the pattern
      match <- sapply(strain_list, function(x) {
        match <- regmatches(x, regexec(pattern, x))
        if (length(match[[1]]) > 1) {
          return(match[[1]][2])  # The first capture group (prefix) is in the second element
        } else {
          return(NA)
        }
      })

      # Remove NA values and add matched collections to the list
      collection_names <- c(collection_names, na.omit(match))
    }

    # Format the names as a sorted table
    collection_names <- table(collection_names)
    collection_names <- sort(collection_names, decreasing = TRUE)

    # Return the top `n` names
    return(names(collection_names)[1:n])
  }

  #' Split Taxonomy String into Taxonomic Ranks
  #'
  #' This function processes a single Greengenes-like taxonomy string and splits it into
  #' individual taxonomic ranks. By default, it extracts the species epithet
  #' (e.g., "coli" in "Escherichia coli").
  #'
  #' @param taxonomy A character string representing a GTDB taxonomy, e.g.,
  #'   "d__Bacteria;p__Pseudomonadota;c__Gammaproteobacteria;o__Burkholderiales;f__Burkholderiaceae;g__Bordetella;s__Bordetella pseudohinzii".
  #' @param extract_species_epithet Logical. If `TRUE` (default), the species
  #'   column contains only the epithet (e.g., "coli" from "Escherichia coli").
  #'   If `FALSE`, the full species name is returned.
  #'
  #' @return A named character vector with elements for each taxonomic rank:
  #'   `Domain`, `Phylum`, `Class`, `Order`, `Family`, `Genus`, and `Species`.
  #'
  #' @examples
  #' taxonomy <- "d__Bacteria;p__Bacillota;c__Bacilli;o__Staphylococcales;f__Staphylococcaceae;g__Staphylococcus;s__Staphylococcus epidermidis"
  #' split_taxonomy(taxonomy)
  #' split_taxonomy(taxonomy, extract_species_epithet = FALSE)
  #' Split Taxonomy String into Taxonomic Ranks
  #'
  #' This function processes a single Greengenes-like taxonomy string and splits it into
  #' individual taxonomic ranks. By default, it extracts the species epithet
  #' (e.g., "coli" in "Escherichia coli").
  #'
  #' @param taxonomy A character string representing a GTDB taxonomy, e.g.,
  #'   "d__Bacteria;p__Pseudomonadota;c__Gammaproteobacteria;o__Burkholderiales;f__Burkholderiaceae;g__Bordetella;s__Bordetella pseudohinzii".
  #' @param extract_species_epithet Logical. If `TRUE` (default), the species
  #'   column contains only the epithet (e.g., "coli" from "Escherichia coli").
  #'   If `FALSE`, the full species name is returned.
  #'
  #' @return A named character vector with elements for each taxonomic rank:
  #'   `Domain`, `Phylum`, `Class`, `Order`, `Family`, `Genus`, and `Species`.
  #'
  #' @examples
  #' taxonomy <- "d__Bacteria;p__Bacillota;c__Bacilli;o__Staphylococcales;f__Staphylococcaceae;g__Staphylococcus;s__Staphylococcus epidermidis"
  #' split_taxonomy(taxonomy)
  #' split_taxonomy(taxonomy, extract_species_epithet = FALSE)
  split_taxonomy <- function(
    taxonomy,
    ranks = c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species"),
    prefixes = paste0(substr(tolower(ranks), 1, 1), "__"),
    extract_species_epithet = TRUE
  ) {
    # Split the taxonomy string by semicolon
    taxon_split <- strsplit(taxonomy, ";")[[1]]
    
    # Create a named vector from prefix to taxon
    taxon_named <- setNames(rep(NA_character_, length(ranks)), ranks)
    for (item in taxon_split) {
      for (i in seq_along(prefixes)) {
        prefix <- prefixes[i]
        if (startsWith(item, prefix)) {
          taxon_named[ranks[i]] <- sub(paste0("^", prefix), "", item)
          break
        }
      }
    }
    
    # Optionally extract the species epithet
    if (extract_species_epithet && "Species" %in% ranks && !is.na(taxon_named["Species"])) {
      taxon_named["Species"] <- sub(".*\\s", "", taxon_named["Species"])
    }
    
    return(taxon_named)
  }

  #' Match an organism to a table based on genus, species, subspecies, and strain.
  #'
  #' This function matches a query organism, defined by its genus, species, subspecies, and strain,
  #' to an organism in a reference table. It optionally considers strain delimiters and can return
  #' the best matches based on ranking criteria.  Subspecies are matched only if present in a reference table.
  #'
  #' @param x_genus Character. Genus of the query organism.
  #' @param x_species Character. Species of the query organism.
  #' @param x_subspecies Character. Subspecies of the query organism (optional, can be NULL).
  #' @param x_strain Character. Strain of the query organism.
  #' @param x_delim Character. Delimiter used to split the query strain string. Default is NULL.
  #' @param table_genus Character vector. Genus column from the reference table.
  #' @param table_species Character vector. Species column from the reference table.
  #' @param table_subspecies Character vector. Subspecies column from the reference table (optional, can be NULL).
  #' @param table_strain Character vector. Strain column from the reference table.
  #' @param table_delim Character. Delimiter used to split the strain strings in the reference table. Default is NULL.
  #' @param collection_names Character vector. List of prefixes of large culture collection names (e.g., DSM, ATCC).
  #' @param best_matches Logical. If TRUE, only the highest-ranked matches are returned. Default is FALSE.
  #'
  #' @return A dataframe with matched entries including a calculated rank.
  #' @import dplyr stringr
  #' @export
  #' @examples
  #' match_organism("Escherichia", "coli", NULL, "ATCC 25922", table_genus, table_species, NULL, table_strain)
  match_organism <- function(x_genus, x_species, x_subspecies = NULL, x_strain, x_delim = NULL,
                             table_genus, table_species, table_subspecies = NULL, table_strain, table_delim = NULL,
                             collection_names = c(
                               "DSM", "JCM", "KCTC", "LMG", "NBRC", "ATCC", "CCUG", "CIP", "CGMCC", "KACC",
                               "IFO", "CECT", "NCTC", "NCIMB", "MCCC", "BCRC", "CCM", "GDMCC", "HAMBI",
                               "NCIB", "CBS", "CCRC", "RIA", "CFBP", "NCCB", "YIM", "IAM", "KCCM",
                               "MTCC", "ICMP", "KMM", "TBRC"
                             ),
                             best_matches = FALSE) {
    # Step 1: Find matches for genus, species, and subspecies separately
    match_genus <- which(x_genus == table_genus)
    match_species <- which(x_species == table_species)
    # match_subspecies <- if (!is.null(x_subspecies) && !is.null(table_subspecies)) which(x_subspecies == table_subspecies) else NULL
    match_subspecies <- if (!is.null(table_subspecies)) which(x_subspecies == table_subspecies) else NULL
    
    # Step 2: Split strain data if delimiters are provided
    if(is.null(x_delim)) {
      x = x_strain
    } else {
      x <- stringr::str_split(x_strain, paste0(x_delim, "\\s*"))[[1]]
    }
    
    if(is.null(table_delim)) {
      y = table_strain
    } else {
      y <- lapply(table_strain, function(str) stringr::str_split(str, paste0(table_delim, "\\s*"))[[1]])
    }
    
    # Step 3: Find matches for strain
    match_strain <- c()
    for (i in seq_along(y)) {
      if (any(x %in% y[[i]])) {
        match_strain <- c(match_strain, i)
      }
    }
    
    # Step 4: Find matches for culture collection (e.g., DSM, ATCC, etc.)
    pattern <- paste0(collection_names, collapse = "|")
    pattern <- paste0("^(", pattern, ")\\s\\d+")
    z <- x[grepl(pattern, x)]
    
    match_culture_collection <- c()
    for (i in seq_along(y)) {
      if (any(z %in% y[[i]])) {
        match_culture_collection <- c(match_culture_collection, i)
      }
    }
    
    # Step 5: Create match dataframe
    match_dataframe <- data.frame(
      Index = seq_along(table_genus),
      Genus_Match = seq_along(table_genus) %in% match_genus,
      Species_Match = seq_along(table_genus) %in% match_species,
      Subspecies_Match = if (!is.null(match_subspecies)) seq_along(table_genus) %in% match_subspecies else NA,
      Strain_Match = seq_along(table_genus) %in% match_strain,
      Culture_Collection_Match = seq_along(table_genus) %in% match_culture_collection
    )
    
    # Step 6: Assign rank based on matching criteria
    match_dataframe <- match_dataframe %>%
      dplyr::mutate(Rank = dplyr::case_when(
        Genus_Match & Species_Match & Subspecies_Match & Strain_Match ~ 1,
        Genus_Match & Species_Match & Strain_Match ~ 2,
        Species_Match & Subspecies_Match & Strain_Match ~ 3,
        Species_Match & Strain_Match ~ 4,
        Genus_Match & Strain_Match ~ 5,
        Culture_Collection_Match ~ 6,
        Genus_Match & Species_Match & Subspecies_Match ~ 7,
        Genus_Match & Species_Match ~ 8,
        TRUE ~ NA_real_
      )) %>%
      dplyr::filter(!is.na(Rank))
    
    # Step 7: Return best matches if specified
    if (nrow(match_dataframe) > 0) {
      match_dataframe <- match_dataframe %>%
        dplyr::arrange(Rank)
      
      if(best_matches){
        match_dataframe = match_dataframe %>% dplyr::filter(Rank == min(Rank))
      }
      return(match_dataframe)
    } else {
      return(data.frame(
        Index = NA_integer_,
        Genus_Match = NA,
        Species_Match = NA,
        Subspecies_Match = NA,
        Strain_Match = NA,
        Culture_Collection_Match = NA,
        Rank = NA_integer_
      ))
    }
  }
  
  #' Perform matching for a set of organisms.
  #'
  #' This function performs organism matching for multiple organisms based on genus, species, subspecies, and strain.
  #' It iterates through a dataset and calls the `match_organism` function to find the best matches.
  #'
  #' @param table_data Dataframe. The reference table containing the genus, species, and strain information.
  #' @param table_genus_col Character. The name of the genus column in the reference table.
  #' @param table_species_col Character. The name of the species column in the reference table.
  #' @param table_subspecies_col Character. The name of the subspecies column in the reference table.
  #' @param table_strain_col Character. The name of the strain column in the reference table.
  #' @param x_data Dataframe. The dataset containing the query organisms.
  #' @param x_genus_col Character. The name of the genus column in the query dataset.
  #' @param x_species_col Character. The name of the species column in the query dataset.
  #' @param x_subspecies_col Character. The name of the subspecies column in the query dataset.
  #' @param x_strain_col Character. The name of the strain column in the query dataset.
  #' @param table_delim Character. Delimiter used to split the strain strings in the reference table. Default is ",".
  #' @param x_delim Character. Delimiter used to split the strain strings in the query dataset. Default is ";".
  #' @param collection_names Character vector. List of names of culture collections (e.g., DSM, ATCC).
  #' @param output_file Character. File path for saving the output as CSV. Default is NULL.
  #'
  #' @return A dataframe of matched organisms with genus, species, strain, and culture collection matches, along with ranks.
  #' @import dplyr stringr
  #' @export
  #' @examples
  #' perform_matching(reference_table, "Genus", "Species", "Strain", query_data, "Genus", "Species", "Strain")
  perform_matching <- function(table_data, table_genus_col, table_species_col, table_subspecies_col = NULL, table_strain_col,
                               x_data, x_genus_col, x_species_col, x_subspecies_col = NULL, x_strain_col,
                               table_delim = ",", x_delim = ";",
                               collection_names = collection_names,
                               output_file=NULL) {
    # Initialize an empty dataframe to store matches
    matches <- data.frame(
      x_index = integer(),
      table_index = integer(),
      Genus_Match = character(),
      Species_Match = character(),
      Subspecies_Match = character(),
      Strain_Match = character(),
      Culture_Collection_Match = character(),
      Rank = character()
    )
    
    table_genus <- table_data[[table_genus_col]]
    table_species <- table_data[[table_species_col]]
    table_subspecies <- if (!is.null(table_subspecies_col)) table_data[[table_subspecies_col]] else NULL
    table_strain <- table_data[[table_strain_col]]
    table_strain <- lapply(table_strain, function(str) stringr::str_split(str, paste0(table_delim, "\\s*"))[[1]])
    
    for (i in 1:nrow(x_data)) {
      x_genus <- x_data[[x_genus_col]][i]
      x_species <- x_data[[x_species_col]][i]
      x_subspecies <- if (!is.null(x_subspecies_col)) x_data[[x_subspecies_col]][i] else NA
      x_strain <- x_data[[x_strain_col]][i]
      x_strain <- stringr::str_split(x_strain, paste0(x_delim, "\\s*"))[[1]]
      
      result <- match_organism(
        x_genus = x_genus,
        x_species = x_species,
        x_subspecies = if (!is.na(x_subspecies)) x_subspecies else NULL,
        x_strain = x_strain,
        x_delim = NULL,
        table_genus = table_genus,
        table_species = table_species,
        table_subspecies = if (!is.null(table_subspecies)) table_subspecies else NULL,
        table_strain = table_strain,
        collection_names = collection_names,
        table_delim = NULL
      )
      
      if (nrow(result) > 0) {
        colnames(result)[colnames(result) == "Index"] <- "table_index"
        result <- cbind(x_index = i, result)
        matches <- rbind(matches, result)
      }
      
      if (!is.null(output_file)) {
        write.csv(matches, output_file, row.names = FALSE)
      }
      
      svMisc::progress(value = i, max = nrow(x_data))
    }
    
    return(matches)
  }

  #' Add Data on BacDive Phenotypes
  #'
  #' This function adds phenotypic data from BacDive to a target dataframe.
  #'
  #' @param target_df Dataframe. The target dataframe to which the new columns will be added.
  #' @param source_df Dataframe. The source dataframe containing the values to add.
  #' @param source_col_names Character vector. A vector of column names in the source dataframe to be added.
  #' @param target_col_names Character vector. A vector of column names in the target dataframe where values will be placed.
  #'
  #' @return The target dataframe with the new columns added.
  #' @export
  add_BacDive_phenotypes <- function(target_df, source_df, source_col_names = NULL, target_col_names = source_col_names) {
    # Fill in missing ID cells with the value above
    source_df$ID <- zoo::na.locf(source_df$ID, na.rm = FALSE)
    source_df$ID <- as.character(source_df$ID)
    
    # Ensure that missing fields are filled for each ID and concatenate unique values
    source_df <- source_df %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(dplyr::across(dplyr::everything(), ~ paste(unique(stats::na.omit(.)), collapse = ";")), .groups = "drop")
    
    # Get names of columns to add
    if (is.null(source_col_names)) {
      source_col_names <- names(source_df)[ncol(source_df)]
    }
    
    # Remove existing columns in target_df if they match target_col_names
    if (!is.null(target_col_names)) {
      target_df <- target_df %>%
        dplyr::select(-dplyr::any_of(target_col_names), everything()) # Removes columns before re-adding
    }
    
    # Join the columns to the target data
    for (col_name in source_col_names) {
      target_df <- target_df %>%
        dplyr::left_join(source_df %>% dplyr::select(ID, dplyr::all_of(col_name)), by = c("BacDive_ID" = "ID"))
    }
    
    # Rename columns if new names are provided
    if (!is.null(target_col_names)) {
      names(target_df)[match(source_col_names, names(target_df))] <- target_col_names
    }
    
    return(target_df)
  }
  

  #' Load Names Data from names.dmp File
  #'
  #' This function reads and processes the `names.dmp` file from the NCBI taxonomy database.
  #' It extracts taxonomic information and filters for scientific names only.
  #'
  #' @param file_path A character string specifying the file path to the `names.dmp` file.
  #' @return A data frame with columns `tax_id` and `name_txt`, containing scientific names.
  #' @importFrom dplyr filter select
  #' @examples
  #' \dontrun{
  #' scientific_names <- load_names_dmp("path/to/names.dmp")
  #' head(scientific_names)
  #' }
  load_names_dmp <- function(file_path) {
    # Read lines from names.dmp file
    names_data <- readLines(file_path)

    # Split each line by the separator "\t|\t"
    names_split <- strsplit(names_data, "\t\\|\t")

    # Convert the split list to a dataframe
    names_df <- do.call(rbind, lapply(names_split, function(x) x[1:4]))
    names_df[,4] <- gsub(pattern = "\t\\|", replacement = "", x = names_df[,4])
    colnames(names_df) <- c("tax_id", "name_txt", "unique_name", "name_class")

    # Convert columns to appropriate types
    names_df <- as.data.frame(names_df, stringsAsFactors = FALSE)
    names_df$tax_id <- as.integer(names_df$tax_id)

    # Filter for scientific names
    scientific_names <- names_df %>%
      dplyr::filter(name_class == "scientific name") %>%
      dplyr::select(tax_id, name_txt)

    return(scientific_names)
  }


  #' Load Nodes Data from nodes.dmp File
  #'
  #' This function reads and processes the `nodes.dmp` file from the NCBI taxonomy database.
  #' It extracts hierarchical relationships between taxonomic IDs.
  #'
  #' @param file_path A character string specifying the file path to the `nodes.dmp` file.
  #' @return A data frame containing columns for taxonomic relationships, such as `tax_id`, `parent_tax_id`, and `rank`.
  #' @examples
  #' \dontrun{
  #' nodes_df <- load_nodes_dmp("path/to/nodes.dmp")
  #' head(nodes_df)
  #' }
  load_nodes_dmp <- function(file_path) {
    # Read lines from nodes.dmp file
    nodes_data <- readLines(file_path)

    # Split each line by the separator "\t|\t"
    nodes_split <- strsplit(nodes_data, "\t\\|\t")

    # Convert the split list to a dataframe
    nodes_df <- do.call(rbind, lapply(nodes_split, function(x) x[1:13]))
    nodes_df[,13] <- gsub(pattern = "\t\\|", replacement = "", x = nodes_df[,13])
    colnames(nodes_df) <- c("tax_id", "parent_tax_id", "rank", "embl_code", "division_id",
                            "inherited_div_flag", "genetic_code_id", "inherited_GC_flag",
                            "mitochondrial_genetic_code_id", "inherited_MGC_flag",
                            "GenBank_hidden_flag", "hidden_subtree_root_flag", "comments")

    # Convert columns to appropriate types
    nodes_df <- as.data.frame(nodes_df, stringsAsFactors = FALSE)
    nodes_df$tax_id <- as.integer(nodes_df$tax_id)
    nodes_df$parent_tax_id <- as.integer(nodes_df$parent_tax_id)

    return(nodes_df)
  }

  #' Extract First Value from Delimited Elements in a Vector
  #'
  #' This function takes a vector of elements, some of which may contain multiple values separated by a specified delimiter,
  #' and returns a vector containing only the first value from each element.
  #'
  #' @param vec A character vector, where some elements may contain delimited values.
  #' @param sep A character string specifying the delimiter. Defaults to a comma.
  #' @return A character vector with only the first value from each delimited element, without names.
  #' @examples
  #' tax_ids <- c("1120917,291968", "2691583", "1960156")
  #' first_tax_ids <- extract_first_value(tax_ids, sep = ",")
  #' print(first_tax_ids) # Should return c("1120917", "2691583", "1960156")
  extract_first_value <- function(vec, sep = ",") {
    # Use sapply to split by the specified separator and take the first part, then remove names
    unname(sapply(vec, function(x) strsplit(as.character(x), sep)[[1]][1]))
  }

  #' Retrieve Taxonomic Lineage Up to Phylum Level
  #'
  #' This function retrieves the lineage for a given taxonomic ID up to the "phylum" level.
  #' It uses data from the `nodes.dmp` and `names.dmp` files to trace the lineage.
  #'
  #' @param tax_id An integer representing the taxonomic ID for the species.
  #' @param nodes_df A data frame from `load_nodes_dmp` containing taxonomic hierarchy information.
  #' @param names_df A data frame from `load_names_dmp` containing scientific names.
  #' @return A data frame with the lineage of the specified taxonomic ID up to the "phylum" level.
  #'         Columns include `tax_id`, `rank`, and `name`. If `tax_id` is `NA` or not found, it returns NA values.
  #' @examples
  #' \dontrun{
  #' lineage <- get_lineage(515635, nodes_df, scientific_names)
  #' print(lineage)
  #' }
  get_lineage <- function(tax_id, nodes_df, names_df) {
    # Return NA if tax_id is NA or if not found in nodes_df
    if (is.na(tax_id) || !(tax_id %in% nodes_df$tax_id)) {
      return(data.frame(
        tax_id = NA_integer_,
        rank = c("phylum", "class", "order", "family", "genus", "species"),
        name = NA_character_,
        stringsAsFactors = FALSE
      ))
    }

    # Initialize an empty data frame to store the lineage with rank, tax_id, and name columns
    lineage_df <- data.frame(
      tax_id = integer(),
      rank = character(),
      name = character(),
      stringsAsFactors = FALSE
    )

    # Loop to trace lineage until reaching "phylum" or the root (no parent)
    current_tax_id <- tax_id
    while(TRUE) {
      # Find the current node in nodes_df
      node <- nodes_df[nodes_df$tax_id == current_tax_id, ]
      if (nrow(node) == 0) break  # Stop if the tax_id is not found

      # Get the rank, tax_id, and parent_tax_id information
      rank <- node$rank
      parent_tax_id <- node$parent_tax_id

      # Find the scientific name for this tax_id in names_df
      name <- names_df[names_df$tax_id == current_tax_id, "name_txt"]
      name <- ifelse(length(name) > 0, name, NA) # Handle missing names

      # Append the current rank information as a new row in lineage_df
      lineage_df <- rbind(lineage_df, data.frame(
        tax_id = current_tax_id,
        rank = rank,
        name = name,
        stringsAsFactors = FALSE
      ))

      # Break if we reached the "phylum" level or if there's no parent
      if (rank == "phylum" || current_tax_id == parent_tax_id) break

      # Move up to the parent tax_id for the next iteration
      current_tax_id <- parent_tax_id
    }

    # Ensure the lineage has all expected ranks even if some are missing
    required_ranks <- c("phylum", "class", "order", "family", "genus", "species")
    missing_ranks <- setdiff(required_ranks, lineage_df$rank)

    if (length(missing_ranks) > 0) {
      # Add missing ranks with NA values for tax_id and name
      missing_df <- data.frame(
        tax_id = NA_integer_,
        rank = missing_ranks,
        name = NA_character_,
        stringsAsFactors = FALSE
      )
      lineage_df <- rbind(lineage_df, missing_df)
    }

    # Arrange the data frame by the order of ranks
    lineage_df <- lineage_df[match(required_ranks, lineage_df$rank), ]

    return(lineage_df)
  }

  #' Retrieve Lineage for Multiple Taxonomy IDs
  #'
  #' This function applies `get_lineage` over a vector of `tax_id`s, returning a data frame
  #' containing taxonomic names for `phylum`, `class`, `order`, `family`, `genus`, and `species` ranks.
  #' It includes a progress indicator in the console for each processed tax_id.
  #'
  #' @param tax_ids A vector of integers representing taxonomy IDs for the species.
  #' @param nodes_df A data frame from `load_nodes_dmp` containing taxonomic hierarchy information.
  #' @param names_df A data frame from `load_names_dmp` containing scientific names.
  #' @return A data frame with columns `tax_id`, `phylum`, `class`, `order`, `family`, `genus`, and `species`.
  #' @importFrom svMisc progress
  #' @examples
  #' \dontrun{
  #' tax_ids <- c(515635, 513050, 13)
  #' lineage_df <- get_multiple_lineages(tax_ids, nodes_df, scientific_names)
  #' print(lineage_df)
  #' }
  get_multiple_lineages <- function(tax_ids, nodes_df, names_df) {
    # Initialize an empty data frame with the necessary columns
    n <- length(tax_ids)
    lineage_df <- data.frame(
      tax_id = tax_ids,
      phylum = rep(NA_character_, n),
      class = rep(NA_character_, n),
      order = rep(NA_character_, n),
      family = rep(NA_character_, n),
      genus = rep(NA_character_, n),
      species = rep(NA_character_, n),
      stringsAsFactors = FALSE
    )

    # Loop through each tax_id and populate the data frame
    for (i in seq_along(tax_ids)) {
      # Retrieve the current tax_id
      tax_id <- tax_ids[i]

      # Get lineage information for the current tax_id
      lineage <- get_lineage(tax_id, nodes_df, names_df)

      # Extract the relevant taxonomic levels
      ranks <- c("phylum", "class", "order", "family", "genus", "species")
      for (rank in ranks) {
        if (rank %in% lineage$rank) {
          lineage_df[i, rank] <- lineage$name[lineage$rank == rank]
        }
      }

      # Show progress
      svMisc::progress(value = i, max = n)
    }

    return(lineage_df)
  }

  #' Write Project IDs to Batch Files
  #'
  #' This function takes a vector of `PROJECT GOLD ID`s, removes missing or invalid entries,
  #' splits the IDs into batches of a specified size, and writes each batch to a separate file.
  #'
  #' @param project_IDs A character vector of `PROJECT GOLD ID`s.
  #' @param batch_size An integer specifying the number of IDs per batch. Default is 500.
  #' @param file_prefix A character string to use as the prefix for output files. Default is "project_IDs_batch".
  #'
  #' @return This function does not return a value; it creates files in the working directory.
  #' @export
  #'
  #' @examples
  #' \dontrun{
  #' project_IDs <- database$`PROJECT GOLD ID`
  #' write_project_ids_to_files(project_IDs, batch_size = 500, file_prefix = "project_IDs_batch")
  #' }
  write_project_ids_to_files <- function(project_IDs, batch_size = 500, file_prefix = "project_IDs_batch") {
    # Remove missing or invalid entries
    project_IDs <- project_IDs[!is.na(project_IDs) & project_IDs != "NA"]

    # Split IDs into batches
    batches <- split(project_IDs, ceiling(seq_along(project_IDs) / batch_size))

    # Write each batch to a separate file
    for (i in seq_along(batches)) {
      comma_delimited_string <- paste(batches[[i]], collapse = ", ")
      file_path <- paste0(file_prefix, i, ".txt")
      write(comma_delimited_string, file = file_path)
    }
  }

  #' Find Match Indices
  #'
  #' This function finds the first matching index for each element in a vector \code{x} within a table \code{table}.
  #' The matching uses regular expressions to ensure whole-word matching.
  #'
  #' @param x A character vector of elements to search for.
  #' @param table A character vector in which to search for matches.
  #'
  #' @return An integer vector of the same length as \code{x}, containing the indices of the first matches in \code{table},
  #' or \code{NA_integer_} if no match is found.
  #' @export
  #'
  #' @examples
  #' \dontrun{
  #' x <- c("ID1", "ID2", "ID3")
  #' table <- c("ID1", "ID4", "ID3")
  #' find_match_indices(x, table)
  #' }
  find_match_indices <- function(x, table) {
    match_indices <- integer(length(x))
    for (i in seq_along(x)) {
      match_idx <- which(grepl(paste0("\\b", x[i], "\\b"), table))
      match_indices[i] <- if (length(match_idx) > 0) match_idx[1] else NA_integer_

      svMisc::progress(value = i, max = length(x))
    }
    match_indices
  }

  #' Update Database Column
  #'
  #' This function updates a column in a database using match indices and corresponding values.
  #' For each match index, the corresponding value is either set or appended to the existing value in the column.
  #'
  #' @param database_col A character vector representing the column to update.
  #' @param match_indices An integer vector of indices indicating where to update values in \code{database_col}.
  #' @param values A character vector of values to insert or append to \code{database_col}.
  #'
  #' @return A character vector with updated values.
  #' @export
  #'
  #' @examples
  #' \dontrun{
  #' database_col <- c(NA, NA, "Existing")
  #' match_indices <- c(1, 2, 3)
  #' values <- c("Value1", "Value2", "Value3")
  #' update_database_column(database_col, match_indices, values)
  #' }
  update_database_column <- function(database_col, match_indices, values) {
    for (i in seq_along(values)) {
      match_idx <- match_indices[i]

      if (!is.na(match_idx)) {
        if (is.na(database_col[match_idx])) {
          database_col[match_idx] <- values[i]
        } else {
          database_col[match_idx] <- paste(database_col[match_idx], values[i], sep = ",")
        }
      }
    }
    database_col
  }

  #' Concatenate Column Names and Values in a Single Column
  #'
  #' This function combines specified columns into a single column, similar to `tidyr::unite`,
  #' but with the added option of concatenating the column names with their respective values.
  #' `NA` values are ignored when concatenating.
  #'
  #' @param data A data frame or tibble.
  #' @param col_name The name of the new column to store the combined text.
  #' @param selected_col A character vector of column names to be united.
  #' @param name_value_sep Separator used between the column name and its value.
  #' @param pair_sep Separator used between each column name-value pair.
  #' @param name_first Logical, if `TRUE`, the column name appears before the value; if `FALSE`, the value appears first.
  #' @return A tibble with the new combined column and without the original columns specified in `selected_col`.
  #' @examples
  #' df <- data.frame(
  #'   cluster = LETTERS[1:4],
  #'   group = c(rep("m", 2), rep("f", 2)),
  #'   point = rnorm(4),
  #'   err = c(NA, 0.142428504256532, NA, 0.125111019192263)
  #' )
  #' unite_with_names(df, "text", selected_col = c("cluster", "group", "point", "err"),
  #'                  name_value_sep = ": ", pair_sep = "\n", name_first = TRUE)
  #'
  #' @export
  unite_with_names <- function(data, col_name, selected_col, name_value_sep = " ", pair_sep = ";", name_first = TRUE) {
    # Select specified columns
    columns <- dplyr::select(data, dplyr::all_of(selected_col))

    # Concatenate column names with values, skipping NAs, and based on name_first
    columns <- purrr::imap(columns, ~ ifelse(
      is.na(.x),
      NA,
      if (name_first) paste(.y, .x, sep = name_value_sep) else paste(.x, .y, sep = name_value_sep)
    ))

    # Convert to tibble and apply unite with pair_sep between each column's name-value combination
    combined <- columns %>%
      tidyr::as_tibble() %>%
      tidyr::unite({{ col_name }}, dplyr::everything(), sep = pair_sep, na.rm = TRUE)

    # Return data with combined column
    dplyr::bind_cols(dplyr::select(data, -dplyr::all_of(selected_col)), combined)
  }

  #' Get the nth Element from a Delimited String
  #'
  #' This function splits a string by a specified delimiter and returns the nth element.
  #'
  #' @param string A character string to be split.
  #' @param delimiter A character string representing the delimiter to split the string by.
  #' @param n An integer specifying the position of the element to retrieve after splitting.
  #'
  #' @return The nth element as a character string if it exists; otherwise, \code{NA}.
  #'
  #' @examples
  #' get_nth_element("apple,banana,cherry", ",", 2)  # Returns "banana"
  #' get_nth_element("dog,cat", ",", 3)  # Returns NA, since there is no 3rd element
  #'
  #' @importFrom stringr str_split
  #'
  get_nth_element <- function(string, delimiter, n) {
    elements <- stringr::str_split(string, delimiter)[[1]]
    if (n <= length(elements)) {
      return(elements[[n]])
    } else {
      return(NA)  # Return NA if n is out of bounds
    }
  }

  #' Coalesce and Drop Columns
  #'
  #' This function coalesces values from one or more columns into a single column
  #' and removes the original columns afterward. It works similarly to
  #' `dplyr::coalesce()` but extends the functionality to handle multiple columns
  #' and streamline the process of column removal.
  #'
  #' @param df A dataframe containing the columns to be coalesced.
  #' @param columns A character vector specifying the names of the columns to coalesce.
  #'   The first non-NA value in each row across these columns will be retained.
  #' @param new_col A string specifying the name of the new column to create
  #'   from the coalesced values. Defaults to the name of the first column in `columns`.
  #'
  #' @return A dataframe with `new_col` as the coalesced column, and the original
  #'   columns in `columns` removed.
  #'
  #' @details This function provides a convenient way to combine values from multiple
  #' columns into a single column while ensuring that the original columns are
  #' dropped from the dataframe. It leverages `dplyr` for mutation and column selection.
  #'
  #' @examples
  #' df <- data.frame(
  #'   col1 = c(NA, "B", NA, "D"),
  #'   col2 = c("A", NA, "C", NA),
  #'   col3 = c("X", NA, NA, "Y")
  #' )
  #'
  #' # Coalesce three columns into one
  #' coalesce_and_drop(df, columns = c("col1", "col2", "col3"), new_col = "final_col")
  #'
  #' # Coalesce two columns with a default new column name
  #' coalesce_and_drop(df, columns = c("col1", "col2"))
  #'
  #' @export
  coalesce_and_drop <- function(df, columns, new_col = columns[1]) {
    df %>%
      dplyr::mutate(!!new_col := dplyr::coalesce(!!!rlang::syms(columns))) %>%
      dplyr::select(-dplyr::all_of(columns))
  }

# === Get database directory ===
  database_directory <- FileLocator::getCurrentFileLocation()

# === Load external R files ===
  setwd(database_directory)
  source("utils\\databaseUtils.R", local = TRUE)

# === Read in data ===
  setwd(database_directory)

  # Read in data from LPSN
    # From LPSN/getLpsnPhylogeny.R script
    lpsn_organisms =  readr::read_csv("LPSN\\data\\lpsn_organisms.csv")
    # From LPSN/getLpsnPhylogeny.R script
    lpsn_phylogeny =  readr::read_csv("LPSN\\data\\lpsn_phylogeny.csv")
    # From getLpsnRibosomalSequencs.R script
    lpsn_ribosomal_sequences =  readr::read_csv("LPSN\\data\\lpsn_ribosomal_sequences.csv")

  # Read in data from GTDB
    # From https://data.gtdb.ecogenomic.org/releases/latest/
    gtdb_bacteria_data =  readr::read_tsv("GTDB\\data\\bac120_metadata.tsv.gz")
    gtdb_archaea_data =  readr::read_tsv("GTDB\\data\\ar53_metadata.tsv.gz")
    
  # Read in data from GOLD
    # From https://gold.jgi.doe.gov/downloads
    # After downloading, open main *.xlsx file and resave tabs as *csv with names below
    GOLD_organism_data <- readr::read_csv("GOLD\\data\\goldDataOrganism.csv",
                                          locale = readr::locale(encoding = "ISO-8859-1"),
                                          na = "",
                                          guess_max = 1000000,
                                          name_repair = "minimal")

    GOLD_sequencing_data <- readr::read_csv("GOLD\\data\\goldDataSequencing.csv",
                                            locale = readr::locale(encoding = "ISO-8859-1"),
                                            na = "",
                                            guess_max = 1000000,
                                            name_repair = "minimal")

  # Read in data from IMG
    # From https://img.jgi.doe.gov/ (follow instructions below to download)
    IMG_data <- openxlsx::read.xlsx("IMG\\data\\IMG.xlsx")

  # Read in data from NCBI
    # From https://ftp.ncbi.nih.gov/pub/taxonomy/taxdmp.zip
    NCBI_nodes <- load_nodes_dmp(file_path = "NCBI\\data\\nodes.dmp")
    NCBI_names <- load_names_dmp(file_path = "NCBI\\data\\names.dmp")

  # Read in data from BacDive
    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1
    BacDive_data = readr::read_csv("BacDive\\data\\advsearch_bacdive_2024-11-07.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Kind+of+utilization+tested&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=fermentation&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=met_util-test_type-4&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfd%5D=Utilization+activity&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfv%5D=%2B&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfvd%5D=met_util-ability-4&fg%5B0%5D%5Bfl%5D%5B8%5D=AND&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfd%5D=Metabolite+%28utilization%29&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfvd%5D=met_util-metabolite_util-4
    Fermentation_substrates <- readr::read_csv("BacDive\\data\\fermentation_substrates.csv")
    
    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B6%5D=AND&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfd%5D=Antibiotic+resistance&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfvd%5D=met_antibiotica-ab_resistant-4&fg%5B0%5D%5Bfl%5D%5B8%5D=AND&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfd%5D=Metabolite+%28antibiotic%29&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfvd%5D=met_antibiotica-metabolite_antib-4
    Antibiotic_resistance = readr::read_csv("BacDive\\data\\antibiotic_resistance.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B6%5D=AND&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfd%5D=Antibiotic+sensitivity&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfvd%5D=met_antibiotica-ab_sensitive-4&fg%5B0%5D%5Bfl%5D%5B8%5D=AND&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfd%5D=Metabolite+%28antibiotic%29&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfvd%5D=met_antibiotica-metabolite_antib-4
    Antibiotic_sensitivity = readr::read_csv("BacDive\\data\\antibiotic_sensitivity.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B6%5D=AND&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfd%5D=Cell+length&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfo%5D=equal&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfvd%5D=cell_morphology-cell_len-2
    Cell_length = readr::read_csv("BacDive\\data\\cell_length.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B6%5D=AND&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfd%5D=Cell+shape&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfvd%5D=cell_morphology-cell_shape-2
    Cell_shape = readr::read_csv("BacDive\\data\\cell_shape.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B6%5D=AND&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfd%5D=Cell+width&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfo%5D=equal&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfvd%5D=cell_morphology-cell_width-2
    Cell_width = readr::read_csv("BacDive\\data\\cell_width.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B6%5D=AND&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfd%5D=Colony+size&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfo%5D=equal&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfvd%5D=colony_morphology-colony_len-2
    Colony_size = readr::read_csv("BacDive\\data\\colony_size.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B6%5D=AND&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfd%5D=Flagellum+arrangement&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfvd%5D=cell_morphology-flagellum_arrangement-2
    Flagellum_arrangement = readr::read_csv("BacDive\\data\\flagellum_arrangement.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B6%5D=AND&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfd%5D=Gram+stain&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfvd%5D=cell_morphology-gram_stain-2
    Gram_stain = readr::read_csv("BacDive\\data\\gram_stain.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B6%5D=AND&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfd%5D=Incubation+period+%28in+days%29&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfo%5D=equal&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfvd%5D=colony_morphology-incubation_period-2
    Incubation_period = readr::read_csv("BacDive\\data\\incubation_period.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B6%5D=AND&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfd%5D=Indole+test+%28Indole%29&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfvd%5D=met_test-indole_test-4
    Indole_test = readr::read_csv("BacDive\\data\\indole_test.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B6%5D=AND&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfd%5D=Motility&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfvd%5D=cell_morphology-motility-2
    Motility = readr::read_csv("BacDive\\data\\motility.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B6%5D=AND&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfd%5D=Oxygen+tolerance&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfvd%5D=oxygen_tolerance-oxygen_tol-4
    Oxygen_tolerance = readr::read_csv("BacDive\\data\\oxygen_tolerance.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfd%5D=Kind+of+pH&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfv%5D=growth&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfvd%5D=culture_pH-test_type-3&fg%5B0%5D%5Bfl%5D%5B10%5D=AND&fg%5B0%5D%5Bfl%5D%5B11%5D%5Bfd%5D=Testresult+%28pH%29&fg%5B0%5D%5Bfl%5D%5B11%5D%5Bfv%5D=positive&fg%5B0%5D%5Bfl%5D%5B11%5D%5Bfvd%5D=culture_pH-ability-3&fg%5B0%5D%5Bfl%5D%5B12%5D=AND&fg%5B0%5D%5Bfl%5D%5B13%5D%5Bfd%5D=pH&fg%5B0%5D%5Bfl%5D%5B13%5D%5Bfo%5D=equal&fg%5B0%5D%5Bfl%5D%5B13%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B13%5D%5Bfvd%5D=culture_pH-pH-3
    pH_for_growth = readr::read_csv("BacDive\\data\\pH_for_growth.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B6%5D=AND&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfd%5D=Pathogenicity+%28animal%29&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfvd%5D=risk_assessment-pathogenicity_animal-6
    Pathogenicity_animal = readr::read_csv("BacDive\\data\\pathogenicity_animal.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B6%5D=AND&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfd%5D=Pathogenicity+%28human%29&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfvd%5D=risk_assessment-pathogenicity_human-6
    Pathogenicity_human = readr::read_csv("BacDive\\data\\pathogenicity_human.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B6%5D=AND&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfd%5D=Pathogenicity+%28plant%29&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfvd%5D=risk_assessment-pathogenicity_plant-6
    Pathogenicity_plant = readr::read_csv("BacDive\\data\\pathogenicity_plant.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B6%5D=AND&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfd%5D=Salt+conc.&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfo%5D=equal&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfvd%5D=halophily-salt_concentration-4
    Salt_concentration = readr::read_csv("BacDive\\data\\salt_concentration.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B6%5D=AND&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfd%5D=salt+concentration+unit&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfvd%5D=halophily-salt_concentration_unit-4
    Salt_concentration_unit = readr::read_csv("BacDive\\data\\salt_concentration_unit.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B6%5D=AND&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfd%5D=Ability+of+spore+formation&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfvd%5D=spore_formation-ability-4
    Spore_formation = readr::read_csv("BacDive\\data\\spore_formation.csv")

    # https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfd%5D=Kind+of+temperature&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfv%5D=growth&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfvd%5D=culture_temp-test_type-3&fg%5B0%5D%5Bfl%5D%5B8%5D=AND&fg%5B0%5D%5Bfl%5D%5B11%5D%5Bfd%5D=Testresult+%28temperature%29&fg%5B0%5D%5Bfl%5D%5B11%5D%5Bfv%5D=positive&fg%5B0%5D%5Bfl%5D%5B11%5D%5Bfvd%5D=culture_temp-ability-3&fg%5B0%5D%5Bfl%5D%5B12%5D=AND&fg%5B0%5D%5Bfl%5D%5B13%5D%5Bfd%5D=Temperature&fg%5B0%5D%5Bfl%5D%5B13%5D%5Bfo%5D=equal&fg%5B0%5D%5Bfl%5D%5B13%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B13%5D%5Bfvd%5D=culture_temp-temp-3
    Temperature_for_growth = readr::read_csv("BacDive\\data\\temperature_for_growth.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B10%5D=AND&fg%5B0%5D%5Bfl%5D%5B11%5D%5Bfd%5D=Voges-Proskauer-test+%28Acetoin%29&fg%5B0%5D%5Bfl%5D%5B11%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B11%5D%5Bfvd%5D=met_test-voges_proskauer-4
    Voges_proskauer = readr::read_csv("BacDive\\data\\voges_proskauer.csv")

    # From https://bacdive.dsmz.de/isolation-sources
    Isolation_sources = readr::read_csv("BacDive\\data\\isolation_sources.csv")

  # Read in data from Bergey's Manual
    # From Bergey/addBergeyLabels.R script
    Bergey_data = readr::read_csv("Bergey\\data\\Bergey_data_with_labels.csv")

  # Read in data from VPI Anaerobe Laboratory Manual
    VPI_data = readr::read_csv("VPI\\data\\VPI_data.csv")

  # Read in data from primary literature
    primary_literature_data = readr::read_csv("primary_literature\\data\\primary_literature_data.csv")

  # Read in data from FAPROTAX
    # From FAPROTAX/getFAPROTAXpredictions.R script
    FAPROTAX_data = readr::read_csv("FAPROTAX\\data\\FAPROTAX_data.csv")

# === Start database using data from LPSN ===
  database = lpsn_organisms

  # Add phylogeny
  matches_LPSN = match(x = lpsn_phylogeny$LPSN_ID, table = database$LPSN_ID)

  # Use indices to add phylogeny to database
  database <- add_columns_based_on_indices(
    target_df =  database,
    source_df = lpsn_phylogeny,
    target_index = matches_LPSN,
    source_index = seq_along(matches_LPSN),
    source_col_names = c(
      "Domain",
      "Phylum",
      "Class",
      "Order",
      "Family",
      "Genus...1",
      "Species",
      "Subspecies",
      "Strain"
    ),
    target_col_names = c(
      "LPSN_Domain",
      "LPSN_Phylum",
      "LPSN_Class",
      "LPSN_Order",
      "LPSN_Family",
      "LPSN_Genus",
      "LPSN_Species",
      "LPSN_Subspecies",
      "LPSN_Strain"
    )
  )

  # Add 16S ribosomal sequence
  matches_LPSN = match(x = lpsn_ribosomal_sequences$LPSN_ID, table = database$LPSN_ID)

  # Use indices to add sequences to database
  database <- add_columns_based_on_indices(
    target_df =  database,
    source_df = lpsn_ribosomal_sequences,
    target_index = matches_LPSN,
    source_index = seq_along(matches_LPSN),
    source_col_names = c(
      "16S_ribosomal_sequence"
    ),
    target_col_names = c(
      "LPSN_16S_Ribosomal_sequence"
    )
  )

  # Rename columns
  database <- database %>% dplyr::rename(LPSN_Page_link = address)
  database <- database %>% dplyr::select(-LPSN_Page_link,LPSN_Page_link)
  database <- database %>% dplyr::rename(LPSN_status = Status)
  database <- database %>% dplyr::select(-LPSN_status,LPSN_status)

# === Get names of culture collections ====
  # Names are used in matching strains in database to other sources of data
  collection_names = get_collection_names(strains = database$Strain, n = 32)

# === Add data from GTDB ===
  # Format GTDB data
  # Combine data for bacteria and archaea
  gtdb_data <- rbind(gtdb_bacteria_data, gtdb_archaea_data)
  
  # Keep only records for type strains, and select only essential columns
  gtdb_data <- gtdb_data %>%
    dplyr::filter((gtdb_type_designation_ncbi_taxa_sources == "LPSN")) %>%
    dplyr::select(accession, gtdb_taxonomy, ncbi_strain_identifiers)
  
  # Split taxonomy into ranks
  gtdb_data <- gtdb_data %>%
    dplyr::mutate(
      taxonomy_split = lapply(gtdb_taxonomy, split_taxonomy)
    ) %>%
    tidyr::unnest_wider(taxonomy_split)
  
  # Find matches between database and GOLD
  matches_GTDB = perform_matching(
    table_data = gtdb_data,
    table_genus_col = "Genus",
    table_species_col = "Species",
    table_strain_col = "ncbi_strain_identifiers",
    table_delim = ",",
    x_data = database,
    x_genus_col = "Genus",
    x_species_col = "Species",
    x_strain_col = "Strain",
    x_delim = ";",
    collection_names = collection_names,
    output_file = NULL
  )
  
  # Filter indices to keep only the best matches
  matches_filtered <- matches_GTDB %>% dplyr::group_by(x_index) %>%
    dplyr::slice_min(`Rank`, with_ties = FALSE)
  # Remove rank 8 matches (likely to contain non-type strains)
  matches_filtered <- matches_filtered %>% dplyr::filter(Rank != 8)
  # Remove multiple matches
  matches_filtered <- matches_filtered %>% dplyr::group_by(table_index) %>%
    dplyr::slice_min(`Rank`, with_ties = FALSE)
  
  # Use indices to add GTDB data to database
  database <- add_columns_based_on_indices(
    target_df =  database,
    source_df = gtdb_data,
    target_index = matches_filtered$x_index, # debug
    source_index = matches_filtered$table_index, # debug
    source_col_names = c(
      "Domain",
      "Phylum",
      "Class",
      "Order",
      "Family",
      "Genus",
      "Species",
      "accession"
    ),
    target_col_names = c(
      "GTDB_Domain",
      "GTDB_Phylum",
      "GTDB_Class",
      "GTDB_Order",
      "GTDB_Family",
      "GTDB_Genus",
      "GTDB_Species",
      "GTDB_ID"
    )
  )

# === Add data from GOLD database ===
  # Format GOLD organism data
    # Select only bacteria and archaea
    GOLD_organism_data <- GOLD_organism_data %>%
      dplyr::filter(`ORGANISM NCBI SUPERKINGDOM` == "Bacteria" | `ORGANISM NCBI SUPERKINGDOM` == "Archaea")
    # Format genus names
    GOLD_organism_data$`ORGANISM SPECIES` <- stringr::str_remove(GOLD_organism_data$`ORGANISM SPECIES`, "^\\S+\\s+")

    # Add sequencing project
    GOLD_sequencing_data <- GOLD_sequencing_data %>% dplyr::select(`ORGANISM GOLD ID`, `PROJECT GOLD ID`) %>% dplyr::group_by(`ORGANISM GOLD ID`) %>%
      dplyr::summarize(`PROJECT GOLD ID` = paste(`PROJECT GOLD ID`, collapse = ", "))
    GOLD_organism_data <- dplyr::left_join(x = GOLD_organism_data, y = GOLD_sequencing_data, by = "ORGANISM GOLD ID")

  # Find matches between database and GOLD
  matches_GOLD = perform_matching(
    table_data = GOLD_organism_data,
    table_genus_col = "ORGANISM GENUS",
    table_species_col = "ORGANISM SPECIES",
    table_strain_col = "ORGANISM STRAIN",
    table_delim = ",",
    x_data = database,
    x_genus_col = "Genus",
    x_species_col = "Species",
    x_strain_col = "Strain",
    x_delim = ";",
    collection_names = collection_names,
    output_file = NULL
  )

  # Filter indices to keep only the best matches
    matches_filtered =  matches_GOLD
    matches_filtered$Project = GOLD_organism_data$'PROJECT GOLD ID'[matches_GOLD$table_index]
    matches_filtered = matches_filtered %>% dplyr::filter(Rank != 5) # Remove rank 5 matches (likely to contain non-type strain)

    # Pick the highest rank with a sequencing project
    matches_filtered <- matches_filtered %>%
      dplyr::group_by(x_index) %>%
      dplyr::arrange(Rank) %>%
      dplyr::mutate(Project_NA = all(is.na(Project))) %>%
      dplyr::filter(ifelse(Project_NA, Rank == min(Rank), Rank == min(Rank[!is.na(Project)], na.rm = TRUE))) %>%
      dplyr::ungroup() %>%
      dplyr::select(-Project_NA)  # Remove the helper column

  # Use indices to add GOLD data to database
    database <- add_columns_based_on_indices(
      target_df =  database,
      source_df = GOLD_organism_data,
      target_index = matches_filtered$x_index,
      source_index = matches_filtered$table_index,
      source_col_names = c(
        "ORGANISM GOLD ID",
        "PROJECT GOLD ID",
        "ORGANISM NCBI TAX ID"
      ),
      target_col_names = c(
        "GOLD_Organism_ID",
        "GOLD_Project_ID",
        "NCBI_Taxonomy_ID"
      )
    )

# === Add data from IMG ===
  # Instructions for downloading data (IMG.xlsx) from IMG
    ## Get GOLD organism IDs from database and output to file using command below (uncomment to run)
    # write_project_ids_to_files(project_IDs = database$'PROJECT GOLD ID', batch_size = 500, file_prefix = "project_IDs_batch")
    ## Then navigate to IMG, log on, and then navigate to Find Genomes (https://img.jgi.doe.gov/cgi-bin/mer/main.cgi?section=GenomeSearch&page=searchForm)
    ## Paste contents of each file outputted above (one at a time in search bar).  In "Search by ID (list)" field, choose "GOLD Sequencing Project ID".  Click "Search".
    ## In the screen that appears, click "Select All" and "Add Selected to Genome Cart".  Repeat for remaining files.
    ## In the Genome Cart screen that appears, click "GOLD Sequencing Project ID" and "Redisplay" below the table.
    ## Then click the check box in the left corner (to select all genomes), then click "Export".
    ## Save as "IMG.xlsx" in the IMG/data folder.

    # Get IMG genome IDs
    x <- IMG_data$`GOLD.Sequencing.Project.ID`
    table <- database$`GOLD_Project_ID`

    # Find matches
    matches_IMG <- find_match_indices(x, table)
    IMG_data$match_indices <- matches_IMG

    # Add IMG genome IDs to database
    database$`IMG_Genome_ID` <- NA
    database$`IMG_Genome_ID` <- update_database_column(
      database_col = database$`IMG_Genome_ID`,
      match_indices = IMG_data$match_indices,
      values = IMG_data$`IMG.Genome.ID`
    )

    # Get IMG genome ID for genome with max genes
    IMG_data_filtered <- IMG_data %>%
      dplyr::group_by(match_indices) %>%
      dplyr::slice_max(`Gene.Count.*.assembled`, with_ties = FALSE)

    # All genome IDs with max genes database
    database$`IMG_Genome_ID_max_genes` <- NA
    database$`IMG_Genome_ID_max_genes` <- update_database_column(
      database_col = database$`IMG_Genome_ID_max_genes`,
      match_indices = IMG_data_filtered$match_indices,
      values = IMG_data_filtered$`IMG.Genome.ID`
    )

# === Add data from NCBI ===
    # Format data
    tax_ids <- extract_first_value(vec = database$`NCBI_Taxonomy_ID`)
    NCBI_data = get_multiple_lineages(tax_ids = tax_ids, nodes_df = NCBI_nodes, names_df = NCBI_names)

    # Find matches between database and NCBI
    matches_NCBI = match(x =  NCBI_data$tax_id, table = database$`NCBI_Taxonomy_ID`)

    # Add NCBI data to database
    database <- add_columns_based_on_indices(
      target_df =  database,
      source_df = NCBI_data,
      target_index = seq_along(matches_NCBI),
      source_index = matches_NCBI,
      source_col_names = c(
        "phylum",
        "class",
        "order",
        "family",
        "genus",
        "species"
      ),
      target_col_names = c(
        "NCBI_Phylum",
        "NCBI_Class",
        "NCBI_Order",
        "NCBI_Family",
        "NCBI_Genus",
        "NCBI_Species"
      )
    )

# === Add data from BacDive ===
    # Format BacDive data
      BacDive_data <- BacDive_data %>%
        tidyr::separate(col = species, into = c("genus", "species"), sep = " ", extra = "merge")

    # Find matches between database and BacDive
      matches_BacDive = perform_matching(
        table_data = BacDive_data,
        table_genus_col = "genus",
        table_species_col = "species",
        table_strain_col = "strain_number_header",
        table_delim = ", ",
        x_data = database,
        x_genus_col = "Genus",
        x_species_col = "Species",
        x_strain_col = "Strain",
        x_delim = ";",
        collection_names = collection_names,
        output_file = NULL
      )

    # Filter indices to keep only the best matches
      matches_filtered = matches_BacDive %>% dplyr::group_by(x_index) %>%
        dplyr::slice_min(`Rank`, with_ties = FALSE)

    # Use indices to add BacDive data to database
      database <- add_columns_based_on_indices(
        target_df =  database,
        source_df = BacDive_data,
        target_index = matches_filtered$x_index,
        source_index = matches_filtered$table_index,
        source_col_names = c(
          "ID"
        ),
        target_col_names = c(
          "BacDive_ID"
        )
      )

    # Use BacDive IDs to add more data
      # Add most phenotypes
      phenotype_names <- c("Antibiotic_resistance", "Antibiotic_sensitivity",
                           "Cell_length", "Cell_shape", "Cell_width",
                           "Colony_size",                            
                           "Fermentation_substrates",
                           "Flagellum_arrangement", "Gram_stain",
                           "Incubation_period", "Indole_test", "Motility",
                           "Oxygen_tolerance", "pH_for_growth",
                           "Pathogenicity_animal", "Pathogenicity_human", "Pathogenicity_plant",
                           "Salt_concentration", "Salt_concentration_unit",
                           "Spore_formation",
                           "Temperature_for_growth",
                           "Voges_proskauer")

      for (phenotype in phenotype_names) {
        source_df <- get(phenotype)
        database <- add_BacDive_phenotypes(
          target_df = database,
          source_df = source_df,
          source_col_names = NULL,
          target_col_names = paste0("BacDive_", phenotype)
        )
      }

      # Add isolation categories
      source_df <- Isolation_sources
      database <- add_BacDive_phenotypes(
        target_df = database,
        source_df = source_df,
        source_col_names = c(
          "Category 1",
          "Category 2",
          "Category 3"
          ),
        target_col_names = c(
          "BacDive_Isolation_category_1",
          "BacDive_Isolation_category_2",
          "BacDive_Isolation_category_3"
          )
      )

# === Add data from Bergey's Manual ===
  # Clean strain IDs in Bergey data
  Bergey_data$Strain <- gsub(pattern = "DSMZ", replacement = "DSM", x = Bergey_data$Strain)

  # Find matches between database and Bergey's Manual
  matches_Bergey = perform_matching(
    table_data = database,
    table_genus_col = "Genus",
    table_species_col = "Species",
    table_subspecies_col = "Subspecies",
    table_strain_col = "Strain",
    x_data = Bergey_data,
    x_genus_col = "Genus",
    x_species_col = "Species",
    x_subspecies_col = "Subspecies",
    x_strain_col = "Strain",
    table_delim = ";",
    x_delim = ";",
    collection_names = collection_names,
    output_file = NULL
  )

  # Filter indices to keep only the best matches
  matches_filtered = matches_Bergey %>% dplyr::group_by(x_index) %>%
    dplyr::slice_min(`Rank`, with_ties = FALSE)

  # Remove any multiple matches still remaining
  # (caused by organisms appearing in multiple articles in Bergey's Manual or 
  # or organisms have subspecies names with incorrect name)
  matches_filtered <- matches_filtered %>% dplyr::group_by(table_index) %>%
    dplyr::slice_min(`Rank`, with_ties = FALSE)
  
  # Use indices to add data from Bergey's Manual to database
  database <- add_columns_based_on_indices(
    target_df =  database,
    source_df = Bergey_data,
    target_index = matches_filtered$table_index,
    source_index = matches_filtered$x_index,
    source_col_names = c(
      "Phylum",
      "Class",
      "Order",
      "Family",
      "Genus",
      "Species",
      "Subspecies",
      "Strain",
      "Article_link",
      "Type_of_metabolism",
      "Text_for_end_products",
      "Major_end_products",
      "Minor_end_products",
      "Text_for_substrates",
      "Substrates_for_end_products"
    ),
    target_col_names = c(
      "Bergey_Phylum",
      "Bergey_Class",
      "Bergey_Order",
      "Bergey_Family",
      "Bergey_Genus",
      "Bergey_Species",
      "Bergey_Subspecies",
      "Bergey_Strain",
      "Bergey_Article_link",
      "Bergey_Type_of_metabolism",
      "Bergey_Text_for_end_products",
      "Bergey_Major_end_products",
      "Bergey_Minor_end_products",
      "Bergey_Text_for_substrates",
      "Bergey_Substrates_for_end_products"
    )
  )

# === Add data from VPI Anaerobe Manual ===
    # Find matches between database and primary literature
    matches_VPI = perform_matching(
      table_data = database,
      table_genus_col = "Genus",
      table_species_col = "Species",
      table_strain_col = "Strain",
      x_data = VPI_data,
      x_genus_col = "Genus",
      x_species_col = "Species",
      x_strain_col = "Strains",
      table_delim = ";",
      x_delim = ",",
      collection_names = collection_names,
      output_file = NULL
    )

    # Filter indices to keep only the best matches
    matches_filtered <- matches_VPI %>% dplyr::group_by(x_index) %>%
      dplyr::slice_min(`Rank`, with_ties = FALSE)
    # Remove rank 8 matches (likely to contain non-type strains)
    matches_filtered <- matches_filtered %>% dplyr::filter(Rank != 8)
    # Remove multiple matches (caused by strains appearing multiple times in VPI Anaerobe Manual)
    matches_filtered <- matches_filtered %>% dplyr::group_by(table_index) %>%
      dplyr::slice_min(`Rank`, with_ties = FALSE)
    
    # Use indices to add VPI data to database
    database <- add_columns_based_on_indices(
      target_df =  database,
      source_df = VPI_data,
      target_index = matches_filtered$table_index,
      source_index = matches_filtered$x_index,
      source_col_names = c(
        "Genus",
        "Species",
        "Species",
        "Type_of_metabolism",
        "Major_end_products",
        "Minor_end_products"
      ),
      target_col_names = c(
        "VPI_Genus",
        "VPI_Species",
        "VPI_Subspecies",
        "VPI_Type_of_metabolism",
        "VPI_Major_end_products",
        "VPI_Minor_end_products"
      )
    )

# === Add data from primary literature ===
  # Format names of columns
  selected_columns <- c(
    "Formate", "Acetate", "Propionate", "Butyrate", "Isobutyrate", "Valerate",
    "Isovalerate", "Caproate", "Isocaproate", "Heptanoate", "Octanoate", "Pyruvate", "Lactate",
    "Succinate", "Glycerol", "Malate", "Ornithine", "Ethanol", "Butanol", "CO2",
    "H2", "NH3", "CH4"
  )

  primary_literature_data =
    primary_literature_data %>% unite_with_names(
      col_name = "Text_for_end_products",
      selected_col = selected_columns,
      name_first = FALSE
    )

  # Find matches between database and primary literature
  matches_literature = perform_matching(
    table_data = database,
    table_genus_col = "Genus",
    table_species_col = "Species",
    table_strain_col = "Strain",
    x_data = primary_literature_data,
    x_genus_col = "Genus",
    x_species_col = "Species",
    x_strain_col = "Strain",
    table_delim = ";",
    x_delim = ",",
    collection_names = collection_names,
    output_file = NULL
  )

  # Filter indices to keep only the best matches
  matches_filtered <- matches_literature %>% dplyr::group_by(x_index) %>%
    dplyr::slice_min(`Rank`, with_ties = FALSE)
  matches_filtered = matches_filtered %>% dplyr::filter(Rank != 8) # Remove rank 8 matches (removes strains with incorrect name)

  # Use indices to add primary literature data to database
  database <- add_columns_based_on_indices(
    target_df =  database,
    source_df = primary_literature_data,
    target_index = matches_filtered$table_index,
    source_index = matches_filtered$x_index,
    source_col_names = c(
      "Genus",
      "Species",
      "Subspecies",
      "Strain",
      "Citation",
      "Type_of_metabolism",
      "Major_end_products",
      "Minor_end_products",
      "Text_for_end_products",
      "Substrates_for_end_products"
    ),
    target_col_names = c(
      "Literature_Genus",
      "Literature_Species",
      "Literature_Subspecies",
      "Literature_Strain",
      "Literature_Citation",
      "Literature_Type_of_metabolism",
      "Literature_Major_end_products",
      "Literature_Minor_end_products",
      "Literature_Text_for_end_products",
      "Literature_Substrates_for_end_products"
    )
  )

# === Add data from FAPROTAX ===
  # Format names of columns
  FAPROTAX_data$Genus = sapply(FAPROTAX_data$taxonomy, get_nth_element, delimiter = ";", n = 5)
  FAPROTAX_data$Species = sapply(FAPROTAX_data$taxonomy, get_nth_element, delimiter = ";", n = 6)

  # Find matches between database and FAPROTAX
  matches_FAPROTAX = FAPROTAX_data %>%
    dplyr::mutate(
      index = match(
        paste(Genus, Species),
        paste(database$Genus, database$Species)
      )
    ) %>%
    dplyr::pull(index)

  # Use indices to add primary literature data to database
  database <- add_columns_based_on_indices(
    target_df =  database,
    source_df = FAPROTAX_data,
    target_index = matches_FAPROTAX,
    source_index = seq_along(matches_FAPROTAX),
    source_col_names = c(
      "group"
    ),
    target_col_names = c(
      "FAPROTAX_traits"
    )
  )

# === Format data in database ===
  # Format all missing values consistently
      database[database == ""] <- NA
      database[database == "NA"] <- NA

# === Export database ===
    save_as_zip(database, "database.csv")
    