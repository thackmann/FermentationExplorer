# --- Clean database file and add links ---
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
    # Making naming of "positive" and "negative" consistent
    x <- gsub(pattern = "(?<=^|;)0(?=;|$)", replacement = "negative", x = x, perl = TRUE)
    x <- gsub(pattern = "(?<=^|;)1(?=;|$)", replacement = "positive", x = x, perl = TRUE)
    x <- gsub(pattern = "(?<=^|;)-(?=;|$)", replacement = "negative", x = x, perl = TRUE)
    x <- gsub(pattern = "(?<=^|;)\\+(?=;|$)", replacement = "positive", x = x, perl = TRUE)
    x <- gsub(pattern = "(?<=^|;)\\+/-(?=;|$)", replacement = "variable", x = x, perl = TRUE)
    
    # Remove alpha-, beta-, D-, and L- in chemical names
    x <- gsub(pattern = "(^|;)alpha-", replacement = "\\1", x = x)
    x <- gsub(pattern = "(^|;)beta-", replacement = "\\1", x = x)
    x <- gsub(pattern = "(^|;)D-", replacement = "\\1", x = x)
    x <- gsub(pattern = "(^|;)d-", replacement = "\\1", x = x)
    x <- gsub(pattern = "-D-", replacement = "-", x = x)
    x <- gsub(pattern = "(^|;)L-", replacement = "\\1", x = x)
    
    # Remove other characters
    x <- gsub(pattern = "#", replacement = "", x = x)
    x <- gsub(pattern = "_", replacement = " ", x = x)
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

#' Generate Random Forest Models
#' 
#' This function formats predictors and response data, fits a random forest model,
#' evaluates the model, and saves results.
#' 
#' @param data The dataset to use. Default is `load_database()`.
#' @param gene_functions The gene functions dataset to use. Default is `load_gene_functions()`.
#' @param predictors_to_keep Number of predictors to retain. Default is 0.1.
#' @param responses_to_keep Number of response variables to retain. Default is 1.
#' @param seed Random seed for reproducibility. Default is 123.
#' @param ignore_NA Boolean indicating whether to ignore NA values. Default is TRUE.
#' @param ntree Number of trees in the random forest. Default is 50.
#' @param maxnodes Maximum number of nodes in trees. Default is 30.
#' @param training_split Proportion of data used for training. Default is 0.7.
#' @param var_name A string representing the name of the variable being processed.
#' @param query_string A string representing the filtering condition for selecting response data.
#' 
#' @return None. Saves model-related outputs to files.
#' @export
generate_rf <- function(
    data = load_database(force_reload = TRUE),
    gene_functions = load_gene_functions(),
    predictors_to_keep = 0.1,
    responses_to_keep = 1,
    seed = 123,
    ignore_NA = TRUE,
    ntree = 50,
    maxnodes = 30,
    training_split = 0.7,
    var_name, 
    query_string
) {
  message("Processing: ", var_name)
  cat(file = stderr(), paste0("Started training at ", Sys.time(), "\n"))
  
  # Process query string for more exact matching
  query_string <- process_query_string(query_string)
  
  # Format predictors
  predictors <- format_predictors(
    gene_functions = gene_functions, 
    responses_to_keep = responses_to_keep, 
    predictors_to_keep = predictors_to_keep,
    seed = seed
  )
  
  # Format response
  response <- format_response(
    data = data, 
    query_string = query_string, 
    ignore_NA = ignore_NA
  )
  
  # Format data
  rf_data <- format_rf_data(predictors = predictors, response = response)
  
  # Build the random forest model
  rf <- build_rf(
    data = rf_data, 
    seed = seed, 
    training_split = training_split, 
    ntree = ntree, 
    maxnodes = maxnodes
  )
  
  cat(file = stderr(), paste0("Ended training at ", Sys.time(), "\n"))
  
  # Print model evaluation
  print(rf$evaluation_results$table)
  print(rf$evaluation_results$byClass)
  
  # Save response variable
  save_as_zip(data = response, fp = paste0("data/response_", var_name, ".csv"))
  
  # Save predictor variables
  save_as_zip(data = predictors, fp = paste0("data/predictors_", (var_name), ".csv"))
  
  # Save random forest model
  save_rf(rf = rf, data_fp = paste0("data/random_forest_", var_name, ".rds"))
}