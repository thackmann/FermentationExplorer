# Preprocess Data for Shiny App
# This script generates data files for the app
# It is not called during app execution
# Author: Timothy Hackmann
# Date: 6 September 2024

# === Set system locale ===
Sys.setlocale("LC_ALL", "C")

# === Preprocess data ===
# Set directory for app
  setwd("C:/My Directory")
  
# Load external R files
source("utils.R", local = TRUE)

# --- Clean database file and add links ---
  #' Create HTML Link to External Website with Comma-Separated IDs
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
        ID_split <- strsplit(x = ID, split = ", ")[[1]]
        individual_links <- sprintf('<a href="%s" target="_blank">%s</a>', utils::URLdecode(paste0(url, ID_split)), ID_split)
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
  #' Each URL is converted into a clickable button.
  #'
  #' @param urls A character vector of URLs.
  #' @return A character vector of HTML link buttons corresponding to the URLs.
  #' @export
  createLinkButton <- function(urls) {
    urls <- as.character(urls)
    urls[urls == "NA"] <- NA
    links <- sprintf('<a href="%s" target="_blank" class="btn btn-primary">Link</a>', utils::URLdecode(urls))
    links[is.na(links)] <- "NA"
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
      z = strsplit(x, "-")
      z = lapply(z, function(y) {
        num_values <- y[!grepl("\\D", y)]  # Filter out non-numeric values
        if(length(num_values) > 0) as.numeric(num_values) else NA
      })
      x = sapply(z, mean, na.rm = TRUE)
      
      x = as.numeric(x)
      
    } else if(is_numeric == FALSE) {
      x = gsub(pattern = "^0$", replacement = "-", x = x)
      x = gsub(pattern = "^1$", replacement = "+", x = x)
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
  
  data_fp = paste0("data/database.csv")
  data = read.csv(data_fp)
  
  #Clean data
  data[] <- lapply(data, as.character)
  
  data_vars <- c("Cell_length", "Cell_shape", "Cell_width", "Colony_size",
                 "Flagellum_arrangement", "Growth_temperature", "Gram_stain",
                 "Incubation_period", "Indole_test", "Isolation_source_category_1",
                 "Isolation_source_category_2", "Isolation_source_category_3",
                 "Oxygen_tolerance", "pH_for_growth", "Spore_formation",
                 "Salt_concentration_amount", "Salt_concentration_unit", "FAPROTAX_predicted_metabolism")
  
  is_numeric_vars <- c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE,
                       FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE)
  data[data_vars] <- mapply(clean_external_data, x = data[data_vars], is_numeric = is_numeric_vars, SIMPLIFY = FALSE)
  
  # Rename variables for clarity
  data <- dplyr::rename(data,
                        Cell_length_in_microns = "Cell_length",
                        Cell_width_in_microns = "Cell_width",
                        Incubation_period_in_days = "Incubation_period"
  )
  
  # Convert salt concentration to uniform units (mol L-1)
  data$Salt_in_moles_per_liter <- convert_salt_concentration(concentration = data$Salt_concentration_amount, unit = data$Salt_concentration_unit)
  data = data %>% dplyr::select(-Salt_concentration_amount, -Salt_concentration_unit)
  
  # Remove underscores in variable and phylum names
  colnames(data) <- gsub(pattern = "_", replacement = " ", x = colnames(data))
  data$Phylum <- gsub(pattern = "[_\\.]", replacement = " ", x = data$Phylum)
  
  # Remove row index
  data <- dplyr::select(data, -X)
  
  # Convert character columns to factor
  data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)
  
  # Add links
  data$`NCBI Taxonomy ID link` <- createLink(data$`NCBI Taxonomy ID`, "https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?mode=Info&id=")
  data$`GOLD Organism ID link` <- createLink(data$`GOLD Organism ID`, "https://gold.jgi.doe.gov/organism?id=")
  data$`GOLD Project ID link` <- createLink(data$`GOLD Project ID`, "https://gold.jgi.doe.gov/project?id=")
  data$`IMG Genome ID link` <- createLink(data$`IMG Genome ID`, "https://img.jgi.doe.gov/cgi-bin/m/main.cgi?section=TaxonDetail&page=taxonDetail&taxon_oid=")
  data$`IMG Genome ID max genes link` <- createLink(data$`IMG Genome ID max genes`, "https://img.jgi.doe.gov/cgi-bin/m/main.cgi?section=TaxonDetail&page=taxonDetail&taxon_oid=")
  data$`BacDive Organism ID link` <- createLink(data$`BacDive Organism ID`, "https://bacdive.dsmz.de/strain/")
  data$`Article link button` <- createLinkButton(data$`Article link`)
  
  # Save file
  data_fp = paste0("data/database_clean.csv")
  write.csv(data, file = data_fp)

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
      unique_values <- base::unique(vec)
      if (remove_na) {
        unique_values <- unique_values[!base::is.na(unique_values) & unique_values != "NA"]
      }
      if (sort) {
        unique_values <- base::sort(unique_values)
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
    data_fp = paste0("data/database_clean.csv")
    data <- read.csv(data_fp, check.names=FALSE)
    
  # Generate filters
  query_filters <- list(
    list(id = "Phylum", label = "Phylum", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$Phylum)),
    list(id = "Class", label = "Class", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$Class)),
    list(id = "Order", label = "Order", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$Order)),
    list(id = "Family", label = "Family", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$Family)),
    list(id = "Genus", label = "Genus", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$Genus)),
    list(id = "Species", label = "Species", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$Species)),
    list(id = "Subspecies", label = "Subspecies", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$Subspecies)),
    list(id = "Strain ID", label = "Strain ID", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Strain ID`)),
    list(id = "Type of metabolism", label = "Type of metabolism", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Type of metabolism`)),
    list(id = "Major end products", label = "Major end products", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Major end products`, delimited = TRUE, delimiter = ";")),
    list(id = "Minor end products", label = "Minor end products", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Minor end products`, delimited = TRUE, delimiter = ";")),
    list(id = "Substrates for end products", label = "Substrates for end products", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Substrates for end products`, delimited = TRUE, delimiter = ";")),
    list(id = "GOLD Organism ID", label = "GOLD Organism ID", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`GOLD Organism ID`, delimited = TRUE, delimiter = ",")),
    list(id = "GOLD Project ID", label = "GOLD Project ID", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`GOLD Project ID`, delimited = TRUE, delimiter = ",")),
    list(id = "IMG Genome ID", label = "IMG Genome ID", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`IMG Genome ID`, delimited = TRUE, delimiter = ",")),
    list(id = "IMG Genome ID max genes", label = "IMG Genome ID max genes", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`IMG Genome ID max genes`)),
    list(id = "NCBI Taxonomy ID", label = "NCBI Taxonomy ID", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`NCBI Taxonomy ID`)),
    list(id = "NCBI Phylum", label = "NCBI Phylum", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`NCBI Phylum`, delimited = TRUE, delimiter = ",")),
    list(id = "NCBI Class", label = "NCBI Class", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`NCBI Class`)),
    list(id = "NCBI Order", label = "NCBI Order", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`NCBI Order`)),
    list(id = "NCBI Family", label = "NCBI Family", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`NCBI Family`)),
    list(id = "NCBI Genus", label = "NCBI Genus", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`NCBI Genus`)),
    list(id = "NCBI Species", label = "NCBI Species", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`NCBI Species`)),
    list(id = "BacDive Organism ID", label = "BacDive Organism ID", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`BacDive Organism ID`)),
    list(id = "Cell length in microns", label = "Cell length in microns", type = "double", plugin = "slider", plugin_config = create_slider_plugin_config(data$`Cell length in microns`)),
    list(id = "Cell shape", label = "Cell shape", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Cell shape`)),
    list(id = "Cell width in microns", label = "Cell width in microns", type = "double", plugin = "slider", plugin_config = create_slider_plugin_config(data$`Cell width in microns`)),
    list(id = "Colony size", label = "Colony size", type = "double", plugin = "slider", plugin_config = create_slider_plugin_config(data$`Colony size`)),
    list(id = "Flagellum arrangement", label = "Flagellum arrangement", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Flagellum arrangement`)),
    list(id = "Growth temperature", label = "Growth temperature", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Growth temperature`)),
    list(id = "Gram stain", label = "Gram stain", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Gram stain`)),
    list(id = "Incubation period in days", label = "Incubation period in days", type = "double", plugin = "slider", plugin_config = create_slider_plugin_config(data$`Incubation period in days`)),
    list(id = "Indole test", label = "Indole test", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Indole test`)),
    list(id = "Isolation source category 1", label = "Isolation source category 1", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Isolation source category 1`)),
    list(id = "Isolation source category 2", label = "Isolation source category 2", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Isolation source category 2`)),
    list(id = "Isolation source category 3", label = "Isolation source category 3", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Isolation source category 3`)),
    list(id = "Oxygen tolerance", label = "Oxygen tolerance", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Oxygen tolerance`)),
    list(id = "pH for growth", label = "pH for growth", type = "double", plugin = "slider", plugin_config = create_slider_plugin_config(data$`pH for growth`)),
    list(id = "Spore formation", label = "Spore formation", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Spore formation`)),
    list(id = "Salt in moles per liter", label = "Salt in moles per liter", type = "double", plugin = "slider", plugin_config = create_slider_plugin_config(data$`Salt in moles per liter`)),
    list(id = "FAPROTAX predicted metabolism", label = "FAPROTAX predicted metabolism", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`FAPROTAX predicted metabolism`, delimited = TRUE, delimiter = ";"))
  )
    
  # Save object to file
  data_fp = paste0("data/query_filters.rds")
  saveRDS(query_filters, file = data_fp)

# --- Generate files for phylogenetic tree ---
  # Load tree
  data_fp <- "data/tree.tre"
  tree <- ape::read.tree(data_fp)
  
  # Get layouts for phylogenetic tree
    layout_types <- c("rectangular", "daylight", "equal_angle")
    
    for (layout_type in layout_types) {
      layout <- get_tree_layout(tree, layout_type = layout_type)
      data_fp <- paste0("data/layout_tree_", layout_type, ".csv")
      write.csv(layout, data_fp, row.names = FALSE)
    }
  
  # Get nodes from tips to root
      nodes_to_root = get_nodes_to_root(tree = tree)
      data_fp = "data/nodes_to_root.csv"
      write.csv(nodes_to_root, file = data_fp, row.names = FALSE)
    
  # Plot branches for all organisms
    for (layout_type in layout_types) {
      # Load layout
      data_fp = paste0("data/layout_tree_", layout_type, ".csv")
      layout <- read.csv(data_fp)
      
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
      data_fp = paste0("data/layout_tree_", layout_type, ".csv")
      layout <- read.csv(data_fp)
      data_fp = paste0("data/database.csv")
      data = read.csv(data_fp)
      
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
      data_fp = paste0("data/layout_tsne.csv")
      layout <- read.csv(data_fp)
      data = raw_data
      
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
    # Define functions
      #' Format Predictors for Random Forest Model
      #'
      #' This function formats the predictors for a random forest model by processing genome and KO ID columns.
      #' It optionally subsamples a proportion of the columns to reduce the number of predictors.
      #'
      #' @param gene_functions A data frame containing gene functions with KO IDs and Genome IDs.
      #' @param seed An optional seed value for reproducibility. Default is NULL.
      #' @param proportion_to_keep An optional proportion of columns to keep when subsampling. Must be between 0 and 1. Default is NULL.
      #' @return A data frame of formatted predictors for the random forest model.
      #' @export
      #' @importFrom dplyr select mutate distinct
      #' @importFrom tidyr pivot_wider pivot_longer
      #' @importFrom rlang sym
      format_predictors = function(gene_functions, seed = NULL, proportion_to_keep = NULL)
      {
        predictors = gene_functions
        
        ##Genome IDs in one column and KO IDs in another column
        if ("Genome" %in% colnames(predictors) & !is.na(detect_pattern_column(data = predictors, pattern = "^K[0-9]{5}$"))) {
          column_with_pattern <- detect_pattern_column(data = predictors, pattern = "^K[0-9]{5}$")
          predictors <- predictors %>% 
            dplyr::select(Genome, !!rlang::sym(column_with_pattern)) %>% 
            dplyr::distinct() %>% 
            dplyr::mutate(value = 1) %>% 
            tidyr::pivot_wider(names_from = !!rlang::sym(column_with_pattern), values_from = value, values_fill = list(value = 0))
        } else if (!is.na(detect_pattern_column(data = predictors, pattern = "^K[0-9]{5}$"))) {
          ##KO IDs in one or more columns
          column_with_pattern <- detect_pattern_column(data = predictors, pattern = "^K[0-5]{5}$")
          predictors = predictors[[column_with_pattern]]
          predictors = predictors %>% tidyr::pivot_longer(cols = everything(), names_to = "Genome", values_to = "Database_ID")
          column_with_pattern <- detect_pattern_column(data = predictors, pattern = "^K[0-9]{5}$")
          predictors <- predictors %>% 
            dplyr::select(Genome, !!rlang::sym(column_with_pattern)) %>% 
            dplyr::distinct() %>% 
            dplyr::mutate(value = 1) %>% 
            tidyr::pivot_wider(names_from = !!rlang::sym(column_with_pattern), values_from = value, values_fill = list(value = 0))
        } else {
          predictors = NULL
        }
        
        predictors$Genome <- as.character(predictors$Genome)
        
        
        # Randomly subsample columns (reduces number of predictors)
        # Set seed
        if (!is.null(seed)) {
          set.seed(seed)
        }
        
        #Subsample columns
        if (!is.null(proportion_to_keep)) {
          if (proportion_to_keep > 1 || proportion_to_keep < 0) {
            stop("Proportion must be between 0 and 1.")
          }
          total_cols <- length(colnames(predictors))
          n_cols <- round(total_cols * proportion_to_keep)
          selected_cols <- sample(colnames(predictors), n_cols)
          predictors <- predictors %>% dplyr::select(all_of(selected_cols))
        }
        
        return(predictors)
      }
      
      #' Format Response Variable for Random Forest Model
      #'
      #' This function formats the response variable for a random forest model by selecting and mutating the response column.
      #' It converts the response variable to a binary format.
      #'
      #' @param data A data frame containing the response data and genome IDs.
      #' @param genome_column The name of the column containing the genome IDs.
      #' @param response_column The name of the column containing the response variable.
      #' @param positive_value The value of the response variable to be considered positive (1).
      #' @return A data frame with formatted response variables.
      #' @export
      #' @importFrom dplyr select mutate if_else
      #' @importFrom rlang sym
      format_response <- function(data, genome_column, response_column, positive_value) {
        data = data %>%
          dplyr::select(Genome = !!rlang::sym(genome_column), Response = !!rlang::sym(response_column)) %>%
          dplyr::mutate(Response = dplyr::if_else(Response == positive_value, 1, 0, missing = 0))
        
        data$Genome <- as.character(data$Genome)
        
        return(data)
      }
      
      #' Build Random Forest Model
      #'
      #' This function builds a random forest model using formatted predictors and response variables.
      #' The model can be configured with a specified number of trees and maximum nodes.
      #'
      #' @param predictors A data frame of formatted predictors.
      #' @param response A data frame of formatted response variables.
      #' @param seed An optional seed value for reproducibility. Default is 123.
      #' @param ntree The number of trees to grow in the random forest. Default is 500.
      #' @param maxnodes The maximum number of terminal nodes trees in the forest can have. Default is NULL.
      #' @return A random forest model object.
      #' @export
      #' @importFrom randomForest randomForest
      #' @importFrom dplyr inner_join select
      build_rf = function(predictors, response, seed=123, ntree = 500, maxnodes = NULL){
        # Format data
        data <- predictors %>% dplyr::inner_join(response, by = "Genome")
        data <- data %>% dplyr::select(-Genome)
        data$Response <- as.factor(data$Response)
        
        # Get training and test data
        set.seed(seed)
        ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
        train <- data[ind == 1, ]
        test <- data[ind == 2, ]
        
        # Fit model
        rf <- randomForest::randomForest(Response ~ ., data = train, ntree = ntree, maxnodes = maxnodes, proximity = TRUE) 
        
        return(rf)
      }
      
      #' Evaluate Random Forest Model
      #'
      #' This function evaluates a random forest model by predicting on the test set and calculating a confusion matrix.
      #'
      #' @param rf A random forest model object to evaluate.
      #' @param predictors A data frame of formatted predictors.
      #' @param response A data frame of formatted response variables.
      #' @param seed An optional seed value for reproducibility. Default is 123.
      #' @return A confusion matrix object summarizing the model's performance.
      #' @export
      #' @importFrom caret confusionMatrix
      #' @importFrom dplyr inner_join select
      evaluate_rf = function(rf, predictors, response, seed=123){
        # Format data
        data <- predictors %>% dplyr::inner_join(response, by = "Genome")
        data <- data %>% dplyr::select(-Genome)
        data$Response <- as.factor(data$Response)
        
        # Get training and test data
        set.seed(seed)
        ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
        train <- data[ind == 1, ]
        test <- data[ind == 2, ]
        
        #Evaluate predictions
        p <- predict(rf, test)
        confusion_matrix = caret::confusionMatrix(p, test$Response)
        
        return(confusion_matrix)
      }
      
      #' Save Random Forest Model
      #'
      #' This function saves a random forest model to an RDS file with optional compression and environment cleaning to reduce file size.
      #'
      #' @param rf A random forest model object to save.
      #' @param data_fp The file path where the model should be saved.
      #' @param remove_proximity Logical. If TRUE, removes proximity data from the model to reduce file size. Default is TRUE.
      #' @param clean_environment Logical. If TRUE, removes non-essential objects from the environment to reduce file size. Default is TRUE.
      #' @param compress The compression method to use when saving the RDS file. Default is "xz".
      #' @return Saves the random forest model to the specified file path.
      #' @export
      save_rf = function(rf, data_fp, remove_proximity=TRUE, clean_environment = TRUE, compress="xz")
      {
        #Remove data for proximity (reduces file size)
        if(remove_proximity==TRUE)
        {
          rf$proximity <- NULL 
        }
        
        # Remove non-essential objects from the environment (reduces file size)
        if(clean_environment==TRUE)
        {
          rm(list = setdiff(ls(envir = attr(rf$terms, ".Environment")), c("train", "test", "rf", "var")), envir = attr(rf$terms, ".Environment"))
        }
        
        saveRDS(object=rf, file = data_fp, compress = compress)
      }
      
      
    # Generate models
      #Load data
      gene_functions_fp <- "data/gene_functions_database.rds"
      gene_functions_database <- readRDS(file = gene_functions_fp)
      gene_functions = gene_functions_database %>% dplyr::filter(grepl("^K", Database_ID)) # Keep only KO IDs
      
      data_fp <- "data/database.csv"
      data <- read.csv(data_fp)
      
      # Perform model fitting
      positive_values <- c("Fermentation", "Methanogenesis")
      for (positive_value in positive_values) {
        # Format predictors
        predictors <- format_predictors(gene_functions = gene_functions, seed = 123, proportion_to_keep = 0.1)
        
        # Format response
        response <- format_response(data = data, genome_column = "IMG_Genome_ID_max_genes", response_column = "Type_of_metabolism", positive_value = positive_value)
        
        # Build random forest model
        rf <- build_rf(predictors = predictors, response = response, seed = 123, ntree = 50, maxnodes = 30)
        
        # Evaluate random forest model
        confusion_matrix <- evaluate_rf(predictors = predictors, response = response, rf = rf)
        
        # Print confusion matrix
        print(paste("Confusion Matrix for", positive_value))
        print(confusion_matrix)
        
        # Save random forest model
        save_fp <- paste0("data/random_forest_", tolower(positive_value), ".rds")
        save_rf(rf = rf, data_fp = save_fp)
      }