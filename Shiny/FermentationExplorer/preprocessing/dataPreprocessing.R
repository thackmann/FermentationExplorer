# Preprocess Data for Shiny App
# This script generates data files for the app
# It is not called during app execution
# Requirements
# - Packages in install//installPackages.R
# Author: Timothy Hackmann
# Date: 26 February 2025

# === Set system locale ===
Sys.setlocale("LC_ALL", "C")

# === Get app directory ===
  app_directory <- FileLocator::getCurrentFileLocation()
  app_directory <- dirname(app_directory)
  
# === Load external R files ===
  # Load external R files
  setwd(app_directory)
  source("functions//helperFunctions.R", local = TRUE) 
  source("functions//loadDataFunctions.R", local = TRUE) 
  source("functions//plotFunctions.R", local = TRUE) 
  source("modules//predictionsMachineLearning//functions.R", local = TRUE) 
  source("preprocessing//functions.R", local = TRUE) 
  
# === Preprocess data ===
  # --- Clean database file and add links ---
    # Get data
      data <- load_raw_database()

    # Clean data
      data[] <- lapply(data, as.character)

      data_vars <- c("BacDive_Antibiotic_resistance", "BacDive_Antibiotic_sensitivity", "BacDive_Cell_length",
                     "BacDive_Cell_shape", "BacDive_Cell_width", "BacDive_Colony_size",
                     "BacDive_Fermentation_substrates", 
                     "BacDive_Flagellum_arrangement", "BacDive_Gram_stain", "BacDive_Incubation_period",
                     "BacDive_Indole_test", "BacDive_Motility", "BacDive_Oxygen_tolerance",
                     "BacDive_pH_for_growth", "BacDive_Pathogenicity_animal", "BacDive_Pathogenicity_human",
                     "BacDive_Pathogenicity_plant", "BacDive_Salt_concentration", "BacDive_Salt_concentration_unit",
                     "BacDive_Spore_formation", "BacDive_Temperature_for_growth", "BacDive_Voges_proskauer",
                     "BacDive_Isolation_category_1", "BacDive_Isolation_category_2", "BacDive_Isolation_category_3",
                     "FAPROTAX_traits")

      is_numeric_vars <- c(FALSE, FALSE, TRUE,
                           FALSE, TRUE, TRUE,
                           FALSE,
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
                              positive_value ="positive",
                              negative_value="negative")

      # Type of metabolism
      data <- data %>% 
        dplyr::mutate(BacDive_Type_of_metabolism = dplyr::if_else(!is.na(BacDive_Fermentation_substrates), "Fermentation", NA))
      
      data <- data %>%
        dplyr::mutate(Type_of_metabolism = dplyr::coalesce(
          Literature_Type_of_metabolism,
          VPI_Type_of_metabolism,
          BacDive_Type_of_metabolism,
          Bergey_Type_of_metabolism
        ))

      # End products
      data <- data %>%
        dplyr::mutate(
          Major_end_products = dplyr::coalesce(Literature_Major_end_products, VPI_Major_end_products, Bergey_Major_end_products),
          Minor_end_products = dplyr::if_else(
            !is.na(Literature_Major_end_products),  # If Literature_Major_end_products exists
            Literature_Minor_end_products,         # Pick Literature_Minor_end_products only
            dplyr::coalesce(VPI_Minor_end_products, Bergey_Minor_end_products)  # Otherwise, use coalesce for others
          )
        )

      data$Major_end_products[data$Major_end_products == "NA"] <- NA
      data$Minor_end_products[data$Minor_end_products == "NA"] <- NA
      data$End_products <- ifelse(is.na(data$Major_end_products), data$Minor_end_products, ifelse(is.na(data$Minor_end_products), data$Major_end_products, paste(data$Major_end_products, data$Minor_end_products, sep = ";")))

      # Substrates for end products
      data <- data %>%
        dplyr::mutate(Substrates_for_end_products = dplyr::coalesce(
          Literature_Substrates_for_end_products,
          Bergey_Substrates_for_end_products
        ))

      # Substrates for fermentation
      data <- data %>% 
        dplyr::mutate(Literature_Fermentation_substrates = dplyr::if_else(Literature_Type_of_metabolism == "Fermentation", 
                      Literature_Substrates_for_end_products, NA))
      data <- data %>% 
        dplyr::mutate(Bergey_Fermentation_substrates = dplyr::if_else(Bergey_Type_of_metabolism == "Fermentation", 
                      Bergey_Substrates_for_end_products, NA))
      
      data <- data %>%
        dplyr::mutate(Fermentation_substrates = dplyr::coalesce(
          Literature_Fermentation_substrates,
          BacDive_Fermentation_substrates,
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
        "Type_of_metabolism", "Major_end_products", "Minor_end_products", "End_products", "Substrates_for_end_products", "Fermentation_substrates",
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
                            Incubation_period_in_days = "Incubation_period",
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
        list(id = "Type of metabolism", label = "Type of metabolism", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Type of metabolism`, delimited = TRUE, delimiter = ";")),
        list(id = "End products", label = "End products", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`End products`, delimited = TRUE, delimiter = ";")),
        list(id = "Major end products", label = "Major end products", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Major end products`, delimited = TRUE, delimiter = ";")),
        list(id = "Minor end products", label = "Minor end products", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Minor end products`, delimited = TRUE, delimiter = ";")),
        list(id = "Substrates for end products", label = "Substrates for end products", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Substrates for end products`, delimited = TRUE, delimiter = ";")),
        list(id = "Fermentation substrates", label = "Fermentation substrates", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Fermentation substrates`, delimited = TRUE, delimiter = ";")),
        list(id = "Oxygen tolerance", label = "Oxygen tolerance", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Oxygen tolerance`, delimited = TRUE, delimiter = ";")),
        list(id = "Pathogenicity", label = "Pathogenicity", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Pathogenicity`, delimited = TRUE, delimiter = ";")),
        list(id = "Temperature for growth in degrees", label = "Temperature for growth in degrees", type = "double", plugin = "slider", plugin_config = create_slider_plugin_config(data$`Temperature for growth in degrees`)),
        list(id = "Salt for growth in moles per liter", label = "Salt for growth in moles per liter", type = "double", plugin = "slider", plugin_config = create_slider_plugin_config(data$`Salt for growth in moles per liter`)),
        list(id = "pH for growth", label = "pH for growth", type = "double", plugin = "slider", plugin_config = create_slider_plugin_config(data$`pH for growth`)),
        list(id = "Incubation period in days", label = "Incubation period in days", type = "double", plugin = "slider", plugin_config = create_slider_plugin_config(data$`Incubation period in days`)),
        list(id = "Indole test", label = "Indole test", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Indole test`, delimited = TRUE, delimiter = ";")),
        list(id = "Voges Proskauer", label = "Voges Proskauer", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Voges Proskauer`, delimited = TRUE, delimiter = ";")),
        list(id = "Motility", label = "Motility", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Motility`, delimited = TRUE, delimiter = ";")),
        list(id = "Antibiotic resistance", label = "Antibiotic resistance", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Antibiotic resistance`, delimited = TRUE, delimiter = ";")),
        list(id = "Antibiotic sensitivity", label = "Antibiotic sensitivity", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Antibiotic sensitivity`, delimited = TRUE, delimiter = ";")),
        list(id = "FAPROTAX traits", label = "FAPROTAX traits", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`FAPROTAX traits`, delimited = TRUE, delimiter = ";")),

        # Morphology
        list(id = "Cell length in microns", label = "Cell length in microns", type = "double", plugin = "slider", plugin_config = create_slider_plugin_config(data$`Cell length in microns`)),
        list(id = "Cell width in microns", label = "Cell width in microns", type = "double", plugin = "slider", plugin_config = create_slider_plugin_config(data$`Cell width in microns`)),
        list(id = "Cell shape", label = "Cell shape", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Cell shape`, delimited = TRUE, delimiter = ";")),
        list(id = "Colony size", label = "Colony size", type = "double", plugin = "slider", plugin_config = create_slider_plugin_config(data$`Colony size`)),
        list(id = "Flagellum arrangement", label = "Flagellum arrangement", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Flagellum arrangement`, delimited = TRUE, delimiter = ";")),
        list(id = "Gram stain", label = "Gram stain", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Gram stain`, delimited = TRUE, delimiter = ";")),
        list(id = "Spore formation", label = "Spore formation", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Spore formation`, delimited = TRUE, delimiter = ";")),

        # Isolation traits
        list(id = "Isolation category 1", label = "Isolation category 1", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Isolation category 1`, delimited = TRUE, delimiter = ";")),
        list(id = "Isolation category 2", label = "Isolation category 2", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Isolation category 2`, delimited = TRUE, delimiter = ";")),
        list(id = "Isolation category 3", label = "Isolation category 3", type = "string", input = "select", multiple = TRUE, plugin = "selectize", plugin_config = create_selectize_plugin_config(data$`Isolation category 3`, delimited = TRUE, delimiter = ";"))
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
        # Define variables and query strings
        variables <- list(
          
          # Type of metabolism
          "fermentation" = "grepl(\"Fermentation\", `Type of metabolism`)",
          "methanogenesis" = "grepl(\"Methanogenesis\", `Type of metabolism`)",
          
          # Substrates for end products
          "glucose" = "grepl(\"glucose\", `Substrates for end products`)",
          
          # End products
          "acetate" = "grepl(\"acetate\", `End products`)",
          "butyrate" = "grepl(\"butyrate\", `End products`)",
          "CH4" = "grepl(\"CH4\", `End products`)",
          "ethanol" = "grepl(\"ethanol\", `End products`)",
          "formate" = "grepl(\"formate\", `End products`)",
          "H2" = "grepl(\"H2\", `End products`)",
          "isobutyrate" = "grepl(\"isobutyrate\", `End products`)",
          "isovalerate" = "grepl(\"isovalerate\", `End products`)",
          "lactate" = "grepl(\"lactate\", `End products`) | grepl(\"D-lactate\", `End products`) | grepl(\"L-lactate\", `End products`)",
          "propionate" = "grepl(\"propionate\", `End products`)",
          "pyruvate" = "grepl(\"pyruvate\", `End products`)",
          "succinate" = "grepl(\"succinate\", `End products`)",
          
          # Physiology/morphology
          "anaerobe" = "grepl(\"anaerobe\", `Oxygen tolerance`) | grepl(\"obligate anaerobe\", `Oxygen tolerance`)",
          "gram_positive" = "grepl(\"positive\", `Gram stain`)",
          "spore_formation" = "grepl(\"positive\", `Spore formation`)",
          "motility_non_gliding" = "grepl(\"positive\", `Motility`) & !grepl(\"gliding\", `Flagellum arrangement`)",
          
          # Growth
          "thermophile" = "`Temperature for growth in degrees` > 45",
          "halophile" = "`Salt for growth in moles per liter` > 3",
          "slow_growth" = "`Incubation period in days` > 7",
          
          # Pathogenecity
          "animal_pathogen" = "grepl(\"animal\", `Pathogenicity`)",
          "plant_pathogen" = "grepl(\"plant\", `Pathogenicity`)"

        )
        
      # Process each variable
      purrr::walk2(
        .x = names(variables),
        .y = variables,
        .f = ~ generate_rf(
          var_name = .x, 
          query_string = .y,
          predictors_to_keep = 1
        )
      )
      
      