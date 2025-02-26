# Define the Predictions with Metabolic Networks Module in Shiny App
# This script defines the user interface (UI) and server for the predictions with metabolic networks module.
# It also includes functions and variables specific to this module.  
# Author: Timothy Hackmann
# Date: 14 October 2024

# === Define functions ===
    # --- Functions for metabolic networks ---
    #' Add One Enzymatic Reaction
    #' 
    #' This function adds one enzymatic reaction to a metabolic network model
    #' 
    #' @param df A data frame of the metabolic network model
    #' @param abbreviation A character vector of abbreviations for each reaction
    #' @param lowbnd A numeric vector of lower bounds for each reaction
    #' @param uppbnd A numeric vector of upper bounds for each reaction
    #' @param obj_coef A numeric vector of objective coefficients for each reaction
    #' @param equation A character vector of enzymatic reactions
    #' @param officialName A character vector of the full name of the reaction
    #' @param geneAssociation A character vector of genes that control the reaction
    #' @param subsystem A character vector of subsystems for each reaction
    #' @return A data frame of the metabolic network model with the reaction added
    #' @export
    add_one_reaction = function(df = NULL, abbreviation = NA, lowbnd = -1000, uppbnd = 1000, obj_coef = 0, equation, officialName = NA, geneAssociation = NA, subsystem = NA) {
      #Add a reaction
      append = data.frame(abbreviation, lowbnd, uppbnd, obj_coef, equation, officialName, geneAssociation, subsystem)
      
      #Format equations
      append$equation = format_metabolite_name(name = append$equation, remove_coefficient = FALSE)
      
      if (is.null(df) | is.function(df) | length(df) == 0) {
        df = append
      } else {
        df = rbind(df, append)
      }
      
      return(df)
    }
    
    #' Delete One Enzymatic Reaction
    #' 
    #' This function deletes one enzymatic reaction to a metabolic network model
    #' 
    #' @param df A data frame of the metabolic network model
    #' @param officialName A character vector of the full name of the reaction
    #' @return A data frame of the metabolic network model with the reaction deleted
    #' @export
    delete_one_reaction = function(df, officialName) {
      #Delete a reaction
      df = df[-which(df$officialName == officialName),]
      
      return(df)
    }
    
    #' Format Names of Metabolites and Equations
    #' 
    #' This function formats the names of metabolites and equations in a metabolic network model
    #' It is designed to work with reactions from KEGG database
    #' It removes extra spaces, coefficients, and charges
    #' 
    #' @param name A character vector of metabolite or reaction names
    #' @param remove_coefficient A logical indicating whether to remove coefficients from the names
    #' @param add_underscore A logical indicating whether to add underscores to the names
    #' @param remove_charge A logical indicating whether to remove charges from the names
    #' @return A character vector of formatted names
    #' @export
    format_metabolite_name = function(name, remove_coefficient = TRUE, add_underscore = TRUE, remove_charge = TRUE) {
      name = gsub(pattern = "alpha-D", replacement = "D", x = name)
      name = gsub(pattern = "beta-D", replacement = "D", x = name)
      name = gsub(pattern = "^ ", replacement = "", x = name)
      name = gsub(pattern = " $", replacement = "", x = name)
      name = gsub(pattern = "^n ", replacement = "", x = name)
      
      if (add_underscore == TRUE) {
        name = gsub(pattern = "-", replacement = "_", x = name)
        name = gsub(pattern = "([aA-zZ])( )([aA-zZ0-9])", replacement = "\\1_\\3", x = name)
        name = gsub(pattern = "(\\))( )", replacement = "\\1_", x = name)
        name = gsub(pattern = "(,)", replacement = "_", x = name)
      }
      
      if (remove_charge == TRUE) {
        name = gsub(pattern = "([aA-zZ0-9])(\\+)", replacement = "\\1", x = name)
      }
      
      if (remove_coefficient == TRUE) {
        name = gsub(pattern = "^[1-9] ", replacement = "", x = name)
      }
      
      return(name)
    }
    
    #' Remove Redundant Enzymatic Reactions
    #' 
    #' This function removes redundant enzymatic reactions from a metabolic network model
    #' It keeps only one reaction for each unique equation
    #' 
    #' @param df A data frame of the metabolic network model
    #' @return A data frame of the metabolic network model with redundant reactions removed
    #' @export
    #' @importFrom dplyr distinct
    remove_redundant_reactions = function(df) {
      df = dplyr::distinct(df, equation, .keep_all = TRUE)
      
      return(df)
    }
    
    #' Add Reactions of Glycolysis
    #' 
    #' This function adds the 10 reactions that make up glycolysis to a metabolic network model
    #' 
    #' @param df A data frame of the metabolic network model
    #' @return A data frame of the metabolic network model with glycolysis reactions added
    #' @export
    add_glycolysis = function(df = NULL) {
      df = add_one_reaction(df, abbreviation = "Extra_reaction_database", lowbnd = 0, uppbnd = 1000, obj_coef = 0, equation = "ATP + D-Glucose <=> ADP + D-Glucose 6-phosphate", officialName = NA, geneAssociation = NA, subsystem = NA)
      df = add_one_reaction(df, abbreviation = "Extra_reaction_file_upload", lowbnd = -1000, uppbnd = 1000, obj_coef = 0, equation = "D-Glucose 6-phosphate <=> D-Fructose 6-phosphate", officialName = NA, geneAssociation = NA, subsystem = NA)
      df = add_one_reaction(df, abbreviation = "Extra_reaction_3", lowbnd = -1000, uppbnd = 1000, obj_coef = 0, equation = "ATP + D-Fructose 6-phosphate <=> ADP + D-Fructose 1,6-bisphosphate", officialName = NA, geneAssociation = NA, subsystem = NA)
      df = add_one_reaction(df, abbreviation = "Extra_reaction_4", lowbnd = 0, uppbnd = 1000, obj_coef = 0, equation = "D-Fructose 1,6-bisphosphate <=> Glycerone phosphate + D-Glyceraldehyde 3-phosphate", officialName = NA, geneAssociation = NA, subsystem = NA)
      df = add_one_reaction(df, abbreviation = "Extra_reaction_5", lowbnd = -1000, uppbnd = 1000, obj_coef = 0, equation = "D-Glyceraldehyde 3-phosphate <=> Glycerone phosphate", officialName = NA, geneAssociation = NA, subsystem = NA)
      df = add_one_reaction(df, abbreviation = "Extra_reaction_6", lowbnd = -1000, uppbnd = 1000, obj_coef = 0, equation = "D-Glyceraldehyde 3-phosphate + Orthophosphate + NAD+ <=> 3-Phospho-D-glyceroyl phosphate + NADH + H+", officialName = NA, geneAssociation = NA, subsystem = NA)
      df = add_one_reaction(df, abbreviation = "Extra_reaction_7", lowbnd = -1000, uppbnd = 1000, obj_coef = 0, equation = "ATP + 3-Phospho-D-glycerate <=> ADP + 3-Phospho-D-glyceroyl phosphate", officialName = NA, geneAssociation = NA, subsystem = NA)
      df = add_one_reaction(df, abbreviation = "Extra_reaction_8", lowbnd = -1000, uppbnd = 1000, obj_coef = 0, equation = "2-Phospho-D-glycerate <=> 3-Phospho-D-glycerate", officialName = NA, geneAssociation = NA, subsystem = NA)
      df = add_one_reaction(df, abbreviation = "Extra_reaction_9", lowbnd = -1000, uppbnd = 1000, obj_coef = 0, equation = "2-Phospho-D-glycerate <=> Phosphoenolpyruvate + H2O", officialName = NA, geneAssociation = NA, subsystem = NA)
      df = add_one_reaction(df, abbreviation = "Extra_reaction_10", lowbnd = -1000, uppbnd = 0, obj_coef = 0, equation = "ATP + Pyruvate <=> ADP + Phosphoenolpyruvate", officialName = NA, geneAssociation = NA, subsystem = NA)
      
      df$equation = format_metabolite_name(name = df$equation, remove_coefficient = FALSE)
      
      return(df)
    }
    
    #' Specify Which Metabolites Are Unbalanced
    #'
    #' This function specifies which metabolites are unbalanced
    #' Unbalanced metabolites which can accumulate (or be consumed) in unlimited quantities
    #' NADH and ATP are examples of metabolites usually assumed to be unbalanced
    #' In the metabolic network, these can accumulate without needing to be regenerated to NAD+ or ADP
    #' This simplifies the network, as reactions for consuming NADH and ATP don't have to be included
    #' 
    #' @param df A data frame of the metabolic network model
    #' @param names A character vector of metabolite names
    #' @param lowbnd A numeric vector of lower bounds for each reaction
    #' @param uppbnd A numeric vector of upper bounds for each reaction
    #' @return A data frame of the metabolic network model with unbalanced metabolites added
    #' @export
    #' @importFrom stringr str_split
    add_unbalanced_metabolites = function(df, names, lowbnd = -10^6, uppbnd = 10^6) {
      #Get names of all metabolites
      metabolites = df
      metabolites = stringr::str_split(string = metabolites$equation, pattern = "<=>", simplify = TRUE)
      metabolites = as.character(metabolites)
      metabolites = stringr::str_split(string = metabolites, pattern = " \\+", simplify = TRUE)
      metabolites = as.character(metabolites)
      metabolites = format_metabolite_name(metabolites)
      metabolites = unique(metabolites)
      
      #Get names of unbalanced metabolites
      index = names %in% metabolites
      names = names[index]
      
      #Format equations for unbalanced metabolites
      abbreviation = paste0("Unbalanced_metabolite_", seq_len(length(names)))
      lowbnd = lowbnd[index]
      uppbnd = uppbnd[index]
      obj_coef = 0
      equation = paste0(names, " <=>")
      officialName = NA
      geneAssociation = NA
      subsystem = NA
      append = data.frame(abbreviation, lowbnd, uppbnd, obj_coef, equation, officialName, geneAssociation, subsystem)
      
      df = rbind(df, append)
      
      return(df)
    }
    
    #' Specify Starting and Ending Metabolites
    #' 
    #' This function specifies the starting and ending metabolites in a metabolic network model
    #' For glucose fermentation, the starting metabolite would be glucose
    #' and the ending metabolite would be lactate or another product.
    #' 
    #' @param df A data frame of the metabolic network model
    #' @param starting_metabolite A character vector of the starting metabolite in the network
    #' @param ending_metabolite A character vector of the ending metabolite in the network
    #' @param lowbnd A numeric vector of lower bounds for each reaction
    #' @param uppbnd A numeric vector of upper bounds for each reaction
    #' @return A data frame of the metabolic network model with the starting and ending metabolites added
    #' @export
    #' @importFrom dplyr filter distinct
    add_target_metabolites = function(df, starting_metabolite, ending_metabolite, lowbnd = -1000, uppbnd = 10^6) {
      abbreviation = c("Starting_metabolite", "Ending_metabolite")
      lowbnd = c(lowbnd, 0)
      uppbnd = c(0, uppbnd)
      obj_coef = c(0, 1)
      equation = c(paste0(starting_metabolite, " <=>"), paste0(ending_metabolite, " <=>"))
      officialName = NA
      geneAssociation = NA
      subsystem = NA
      
      match = match(x = equation, table = df$equation)
      match = match[!is.na(match)]
      if (length(match) > 0) {
        df = df[-match,]
      }
      
      append = data.frame(abbreviation, lowbnd, uppbnd, obj_coef, equation, officialName, geneAssociation, subsystem)
      df = rbind(df, append)
      
      return(df)
    }
    
    #' Find Enzymes in Genome
    #' 
    #' This function finds which enzyme reactions in a metabolic network are in an organism's genome
    #' The user provides a metabolic network model along with database IDs for the organism's genes
    #' 
    #' @param df A data frame of the metabolic network model
    #' @param gene_functions A character vector of database IDs for the organism's genes
    #' @return A character vector of enzyme reactions in the organism's genome
    #' @export
    #' @importFrom stringr str_split
    find_enzymes = function(df, gene_functions) {
      x = stringr::str_split(df$geneAssociation, pattern = ", ")
      for (i in seq_along(x)) {
        for (j in seq_along(x[[i]])) {
          x[[i]][j] = x[[i]][j] %in% gene_functions
        }
      }  
      
      for (i in seq_along(x)) {
        x[[i]] = all(x[[i]] == TRUE)
      }
      
      x = unlist(x)
      
      enzymes = df$officialName[x]
      
      return(enzymes)
    }
    
    #' Create Initial Dataframe for Metabolic Network Model
    #' 
    #' This function creates an initial dataframe for the metabolic network model
    #' The user specifies the abbreviation, lower bound, upper bound, objective coefficient, equation, 
    #' official name, gene association, and subsystem for each reaction
    #' 
    #' @param abbreviation A character vector of abbreviations for each reaction
    #' @param lowbnd A numeric vector of lower bounds for each reaction
    #' @param uppbnd A numeric vector of upper bounds for each reaction
    #' @param obj_coef A numeric vector of objective coefficients for each reaction
    #' @param equation A character vector of enzymatic reactions
    #' @param officialName A character vector of the full name of the reaction
    #' @param geneAssociation A character vector of genes that control the reaction
    #' @param subsystem A character vector of subsystems for each reaction
    #' @return A data frame of the formatted data
    #' @export
    #' @importFrom dplyr distinct
    create_network_dataframe = function(abbreviation = NA, lowbnd = -1000, uppbnd = 1000, obj_coef = 0, equation, officialName = NA, geneAssociation = NA, subsystem = NA) {
      #Create dataframe
      df = data.frame(abbreviation, lowbnd, uppbnd, obj_coef, equation, officialName, geneAssociation, subsystem)
      
      #Format equations  
      df$equation = format_metabolite_name(name = df$equation, remove_coefficient = FALSE)
      
      #Remove extra rows
      df = dplyr::distinct(df)
      df = df[which(df$equation != ""),]
      
      return(df)
    }
    
    #' Build Metabolic Network Model
    #' This function builds a metabolic network model from a set of enzymatic reactions
    #' The user specifies the equation, direction, abbreviation, official name, gene association, and subsystem for each reaction
    #' If no equation is provided, the function will create a model with reactions of glycolysis as an example 
    #' 
    #' @param equation A character vector of enzymatic reactions
    #' @param direction A character vector of the direction of each reaction (Forward, Reverse, or Bidirectional)
    #' @param abbreviation A character vector of abbreviations for each reaction
    #' @param officialName A character vector of the full name of the reaction
    #' @param geneAssociation A character vector of genes that control the reaction
    #' @param subsystem A character vector of subsystems for each reaction
    #' @param starting_metabolite A character vector of the starting metabolite in the network
    #' @param ending_metabolite A character vector of the ending metabolite in the network
    #' @param unbalanced_intermediates A character vector of metabolites that are unbalanced and can accumulate
    #' @param unbalanced_products A character vector of metabolites that are unbalanced and can accumulate
    #' @param remove_redundant_reactions A logical indicating whether to remove redundant reactions
    #' @param add_glycolysis A logical indicating whether to add the 10 reactions of glycolysis
    #' @return A data frame of the metabolic network model
    #' @examples
    #' build_network_model()
    #' @export
    #' @importFrom dplyr if_else
    build_network_model = function(equation = NULL, direction = "Bidirectional", abbreviation = NA, officialName = NA, geneAssociation = NA, subsystem = NA, starting_metabolite = "D-Glucose", ending_metabolite = "Pyruvate", unbalanced_intermediates = c("NAD+", "NADH", "ATP", "ADP", "Orthophosphate", "H2O", "H+", "CO2"), unbalanced_products = NULL, remove_redundant_reactions = TRUE, add_glycolysis = FALSE) {
      if (is.null(equation)) {
        equation = add_glycolysis()$equation
      }
      
      # Create Initial Dataframe For Network Model
      uppbnd = vector(length = length(equation))
      lowbnd = vector(length = length(equation))
      uppbnd = dplyr::if_else(direction == "Reverse", 0, 1000)
      lowbnd = dplyr::if_else(direction == "Forward", 0, -1000)
      df = create_network_dataframe(abbreviation, lowbnd = lowbnd, uppbnd = uppbnd, obj_coef = 0, equation = equation, officialName = officialName, geneAssociation = geneAssociation, subsystem = subsystem)
      df$abbreviation = paste0("Enzyme_", seq_len(nrow(df)))
      
      # Add Enzymes of Glycolysis
      if (add_glycolysis == TRUE) {
        df = add_glycolysis(df = df)
      }
      
      # Remove Redundant Reactions
      if (remove_redundant_reactions == TRUE) {
        df = remove_redundant_reactions(df)
      }
      
      # Add Target Metabolites
      if (!is.null(starting_metabolite) & !is.null(ending_metabolite)) {
        lowbnd = -1000
        uppbnd = 10^6
        starting_metabolite = format_metabolite_name(starting_metabolite)
        ending_metabolite = format_metabolite_name(ending_metabolite)
        df = add_target_metabolites(df = df, starting_metabolite = starting_metabolite, ending_metabolite = ending_metabolite, lowbnd = lowbnd, uppbnd = uppbnd)
      }
      
      # Add Unbalanced Metabolites
      if (!is.null(unbalanced_intermediates) & !is.null(unbalanced_products)) {
        unbalanced_intermediates = format_metabolite_name(unbalanced_intermediates)
        unbalanced_products = format_metabolite_name(unbalanced_products)
        unbalanced_intermediates = unbalanced_intermediates[!unbalanced_intermediates %in% ending_metabolite]
        unbalanced_products = unbalanced_products[!unbalanced_products %in% ending_metabolite]
        names = c(unbalanced_intermediates, unbalanced_products)
        lowbnd = c(rep(-10^6, times = length(unbalanced_intermediates)), rep(0, times = length(unbalanced_products)))
        uppbnd = c(rep(10^6, times = length(unbalanced_intermediates)), rep(10^6, times = length(unbalanced_products)))
        df = add_unbalanced_metabolites(df = df, names = names, lowbnd = lowbnd, uppbnd = uppbnd)
      }
      
      return(df)
    }
    
    #' Simplify Network Model
    #' 
    #' This function removes extra reactions from a network model
    #' The user specifies which reactions to keep (according to officialName)
    #' The rest are removed (or have flux is constrained to 0)
    #' The reactions that are removed are usually ones that are absent from a specific organism
    #' 
    #' @param df A data frame of the metabolic network model
    #' @param officialName A character vector of the full name of the reaction
    #' @param constrain_flux A logical indicating whether to constrain flux to 0 for removed reactions
    #' @param remove_reactions A logical indicating whether to remove reactions
    #' @return A data frame of the simplified metabolic network model
    #' @export
    #' @importFrom dplyr filter
    simplify_network_model = function(df, officialName = NULL, constrain_flux = TRUE, remove_reactions = FALSE) {
      to_remove = dplyr::filter(df, !grepl(pattern = "Unbalanced_metabolite", abbreviation))
      to_remove = which(to_remove$officialName %nin% officialName)
      
      if (constrain_flux == TRUE) {
        df$lowbnd[to_remove] = 0
        df$uppbnd[to_remove] = 0
      }
      
      if (remove_reactions == TRUE) {
        df = df[-to_remove,]
      }
      
      return(df)
    }
    
    #' Solve Metabolic Network Models for An Organism
    #'
    #' This function builds and solves metabolic network model specific for an organism.
    #' The user provide the network model (usually a reference model containing reactions occurring across all organisms),
    #' the database IDs of the organism's genes, a set of substrates, and a set of products.
    #' The function returns a set of solved network models encompassing all combinations substrates and products.
    #'
    #' @param df  A data frame of the metabolic network model (usually a reference model)
    #' @param gene_functions A vector of database IDs corresponding to the organism's genes.
    #' @param products A vector of product metabolites.
    #' @param substrates A vector of substrate metabolites.
    #' @return A nested list of solved network models for each combination of substrate and product.
    #' @export
    #' @importFrom fbar find_fluxes_df
    solve_network_model <- function(df, gene_functions, substrates, products) {
      # Find Enzymes
      officialName <- find_enzymes(df = df, gene_functions = gene_functions)
      
      # Simplify Model
      df <- simplify_network_model(df = df, officialName = officialName)
      
      # Initialize results list
      s <- vector("list", length(substrates))
      
      for (j in seq_along(substrates)) {
        substrate <- substrates[j]
        starting_metabolite <- rep(substrate, times = length(products))
        ending_metabolite <- products
        
        for (k in seq_along(ending_metabolite)) {
          # Add Target Metabolites
          s[[j]][[k]] <- add_target_metabolites(df = df, starting_metabolite = format_metabolite_name(starting_metabolite[k]), ending_metabolite = format_metabolite_name(ending_metabolite[k]), lowbnd = -1000, uppbnd = 10^6)
          
          # Find fluxes
          s[[j]][[k]] <- fbar::find_fluxes_df(s[[j]][[k]])
        }
      }
      
      return(s)
    }
    
    #' Construct Graph of Metabolic Network
    #' 
    #' This function constructs a graph (igraph object) from a metabolic network model
    #' The metabolic network model is usually solved before being passed to this function
    #' 
    #' @param s A data frame of a solved metabolic network model
    #' @param add_flux A logical indicating whether to add fluxes to the graph
    #' @param to_remove A character vector of metabolites not to be displayed in the graph
    #' @return An igraph object of the metabolic network
    #' @export
    #' @importFrom igraph graph_from_data_frame
    #' @importFrom tidyr separate separate_rows
    #' @importFrom dplyr distinct select
    make_network_graph <- function(s, add_flux=TRUE, to_remove = c("NAD+", "NADH", "ATP", "ADP", "Orthophosphate", "H2O", "H+", "CO2")) {
      #Get data
      df <- s
      
      #Get names of reactants and products
      df <- tidyr::separate(df, col = equation, into = c("reactant", "product"), sep = "<=>")
      df <- df[which(df$reactant != ""), ] # Remove reactions with no reactants
      df <- df[which(df$product != ""), ] # Remove reactions with no products
      df <- tidyr::separate_rows(df, reactant, sep = " \\+ ")
      df <- tidyr::separate_rows(df, product, sep = " \\+ ")
      
      #Format names of reactants and products
      df$reactant <- format_metabolite_name(name = df$reactant, add_underscore = FALSE)
      df$product <- format_metabolite_name(name = df$product, add_underscore = FALSE)
      df <- dplyr::distinct(df)
      
      # Remove any names (usually unbalanced metabolites) not to be displayed in graph
      to_remove = format_metabolite_name(to_remove)
      df = df[!(df$reactant %in% to_remove), ]
      df = df[!(df$product %in% to_remove), ]
      
      #Do further formatting
      df = df[!is.na(df$officialName),]
      
      if(add_flux){
        #Add fluxes
        match = match(x = paste0(df$officialName, df$geneAssociation), table = paste0(s$officialName, s$geneAssociation))
        df$flux = s$flux[match]
        df = dplyr::select(df, reactant, product, officialName, subsystem, flux, lowbnd, uppbnd)
      }else{
        df = dplyr::select(df, reactant, product, officialName, subsystem, lowbnd, uppbnd)
      }
      
      #Get graph
      g = igraph::graph_from_data_frame(d = df, directed = FALSE)
      
      return(g)
    }
    
    #' Set Layout for Graph of Metabolic Network
    #' 
    #' This function sets the layout for the graph
    #' 
    #' @param graph An igraph object of the metabolic network
    #' @param type A character vector of the layout type
    #' @param dimensions A numeric vector of the number of dimensions
    #' @return A numeric vector of the layout
    #' @export
    #' @importFrom igraph layout_with_fr layout_with_kk layout_with_lgl layout_with_dh layout_with_gem layout_with_drl layout_with_mds layout_with_graphopt layout_with_sugiyama layout_randomly layout_in_circle layout_on_sphere layout_as_star layout_as_tree
    set_network_layout <- function(graph, type = "FR", dimensions = 2) {
      # Get graph
      g <- graph
      
      # Define a list of layout functions
      layout_functions <- list(
        FR = igraph::layout_with_fr,
        KK = igraph::layout_with_kk,
        LGL = igraph::layout_with_lgl,
        DH = igraph::layout_with_dh,
        GEM = igraph::layout_with_gem,
        DRL = igraph::layout_with_drl,
        MDS = igraph::layout_with_mds,
        Graphopt = igraph::layout_with_graphopt,
        Sugiyama = igraph::layout_with_sugiyama,
        Random = igraph::layout_randomly,
        Circle = igraph::layout_in_circle,
        Sphere = igraph::layout_on_sphere,
        Star = igraph::layout_as_star,
        Tree = igraph::layout_as_tree
      )
      
      # Check if the specified type is available
      if (!type %in% names(layout_functions)) {
        stop("Invalid layout type. Please choose from: ", paste(names(layout_functions), collapse = ", "))
      }
      
      # Get the appropriate layout function
      layout_function <- layout_functions[[type]]
      
      # Set layout with dimensions if applicable
      if (type %in% c("FR", "KK", "DRL", "MDS")) {
        layout <- layout_function(g, dim = dimensions)
      } else {
        layout <- layout_function(g)
      }
      
      return(layout)
    }
    
    #' Get Metabolite Names
    #' 
    #' This function gets names of metabolites from a reaction equation
    #' It is designed to work with reactions from KEGG database
    #' 
    #' @param equation A character vector of enzymatic reactions
    #' @param to_remove A character vector of metabolites to remove
    #' @return A character vector of metabolite names
    #' @export
    #' @importFrom stringr str_split
    get_metabolite_names = function(equation, to_remove = NULL) {
      name = stringr::str_split(string = equation, pattern = "<=>", simplify = TRUE)
      name = as.character(name)
      name = stringr::str_split(string = name, pattern = "[ ]\\+", simplify = TRUE)
      name = as.character(name)
      name = format_metabolite_name(name, add_underscore = FALSE, remove_charge = FALSE)
      
      if (!is.null(to_remove)) {
        name = name[!name %in% format_metabolite_name(to_remove, add_underscore = FALSE, remove_charge = FALSE)]
      }      
      
      name = unique(name)
      name[name != ""]
      name = sort(name)
      
      return(name)
    }
    
  # --- Other functions ---
    #' Format Network Results for Plots
    #' 
    #' This function formats the results of a metabolic network model into a format suitable 
    #' for different types of plots such as summary, heatmap, or treemap.
    #' 
    #' @param df A data frame of results
    #' @param plot_type A character vector of the type of plot
    #' @param min_flux A numeric vector of the minimum flux
    #' @param var_name A character vector of the variable name
    #' @return A data frame of the formatted results
    #' @export
    #' @importFrom dplyr group_by summarize pivot_wider filter select
    #' @importFrom tidyr pivot_wider complete 
    network_results_to_plot <- function(df, plot_type, min_flux = 1, var_name = NULL) {
      # Rename columns
      colnames(df) <- c("x", "var", "y", "z")
      
      if (plot_type == "summary") {
        # Apply min_flux threshold and summarize
        if (!is.null(var_name)) {
          df <- df %>%
            dplyr::mutate(z = dplyr::if_else(z > min_flux, 100, 0)) %>%
            dplyr::group_by(var, y) %>%
            dplyr::summarize(z = mean(z), .groups = 'drop')
        }
        
        # Filter by var
        df = filter_var(df, var_name)
        
        # Pivot wider
        df <- df %>%
          tidyr::pivot_wider(names_from = y, values_from = z, values_fill = list(z = 0))
        
      }else if (plot_type == "heatmap") {
        # Fill in missing combinations of x and y
        df <- df %>%
          dplyr::group_by(y) %>%
          tidyr::complete(x = unique(df$x), fill = list(z = 0)) %>%
          dplyr::ungroup()
        
        # Fill missing var values based on the most common var for each y
        if (!is.null(var_name)) {
          df <- df %>%
            dplyr::group_by(y) %>%
            dplyr::mutate(var = ifelse(is.na(var), var[!is.na(var)][1], var)) %>%
            dplyr::ungroup()
        }
        
        # Filter by var
        if (!is.null(var_name)) {
          df <- df %>%
            dplyr::filter(var == var_name) %>% dplyr::select(-var)
        }
        
        # Pivot wider
        df <- df %>%
          tidyr::pivot_wider(names_from = y, values_from = z)
        
      }else if (plot_type == "treemap") {
        # Filter by min_flux
        df <- df %>%
          dplyr::filter(z > min_flux)
        
        # Select relevant columns
        df <- df %>%
          dplyr::select(-z)
        
        # Filter by var
        if (!is.null(var_name)) {
          df <- df %>%
            dplyr::filter(var == var_name) %>% dplyr::select(-var)
        }
        
        #Summarize by y
        df <- df %>%
          dplyr::group_by(y) %>%
          dplyr::summarise(z = dplyr::n(), .groups = 'drop') %>%
          dplyr::mutate(z = z / sum(z))
        
        #Convert to percentage
        df$z = df$z*100
      }
      
      
      return(df)
    }
    
    #' Format Network Graph
    #' 
    #' Format appearance of the graph of the metabolic network for the Shiny app
    #'
    #' @param graph An igraph object of the metabolic network
    #' @param show_flux A logical indicating whether to show fluxes
    #' @param show_subsystems A logical indicating whether to show subsystems
    #' @param vertex_default_color A character vector of the default color for vertices
    #' @param vertex_highlight_color A character vector of the color for highlighted vertices
    #' @param vertex_missing_reaction_color A character vector of the color for missing reaction vertices
    #' @param vertex_default_frame.color A character vector of the default frame color for vertices
    #' @param vertex_missing_reaction_frame.color A character vector of the frame color for missing reaction vertices
    #' @param vertex_highlight_frame.color A character vector of the frame color for highlighted vertices
    #' @param vertex_default_frame.width A numeric vector of the default frame width for vertices
    #' @param vertex_missing_reaction_frame.width A numeric vector of the frame width for missing reaction vertices
    #' @param vertex_highlight_frame.width A numeric vector of the frame width for highlighted vertices
    #' @param vertex_default_opacity A numeric vector of the default opacity for vertices
    #' @param vertex_missing_reaction_opacity A numeric vector of the opacity for missing reaction vertices
    #' @param vertex_default_size A numeric vector of the default size for vertices
    #' @param vertex_highlight_size A numeric vector of the size for highlighted vertices
    #' @param vertex_highlight_frame.size A numeric vector of the frame size for highlighted vertices
    #' @param vertex_color_lighten A numeric vector of the amount to lighten the vertex color
    #' @param vertex_label A character vector of the vertex label
    #' @param edge_default_color A character vector of the default color for edges
    #' @param edge_missing_reaction_color A character vector of the color for missing reaction edges
    #' @param edge_zero_flux_color A character vector of the color for edges with zero flux
    #' @param edge_positive_flux_color A character vector of the color for edges with positive flux
    #' @param edge_default_opacity A numeric vector of the default opacity for edges
    #' @param edge_missing_reaction_opacity A numeric vector of the opacity for missing reaction edges
    #' @param edge_zero_flux_opacity A numeric vector of the opacity for edges with zero flux
    #' @param edge_positive_flux_opacity A numeric vector of the opacity for edges with positive flux
    #' @param edge_default_width A numeric vector of the default width for edges
    #' @param edge_missing_reaction_width A numeric vector of the width for missing reaction edges
    #' @param edge_low_flux_width A numeric vector of the width for edges with low flux
    #' @param edge_medium_flux_width A numeric vector of the width for edges with medium flux
    #' @param edge_high_flux_width A numeric vector of the width for edges with high flux
    #' @param vertices_to_highlight A character vector of vertices to highlight
    #' @return An igraph object of the metabolic network with formatted appearance
    #' @export
    #' @importFrom colorspace lighten
    #' @importFrom dplyr bind_rows filter full_join group_by pull rename select summarize ungroup
    #' @importFrom igraph V as_data_frame
    format_network_graph <- function(graph, show_flux = FALSE, show_subsystems = FALSE,
                                     vertex_default_color = "#7f7f7f",
                                     vertex_highlight_color = "#ff0000",
                                     vertex_missing_reaction_color = "#ffffff",
                                     vertex_default_frame.color = "#7f7f7f",
                                     vertex_missing_reaction_frame.color = "#ffffff",
                                     vertex_highlight_frame.color = "#ff0000",
                                     vertex_default_frame.width = 2,
                                     vertex_missing_reaction_frame.width = 1,
                                     vertex_highlight_frame.width = 3,
                                     vertex_default_opacity = 1,
                                     vertex_missing_reaction_opacity = 0.1,
                                     vertex_default_size = 10,
                                     vertex_highlight_size = 15,
                                     vertex_highlight_frame.size = 5,
                                     vertex_color_lighten = 0.2,
                                     vertex_label = NA,
                                     edge_default_color = "#7f7f7f",
                                     edge_missing_reaction_color = "#7f7f7f",
                                     edge_zero_flux_color = "#7f7f7f",
                                     edge_positive_flux_color = "#00B050",
                                     edge_default_opacity = 1,
                                     edge_missing_reaction_opacity = 0.1,
                                     edge_zero_flux_opacity = 1,
                                     edge_positive_flux_opacity = 1,
                                     edge_default_width = 0.5,
                                     edge_missing_reaction_width = 0.5,
                                     edge_low_flux_width = 0.5,
                                     edge_medium_flux_width = 1,
                                     edge_high_flux_width = 2,
                                     vertices_to_highlight = NULL) {
      
      # Convert graph to data frame
      df <- igraph::as_data_frame(graph, what = "edges")
      
      # Initialize vertices data frame
      vertices <- igraph::V(graph)$name
      vertices <- data.frame(name = vertices, color = vertex_default_color, size = vertex_default_size)
      
      # Initialize fluxes data frame
      fluxes <- df %>% dplyr::select(lowbnd, uppbnd, flux)
      
      # Set default vertex attributes
      vertices$color <- vertex_default_color
      vertices$size <- vertex_default_size
      vertices$frame.color <- vertex_default_frame.color
      vertices$frame.width <- vertex_default_frame.width
      vertices$opacity <- vertex_default_opacity
      vertices$label <- vertex_label
      
      # Modify attributes based on flux
      if (show_flux) {
        active_vertices <- df %>%
          dplyr::select(from, uppbnd, lowbnd) %>%
          dplyr::rename(name = from) %>%
          dplyr::bind_rows(
            df %>%
              dplyr::select(to, uppbnd, lowbnd) %>%
              dplyr::rename(name = to)
          ) %>%
          dplyr::filter(!(uppbnd == 0 & lowbnd == 0)) %>%
          dplyr::pull(name) %>%
          unique()
        
        vertices$color <- ifelse(vertices$name %in% active_vertices, vertices$color, vertex_missing_reaction_color)
        vertices$frame.color <- ifelse(vertices$name %in% active_vertices, vertices$frame.color, vertex_missing_reaction_frame.color)
        vertices$opacity <- ifelse(vertices$name %in% active_vertices, vertices$opacity, vertex_missing_reaction_opacity)
      }else{
        active_vertices = NULL
      }
      
      # Modify attributes based on subsystems
      if (show_subsystems) {
        most_common_subsystem <- df %>%
          dplyr::select(from, subsystem) %>%
          dplyr::rename(name = from) %>%
          dplyr::bind_rows(
            df %>%
              dplyr::select(to, subsystem) %>%
              dplyr::rename(name = to)
          ) %>%
          dplyr::group_by(name, subsystem) %>%
          dplyr::summarise(count = dplyr::n(), .groups = 'drop') %>%
          dplyr::group_by(name) %>%
          dplyr::slice_max(order_by = count, n = 1, with_ties = FALSE) %>%
          dplyr::ungroup() %>%
          dplyr::select(name, subsystem)
        
        vertices <- vertices %>%
          dplyr::full_join(most_common_subsystem, by = "name")
        
        n <- length(unique(vertices$subsystem))
        color_palette <- colorspace::qualitative_hcl(n, h = c(15, 375 * (n - 1) / n), c = 100, l = 65, fixup = TRUE, alpha = 1)
        
        vertices$subsystem <- as.factor(vertices$subsystem)
        levels(vertices$subsystem) <- color_palette
        vertices$subsystem <- as.character(vertices$subsystem)
        vertices$frame.color <- vertices$subsystem
        
        vertices$color <- vertices$subsystem
        vertices$color = colorspace::lighten(vertices$color, amount = vertex_color_lighten)
        
        vertices <- vertices %>% dplyr::select(-subsystem)
      }
      
      # Highlight specific vertices
      if (!is.null(vertices_to_highlight)) {
        vertices$size <- ifelse(vertices$name %in% vertices_to_highlight, vertex_highlight_size, vertices$size)
        vertices$frame.color <- ifelse(vertices$name %in% vertices_to_highlight, vertex_highlight_frame.color, vertices$frame.color)
        vertices$frame.width <- ifelse(vertices$name %in% vertices_to_highlight, vertex_highlight_frame.width, vertices$frame.width)
        vertices$label <- ifelse(vertices$name %in% vertices_to_highlight, vertices$name, vertices$label)
      }
      
      # Initialize edges data frame
      edges <- df %>% dplyr::select(to, from)
      edges$color <- edge_default_color
      edges$width <- edge_default_width
      edges$opacity <- edge_default_opacity
      
      # Modify edge attributes based on flux
      if (show_flux) {
        edges$color <- ifelse(fluxes$uppbnd == 0 & fluxes$lowbnd == 0, edge_missing_reaction_color, 
                              ifelse(abs(fluxes$flux) > 1, edge_positive_flux_color, edge_zero_flux_color))
        
        edges$opacity <- ifelse(fluxes$uppbnd == 0 & fluxes$lowbnd == 0, edge_missing_reaction_opacity, 
                                ifelse(abs(fluxes$flux) > 1, edge_positive_flux_opacity, edge_zero_flux_opacity))
        
        edges$width <- abs(fluxes$flux) / 10 + 1
        edges$width <- ifelse(fluxes$uppbnd == 0 & fluxes$lowbnd == 0, edge_missing_reaction_width, edges$width)
        
        edges$width <- ifelse(edges$width <= 2, edge_low_flux_width, 
                              ifelse(edges$width < 20, edge_medium_flux_width, edge_high_flux_width))  
      }
      
      # Add attributes back to the graph
      igraph::V(graph)$color <- vertices$color
      igraph::V(graph)$size <- vertices$size
      igraph::V(graph)$frame.color <- vertices$frame.color
      igraph::V(graph)$frame.width <- vertices$frame.width
      igraph::V(graph)$opacity <- vertices$opacity
      igraph::V(graph)$label <- vertices$label
      igraph::E(graph)$color <- edges$color
      igraph::E(graph)$width <- edges$width
      igraph::E(graph)$opacity <- edges$opacity
      
      return(graph)
    }
    
    # === Set variables ===
    # Choices for variables
    choices_unbalanced_intermediates <- c("NAD+", "NADH", "NADP+", "NADPH", "FAD", "FADH2", 
                                        "Reduced ferredoxin", "Oxidized ferredoxin", "Ubiquinone", 
                                        "Ubiquinol", "Quinone", "Hydroquinone", "Menaquinone", 
                                        "Menaquinol", "Oxidized hydrogenase", "Reduced hydrogenase",  
                                        "ATP", "ADP", "AMP", "GTP", "GDP", "Orthophosphate", 
                                        "Diphosphate", "Polyphosphate", "H2O", "H+", "H+[side 1]", 
                                        "H+[side 2]", "Na+[side 1]", "Na+[side 2]", "Sodium cation", "HCO3-")
    choices_products <- c("Acetate", "(S)-Lactate", "(R)-Lactate", "Ethanol", 
                          "Succinate", "Propanoate", "Butanoic acid", "Formate", 
                          "Hydrogen", "CO2")
    
    # Key for the legend of the network plot (by color and width of lines)
    network_legend_key <- data.frame(
      name = c("No flux", "Low flux", "Medium flux", "High flux"),
      line_color = c("#7f7f7f", "#00B050", "#00B050", "#00B050"),
      line_width = c(0.5, 0.5, 1, 2),
      stringsAsFactors = FALSE
    )
    
# === Define user interface (UI) ===
  predictionsNetworkUI <- function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
      #Call JavaScript functions
      inject_js_resize(ns, "treemap-container"),
      
      #Title
      create_title_div("Predict traits with metabolic networks"),
      
      #Content
      bslib::layout_sidebar(
        #Sidebar
        sidebar = bslib::sidebar(
              width = "30%",
              
              # Select data
              div("Choose gene functions"),
              bslib::navset_tab(id = ns("function_tabs"),
                                bslib::nav_panel(title = "Database",
                                                 create_selectize_input(inputId = ns("gene_functions_database")),
                                ),
                                bslib::nav_panel(title = "File upload",
                                                 fileInput_modal(ns("gene_functions_upload"), modalId = ns("gene_functions_modal"))
                                )
              ),
              div("Choose reference reactions"),
              bslib::navset_tab(id = ns("reaction_tabs"),
                                bslib::nav_panel(title = "Database",
                                                 create_selectize_input(inputId = ns("reference_reactions_database"), multiple = FALSE),
                                 ),
                                bslib::nav_panel(title = "File upload",
                                                 fileInput_modal(ns("reference_reactions_upload"), modalId = ns("reference_reactions_modal"))
                                 )
                ),
              
              # Set parameters
              shiny::conditionalPanel(
                condition = "input.reaction_tabs == 'Database'",
                ns = ns, 
                create_selectize_input(inputId = ns("substrates_database"), label = "Substrates"), 
                create_selectize_input(inputId = ns("products_database"), label = "End products"),
                create_selectize_input(inputId = ns("unbalanced_intermediates_database"), label = "Unbalanced intermediates"),
              ),
              shiny::conditionalPanel(
                condition = "input.reaction_tabs == 'File upload'",
                ns = ns, 
                create_selectize_input(inputId = ns("substrates_upload"), label = "Substrates"), 
                create_selectize_input(inputId = ns("products_upload"), label = "End products"),
                create_selectize_input(inputId = ns("unbalanced_intermediates_upload")),
              ),
              
              # Make predictions
              shiny::actionButton(ns("make_predictions"), "Make predictions", class = "btn btn-primary")
          ),
        
        #Main content area
        div(
           id = ns("results_page"),
           
           # Message for missing selections
           shiny::conditionalPanel(
             condition = "!output.flag_results",
             ns = ns,
             shiny::h4("Please make selections at left")
           ),
           
           # Results panel
           shiny::conditionalPanel(
             condition = "output.flag_results",
             ns = ns,

             # Summary and download button
             bslib::card(
               bslib::card_header(shiny::textOutput(ns("summary_text"))),
               create_download_button(ns('download_data'))
             ),
             
              # Tabs for plots
              bslib::navset_card_underline(
                id = ns("results_tabs"),
                title = "Prediction results",
                full_screen = TRUE,
                  bslib::nav_panel(
                    title = "Summary",
                    create_plot_div(ns = ns, plot_type = "summary"),
                  ),
                  bslib::nav_panel(
                    title = "Treemap",
                    create_plot_div(ns = ns, plot_type = "treemap"),
                  ),
                  bslib::nav_panel(
                    title = "Heatmap",
                    create_plot_div(ns = ns, plot_type = "heatmap"),
                  ),
                  bslib::nav_panel(
                    title = "Metabolic network",
                    create_plot_div(ns = ns, plot_type = "network", height = "50vh"),
                  ),
                
                # Plot options
                  div(
                    class = "flex-container",
                    shiny::conditionalPanel(
                      condition = "input.results_tabs == 'Metabolic network' && output.flag_multiple_organisms",
                      ns = ns, 
                      div(
                        class = "flex-item",
                        create_picker_input(inputId = ns("organism_to_display"), label = "Organism")
                      )
                    ),
                    shiny::conditionalPanel(
                      condition = "output.flag_multiple_substrates",
                      ns = ns, 
                      div(
                        class = "flex-item",
                        create_picker_input(inputId = ns("substrate_to_display"), label = "Substrate")
                      )
                    ),
                    shiny::conditionalPanel(
                      condition = "input.results_tabs == 'Metabolic network' && output.flag_multiple_products",
                      ns = ns, 
                      div(
                        class = "flex-item",
                        create_picker_input(inputId = ns("product_to_display"), label = "End product")
                      )
                    ),
                    shiny::conditionalPanel(
                      condition = "input.results_tabs == 'Metabolic network'",
                      ns = ns, 
                      div(
                        class = "flex-item",
                        create_picker_input(inputId = ns("set_network_layout"), label = "Layout")
                      )
                    ),
                    shiny::conditionalPanel(
                      condition = "input.results_tabs == 'Metabolic network'",
                      ns = ns, 
                      div(
                        class = "flex-item",
                        create_picker_input(inputId = ns("set_network_dimensions"), label = "Dimensions", choices = c("2", "3"), selected = "3")
                      )
                    )
                  ),
                  div(
                    class = "flex-container",
                    shiny::conditionalPanel(
                      condition = "input.results_tabs == 'Metabolic network'",
                      ns = ns, 
                      create_download_button(ns('download_network_model'), label = "Download network model")
                  )
              )
            )
          )
        )
      )
    )
  }
  
  # === Define server ===
  predictionsNetworkServer <- function(input, output, session, x, selected_tab) {
    #Set namespace
    ns <- session$ns
    
    # --- Define triggers for reactive expressions ---
    make_predictions_trigger <- reactive({
      req(input$make_predictions > 0)
      TRUE
    })
    
    get_reference_reactions_trigger <- reactive({
      list(input$reference_reactions_database, input$reference_reactions_upload)
    })
    
    get_graph_trigger <- reactive({
      list(input$make_predictions, input$substrate_to_display, input$product_to_display, input$set_network_layout, input$set_network_dimensions, input$organism_to_display)
    })
    
    get_layout_trigger <- reactive({
      list(make_predictions_trigger(), input$set_network_layout, input$set_network_dimensions, input$organism_to_display)
    })
    
    tab_selected_trigger <- reactive({
      if (selected_tab() == "predictionsNetwork") {
        return(TRUE)
      }
    })
    
    # --- Get user input (events) ---
    #Get gene functions
    get_gene_functions <- shiny::eventReactive({make_predictions_trigger()},
    {
      # Get data
      database <- load_database()
      
      if (input$function_tabs == "Database") {
        # Get gene functions for selected organisms in the database
        gene_functions <- load_gene_functions()
        organism_by_genome <- get_organism_by_genome(database = database)
        selected_organisms <- input$gene_functions_database

        gene_functions <- process_database_gene_functions(gene_functions, organism_by_genome, selected_organisms)
        
        runValidationModal(session = session, need(gene_functions != "", "Please choose at least one organism."))
        
      } else if (input$function_tabs == "File upload") {
        # Validate, read, and process the gene functions file
        gene_functions <- validate_and_read_csv(session = session, file_path = input$gene_functions_upload$datapath)
        gene_functions <- process_uploaded_gene_functions(gene_functions)
        
        runValidationModal(session = session, need(gene_functions != "", "Please check the format of your predicted gene functions file and try again."))
      }
      
      # Do further formatting
      gene_functions <- as.data.frame(gene_functions)
      
      # Replace default column name with a more descriptive one
      if (any(c("database_ID", "Database_ID") %in% colnames(gene_functions))) {
        colnames(gene_functions)[colnames(gene_functions) %in% c("database_ID", "Database_ID")] <- "Organism"
      }
      
      return(gene_functions)
    }, 
    ignoreNULL = TRUE, ignoreInit = FALSE, label="get_gene_functions")
    
    #Get reference reactions
    get_reference_reactions <- shiny::eventReactive({get_reference_reactions_trigger()},
    {
      if (input$reaction_tabs == "Database") {
        # Get data
        reference_reactions_glucose_fermentation <- load_reference_reactions_glucose_fermentation()
        reference_reactions_fructose_fermentation <- load_reference_reactions_fructose_fermentation()
        reference_reactions_methanogenesis <- load_reference_reactions_methanogenesis()
        
        # Get reactions
        reference_reactions <- switch(
          input$reference_reactions_database,
          "Glucose fermentation" = reference_reactions_glucose_fermentation,
          "Fructose fermentation" = reference_reactions_fructose_fermentation,
          "Methanogenesis" = reference_reactions_methanogenesis,
          NULL
        )
      } else if (input$reaction_tabs == "File upload") {
        # Get data
        reference_reactions = validate_and_read_csv(session = session, file_path = input$reference_reactions_upload$datapath)

        # Get reactions
        reference_reactions <- validate_reference_reactions(reference_reactions)
        
        runValidationModal(session = session, need(reference_reactions != "", "Please check the format of the reference reactions file and try again."))
      }

      return(reference_reactions)
    }, 
    ignoreNULL = TRUE, ignoreInit = TRUE, label="get_reference_reactions")  # Must have ignoreInit = TRUE, or module runs on app start up
    
    #Get substrate
    get_input_substrates <- shiny::eventReactive({make_predictions_trigger()},
    {
      substrates <- switch(
        input$reaction_tabs,
        "Database" = input$substrates_database,
        "File upload" = input$substrates_upload,
        NULL
      )
      
      runValidationModal(session = session, need(substrates != "", "Please choose at least one substrate"))
      
      return(substrates)  
    }, 
    ignoreNULL = TRUE, ignoreInit = FALSE, label="get_input_substrates")
    
    #Get products
    get_input_products <- shiny::eventReactive({make_predictions_trigger()},
    {
      products <- switch(
        input$reaction_tabs,
        "Database" = input$products_database,
        "File upload" = input$products_upload,
        NULL
      )
        
      runValidationModal(session = session, need(products != "", "Please choose at least one product"))
      
      return(products)  
    }, 
    ignoreNULL = TRUE, ignoreInit = FALSE, label="get_input_products")
    
    #Get unbalanced intermediates
    get_unbalanced_intermediates <- shiny::eventReactive({make_predictions_trigger()},
    {
      unbalanced_intermediates <- switch(
        input$reaction_tabs,
        "Database" = input$unbalanced_intermediates_database,
        "File upload" = input$unbalanced_intermediates_upload,
        NULL
      )

      return(unbalanced_intermediates)  
    }, 
    ignoreNULL = TRUE, ignoreInit = FALSE, label="get_unbalanced_intermediates")
    
    # --- Process input ---
    # Build reference network model
    build_reference_network_model <- shiny::eventReactive({make_predictions_trigger()},
    {
      reference_reactions = get_reference_reactions()
      
      abbreviation = reference_reactions$abbreviation
      equation = reference_reactions$equation
      direction = reference_reactions$direction
      officialName = reference_reactions$officialName
      geneAssociation = reference_reactions$geneAssociation
      subsystem = reference_reactions$subsystem
      unbalanced_intermediates = get_unbalanced_intermediates()
      unbalanced_products = get_input_products()[get_input_products() %nin% get_unbalanced_intermediates()]
      
      # Build metabolic model
      reference_network_model = build_network_model(equation = equation, direction = direction, abbreviation = abbreviation, officialName = officialName, geneAssociation = geneAssociation, subsystem = subsystem, starting_metabolite = NULL, ending_metabolite = NULL, unbalanced_intermediates = unbalanced_intermediates, unbalanced_products = unbalanced_products, remove_redundant_reactions = FALSE)
      
      return(reference_network_model)
    }, 
    ignoreNULL = TRUE, ignoreInit = FALSE, label="build_reference_network_model")
    
    # Build and solve organism-specific model
    get_solved_model <- shiny::eventReactive({make_predictions_trigger()},
    {
      # Get inputs
      reference_network_model <- build_reference_network_model()
      gene_functions <- get_gene_functions()
      products <- get_input_products()
      substrates <- get_input_substrates() 
      
      # Set variables
      if (length(products) == 0) {
        products <- "None"
      }
      
      # Initialize values
      s <- lapply(1:ncol(gene_functions), function(x) {
        vector("list", length(substrates))
      })
      
      #Launch modal with progress bar
      display_modal(session, ns("pb"), message = "Loading gene functions")
      
      # Print status to log
      cat(file = stderr(), paste0("Started prediction at ", Sys.time(), "\n"))
      
      for (i in seq_len(ncol(gene_functions))) {
        # Filter database IDs
        filtered_gene_functions <- gene_functions[, i][!is.na(gene_functions[, i]) & gene_functions[, i] != ""]
        
        # Solve the network model
        s[[i]] <- solve_network_model(df = reference_network_model, gene_functions = filtered_gene_functions, substrates = substrates, products = products)
        
        # Update modal with progress bar
        display_modal(session, ns("pb"), message = "Prediction in progress", value = 1 / ncol(gene_functions) * 100 * i)
      }
      
      # Hide the modal with progress bar
      hide_modal_with_progress(session, ns("pb"), delay_time = 1000)
      
      # Print status to log
      cat(file = stderr(), paste0("Ended prediction at ", Sys.time(), "\n"))
      
      return(s)
    }, 
    ignoreNULL = TRUE, ignoreInit = FALSE, label="get_solved_model")
    
    # Get fluxes from network models
    get_fluxes <- shiny::eventReactive({make_predictions_trigger()},
    {
      # Get inputs
      s = get_solved_model()
      substrates = get_input_substrates()
      products = get_input_products()
      organism = colnames(get_gene_functions())
      
      # Set variables
      if (length(products) > 0) {
        products = products
      } else {
        products = "None"
      }
      ending_metabolite = products
      
      # Get fluxes
      flux <- data.frame(matrix(NA, nrow = length(organism) * length(substrates) * length(ending_metabolite), ncol = 4))
      colnames(flux) <- c("Organism", "Substrate", "End_product", "Flux")
      row_counter <- 1
      
      for (i in seq_along(organism)) {
        for (j in seq_along(substrates)) {
          substrate <- substrates[j]
          
          for (k in seq_along(ending_metabolite)) {
            product <- ending_metabolite[k]
            match_index <- which(s[[i]][[j]][[k]]$abbreviation == "Ending_metabolite")
            flux_value <- if (length(match_index) > 0) s[[i]][[j]][[k]]$flux[match_index] else NA
            
            # Fill the flux data frame using row_counter
            flux[row_counter, ] <- list(organism[i], substrate, product, flux_value)
            
            # Increment the row counter
            row_counter <- row_counter + 1
          }
        }
      }
      
      return(flux)
    }, 
    ignoreNULL = TRUE, ignoreInit = FALSE, label="get_fluxes")

    # Make network graph
    get_network_graph <- shiny::eventReactive({get_graph_trigger()},
    {
      # Get unbalanced intermediates
      unbalanced_intermediates = get_unbalanced_intermediates()
      
      # Get index of selected organism
      if (!is.null(input$organism_to_display)) {
        i = which(colnames(get_gene_functions()) == input$organism_to_display)
      } else {
        i = 1
      }

      # Get index of selected substrates
      j = which(get_input_substrates() == input$substrate_to_display)
      
      # Get index of selected products
      k = which(get_input_products() == input$product_to_display)
      
      # Get model
      s = get_solved_model()[[i]][[j]][[k]]
      
      # Change fluxes to 0 if product has flux less than min_flux
      min_flux = 1
      if (s$flux[which(s$abbreviation == "Ending_metabolite")] < min_flux) {
        s$flux = 0
      }
      
      # Make graph
      g = make_network_graph(s = s, to_remove = unbalanced_intermediates)
      
      return(g)
    }, 
    ignoreNULL = TRUE, ignoreInit = FALSE, label="get_network_graph")
  
    # Set layout for graph
    get_network_layout <- shiny::eventReactive(get_layout_trigger(),
    {
      g <- get_network_graph()

      layout <- set_network_layout(graph = g, type = input$set_network_layout, dimensions = input$set_network_dimensions)
      
      return(layout)
    },
    ignoreNULL = TRUE, ignoreInit = FALSE, label="get_network_layout")
    
    # Get endproducts from fluxes
    count_predictions <- shiny::eventReactive({make_predictions_trigger()}, {
      df <- get_fluxes()
      count_fluxes(df, min_value = 1)
    }, ignoreNULL = TRUE, ignoreInit = FALSE, label = "count_predictions")

  # --- Update selections ---
    # Update choices for gene functions (organisms)
    shiny::observeEvent({list(tab_selected_trigger(), get_reference_reactions_trigger(), input$reaction_tabs)}, 
    {
        database <- load_database()
        
        if (input$reaction_tabs == "Database") {
          choices <- switch(
            input$reference_reactions_database,
            "Glucose fermentation" = get_organism_choices(database = database, metabolism_type = "Fermentation"),
            "Fructose fermentation" = get_organism_choices(database = database, metabolism_type = "Fermentation"),
            "Methanogenesis" = get_organism_choices(database = database, metabolism_type = "Methanogenesis"),
            NULL
          )
        } else if (input$reaction_tabs == "File upload") {
          choices <- get_organism_choices(database)
        }

        update_select_input(session = session,  inputId = "gene_functions_database", choices = choices)
    },
    ignoreNULL = TRUE, ignoreInit = FALSE, label="update_gene_function_choices")

    # Update choices for substrates
    shiny::observeEvent({list(tab_selected_trigger(), get_reference_reactions_trigger())},
    {
      choices <- get_metabolite_names(equation = get_reference_reactions()$equation)
      
      if (input$reaction_tabs == "Database") {
        inputId = "substrates_database"
        selected <- switch(input$reference_reactions_database,
                           "Glucose fermentation" = "D-Glucose",
                           "Fructose fermentation" = "D-Fructose",
                           "Methanogenesis" = c("CO2", "Formate"),
                           NULL)
      } else if (input$reaction_tabs == "File upload") {
        inputId = "substrates_upload"
        selected <- "D-Glucose"
      }
      
      update_select_input(session = session,  inputId = inputId, choices = choices, selected = selected)
    }, 
    ignoreNULL = TRUE, ignoreInit = FALSE, label="update_substrate_choices")
    
    # Update choices for reference reactions
    shiny::observeEvent({tab_selected_trigger()},
    {
      choices <- c("Glucose fermentation", "Fructose fermentation", "Methanogenesis")
      update_select_input(session = session,  inputId = "reference_reactions_database", choices = choices)
          
    },
    ignoreNULL = TRUE, ignoreInit = FALSE, label="update_reference_reactions_choices")

    # Update choices for products
    shiny::observeEvent({list(tab_selected_trigger(), get_reference_reactions_trigger())},
    {
      choices <- get_metabolite_names(equation = get_reference_reactions()$equation,
                                to_remove = choices_unbalanced_intermediates)

      if (input$reaction_tabs == "Database") {
        inputId <- "products_database"
        selected <- switch(input$reference_reactions_database,
                           "Glucose fermentation" = choices_products,
                           "Fructose fermentation" = choices_products,
                           "Methanogenesis" = c("Methane"),
                           NULL)
      } else if (input$reaction_tabs == "File upload") {
        inputId <- "products_upload"
        selected <- choices_products
      }

      update_select_input(session = session,  inputId = inputId, choices = choices, selected = selected)
    },
    ignoreNULL = TRUE, ignoreInit = FALSE, label="update_product_choices")

    # Update choices for unbalanced intermediates
    shiny::observeEvent({list(tab_selected_trigger(), get_reference_reactions_trigger())},
    {
      choices <- get_metabolite_names(equation = get_reference_reactions()$equation)

      if (input$reaction_tabs == "Database") {
        inputId = "unbalanced_intermediates_database"
        selected <- switch(input$reference_reactions_database,
                            "Glucose fermentation" = choices_unbalanced_intermediates,
                            "Fructose fermentation" = choices_unbalanced_intermediates,
                            "Methanogenesis" = c("NAD+", "NADH", "NADP+", "NADPH", "FAD", "FADH2",
                                                   "Ubiquinone",
                                                   "Ubiquinol", "Quinone", "Hydroquinone", "Menaquinone",
                                                   "Menaquinol", "Oxidized hydrogenase", "Reduced hydrogenase",
                                                   "ATP", "ADP", "AMP", "GTP", "GDP", "Orthophosphate",
                                                   "Diphosphate", "Polyphosphate", "H2O", "H+", "H+[side 1]",
                                                   "H+[side 2]", "Na+[side 1]", "Na+[side 2]", "Sodium cation", "HCO3-",
                                                   "Hydrogen"),
                            NULL)
      } else if (input$reaction_tabs == "File upload") {
        inputId = "unbalanced_intermediates_upload"
        selected <- choices_unbalanced_intermediates
      }
      
      update_select_input(session = session,  inputId = inputId, choices = choices, selected = selected)
    },
    ignoreNULL = TRUE, ignoreInit = FALSE, label="update_unbalanced_intermediate_choices")
     
    # Update choices for substrates to display
    shiny::observeEvent({make_predictions_trigger()},
    {
      choices <- get_input_substrates()
      update_picker_input(session = session,  inputId = "substrate_to_display", choices = choices)
    },
    ignoreNULL = TRUE, ignoreInit = FALSE, label="update_substrate_display")
    
    # Update choices for products to display
    shiny::observeEvent({make_predictions_trigger()},
    {
      choices <- get_input_products()
      update_picker_input(session = session,  inputId = "product_to_display", choices = choices)
    },
    ignoreNULL = TRUE, ignoreInit = FALSE, label="update_product_display")
    
    # Update choices for organisms to display
    shiny::observeEvent({make_predictions_trigger()},
    {
      choices <- colnames(get_gene_functions())
      update_picker_input(session = session,  inputId = "organism_to_display", choices = choices)
    },
    ignoreNULL = TRUE, ignoreInit = FALSE, label="update_organism_display")
    
    # Update choices for network layout
    shiny::observeEvent({input$set_network_dimensions},
    {
      choices <- switch(input$set_network_dimensions,
                        "3" = c("FR", "KK", "DH", "GEM", "DRL", "MDS"),
                        "2" = c("FR", "KK", "DH", "GEM", "DRL", "MDS", "Graphopt"),
                        NULL)
      update_picker_input(session = session,  inputId = "set_network_layout", choices = choices)
    },
    ignoreNULL = TRUE, ignoreInit = FALSE, label="update_layout_display")

  # --- Generate outputs ---
  # Output modals with example data
    # Output modals with example data
    shiny::observeEvent(input$gene_functions_modal, 
    {
      # Get data
      output$downloadFunctions_1 <- create_download_handler("gene_functions_e_coli", function() load_gene_functions_e_coli())
      output$downloadFunctions_2 <- create_download_handler("gene_functions_uncharacterized", function() load_gene_functions_uncharacterized())
      output$downloadFunctions_3 <- create_download_handler("gene_functions_rumen_cultured", function() load_gene_functions_rumen_cultured())
      output$downloadFunctions_4 <- create_download_handler("gene_functions_rumen_MAGs", function() load_gene_functions_rumen_MAGs())
      
      # Show modal
      showDownloadModal(
        ns = ns,
        title = "Example files",
        downloads = list(
          "downloadFunctions_1" = "E. coli",
          "downloadFunctions_2" = "Previously uncharacterized bacteria",
          "downloadFunctions_3" = "Cultured prokaryotes from rumen",
          "downloadFunctions_4" = "MAGs from rumen"
        )
      )
    },
    ignoreNULL = TRUE,  ignoreInit = FALSE, label="show_gene_functions_modal")
  
  shiny::observeEvent(input$reference_reactions_modal, 
  {
    # Get download handlers
    output$downloadReference_1 <- create_download_handler("reference_reactions_glucose_fermentation", function() load_reference_reactions_glucose_fermentation())
    output$downloadReference_2 <- create_download_handler("reference_reactions_fructose_fermentation", function() load_reference_reactions_fructose_fermentation())
    output$downloadReference_3 <- create_download_handler("reference_reactions_methanogenesis", function() load_reference_reactions_methanogenesis())
    
    # Show modal
    showDownloadModal(
      ns = ns,
      title = "Example files",
      downloads = list(
        "downloadReference_1" = "Glucose fermentation",
        "downloadReference_2" = "Fructose fermentation",
        "downloadReference_3" = "Methanogenesis"
      )
    )
  },
  ignoreNULL = TRUE,  ignoreInit = FALSE, label="show_reference_reactions_modal")
  
  shiny::observeEvent(input$go_to_help, {
    shiny::updateNavbarPage(session = x, inputId = "tabs", selected = "help")
    shiny::updateNavlistPanel(session = x, inputId = "navlist_panel", selected = "Predict traits with metabolic networks")
    shiny::removeModal()
  })
  
  #Output flag for multiple organisms
  output$flag_multiple_organisms = shiny::eventReactive({make_predictions_trigger()},
  {
    ncol(get_gene_functions()) > 1
  })
  shiny::outputOptions(output, "flag_multiple_organisms", suspendWhenHidden = FALSE)
  
  #Output flag for substrates
  output$flag_multiple_substrates = shiny::reactive({
    length(get_input_substrates()) > 1
  })
  shiny::outputOptions(output, "flag_multiple_substrates", suspendWhenHidden = FALSE)
  
  #Output flag for products
  output$flag_multiple_products = shiny::reactive({
    length(get_input_products()) > 1
  })
  shiny::outputOptions(output, "flag_multiple_products", suspendWhenHidden = FALSE)
  
  # Output flag for results
  output$flag_results = shiny::reactive({
    !is.null(get_fluxes())
  })
  shiny::outputOptions(output, "flag_results", suspendWhenHidden = FALSE)
  
  # Output summary text
  output$summary_text <- shiny::renderText({
    counts <- count_predictions()
    format_summary_text(
      count1 = counts$endproducts_predictions, 
      count2 = counts$organisms_predictions, 
      label1 = "end products", 
      label2 = "organisms", 
      total2 = counts$organisms_total
    )
  })
  
  # Output downloadable csv of fluxes
  output$download_data <- create_download_handler(
    filename_prefix = "fluxes",
    data_source = function() get_fluxes()
  )
  
  # Output overview plots
  shiny::observe({
    df = get_fluxes()
    var_name = input$substrate_to_display
    
    #Summary plot
    output$summary_plot <- plotly::renderPlotly({
      df = network_results_to_plot(df = df, plot_type="summary", var_name = var_name)
      plot = plot_summary(df, 
                          coord_fixed = TRUE, 
                          hovertemplate = "<b>Endproduct: %{x}</b><br><b>% organisms positive: %{z:.2f}</b><br><extra></extra>",
                          legend_labels = c("0", "25", "50", "75", "100"), 
                          legend_title = "% organisms positive")
      plot
    })
    
    # Treemap plot
    output$treemap_plot <- plotly::renderPlotly({
      df = network_results_to_plot(df = df, plot_type="treemap", var_name = var_name)
      plot = plot_treemap(df = df,
                          hovertemplate = "<b>Endproduct: %{label}</b><br><b>% total: %{value:.2f}</b><br><extra></extra>")
      plot
    })
    
    # Heatmap plot
    output$heatmap_plot <- plotly::renderPlotly({
      df = network_results_to_plot(df = df, plot_type="heatmap", var_name = var_name)
      plot = plot_heatmap(df, 
                          coord_fixed=TRUE,
                          hovertemplate = "<b>Endproduct: %{x}</b><br><b>Organism: %{y}</b><br><b>Flux: %{z:.0f}</b><br><extra></extra>",
                          legend_labels = c("0", "250", "500", "750", "1000"), 
                          legend_title = "Flux")
      plot
    })
  })
  
  # Output network graph
  output$network_plot <- plotly::renderPlotly(exp = {
    g = get_network_graph()
    layout = get_network_layout()
    
    vertices_to_highlight = c(format_metabolite_name(input$substrate_to_display), format_metabolite_name(input$product_to_display))
    
    g = format_network_graph(graph = g, show_flux = TRUE, show_subsystems = TRUE, vertices_to_highlight = vertices_to_highlight)

    plot = plot_network(graph = g, layout = layout, network_legend_key = network_legend_key, spread = 0.05)

    plot
  })
  
  # Output downloadable csv of results
  output$download_network_model <- create_download_handler(
    filename_prefix = "model",
    data_source = function() {
      #Get index of selected organism
      if (!is.null(input$organism_to_display)) {
        i = which(colnames(get_gene_functions()) == input$organism_to_display)
      } else {
        i = 1
      }
      
      #Get index of selected substrates
      j = which(get_input_substrates() == input$substrate_to_display)
      
      #Get index of selected products
      k = which(get_input_products() == input$product_to_display)
      
      #Get network model
      s = get_solved_model()[[i]][[j]][[k]]
      s
    },
  )
}