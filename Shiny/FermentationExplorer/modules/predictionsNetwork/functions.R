# Define Functions for Predictions with Metabolic Networks Module
# These are functions specific to this module
# Author: Timothy Hackmann
# Date: 9 Mar 2025

# === Functions for metabolic networks ===
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
  
  #' Remove Null Reactions
  #'
  #' This function removes reactions where the substrates and products are identical,
  #' such as "A <=> A", or "A + B <=> A + B".
  #'
  #' @param df A data frame of the metabolic network model
  #' @return A data frame with null reactions removed
  #' @export
  #' @importFrom stringr str_split
  remove_null_reactions <- function(df) {
    eq_split <- stringr::str_split(df$equation, pattern = "<=>", simplify = TRUE)
    lhs <- trimws(eq_split[, 1])
    rhs <- trimws(eq_split[, 2])
    
    # Split into metabolite sets
    lhs_split <- lapply(lhs, function(x) sort(trimws(unlist(strsplit(x, " \\+ ")))))
    rhs_split <- lapply(rhs, function(x) sort(trimws(unlist(strsplit(x, " \\+ ")))))
    
    # Identify reactions where the sets are identical
    is_null <- mapply(function(a, b) identical(a, b), lhs_split, rhs_split)
    
    # Remove null reactions
    df <- df[!is_null, ]
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
  #' @param remove_null_reactions A logical indicating whether to remove null reactions
  #' @param add_glycolysis A logical indicating whether to add the 10 reactions of glycolysis
  #' @return A data frame of the metabolic network model
  #' @examples
  #' build_network_model()
  #' @export
  #' @importFrom dplyr if_else
  build_network_model = function(equation = NULL, direction = "Bidirectional", 
                                 abbreviation = NA, officialName = NA, geneAssociation = NA, 
                                 subsystem = NA, starting_metabolite = "D-Glucose", 
                                 ending_metabolite = "Pyruvate", 
                                 unbalanced_intermediates = c("NAD+", "NADH", "ATP", "ADP", "Orthophosphate", "H2O", "H+", "CO2"), 
                                 unbalanced_products = NULL, remove_redundant_reactions = TRUE, 
                                 remove_null_reactions = TRUE, add_glycolysis = FALSE) {
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
    
    # Remove Null Reactions
    if (remove_null_reactions == TRUE) {
      df = remove_null_reactions(df)
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
    
    # Initialize named list for substrates
    s <- setNames(vector("list", length(substrates)), substrates)
    
    for (j in seq_along(substrates)) {
      substrate <- substrates[j]
      starting_metabolite <- rep(substrate, times = length(products))

      # Initialize named list for products
      s[[substrate]] <- setNames(vector("list", length(products)), products)
      
      for (k in seq_along(products)) {
        product <- products[k]
        # Add target metabolites and find fluxes
        s[[substrate]][[product]] <- add_target_metabolites(
          df = df,
          starting_metabolite = format_metabolite_name(starting_metabolite[k]),
          ending_metabolite = format_metabolite_name(product),
          lowbnd = -1000,
          uppbnd = 1e6
        ) |> fbar::find_fluxes_df()
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

# === Other functions ===
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
  
  #' Validate Reference Reactions Data
  #'
  #' This function checks whether the uploaded reference reactions data contains all required columns.
  #' If any required columns are missing, it returns an empty string.
  #'
  #' @param reference_reactions Dataframe. The uploaded reference reactions data.
  #' 
  #' @return The validated dataframe or an empty string if required columns are missing.
  #' @export
  #' @importFrom dplyr select
  validate_reference_reactions <- function(reference_reactions) {
    required_columns <- c("abbreviation", "equation", "direction", "officialName", 
                          "geneAssociation", "subsystem", "reaction_ID", "Enzyme", "Genes")
    
    if (!all(required_columns %in% colnames(reference_reactions))) {
      return("")
    }
    
    return(reference_reactions)
  }
  
  #' Keep only elements that are allowed
  #'
  #' This function selects elements from a vector `x` that are present in a given `allowed` vector
  #' and not present in a `disallowed` vector. It is useful for filtering based on inclusion and
  #' exclusion criteria simultaneously.
  #'
  #' @param x A character vector (or any atomic vector) to be filtered.
  #' @param allowed A vector of values that are permitted (inclusion list).
  #' @param disallowed A vector of values that should be excluded (exclusion list).
  #'
  #' @return A vector containing elements of `x` that are in `allowed` but not in `disallowed`.
  #' The order of elements in the result follows their order in `x`.
  #'
  #' @examples
  #' x <- c("NADH", "ATP", "CO2", "Hydrogen", "Ethanol")
  #' allowed <- c("NADH", "ATP", "CO2", "Hydrogen")
  #' disallowed <- c("CO2", "Hydrogen")
  #' keep_only_allowed(x, allowed, disallowed)
  #' # Returns: "NADH" "ATP"
  #'
  #' @export
  keep_only_allowed <- function(x, allowed, disallowed) {
    x[x %in% allowed & x %nin% disallowed]
  }
  