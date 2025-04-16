# Define Functions for Search Database Module
# These are functions specific to this module
# Author: Timothy Hackmann
# Date: 9 Mar 2025

#' Rename and Overwrite Column Names
#'
#' This function renames columns in a dataframe by replacing a specified pattern with a replacement string.
#' If the renaming results in duplicate column names, only the last occurrence of each name is retained.
#'
#' @param data A dataframe whose column names need to be renamed.
#' @param pattern A character string containing a regular expression to match in column names.
#' @param replacement A character string that will replace the matched pattern in column names.
#'
#' @return The input dataframe with renamed column names. If duplicates arise after renaming, earlier occurrences are removed, keeping only the last occurrence of each duplicated column name.
#' @export
#'
#' @examples
rename_and_overwrite <- function(data, pattern, replacement) {
  colnames(data) <- colnames(data) %>%
    stringr::str_replace(pattern, replacement)
  
  data <- data[, !duplicated(names(data), fromLast = TRUE)]
  
  data
}

#' Format Search Results for Plots
#'
#' This function formats taxonomy results into a format suitable 
#' for different types of plots, though only treemap plots are
#' supported at present.   
#'
#' @param df A dataframe containing the taxonomy results.
#' @param plot_type A character string specifying the type of plot ("treemap").
#' @param var_name Optional. A character string specifying the variable name to filter by.
#' @return A formatted dataframe ready for plotting.
#' @export
#' @importFrom dplyr  mutate select group_by n summarize
search_results_to_plot <- function(df, plot_type, var_name = NULL) {
  if (plot_type == "treemap") {
    df = df %>% 
      dplyr::select(all_of(var_name)) %>% 
      dplyr::rename(y = all_of(var_name)) %>%
      tidyr::drop_na() %>%  
      dplyr::filter(!is.na(y)) 
    
    df <- df %>%
      dplyr::group_by(y) %>%
      dplyr::summarise(z = dplyr::n(), .groups = 'drop') %>%
      dplyr::mutate(z = z / sum(z))
    
    # Convert to percentage
    df$z = df$z * 100
  }
  
  return(df)
}

#' Filter Tree Layout Based on Selected Organisms
#'
#' This function filters a tree layout to retain only the branches that lead to 
#' organisms matching a given set of IDs from a specified column. It ensures that
#' only relevant parent-child relationships are kept, while preserving root nodes.
#'
#' @param layout A dataframe representing the phylogenetic tree layout, 
#'        containing columns `node`, `label`, `parent`, etc.
#' @param data A dataframe containing the selected organisms.
#' @param nodes_to_root A dataframe mapping tip nodes to their parent nodes, 
#'        containing columns `tip_node`, `parent_node`, and `child_node`.
#' @param id_column A string specifying the column in `data` that contains the organism IDs.
#'        Defaults to `"IMG Genome ID max genes"`.
#'
#' @return A filtered version of `layout` containing only the branches leading 
#'         to the selected organisms, along with root nodes.
#' 
#' @export
filter_tree_layout <- function(layout, data, nodes_to_root, id_column = "IMG Genome ID max genes") {
  if (!id_column %in% names(data)) {
    stop(glue::glue("Column '{id_column}' not found in data"))
  }
  
  # Select matching tip nodes based on the given ID column
  selected_tips <- layout$node[which(layout$label %in% data[[id_column]])]
  
  # Find matching parent-child node pairs
  match <- nodes_to_root %>%
    dplyr::filter(tip_node %in% selected_tips) %>%
    dplyr::distinct(parent_node, child_node)
  
  # Retain only relevant branches and root nodes
  layout_filtered <- layout %>%
    dplyr::semi_join(match, by = c("parent" = "parent_node", "node" = "child_node")) %>%
    dplyr::bind_rows(layout %>% dplyr::filter(parent == node))
  
  return(layout_filtered)
}

#' Get Tree Layout Parameters
#'
#' This function returns a list of parameters based on the selected tree layout type.
#' It determines whether the coordinate system should be fixed and assigns an appropriate 
#' x-to-y ratio for different layout types.
#'
#' @param layout_type A character string specifying the tree layout type. 
#'        Possible values: "Daylight", "Equal angle", or "Rectangular".
#'
#' @return A list containing:
#'   \item{coord_fixed}{Logical value indicating whether the coordinate system should be fixed.}
#'   \item{x_to_y_ratio}{Numeric value specifying the x-to-y ratio for the layout, or NULL if not applicable.}
#'
#' @examples
#' get_layout_param("Daylight")
#' get_layout_param("Rectangular")
#'
#' @export
get_layout_param <- function(layout_type) {
  list(
    coord_fixed = switch(layout_type,
                         "Daylight" = TRUE,
                         "Equal angle" = TRUE,
                         "Rectangular" = FALSE
    ),
    x_to_y_ratio = switch(layout_type,
                          "Daylight" = 0.8,
                          "Equal angle" = 0.8,
                          "Rectangular" = NULL
    )
  )
}