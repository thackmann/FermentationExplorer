# Define Variables for Search Database Module
# These are variables specific to this module
# Author: Timothy Hackmann
# Date: 5 Mar 2025

# Choices for variables for searching
choices_traits_search = c(
  taxonomy_var,
  # database_var, # commenting this out speeds up execution by ~20 s
  metabolism_var,
  physiology_var,
  growth_var,
  morphology_var,
  isolation_var)

# Choices for checkboxes
choices_checkboxes_search <- list(
  organism = list(names = taxonomy_var),
  databases = list(names = database_var),
  metabolism = list(names = metabolism_var),
  traits = list(names = c(
    physiology_var, 
    morphology_var, 
    growth_var, 
    isolation_var))
)

choices_checkboxes_search <- lapply(choices_checkboxes_search, function(category) {
  category$choices <- setNames(category$names, category$names)
  return(category)
})