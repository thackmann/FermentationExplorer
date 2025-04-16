# Define Variables for Predictions Using Machine Learning Module
# These are variables specific to this module
# Author: Timothy Hackmann
# Date: 9 Mar 2025

# Choices for variables
choices_traits_ML = c(
  metabolism_var, 
  physiology_var,
  growth_var,
  morphology_var,
  isolation_var
)

# File paths for random forest models
model_paths <- list(
  
  # Type of metabolism
  `Fermentation (type of metabolism)` = "data/random_forest_fermentation.rds",
  `Methanogenesis (type of metabolism)` = "data/random_forest_methanogenesis.rds",
  
  # Substrates for end products
  `Glucose (substrate for end products)` = "data/random_forest_glucose.rds",
  
  # End products
  `Acetate (end product)` = "data/random_forest_acetate.rds",
  `Butyrate (end product)` = "data/random_forest_butyrate.rds",
  `CH4 (end product)` = "data/random_forest_CH4.rds",
  `Ethanol (end product)` = "data/random_forest_ethanol.rds",
  `Formate (end product)` = "data/random_forest_formate.rds",
  `H2 (end product)` = "data/random_forest_H2.rds",
  `Isobutyrate (end product)` = "data/random_forest_isobutyrate.rds",
  `Isovalerate (end product)` = "data/random_forest_isovalerate.rds",
  `Lactate (end product)` = "data/random_forest_lactate.rds",
  `Propionate (end product)` = "data/random_forest_propionate.rds",
  `Pyruvate (end product)` = "data/random_forest_pyruvate.rds",
  `Succinate (end product)` = "data/random_forest_succinate.rds",
  
  # Physiology/morphology
  `Anaerobe (oxygen tolerance)` = "data/random_forest_anaerobe.rds",
  `Gram positive (gram stain)` = "data/random_forest_gram_positive.rds",
  `Spore positive (spore formation)` = "data/random_forest_spore_formation.rds",
  `Motility positive (motility)` = "data/random_forest_motility_positive.rds",
  
  # Growth
  `Thermophile (temperature for growth)` = "data/random_forest_thermophile.rds",
  `Halophile (salt for growth)` = "data/random_forest_halophile.rds",
  `Slow growth (incubation period)` = "data/random_forest_slow_growth.rds"
)
