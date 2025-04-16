# Define Variables for Predictions with Metabolic Networks Module
# These are variables specific to this module
# Author: Timothy Hackmann
# Date: 23 Mar 2025

# Selections for variables
selected_unbalanced_intermediates_fermentation <- c(
  "NAD+", "NADH", "NADP+", "NADPH", "FAD", "FADH2", "Reduced ferredoxin", "Oxidized ferredoxin",
  "Ubiquinone", "Ubiquinol", "Quinone", "Hydroquinone", "Menaquinone", "Menaquinol",
  "Oxidized hydrogenase", "Reduced hydrogenase", "ATP", "ADP", "AMP", "GTP", "GDP",
  "Orthophosphate", "Diphosphate", "Polyphosphate", "H2O", "H+", "H+[side 1]", "H+[side 2]",
  "Na+[side 1]", "Na+[side 2]", "Sodium cation", "HCO3-", "CO2", "Hydrogen"
)

selected_unbalanced_intermediates_methanogenesis <- c(
  "NAD+", "NADH", "NADP+", "NADPH", "FAD", "FADH2", "Ubiquinone", "Ubiquinol", "Quinone",
  "Hydroquinone", "Menaquinone", "Menaquinol", "Oxidized hydrogenase", "Reduced hydrogenase",
  "ATP", "ADP", "AMP", "GTP", "GDP", "Orthophosphate", "Diphosphate", "Polyphosphate",
  "H2O", "H+", "H+[side 1]", "H+[side 2]", "Na+[side 1]", "Na+[side 2]",
  "Sodium cation", "HCO3-", "Hydrogen"
)

selected_products_fermentation <- 
  c("Acetate", "(S)-Lactate", "(R)-Lactate", "Ethanol", 
  "Succinate", "Propanoate", "Butanoic acid", "Formate", 
  "Hydrogen", "CO2")

selected_products_methanogenesis <- 
  c("Methane")

selected_products_glycolysis <- 
  c("Pyruvate", "(S)-Lactate", "Acetate", "Ethanol", "CO2")

# Key for the legend of the network plot (by color and width of lines)
network_legend_key <- data.frame(
  name = c("No flux", "Low flux", "Medium flux", "High flux"),
  line_color = c("#7f7f7f", "#00B050", "#00B050", "#00B050"),
  line_width = c(0.5, 0.5, 1, 2), 
  stringsAsFactors = FALSE
)