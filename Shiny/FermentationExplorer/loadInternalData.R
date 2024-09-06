# Load Internal Data for Shiny App
# This script loads files and objects used by the app.  These are
# files or objects used by multiple modules.  
# Author: Timothy Hackmann
# Date: 6 September 2024

data_fp = "data/database.csv"
raw_data = read.csv(data_fp)
raw_data[is.na(raw_data)] = "NA"

data_fp = "data/database_clean.csv"
clean_data = read.csv(data_fp, check.names=FALSE)

data_fp = "data/gene_functions_e_coli.csv"
gene_functions_e_coli = read.csv(data_fp)

data_fp = "data/gene_functions_uncharacterized.csv"
gene_functions_uncharacterized = read.csv(data_fp)

data_fp = "data/gene_functions_Hungate.csv"
gene_functions_Hungate = read.csv(data_fp)

data_fp = "data/gene_functions_RUG.csv"
gene_functions_RUG = read.csv(data_fp)

data_fp = "data/reference_reactions_glucose_fermentation.csv"
reference_reactions_glucose_fermentation = read.csv(data_fp)

data_fp = "data/reference_reactions_fructose_fermentation.csv"
reference_reactions_fructose_fermentation = read.csv(data_fp)

data_fp = "data/reference_reactions_methanogenesis.csv"
reference_reactions_methanogenesis = read.csv(data_fp)

data_fp = "data/taxa_uncharacterized.csv"
taxa_uncharacterized = read.csv(data_fp)

data_fp = "data/taxa_Hungate.csv"
taxa_Hungate = read.csv(data_fp)

data_fp = "data/taxa_RUG.csv"
taxa_RUG = read.csv(data_fp)

data_fp = "data/taxa_infant.csv"
taxa_infant = read.csv(data_fp)
