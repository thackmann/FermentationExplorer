# Load Internal Data for Shiny App
# This script loads files and objects used by the app.  These are
# files or objects used by multiple modules.  
# Author: Timothy Hackmann
# Date: 14 October 2024

data_fp = "data/database.csv"
raw_data = read.csv(data_fp)
raw_data[is.na(raw_data)] = "NA"

data_fp = "data/database_clean.csv"
clean_data = read.csv(data_fp)
colnames(clean_data) = gsub(pattern="\\.", replacement=" ", x=colnames(clean_data))

data_fp = "data/gene_functions_e_coli.csv"
gene_functions_e_coli = read.csv(data_fp)

data_fp = "data/gene_functions_uncharacterized.csv"
gene_functions_uncharacterized = read.csv(data_fp)

data_fp = "data/gene_functions_rumen_cultured.csv"
gene_functions_rumen_cultured = read.csv(data_fp)

data_fp = "data/gene_functions_rumen_MAGs.csv"
gene_functions_rumen_MAGs = read.csv(data_fp)

data_fp = "data/reference_reactions_glucose_fermentation.csv"
reference_reactions_glucose_fermentation = read.csv(data_fp)

data_fp = "data/reference_reactions_fructose_fermentation.csv"
reference_reactions_fructose_fermentation = read.csv(data_fp)

data_fp = "data/reference_reactions_methanogenesis.csv"
reference_reactions_methanogenesis = read.csv(data_fp)

data_fp = "data/taxa_uncharacterized.csv"
taxa_uncharacterized = read.csv(data_fp)

data_fp = "data/taxa_rumen_cultured.csv"
taxa_rumen_cultured = read.csv(data_fp)

data_fp = "data/taxa_rumen_MAGs.csv"
taxa_rumen_MAGs = read.csv(data_fp)

data_fp = "data/taxa_infant.csv"
taxa_infant = read.csv(data_fp)
