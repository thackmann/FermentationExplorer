# Get LPSN Organisms for App
# This script gets names of organisms from the LPSN database.
# Requirements:
# - Packages in install/installPackages.R
# - Data downloaded from LPSN at links below
# Author: Timothy Hackmann
# Date: 17 December 2024

# === Get database directory ===
  database_directory <- FileLocator::getCurrentFileLocation()
  subdirectory <- "/LPSN"
  database_directory <- gsub(paste0(subdirectory, "$"), "", database_directory)

# === Load external R files ===
  setwd(database_directory)
  source("functions\\helperFunctions.R", local = TRUE)
  source("LPSN\\functions.R", local = TRUE)

# === Read in data ===
  setwd(database_directory)

  # From https://lpsn.dsmz.de/downloads
  lpsn_data =  read.csv(paste0(app_directory, "LPSN\\data\\lpsn_gss_2024-10-17.csv"))

# === Format data ===
  # Select type strains with correct name
  df = lpsn_data
  df = df %>% dplyr::filter(grepl(pattern = "correct name", x = status, ignore.case = TRUE))
  df = df %>% dplyr::filter(sp_epithet!="")

  # Rename columns
  df = df %>% dplyr::select(genus_name, sp_epithet, subsp_epithet, nomenclatural_type, status, record_no, address)
  df = df %>% dplyr::rename(Genus = "genus_name", Species = "sp_epithet", Subspecies = "subsp_epithet", Strain = nomenclatural_type, Status = status, LPSN_ID = record_no)

  # Replace blank values with NA
  df = df %>% dplyr::mutate_all(~ifelse(. == "", NA, .))

  # For subspecies, keep only entries that have subspecies specified (e.g., keep Selenomonas ruminantium lactilytica but not Selenomonas ruminantium)
  df <- df %>% dplyr::group_by(Genus, Species) %>% dplyr::filter(!(dplyr::n_distinct(Subspecies) > 1 & Subspecies == "")) %>% dplyr::ungroup()

# === Export ===
  write.csv(df, paste0(app_directory, "LPSN\\data\\lpsn_organisms.csv"), row.names = FALSE)
