# Get 16S Ribosomal Sequences
# This script downloads the 16S rDNA sequences for type strains of bacterial species from the LPSN database.
# Requirements:
# - Packages in install/installPackages.R
# - Data from getLpsnOrganisms.R script
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

  # Read in data from getLpsnOrganisms.R script
  lpsn_organisms <- read.csv("LPSN\\data\\lpsn_organisms.csv")

# === Download ribosomal sequences ===
  # Handle most strains (those that are not type subspecies)
    most_strains <- lpsn_organisms %>% dplyr::filter(Species!=Subspecies)
    addresses <- most_strains$address
    LPSN_ID <- most_strains$LPSN_ID
    base_url <- "https://lpsn.dsmz.de"
    download_status <- vector("character", length(addresses))

    for (i in seq_along(addresses)) {
      # Get body of the page
      body <- get_web_page_body(url = addresses[i], user_agent = "me")

      # Extract link to FASTA file
      links <- extract_links(body)
      fasta_link <- links[grepl("\\.fasta$", links)]
      fasta_link <- ifelse(grepl("^http", fasta_link), fasta_link, paste0(base_url, fasta_link))

      if (length(fasta_link) > 0) {
        # Construct the file path
        fp <- paste0("LPSN/data/16S_ribosomal_sequences", LPSN_ID[i], ".fasta")

        # Download the FASTA file
        download_result <- download_fasta_file(fasta_link[1], fp)
        download_status[i] <- download_result$message} else {
        download_status[i] <- "No FASTA file found"
      }

      # Show progress
      svMisc::progress(value = i, max.value = length(addresses))
    }

  # Handle strains of type subspecies
    ## These strains do not have sequences on their own page--instead they are on the page of the parent taxon
    type_subspecies <- lpsn_organisms %>% dplyr::filter(Species==Subspecies)
    addresses <- type_subspecies$address
    LPSN_ID <- type_subspecies$LPSN_ID
    base_url <- "https://lpsn.dsmz.de"
    download_status <- vector("character", length(addresses))

    for (i in seq_along(addresses)) {
      # Get body of the page
      body <- get_web_page_body(url = addresses[i], user_agent = "me")

      # Extract parent taxon link
      parent_taxon_link <- body %>%
        rvest::html_nodes(xpath = "//b[contains(text(), 'Parent taxon:')]/following-sibling::a[1]") %>%
        rvest::html_attr("href")

      if (!is.na(parent_taxon_link) && length(parent_taxon_link) > 0) {
        # Get URL of parent taxon
        parent_url <- paste0(base_url, parent_taxon_link)
        body <- get_web_page_body(url = parent_url, user_agent = "me")

        # Extract link to FASTA file
        links <- extract_links(body)
        fasta_link <- links[grepl("\\.fasta$", links)]
        fasta_link <- ifelse(grepl("^http", fasta_link), fasta_link, paste0(base_url, fasta_link))

        if (length(fasta_link) > 0) {
          # Construct the file path
          fp <- paste0("LPSN/data/16S_ribosomal_sequences/", LPSN_ID[i], ".fasta")

          # Download the FASTA file
          download_result <- download_fasta_file(fasta_link[1], fp)
          download_status[i] <- download_result$message
        } else {
          download_status[i] <- "No FASTA file found"
        }
      } else {
        download_status[i] <- "No parent taxon link found"
      }

      # Show progress
      svMisc::progress(value = i, max.value = length(addresses))
    }

# === Remove extra sequences ===
    # Type subspecies
    # Extra sequences are those that contain "subsp."
    LPSN_ID <- type_subspecies$LPSN_ID

    for (i in 1:length(LPSN_ID)) {
      fp <- paste0("LPSN/data/16S_ribosomal_sequences/", LPSN_ID[i], ".fasta")

      # Check if the file exists
      if (!file.exists(fp)) {
        message(paste("File not found:", fp, "- Exiting loop."))
        next
      }

      tryCatch({
        fasta_content <- Biostrings::readDNAStringSet(fp, format = "fasta")

        if (length(fasta_content) > 0) {
          # Filter out sequences with "subsp." in their IDs
          filtered_sequences <- fasta_content[!grepl("subsp\\.", names(fasta_content))]

          if (length(filtered_sequences) > 0) {
            # Save the filtered sequences back to the file
            Biostrings::writeXStringSet(filtered_sequences, filepath = fp, format = "fasta")
            message(paste("Processed and saved:", fp))
          } else {
            # Remove the file if no valid sequences remain
            file.remove(fp)
            message(paste("No valid sequences found in:", fp, "- File removed."))
          }
        }
      }, error = function(e) {
        message(paste("Error processing file:", fp, "-", e$message))
      })
    }

    # Most strains
    # Extra sequences are those that contain "subsp.", if there are two or more sequences
    LPSN_ID <- most_strains$LPSN_ID

    for (i in 1:length(LPSN_ID)) {
      fp <- paste0("LPSN/data/16S_ribosomal_sequences/", LPSN_ID[i], ".fasta")

      # Check if the file exists
      if (!file.exists(fp)) {
        message(paste("File not found:", fp, "- Exiting loop."))
        next
      }

      tryCatch({
        fasta_content <- Biostrings::readDNAStringSet(fp, format = "fasta")

        if (length(fasta_content) > 1) {
          # Filter out sequences with "subsp." in their IDs
          filtered_sequences <- fasta_content[!grepl("subsp\\.", names(fasta_content))]

          if (length(filtered_sequences) > 0) {
            # Save the filtered sequences back to the file
            Biostrings::writeXStringSet(filtered_sequences, filepath = fp, format = "fasta")
            message(paste("Processed and saved:", fp))
          } else {
            # Remove the file if no valid sequences remain
            file.remove(fp)
            message(paste("No valid sequences found in:", fp, "- File removed."))
          }
        }
      }, error = function(e) {
        message(paste("Error processing file:", fp, "-", e$message))
      })
    }

# === Put ribosomal sequences in a single object ===
  # Get names of FASTA files downloaded above
  fasta_files <- list.files(paste0("LPSN/data/16S_ribosomal_sequences/"),
                            pattern = "\\.fasta$", full.names = TRUE)

  # Read each FASTA file and return as a list
  fasta_list <- lapply(seq_along(fasta_files), function(i) {
    svMisc::progress(i, max.value = length(fasta_files))
    return(ShortRead::readFasta(fasta_files[i]))
  })

  # Rename FASTA files according to file name (record)
  fasta_names <- list.files(paste0("LPSN/data/16S_ribosomal_sequences/"),
                            pattern = "\\.fasta$", full.names = FALSE)
  fasta_names <- gsub(pattern="\\.fasta$", replacement="", x = fasta_names)

  fasta_list_renamed <-  lapply(seq_along(fasta_list), function(i) {
    fasta <- fasta_list[[i]]
    new_ids  <- Biostrings::BStringSet(rep(fasta_names[i], length(ShortRead::sread(fasta))))
    ShortRead::ShortRead(sread = ShortRead::sread(fasta), id = new_ids)
  })

  # Combine files
  combined_fasta <- combine_fasta(fasta_list_renamed)

  # Put in DNAStringSet
  seq = ShortRead::sread(combined_fasta)
  names(seq) = ShortRead::id(combined_fasta)


# === Add ribosomal sequences to organism data ===
  idx <- match(lpsn_organisms$LPSN_ID, names(seq))
  lpsn_organisms$`16S_ribosomal_sequence` <- NA
  lpsn_organisms$`16S_ribosomal_sequence`[!is.na(idx)] <- as.character(seq[idx[!is.na(idx)]])

# === Export ===
  Biostrings::writeXStringSet(seq, filepath = paste0("LPSN\\data\\16S_ribosomal_sequences.fasta"), format = "fasta")

  write.csv(lpsn_organisms, "LPSN\\data\\lpsn_ribosomal_sequences.csv")
