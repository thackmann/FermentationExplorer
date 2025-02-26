# Get 16S Ribosomal Sequences
# This script downloads the 16S rDNA sequences for type strains of bacterial species from the LPSN database.
# Requirements:
# - Packages in install/installPackages.R
# - Data from getLpsnOrganisms.R script
# Author: Timothy Hackmann
# Date: 17 December 2024

# === Define functions ===
  #' Extract Links from Web Page Body
  #'
  #' This function extracts all hyperlinks (URLs) from the body of a web page.
  #'
  #' @param body An HTML document object, typically returned by functions like `polite::scrape()` or `rvest::read_html()`.
  #' @param tag A character string specifying the HTML tag to search for. Default is `"a"`.
  #' @param attribute A character string specifying the attribute to extract. Default is `"href"`.
  #'
  #' @return A character vector of extracted links.
  #' @export
  #'
  #' @examples
  #' # Example usage:
  #' body <- polite::scrape(polite::bow("https://example.com"))
  #' links <- extract_links(body)
  #'
  extract_links <- function(body, tag = "a", attribute = "href") {
    # Extract specified attributes from the specified HTML tags
    links <- body %>%
      rvest::html_nodes(tag) %>%
      rvest::html_attr(attribute)

    return(links)
  }

  #' Download a FASTA file from a given link
  #'
  #' This function downloads a FASTA file from the provided URL and saves it to the specified file path.
  #' If the download fails, an error message is returned.
  #'
  #' @param fasta_link A character string containing the URL of the FASTA file to download.
  #' @param fp A character string specifying the file path where the downloaded FASTA file will be saved.
  #'
  #' @return A list with the following elements:
  #' \describe{
  #'   \item{success}{A logical value indicating whether the download was successful.}
  #'   \item{filepath}{The file path where the FASTA file was saved (NULL if unsuccessful).}
  #'   \item{message}{A message indicating the status of the operation (e.g., success or error).}
  #' }
  #'
  #' @examples
  #' # Example usage
  #' fasta_link <- "https://example.com/sample.fasta"
  #' fp <- "path/to/save/sample.fasta"
  #' result <- download_fasta_file(fasta_link, fp)
  #' print(result$message)
  #'
  #' @export
  download_fasta_file <- function(fasta_link, fp) {
    tryCatch({
      # Download and save the file
      fasta_file <- httr::GET(fasta_link)
      writeBin(httr::content(fasta_file, "raw"), fp)

      list(success = TRUE, filepath = fp, message = paste("Downloaded:", fp))
    }, error = function(e) {
      list(success = FALSE, filepath = NULL, message = paste("Error:", e$message))
    })
  }

  #' Combine DNAStringSet Objects
  #'
  #' This function combines a list of `DNAStringSet` objects into a single `DNAStringSet`.
  #'
  #' @param fasta_list A list of `DNAStringSet` objects to be combined.
  #'
  #' @return A combined `DNAStringSet` object containing all sequences and their corresponding IDs.
  #' @import Biostrings ShortRead
  #' @export
  #'
  #' @examples
  #' \dontrun{
  #' combined_fasta <- combine_fasta(fasta_list)
  #' }
  combine_fasta <- function(fasta_list) {
    # Combine sequences
    combined_reads <- do.call(c, lapply(fasta_list, ShortRead::sread))

    # Combine IDs
    combined_ids <- do.call(c, lapply(fasta_list, ShortRead::id))

    # Create a new ShortRead object
    ShortRead::ShortRead(sread = combined_reads, id = combined_ids)
  }

  
# === Get database directory ===
  database_directory <- FileLocator::getCurrentFileLocation()
  subdirectory <- "/LPSN"
  database_directory <- gsub(paste0(subdirectory, "$"), "", database_directory)

# === Load external R files ===
  setwd(database_directory)
  source("utils\\databaseUtils.R", local = TRUE)

# === Read in data ===
  setwd(database_directory)

  # Read in data from getLpsnOrganisms.R script
  lpsn_organisms <- utils::read.csv("LPSN\\data\\lpsn_organisms.csv")

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
