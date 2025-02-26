# Get LPSN Phylogeny for App
# This script retrieves phylogeny of organisms from the LPSN database.
# Requirements:
# - Packages in install/installPackages.R
# - Data from getLpsnOrganisms.R script
# Author: Timothy Hackmann
# Date: 17 November 2024

# === Define functions ===
  #' Extract Taxonomic Ranks from Links
  #'
  #' This function extracts Genus, Family, and other taxonomic ranks from a vector of links.
  #' The links are from the LPSN and follow the format `"/rank/name"`
  #'
  #' @param phylogeny A character vector of links
  #' @param ranks A character vector of  ranks to extract
  #' @return A named character vector with the extracted taxonomic names for each rank. If a rank is not
  #'         present in the phylogeny list, its value will be `NA`.
  #'
  #' @examples
  #' # Example phylogeny input
  #' phylogeny_example <- c(
  #'   "/genus/abditibacterium",
  #'   "/family/abditibacteriaceae",
  #'   "/order/abditibacteriales",
  #'   "/class/abditibacteriia",
  #'   "/phylum/abditibacteriota",
  #'   "/domain/bacteria"
  #' )
  #'
  #' # Desired ranks
  #' ranks <- c("Genus", "Family", "Order", "Class", "Phylum", "Domain")
  #'
  #' # Extract taxonomy
  #' extract_phylogeny(phylogeny_example, ranks)
  #'
  #' @export
  extract_phylogeny <- function(phylogeny, ranks) {
    # Initialize named vector with NA for all ranks
    named_phylogeny <- setNames(rep(NA, length(ranks)), ranks)

    # Loop through the phylogeny elements and match to the ranks
    for (item in phylogeny) {
      # Extract the rank and name (assuming format "/rank/name")
      rank <- stringr::str_extract(item, "(?<=/)[^/]+")  # Get the rank
      name <- stringr::str_extract(item, "[^/]+$")      # Get the name

      # If the rank matches one in our list, assign the name to it
      if (!is.na(rank) && rank %in% tolower(ranks)) {
        named_phylogeny[which(tolower(ranks) == rank)] <- stringr::str_to_title(name)  # Convert name to title case
      }
    }

    return(named_phylogeny)
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
  lpsn_organisms <- read.csv(paste0(app_directory, "LPSN\\data\\lpsn_organisms.csv"))

# === Retrieve phylogeny of type strains ===
  addresses <- lpsn_organisms$address
  base_url <- "https://lpsn.dsmz.de"
  phylogeny = vector("list", length(addresses))

  for (i in seq_along(addresses)) {
    # Visit page of type strain
      # Get body of the page
        url = addresses[i]
        body <- get_web_page_body(url = url)

      # Extract link after "Parent taxon:"
          parent_taxon_link <- body %>%
            rvest::html_nodes(xpath = "//b[contains(text(), 'Parent taxon:')]/following-sibling::a[1]") %>%
            rvest::html_attr("href")

      # Get first parent taxon
      parent_taxonomy = c(parent_taxon_link)

    # Visit pages of parent taxa
    # Repeat until there are no parent taxa left
    repeat {
        # Stop if no parent taxa are found
        if (is.na(parent_taxon_link) || length(parent_taxon_link) == 0) {
          break
        }

        # Get url of parent taxon
        url <- paste0(base_url, parent_taxon_link)

        # Get body of the page
        body <- get_web_page_body(url = url)

        # Extract the parent taxon link
        parent_taxon_link <- body %>%
          rvest::html_nodes(xpath = "//b[contains(text(), 'Parent taxon:')]/following-sibling::a[1]") %>%
          rvest::html_attr("href")

        # Stop if no parent taxa are found
        if (is.na(parent_taxon_link) || length(parent_taxon_link) == 0) {
          break
        }

        # Add the parent taxon link to the taxonomy vector
        parent_taxonomy <- c(parent_taxonomy, parent_taxon_link)
      }

      # Store phylogeny
      phylogeny[[i]] <- parent_taxonomy

      # Show progress of loop
      svMisc::progress(value = i, max.value = length(addresses))
  }

  # Extract phylogeny
  phylogeny_df <- purrr::map_dfr(
    phylogeny,
    ~ extract_phylogeny(.x, c("Domain", "Phylum", "Class", "Order", "Family", "Genus"))
  )

  # Combine phylogeny with species data
  lpsn_organisms <- cbind(lpsn_organisms, phylogeny_df)

# === Export ===
  fp <- paste0(app_directory, "LPSN\\data\\lpsn_phylogeny.csv")
  write.csv(lpsn_organisms, file = fp, row.names = FALSE)
