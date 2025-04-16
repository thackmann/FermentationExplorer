# Get LPSN Phylogeny for App
# This script retrieves phylogeny of organisms from the LPSN database.
# Requirements:
# - Packages in install/installPackages.R
# - Data from getLpsnOrganisms.R script
# Author: Timothy Hackmann
# Date: 17 November 2024

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
