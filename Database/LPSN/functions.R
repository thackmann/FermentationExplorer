# Define Functions for Obtaining Data from LPSN
# These are functions specific to this data source
# Author: Timothy Hackmann
# Date: 4 April 2025

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
  
