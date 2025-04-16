# Define Functions for Obtaining Data from Bergey's Manual
# These are functions specific to this data source
# Author: Timothy Hackmann
# Date: 4 April 2025

# === Functions for downloading articles ==== 
  #' Switch to a Program Using Keyboard Shortcut
  #'
  #' Switches to a program pinned to the taskbar by simulating a keyboard shortcut.
  #' #' Requirements
  #' - Pin program to taskbar and specify position in function
  #' @param taksbar_pin A string representing the taskbar pin number of the program (e.g., "1" for the first pinned program).
  #' @param sleep Number of seconds to pause after switching (default is 1 second).
  #' @return None. This function performs a keyboard action.
  #' @export
  switch_to_program <- function(taksbar_pin = "1", sleep = 1) {
    KeyboardSimulator::keybd.press('win', hold = TRUE)
    KeyboardSimulator::keybd.press(taksbar_pin)
    KeyboardSimulator::keybd.release('win')
    Sys.sleep(sleep)
  }
  
  #' Select the Navigation Bar Using Keyboard Shortcut
  #'
  #' Selects the navigation bar in the active program window.
  #' @param sleep Number of seconds to pause after selecting (default is 1 second).
  #' @return None. This function performs a keyboard action.
  #' @export
  select_navigation_bar <- function(sleep = 1) {
    KeyboardSimulator::keybd.press('ctrl', hold = TRUE)
    KeyboardSimulator::keybd.press('l')
    KeyboardSimulator::keybd.release('ctrl')
    Sys.sleep(sleep)
  }
  
  #' Navigate to a URL
  #'
  #' Types a URL into the active program window and presses enter to navigate.
  #' @param url The URL to navigate to (default is "example.com").
  #' @param sleep Number of seconds to pause after navigating (default is 1 second).
  #' @return None. This function performs a keyboard action.
  #' @export
  navigate_to_url <- function(url = "example.com", sleep = 1) {
    KeyboardSimulator::keybd.type_string(string = url)
    KeyboardSimulator::keybd.press('enter')
    Sys.sleep(sleep)
  }
  
  #' Download a File from a URL
  #'
  #' Simulates a download by opening the save dialog, entering a filename, and pressing save.
  #' @param fp The file path where the download will be saved (default is NULL).
  #' @param overwrite Logical; if TRUE, overwrites an existing file (default is TRUE).
  #' @param sleep Number of seconds to pause at different stages (default is 2 seconds).
  #' @return None. This function performs a keyboard action.
  #' @export
  download_url <- function(fp = NULL, overwrite = TRUE, sleep = 2) {
    KeyboardSimulator::keybd.press('ctrl', hold = TRUE)
    KeyboardSimulator::keybd.press('s')
    KeyboardSimulator::keybd.release('ctrl')
    Sys.sleep(sleep)
    
    if (!is.null(fp)) {
      KeyboardSimulator::keybd.type_string(string = fp)
      Sys.sleep(sleep)
    }
    
    KeyboardSimulator::keybd.press('alt', hold = TRUE)
    KeyboardSimulator::keybd.press('s')
    KeyboardSimulator::keybd.release('alt')
    Sys.sleep(sleep)
    
    KeyboardSimulator::keybd.press('alt', hold = TRUE)
    if (overwrite) {
      KeyboardSimulator::keybd.press('y')
    } else {
      KeyboardSimulator::keybd.press('n')
    }
    KeyboardSimulator::keybd.release('alt')
    Sys.sleep(sleep)
  }
  
  #' Navigate to a URL and Download It
  #'
  #' Combines multiple actions to navigate to a URL and download its contents.
  #' Requirements
  #' - Pin browser to taskbar and specify position in function
  #' - Minimize browser window in advance
  #' - Choose download directory in advance
  #' - Choose file type (e.g., complete webpage or *.html only) in advance
  #'  These settings are chosen in in advance by downloading one file manually with desired settings
  #' @param browser_pin The taskbar pin number of the browser (default is "1").
  #' @param url The URL to navigate to.
  #' @param fp The file path where the download will be saved (default is NULL).
  #' @param switch_sleep Pause after switching programs (default is 1 second).
  #' @param navigation_bar_sleep Pause after selecting the navigation bar (default is 2 seconds).
  #' @param navigate_sleep Pause after navigating (default is 3 seconds).
  #' @param download_sleep Pause after downloading (default is 3 seconds).
  #' @return None. This function performs a series of keyboard actions.
  #' @export
  navigate_and_download <- function(browser_pin = "1", url = "example.com", fp = NULL, switch_sleep = 1, navigation_bar_sleep = 2, navigate_sleep = 3, download_sleep = 3) {
    switch_to_program(taksbar_pin = browser_pin, sleep = switch_sleep)
    select_navigation_bar(sleep = navigation_bar_sleep)
    navigate_to_url(url = url, sleep = navigate_sleep)
    download_url(fp = fp, sleep = download_sleep)
    switch_to_program(taksbar_pin = browser_pin, sleep = switch_sleep)
  }
  
  #' Extract URLs from an HTML File
  #'
  #' Reads an HTML file and extracts all URLs in anchor (`<a>`) tags.
  #' @param fp The file path to the HTML file.
  #' @return A character vector of extracted URLs.
  #' @importFrom rvest read_html html_nodes html_attr
  #' @export
  extract_urls <- function(fp) {
    html_content <- rvest::read_html(fp)
    urls <- html_content %>%
      rvest::html_nodes("a") %>%
      rvest::html_attr("href")
    urls <- urls[!is.na(urls)]
    return(urls)
  }
  
  #' Filter URLs Based on a Pattern
  #'
  #' Filters a vector of URLs based on a specified pattern.
  #' @param urls A character vector of URLs.
  #' @param pattern The pattern to filter URLs by.
  #' @return A character vector of URLs that match the pattern.
  #' @importFrom stringr str_detect
  #' @export
  filter_urls <- function(urls, pattern) {
    filtered_urls <- urls[stringr::str_detect(urls, pattern)]
    
    return(filtered_urls)
  }
  
  #' List Files in a Directory Matching a Pattern
  #'
  #' Lists files in a specified directory that match a given pattern.
  #' If no directory is specified, it uses the current working directory.
  #' @param directory The directory to search in. Defaults to the working directory.
  #' @param pattern The pattern to match file names.
  #' @return A character vector of file paths that match the pattern.
  #' @export
  get_matching_files <- function(directory = getwd(), pattern) {
    matching_files <- list.files(path = directory, pattern = pattern, full.names = TRUE)
    
    return(matching_files)
  }

# === Functions for extracting data ==== 
  
  #' Read HTML File
  #'
  #' Reads an HTML file and returns an XML document for parsing.
  #'
  #' @param fp Character string; the file path to the HTML file.
  #' @return An XML document object representing the parsed HTML file.
  #' @importFrom rvest read_html
  #' @export
  get_xml =  function(fp)
  {
    xml_document = rvest::read_html(fp)
    
    return(xml_document)
  }
  
  #' Convert Text to ASCII Format
  #'
  #' Converts text encoding from UTF-8 to Latin-1 to ASCII, removing non-ASCII characters that may interfere with parsing.
  #' Converting from UTF-8 to latin1 to ASCII (in that order) results in fewest characters being removed.
  #'
  #' @param text Character string; the text to be converted.
  #' @return A character string with ASCII-compatible text.
  #' @importFrom base iconv
  #' @export
  convert_formatting = function(text)
  {
    text = iconv(text, from="UTF-8", to="latin1", "")
    text = iconv(text, from="latin1", to="ASCII", "")
    
    return(text)
  }
  
  #' Extract Taxonomy Information
  #'
  #' Extracts taxonomic ranks (e.g., phylum, class, order, family, genus) from an XML document.
  #'
  #' @param xml_document An XML document object representing the parsed HTML content.
  #' @return A character vector containing taxonomic ranks, with each rank in a separate element.
  #' @importFrom rvest html_nodes html_text
  #' @importFrom stringr str_trim
  #' @export
  get_taxonomy = function(xml_document)
  {
    taxonomy = rep(NA, times=5)
    selector = c(".partTile", ".supPartTile", ".subSubpartTile", ".subSubSubpartTile", ".citation__title")
    
    for(i in 1:length(selector))
    {
      # Get ranks
      rank <- rvest::html_nodes(xml_document, selector[i])
      rank <- rvest::html_text(rank)
      if(identical(rank, character(0))){rank="NA"}
      rank = trimws(rank)
      
      #Convert to ASCII formatting
      rank = convert_formatting(rank)
      
      taxonomy[i] = rank
    }
    
    #Clean up formatting
    taxonomy[5]=gsub(pattern="\n[ ]*", replacement="", x=taxonomy[5]) # Remove line breaks and spaces after genus name
    taxonomy[5]=gsub(pattern="[ ]*gen\\. nov\\.", replacement="", x=taxonomy[5]) # Remove "gen. nov."
    taxonomy[5]=gsub(pattern="[ ]+nov\\.", replacement="", x=taxonomy[5]) # Remove " nov."
    taxonomy[5]=gsub(pattern="\"", replacement="", x=taxonomy[5]) # Remove "\"
    taxonomy[5]=gsub(pattern="'", replacement="", x=taxonomy[5]) # Remove "'"
    taxonomy[5]=gsub(pattern="Jeotigalicoccus", replacement="Jeotgalicoccus", x=taxonomy[5]) # Correct spelling error
    taxonomy[5]=gsub(pattern="Hydrogenoanaerobacterium", replacement="Hydrogenanoaerobacterium", x=taxonomy[5]) # Correct spelling error
    
    return(taxonomy)
  }
  
  #' Extract Tables from Document
  #'
  #' Extracts table contents from an XML document.
  #'
  #' @param xml_document An XML document object representing the parsed HTML content.
  #' @return A character vector containing the extracted table text.
  #' @importFrom rvest html_nodes html_text
  #' @export
  get_tables <- function(xml_document)
  {
    tables <- rvest::html_nodes(xml_document, ".article-table-content")
    tables <- rvest::html_text(tables)
    
    return(tables)
  }
  
  #' Extract Abstract Section
  #'
  #' Extracts the abstract text from an XML document.
  #'
  #' @param xml_document An XML document object representing the parsed HTML content.
  #' @return A character string containing the abstract text, or an empty string if not found.
  #' @importFrom rvest html_nodes html_text
  #' @export
  get_abstract =  function(xml_document)
  {
    text <- rvest::html_nodes(xml_document, xpath = '//*[@id="section-1-en"]')
    text <- rvest::html_text(text)
    if(identical(text, character(0))){text=""}
    
    return(text)
  }
  
  #' Extract Full Text Content
  #'
  #' Extracts the main article text from an XML document.
  #'
  #' @param xml_document An XML document object representing the parsed HTML content.
  #' @return A character string containing the full text, or an empty string if not found.
  #' @importFrom rvest html_nodes html_text
  #' @export
  get_full_text =  function(xml_document)
  {
    text <- rvest::html_nodes(xml_document, ".article-row-left")
    text <- rvest::html_text(text)
    if(identical(text, character(0))){text=""}
    
    return(text)
  }
  
  #' Extract Main Text Section
  #'
  #' Extracts the main text section, removing sections before the abstract and after references, acknowledgements, end notes, and further reading.
  #'
  #' @param full_text Character string containing the full text of the document.
  #' @param special_genus Optional; character string for handling special formatting (default is `NULL`).
  #' @return A character string containing the main text section.
  #' @export
  get_main_text = function(full_text, special_genus = NULL)
  {
    text = full_text
    
    #Remove text before abstract
    text = sub(".*\n[ ]*Abstract\n[ ]*","Abstract\n[ ].*", text)
    
    #Remove text after references, acknowledgements, end notes, and further reading
    text = sub("\n[ ]+End [nN]ote[s]*\n.*","", text)
    text = sub("\n[ ]+Acknowledgment[s]*\n.*","", text)
    text = sub("\n[ ]+Reference[s]*\n.*","", text)
    text = sub("\n[ ]+Further [rR]eading\n.*","", text)
    
    # Handle articles with special format
    if(special_genus=="Bacillus"){
      text = full_text
      text = sub(".*\n[ ]*Abstract\n[ ]*","Abstract\n[ ].*", text)
      text = sub("\n[ ]+References\n.*","", text)
    }
    
    return(text)
  }
  
  #' Remove Tables from Full Text
  #'
  #' Removes table content from the full text to obtain a "tableless" version.
  #'
  #' @param full_text Character string containing the full text of the document.
  #' @param tables Character vector containing table text to be removed.
  #' @return A character string containing the full text without tables.
  #' @importFrom base gsub
  #' @export
  get_tableless_text = function(full_text, tables){
    text = full_text
    
    if(length(tables)>0)
    {
      for(i in 1:length(tables)){
        text = gsub(tables[i], "", text, fixed = TRUE)
      }
    }
    
    return(text)
  }
  
  #' Get Text for Genus Description
  #'
  #' Extracts text describing the genus by removing irrelevant sections from the full text.
  #'
  #' @param tableless_text Character string containing the document text without tables.
  #' @return A character string containing the genus description.
  #' @importFrom base sub gsub
  #' @export
  get_genus_text = function(tableless_text){
    text = tableless_text
    
    #Remove text before abstract
    text = sub(".*\n[ ]*Abstract\n[ ]*","Abstract\n[ ].*", text)
    
    #Remove text after "List of [the][tenative] species of the genus . . . "
    text = sub("List of (the )*(tentative )*species of the genus[ ]*.*", "", text)
    
    #Some articles are worded differently, and following applies instead
    text = sub("List of (the )*(tentative )*species in the genus[ ]*.*", "", text)
    text = sub("Characteristics of the species of the genus[ ]*.*", "", text)
    text = gsub("Differentiation of the species of the genus[ ]*.*", "", text)
    
    #Remove text after references, acknowledgements, end notes, and further reading
    text = sub("\n[ ]+End [nN]ote[s]*\n.*","", text)
    text = sub("\n[ ]+Acknowledgment[s]*\n.*","", text)
    text = sub("\n[ ]+Reference[s]*\n.*","", text)
    text = sub("\n[ ]+Further [rR]eading\n.*","", text)
    
    return(text)
  }
  
  #' Get Text for Species Description
  #'
  #' Extracts text describing species by removing irrelevant sections and handling special cases.
  #'
  #' @param tableless_text Character string containing the document text without tables.
  #' @param special_genus Optional; character string specifying a genus with special formatting (default is `NULL`).
  #' @return A character string containing the species description.
  #' @importFrom base sub gsub
  #' @export
  get_species_text = function(tableless_text, special_genus = NULL){
    text = tableless_text
    
    # Remove text before abstract
    text = sub(".*\n[ ]*Abstract\n[ ]*","Abstract\n[ ].*", text)
    
    # Remove text before "List of [the][tenative] species of the genus . . . "
    text = sub(".*List of (the )*(tentative )*species of the genus[ ]*", "List of species of the genus ",text)
    
    # Some articles are worded differently, and following applies instead
    text = sub(".*List of species in the genus[ ]*", "List of species of the genus ",text)
    text = sub(".*Characteristics of the species of the genus[ ]*", "List of species of the genus ",text)
    
    # Remove text after species incertae sedis
    text = sub("\n[ ]+Species incertae sedis.*","",text)
    
    # Remove text after references, acknowledgements, end notes, and further reading
    text = sub("\n[ ]+End [nN]ote[s]*\n.*","",text)
    text = sub("\n[ ]+Acknowledgment[s]*\n.*","",text)
    text = sub("\n[ ]+Reference[s]*\n.*","",text)
    text = sub("\n[ ]+Further [rR]eading\n.*","",text)
    
    # Handle articles with special format
    if(special_genus=="Thiohalorhabdus")
    {
      text = sub(pattern="Thiohalolorhabdus", replacement="Thiohalorhabdus", x=text) # Correct spelling error
    }
    if(special_genus=="Anaerosinus")
    {
      text = sub(".*List of species of Anaerosinus", "List of species of Anaerosinus",text) # Handle unique wording
    }
    
    # Convert to ASCII formatting
    text = convert_formatting(text)
    
    return(text)
  }
  
  #' Combine Patterns into a Single Regular Expression
  #'
  #' Combines multiple patterns into a single pattern with `|` (OR) operator.
  #'
  #' @param ... Character strings representing individual patterns.
  #' @return A character string containing the combined pattern.
  #' @export
  combine_patterns <- function(...) {
    patterns <- list(...)
    paste0("(", paste(patterns, collapse = ")|("), ")")
  }
  
  #' Set Pattern for Genus Name in Text
  #'
  #' Creates a regular expression pattern to match genus names, accounting for common formatting variations.
  #'
  #' @param genus Character string of the genus name.
  #' @return A character string containing the genus name pattern.
  #' @importFrom base gsub
  #' @export
  set_genus_pattern = function(genus)
  {
    pattern = genus
    pattern = paste("(Candidatus )*", pattern, sep="") #Give option for "Candidatus " to appear in genus name
    pattern = gsub(" gen. nov[.]*", "", pattern) #Remove " gen. nov[.]"
    pattern = gsub("[Ii]ncertae [sS]edis[ IVX.]* ", "", pattern) #Remove "Incertae Sedis [Roman numeral] "
    pattern = gsub(" corrig", "", pattern) #Remove " corrig"
    pattern = gsub('"', "", pattern) #Remove quotation mark (")
    pattern = gsub("'", "", pattern) #Remove single quotation mark (')
    pattern = gsub("Candidatus", "Ca[.]*(ndidatus)*[ ]*", pattern) #Give option to abbreviate "Candidatus" as "Ca." and for a space to be present between Candidatus and genus name
    
    return(pattern)
  }
  
  #' Set Pattern for Organism Names in Text (Pattern 1)
  #'
  #' The pattern is a line starting with a genus name, followed by a species name, then ending with 1) subspecies, serovar, or pathovar name or 2) a line break
  #'
  #' @param genus Character string of the genus name.
  #' @return A character string containing the organism name pattern.
  #' @export
  set_name_pattern_1 <- function(genus) {
    # Define components of the patterns
    start_pattern <- "\\n[ ]+[\"]*"
    genus_pattern <- set_genus_pattern(genus)
    species_pattern <- "[ ][a-z\"*]+"
    subspecies_pattern <- "\\n*\\s*subsp[\\.]*\\s*[a-z\"*]+"
    serovar_pattern <- "\\n*\\s*serovar\\n*\\s*[a-zA-Z]+(?: [A-Z] )?"
    pathovar_pattern <- "\\s*\\n*\\s*pathovar\\s*\\n*\\s*[a-z\"*]+"
    genomospecies_pattern <- "\\s*\\n*\\s*genomospecies\\s*\\n*\\s*[1-9]+"
    biovar_pattern = "\\n*\\s*biovar\\s*[a-z\"*]+"
    comb_nov_pattern <- "[ ]*comb\\. nov\\."
    end_pattern <- "\n"
    
    # Define patterns
    # Name including subspecies
    # https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm00705
    name_pattern_1 <- paste0(start_pattern, genus_pattern, species_pattern, subspecies_pattern)
    
    # Name including subspecies and serovar
    # https://onlinelibrary.wiley.com/doi/10.1002/9781118960608.gbm01166
    name_pattern_2 <- paste0(start_pattern, genus_pattern, species_pattern, subspecies_pattern, serovar_pattern)
    
    # Name including pathovar
    # https://onlinelibrary.wiley.com/doi/10.1002/9781118960608.gbm01210
    # https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm01239
    name_pattern_3 <-paste0(start_pattern, genus_pattern, species_pattern, pathovar_pattern)
    
    # Name including genomospecies
    # https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm00798
    # https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm00938
    # https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm00888
    name_pattern_4 <- paste0(start_pattern, genus_pattern, genomospecies_pattern)
    
    # Name including by "comb. nov.", then ending with a line break
    # https://onlinelibrary.wiley.com/doi/10.1002/9781118960608.gbm01574
    # https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm01871
    # https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm01874
    # https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm01657
    name_pattern_5 =  paste0(start_pattern, genus_pattern, species_pattern, comb_nov_pattern, end_pattern)
    
    # Name ending with line break
    # https://onlinelibrary.wiley.com/doi/10.1002/9781118960608.gbm01147
    name_pattern_6 <- paste0(start_pattern, genus_pattern, species_pattern, end_pattern)
    
    # Name including biovar, then ending with a line break
    # https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm01081
    name_pattern_7 <- paste0(start_pattern, genus_pattern, species_pattern, biovar_pattern, end_pattern)
    
    # Combine patterns (order is important)
    pattern <- combine_patterns(name_pattern_1, name_pattern_2, name_pattern_3, name_pattern_4, name_pattern_5, name_pattern_6, name_pattern_7)
    
    return(pattern)
  }
  
  #' Set Pattern for Organism Names in Text (Pattern 2)
  #'
  #' The pattern is a line starting with genus name and being preceded by a line ending in "Candidatus"
  #' Example: https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm01141
  #'
  #' @param genus Character string of the genus name.
  #' @return A character string containing the organism name pattern.
  #' @export
  set_name_pattern_2 = function(genus)
  {
    # Define components of the patterns
    start_pattern <- "Candidatus[\n ]+"
    
    genus_pattern = gsub("Candidatus ", "", genus) #Remove "Candidatus"
    species_pattern <- "[ ][a-z\"*]+"
    
    # Define pattern
    pattern = paste0(start_pattern, genus_pattern, species_pattern)
    
    return(pattern)
  }
  
  #' Set Pattern for Organism Names in Text (Pattern 3)
  #'
  #' The pattern is any genus name and species name following "Type species: "
  #' Example: https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm00623
  #'
  #' @param genus Character string of the genus name.
  #' @return A character string containing the organism name pattern.
  #' @export
  set_name_pattern_3 = function(genus)
  {
    start_pattern <- "Type species:[ ]"
    genus_pattern <- set_genus_pattern(genus)
    species_pattern <- "[ ][a-z\"*]+"
    
    pattern =  paste0(start_pattern, genus_pattern, species_pattern)
    
    return(pattern)
  }
  
  #' Set Pattern for Organism Names in Text (Pattern 4)
  #'
  #' The pattern is any line that begins with genus name
  #' Example: https://onlinelibrary.wiley.com/doi/10.1002/9781118960608.gbm00530
  #' Example: https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm00646
  #' Example: https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm00001
  #'
  #' @param genus Character string of the genus name.
  #' @return A character string containing the organism name pattern.
  #' @export
  set_name_pattern_4 = function(genus)
  {
    start_pattern <- "\\n[ ]+[\"]*"
    genus_pattern <- set_genus_pattern(genus)
    species_pattern <- "[ ][a-z\"*]+"
    subspecies_pattern <- "\\n*\\s*subsp[\\.]*\\s*[a-z\"*]+"
    
    name_pattern_1 <- paste0(start_pattern, genus_pattern, species_pattern, subspecies_pattern)
    name_pattern_2 <- paste0(start_pattern, genus_pattern, species_pattern)
    
    pattern <- combine_patterns(name_pattern_1, name_pattern_2)
    
    return(pattern)
  }
  
  #' Extract Names Matching a Pattern
  #'
  #' This function extracts names from a text based on a specified pattern.
  #'
  #' @param pattern A regular expression pattern to match names.
  #' @param text Character string containing the text to search.
  #' @return A character vector of names found or NULL if no matches.
  #' @importFrom base gregexpr regmatches
  #' @export
  extract_names <- function(pattern, text) {
    matches = gregexpr(pattern, text)
    names = unlist(regmatches(text, matches))
    if (matches[[1]][1] != -1) {
      return(names)
    }
    return(NULL)
  }
  
  #' Get Organism Names
  #'
  #' This function extracts genus, species, and subspecies names of each organism from text.
  #'
  #' @param genus Character string of the genus name.
  #' @param species_text Character string containing the text to search.
  #' @param special_genus Optional; character string for handling special genus formatting.
  #' @return A character vector of organism names or NA if no matches are found.
  #' @export
  get_organism_names = function(genus, species_text, special_genus = NULL)
  {
    text = species_text
    patterns = list(set_name_pattern_1(genus), set_name_pattern_2(genus),
                    set_name_pattern_3(genus), set_name_pattern_4(genus))
    
    # Try matching names with each pattern
    # If matches found, remaining patterns not used
    # Handle articles with special format
    if (special_genus=="Bacillus") {
      # Pattern 4 only is used
      names = extract_names(set_name_pattern_4(genus), text)
      if (!is.null(names)) {
        return(names)
      }
    }
    
    if (special_genus=="Ornithinibacillus") {
      # Remove duplicated names
      names = extract_names(set_name_pattern_1(genus), text)
      names =  unique(names)
      if (!is.null(names)) {
        return(names)
      }
    }
    
    # Handle all other articles
    for (pattern in patterns) {
      names = extract_names(pattern, text)
      if (!is.null(names)) {
        return(names)
      }
    }
    
    # Return NA if no match
    return(NA)
  }
  
  
  #' Get Species Descriptions
  #'
  #' This function extracts descriptions for each species based on matched species names.
  #'
  #' @param names A character vector of species names.
  #' @param species_text Character string containing the full species text.
  #' @return A character vector of species descriptions for each species name.
  #' @importFrom base gsub paste0 regexpr
  #' @export
  get_species_descriptions = function(names, species_text) {
    
    # Initialize the species description vector
    species_descriptions = vector("character", length(names))
    
    # Loop through each species name
    for (i in seq_along(names)) {
      
      # Pattern to match the species name and extract text
      current_pattern = paste0(gsub("*", "[**]", names[i], fixed = TRUE), ".*")
      species_descriptions[i] = extract_description(current_pattern, species_text)
      
      # Remove text after the description, if there's a next species
      if (i < length(names) && !is.na(species_descriptions[i])) {
        next_pattern = paste0(".*", names[i + 1])
        next_match = regexpr(next_pattern, species_descriptions[i])
        
        # Trim the description up to the next species name
        if (next_match[1] != -1) {
          species_descriptions[i] = regmatches(species_descriptions[i], next_match)
          species_descriptions[i] = gsub(names[i + 1], "\n", species_descriptions[i])
        }
      }
      
      # Handle case where no match is found
      if (is.na(species_descriptions[i])) {
        species_descriptions[i] = NA
      }
    }
    
    return(species_descriptions)
  }
  
  #' Get Species Name
  #'
  #' Extracts the species name from the full organism name, removing genus name and other elements.
  #'
  #' @param genus Character string of the genus name.
  #' @param name Character string containing the full organism name.
  #' @return A character string containing the species name.
  #' @importFrom base gsub sub
  #' @export
  get_species_name = function(genus, name){
    #Remove genus name
    genus =  set_genus_pattern(genus)
    pattern = paste(".*", genus, " ", sep="")
    name = sub(pattern, "", name)
    
    #Clean up formatting of species name
    #Remove "\n" and spaces
    name = gsub("\n[ ]*", "", name)
    
    #Remove "\"
    name = gsub("\"", "", name)
    
    #Remove "'"
    name = gsub("'", "", name)
    
    #Remove "*"
    name = gsub("\\*", "", name)
    
    #Add back a space between species name and subspecies
    name = gsub("subsp", " subsp", name)
    
    #Add back a space between species name and pathovar
    name = gsub("pathovar", " pathovar", name)
    
    #Remove all text after species name
    name = strsplit(unlist(name), " ")[[1]][1]
    
    return(name)
  }
  
  #' Get Subspecies Name
  #'
  #' Extracts the subspecies name, serovar, pathovar, or other additional identifiers from the organism name.
  #'
  #' @param genus Character string of the genus name.
  #' @param name Character string containing the full organism name.
  #' @return A character string containing the subspecies name or other identifier, or NA if not found.
  #' @importFrom base gsub trimws
  #' @export
  get_subspecies_name = function(genus, name){
    if(is.na(name))
    {
      return(NA)
    }
    
    #Subspecies name is from organism name
    
    #Match subspecies names
    subspecies_pattern <- "\\n*\\s*subsp[\\.]*\\s*[a-z\"*]+"
    serovar_pattern <- "\\n*\\s*serovar\\n*\\s*[a-zA-Z]+(?: [A-Z] )?"
    pathovar_pattern <- "\\s*\\n*\\s*pathovar\\s*\\n*\\s*[a-z\"*]+"
    genomospecies_pattern <- "\\s*\\n*\\s*genomospecies\\s*\\n*\\s*[1-9]+"
    biovar_pattern <- "\\n*\\s*biovar\\s*[a-z\"*]+"
    
    pattern <- combine_patterns(subspecies_pattern, serovar_pattern, pathovar_pattern, genomospecies_pattern, biovar_pattern)
    
    #Find matches and extract names
    name = extract_names(pattern, name)
    
    #Clean up formatting of subspecies name
    #Remove "subsp."
    name = gsub("subsp[.]*[ ]*(nov)*[.]*[ ]+", "", name)
    
    #Remove "serovar"
    name = gsub("serovar\\n*\\s*", "", name)
    
    #Remove "pathovar"
    name = gsub("pathovar\\s*\\n*\\s", "", name)
    
    #Remove "genomospecies"
    name = gsub("genomospecies\\s*\\n*\\s*", "", name)
    
    #Remove "biovar"
    name = gsub("biovar[\\.]*\\s*", "", name)
    
    #Remove "\n" and spaces
    name = gsub("\n[ ]*", "", name)
    
    # Remove leading and trailing whitespaces
    name = trimws(name)
    
    #Remove "\"
    name = gsub("\"", "", name)
    
    #Remove "*"
    name = gsub("\\*", "", name)
    
    # Concatenate names
    name = paste(name, collapse=" ")
    
    if (name == "") {
      return(NA)
    }
    
    return(name)
  }
  
  #' Extract Description Matching a Pattern
  #'
  #' Extracts a description from text based on a specified pattern.
  #'
  #' @param pattern A regular expression pattern to match the desired description.
  #' @param text Character string containing the full text to search.
  #' @return A character string with the extracted description, or NA if no match is found.
  #' @importFrom base regexpr regmatches
  #' @export
  extract_description <- function(pattern, text) {
    matches = regexpr(pattern, text)
    if (matches[1] != -1) {
      return(regmatches(text, matches))
    }
    return(NA)
  }
  
  #' Set Strain Pattern (Pattern 1)
  #'
  #' The pattern is a line starting with "Type strain:" or "Type strains:"
  #' The pattern also allows for newline characters and spaces.
  #' Example (standard format):  https://onlinelibrary.wiley.com/doi/10.1002/9781118960608.gbm01147
  #' Example ("Type strains" in plural):  https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm00120.pub2
  #' Example (extra newlines and spaces):  https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm00919.pub2
  #' Example (extra newlines and spaces):  https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm02082
  #'
  #' @return A character string representing the regular expression for type strain.
  #' @export
  set_strain_pattern_1 <- function() {
    pattern <- "\\n\\s*Type strain[s]*\\n*\\s*:\\s*(.*?)\\n"
    
    return(pattern)
  }
  
  #' Set Strain Pattern (Pattern 2)
  #'
  #' The pattern is a line starting with "Deposited strain:"
  #'
  #' @return A character string representing the regular expression for type strain.
  #' @export
  set_strain_pattern_2 <- function() {
    pattern <- "\\n\\s*Deposited strain[s]*\\n*\\s*:\\s*(.*?)\\n"
    
    return(pattern)
  }
  
  #' Set Strain Pattern (Pattern 3)
  #'
  #' The pattern is a line starting with "Reference strain:"
  #'
  #' @return A character string representing the regular expression for type strain.
  #' @export
  set_strain_pattern_3 <- function() {
    pattern <- "\\n\\s*Reference strain[s]*\\n*\\s*:\\s*(.*?)\\n"
    
    return(pattern)
  }
  
  #' Set Strain Pattern (Pattern 4)
  #'
  #' The pattern is a line starting with "Proposed type strain:"
  #'Example:  https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm01739
  #'
  #' @return A character string representing the regular expression for type strain.
  #' @export
  set_strain_pattern_4 <- function() {
    pattern <- "\\n\\s*Proposed type strain[s]*\\n*\\s*:\\s*(.*?)\\n"
    
    return(pattern)
  }
  
  #' Set Strain Pattern (Pattern 5)
  #'
  #' The pattern is a line starting with "Proposal Type strain:"
  #'Example:  https://onlinelibrary.wiley.com/doi/10.1002/9781118960608.gbm01989
  #'
  #' @return A character string representing the regular expression for type strain.
  #' @export
  set_strain_pattern_5 <- function() {
    pattern <- "\\n\\s*Proposal Type strain[s]*\\n*\\s*:\\s*(.*?)\\n"
    
    return(pattern)
  }
  
  #' Set Strain Pattern (Pattern 6)
  #'
  #' The pattern is a line starting with "Type material"
  #' Example: https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm01951
  #'
  #' @return A character string representing the regular expression for type strain.
  #' @export
  set_strain_pattern_6 <- function() {
    pattern <- "\\n\\s*Type material\\n*\\s*:\\s*(.*?)\\n"
    
    return(pattern)
  }
  
  #' Set Strain Pattern (Pattern 7)
  #'
  #' The pattern is a line starting with "Strain:"
  #'Example:  https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm01273.pub2
  #'
  #' @return A character string representing the regular expression for type strain.
  #' @export
  set_strain_pattern_7 <- function() {
    pattern <- "\\n\\s*Strain[s]*\\n*\\s*:\\s*(.*?)\\n"
    
    return(pattern)
  }
  
  #' Set Strain Pattern (Pattern 8)
  #'
  #' The pattern is a line starting with "Original strain:"
  #'Example:  https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm00530
  #'
  #' @return A character string representing the regular expression for type strain.
  #' @export
  set_strain_pattern_8 <- function() {
    pattern <- "\\n\\s*Original strain[s]*\\n*\\s*:\\s*(.*?)\\n"
    
    return(pattern)
  }
  
  #' Set Strain Pattern (Pattern 9)
  #'
  #' The pattern is a line starting with "Representative strain:"
  #'Example:  https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm00530
  #'
  #' @return A character string representing the regular expression for type strain.
  #' @export
  set_strain_pattern_9 <- function() {
    pattern <- "\\n\\s*Representative strain[s]*\\n*\\s*:\\s*(.*?)\\n"
    
    return(pattern)
  }
  
  #' Set Strain Pattern (Pattern 10)
  #'
  #' The pattern is a string (not a new line) starting with "Type strain:"
  #' Example:  https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm00965
  #'
  #' @return A character string representing the regular expression for type strain.
  #' @export
  set_strain_pattern_10 <- function() {
    pattern <- "Type strain[s]*\\n*\\s*:\\s*(.*?)\\n"
    
    return(pattern)
  }
  
  #' Set Strain Pattern (Pattern 11)
  #'
  #' The pattern is a line starting with "The type strain is" (without a colon)
  #'Example:  https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm00607
  #'
  #' @return A character string representing the regular expression for type strain.
  #' @export
  set_strain_pattern_11 <- function() {
    pattern <- "\\n\\s*The type strain is\\n*\\s*(.*?)\\n"
    
    return(pattern)
  }
  
  
  #' Set Strain Pattern (Pattern 12)
  #'
  #' The pattern is a line starting with "Type strain " (without a colon)
  #'Example:  https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm00530
  #'Example:  https://onlinelibrary.wiley.com/doi/full/10.1002/9781118960608.gbm00021
  #'
  #' @return A character string representing the regular expression for type strain.
  #' @export
  set_strain_pattern_12 <- function() {
    pattern <- "\\n\\s*Type strain[s]*\\n*\\s*(.*?)\\n"
    
    return(pattern)
  }
  
  #' Extract Strain IDs from Text
  #'
  #' Extracts strain IDs from text based on a list of patterns.
  #'
  #' @param patterns A list of regular expression patterns for strain IDs.
  #' @param text Character string containing the text to search.
  #' @return A character vector of strain IDs or NA if no matches are found.
  #' @importFrom base gregexpr regmatches
  #' @export
  extract_strain_ID <- function(patterns, text) {
    for (pattern in patterns) {
      matches <- gregexpr(pattern, text, perl = TRUE)
      strain_ID <- unlist(regmatches(text, matches))
      if (matches[[1]][1] != -1) {
        return(strain_ID)
      }
    }
    return(NA)
  }
  
  #' Clean Strain ID Formatting
  #'
  #' Cleans and standardizes the formatting of strain IDs, removing unwanted characters and correcting known issues.
  #'
  #' @param strain_ID Character string of strain IDs to clean.
  #' @return A character vector of cleaned strain IDs or NA if no valid IDs are found.
  #' @importFrom base gsub strsplit trimws
  #' @export
  clean_strain_ID <- function(strain_ID)
  {
    # Remove extraneous text around strain ID
    strain_ID = gsub("character...", NA, strain_ID) #Replace case of no match ["character" or "character(0)"] with "NA"
    strain_ID <- gsub("\\n\\s*Type strain[s]*\\n*\\s*:\\s*", "", strain_ID, perl = TRUE) # Remove "Type strain:"
    strain_ID <- gsub("\\n\\s*Deposited strain[s]*\\n*\\s*:\\s*", "", strain_ID, perl = TRUE) # Remove "Deposited strain:"
    strain_ID <- gsub("\\n\\s*Reference strain[s]*\\n*\\s*:\\s*", "", strain_ID, perl = TRUE) # Remove "Reference strain:"
    strain_ID <- gsub("\\n\\s*Proposed type strain[s]*\\n*\\s*:\\s*", "", strain_ID, perl = TRUE) # Remove "Proposed type strain:"
    strain_ID <- gsub("\\n\\s*Proposal Type strain[s]*\\n*\\s*:\\s*", "", strain_ID, perl = TRUE) # Remove "Proposal Type strain:"
    strain_ID <- gsub("\\n\\s*Type material\\n*\\s*:\\s*", "", strain_ID, perl = TRUE) # Remove "Type material:"
    strain_ID <- gsub("\\n\\s*Strain[s]*\\n*\\s*:\\s*", "", strain_ID, perl = TRUE) # Remove "Strain:"
    strain_ID <- gsub("\\n\\s*Original strain[s]*\\n*\\s*:\\s*", "", strain_ID, perl = TRUE) # Remove "Original strain:"
    strain_ID <- gsub("\\n\\s*Representative strain[s]*\\n*\\s*:\\s*", "", strain_ID, perl = TRUE) # Remove "Representative strain:"
    strain_ID <- gsub("Type strain[s]*\\n*\\s*:\\s*", "", strain_ID, perl = TRUE) # Remove "Type strain:" (without newline)
    strain_ID <- gsub("\\n\\s*The type strain is\\n*\\s*", "", strain_ID, perl = TRUE) # Remove "The type strain is"
    strain_ID <- gsub("\\n\\s*Type strain[s]*\\n*\\s*", "", strain_ID, perl = TRUE) # Remove "Type strain" (no colon)
    
    strain_ID <- gsub("\\(proposed\\)", "", strain_ID) # Remove "(proposed)"
    
    strain_ID = gsub("[.]\n.*", "", strain_ID) #Remove text after ".\n"
    strain_ID = gsub("[.]\n", "", strain_ID) #Remove ".\n"
    strain_ID = gsub('c[(]["]', "", strain_ID) #Remove 'c("'
    strain_ID = gsub('")', "", strain_ID) #Remove '")'
    strain_ID = gsub('\"', "", strain_ID) #Remove '\"'
    
    # Remove "T" when added at end to signify a type strain
    strain_ID = gsub('(DSM )([0-9]*)T', "\\1\\2", strain_ID) #Remove 'T' in "DSM <strain number>T"
    strain_ID = gsub('(ATCC )([0-9]*)T', "\\1\\2", strain_ID) #Remove 'T' in "ATCC <strain number>T""
    strain_ID = gsub('(JCM )([0-9]*)T', "\\1\\2", strain_ID) #Remove 'T' in "JCM <strain number>T""
    strain_ID = gsub('(CIP )([0-9]*)T', "\\1\\2", strain_ID) #Remove 'T' in "CIP <strain number>T""
    strain_ID = gsub('(NCTC )([0-9]*)T', "\\1\\2", strain_ID) #Remove 'T' in "NCTC <strain number>T""
    strain_ID = gsub('(NCIB )([0-9]*)T', "\\1\\2", strain_ID) #Remove 'T' in "NCIB <strain number>T""
    strain_ID = gsub('(KCTC )([0-9]*)T', "\\1\\2", strain_ID) #Remove 'T' in "KCTC <strain number>T""
    
    #Replace "no culture isolated" or similar language with "NA"
    strain_ID = gsub("[Dd]escription, not isolated in axenic culture", "None", strain_ID)
    strain_ID = gsub("[Dd]escription, not isolated in axenic culture", "None", strain_ID)
    strain_ID = gsub("[Dd]escriptions and illustrations serving as type", "None", strain_ID)
    strain_ID = gsub("[Dd]escriptions and illustrations serving as type", "None", strain_ID)
    strain_ID = gsub("[Nn]o culture", "None", strain_ID)
    strain_ID = gsub("[Nn]o culture available", "None", strain_ID)
    strain_ID = gsub("[Nn]o culture has been isolated", "None", strain_ID)
    strain_ID = gsub("[Nn]o culture has been isolated", "None", strain_ID)
    strain_ID = gsub("[Nn]o culture isolated", "None", strain_ID)
    strain_ID = gsub("[Nn]o longer in culture", "None", strain_ID)
    strain_ID = gsub("[Nn]o pure culture has been isolated", "None", strain_ID)
    strain_ID = gsub("[Nn]o pure culture", "None", strain_ID)
    strain_ID = gsub("[Nn]o strain extant", "None", strain_ID)
    strain_ID = gsub("[Nn]o strain isolated", "None", strain_ID)
    strain_ID = gsub("[Nn]o type culture currently available", "None", strain_ID)
    strain_ID = gsub("[Nn]o type culture is currently available", "None", strain_ID)
    strain_ID = gsub("[Nn]o type material is available", "None", strain_ID)
    strain_ID = gsub("[Nn]o type material available", "None", strain_ID)
    strain_ID = gsub("[Nn]o type strain available", "None", strain_ID)
    strain_ID = gsub("[Nn]o type strain is available", "None", strain_ID)
    strain_ID = gsub("[Nn]o type", "None", strain_ID)
    strain_ID = gsub("[Nn]o type", "None", strain_ID)
    strain_ID = gsub("[Nn]one available", "None", strain_ID)
    strain_ID = gsub("[Nn]one cultivated", "None", strain_ID)
    strain_ID = gsub("[Nn]one designated", "None", strain_ID)
    strain_ID = gsub("[Nn]one has been designated", "None", strain_ID)
    strain_ID = gsub("[Nn]one isolated", "None", strain_ID)
    strain_ID = gsub("[Nn]one", "None", strain_ID)
    strain_ID = gsub("[Nn]on-specified due to difficulties in cultivation", "None", strain_ID)
    strain_ID = gsub("[Nn]ot avialable", "None", strain_ID)
    strain_ID = gsub("[Nn]ot cultivated; none designated", "None", strain_ID)
    strain_ID = gsub("[Nn]ot cultivated", "None", strain_ID)
    strain_ID = gsub("[Nn]ot yet grown in pure culture", "None", strain_ID)
    strain_ID = gsub("[Nn]ot designated", "None", strain_ID)
    strain_ID = gsub("[Nn]ot established", "None", strain_ID)
    strain_ID = gsub("[Nn]ot cultivated", "None", strain_ID)
    strain_ID = gsub("(?<!\\S)-(?!\\S)", "None", strain_ID, perl=TRUE) #"-" if not preceded or followed by any character (except space)
    
    strain_ID = gsub("[Ss]train", "", strain_ID) #Remove "Strain"
    strain_ID = gsub("[(]neopathotype strain[)]", "", strain_ID) #Remove "(neopathotype strain)"
    strain_ID = gsub("[Hh]olotype", "", strain_ID) #Remove "holotype"
    strain_ID = gsub("[Nn]eotype", "", strain_ID) #Remove "neotype"
    strain_ID = gsub("[Tt]ype", "", strain_ID) #Remove "type"
    strain_ID = gsub("[Ff]ormerly", "", strain_ID) #Remove "formely"
    strain_ID = gsub("[Nn]ow", "", strain_ID) #Remove "now"
    
    #Delineate strains with commas
    #Strains are already delineated with commas in most articles, but not all
    #Replace any characters that can delineate strains with commas
    strain_ID = gsub(', "', ", ", strain_ID) #Replace ', "' with ', '
    strain_ID = gsub("[(]", ",", strain_ID) #Replace parentheses with ","
    strain_ID = gsub("\\[", ",", strain_ID) #Replace parentheses with ","
    strain_ID = gsub("]", ",", strain_ID) #Replace bracket with ","
    strain_ID = gsub("[)]", ",", strain_ID) #Replace bracket with ","
    strain_ID = gsub("=", ",", strain_ID) #Replace "=" with ","
    strain_ID = gsub(";", ",", strain_ID) #Replace ";" with ","
    
    #Split into list at commas
    strain_ID = strsplit(strain_ID, ",")[[1]]
    
    #Clean up formatting again
    strain_ID = gsub("[(].*[)][ ]*", "", strain_ID) #Remove any paranthetical statements not removed above
    strain_ID = trimws(strain_ID) #Remove leading and trailing spaces
    strain_ID = strain_ID[!strain_ID %in% ""] #Remove any empty elements
    
    # Remove elements with five or more spaces (likely full sentences)
    strain_ID <- strain_ID[!grepl("^([^ ]*[ ]){5,}[^ ]*$", strain_ID)]
    
    # Handle the case where no strain ID is found
    if (length(strain_ID) == 0) {
      strain_ID <- NA
    }
    
    return(strain_ID)
  }
  
  #' Get Strain ID
  #'
  #' Extracts the type strain ID for a given organism from its species description using predefined patterns.
  #'
  #' @param name Character string representing the name of the organism.
  #' @param species_description Character string containing the species description text.
  #' @param special_genus Optional; character string for handling special formatting (default is `NULL`).
  #' @return A character vector containing the extracted strain IDs, or `NA` if no strain ID is found.
  #' @details This function uses multiple patterns to locate the type strain ID within the species description.
  #' The `clean_strain_ID` function is applied to standardize and clean the extracted strain ID format.
  #' @importFrom stringr str_extract
  #' @export
  get_strain_ID = function(name, species_description, special_genus = NULL){
    if(is.na(name)|is.na(species_description)){
      return(NA)
    }
    
    # Extract IDs
    patterns <- list(set_strain_pattern_1(), set_strain_pattern_2(), set_strain_pattern_3(),
                     set_strain_pattern_4(), set_strain_pattern_5(), set_strain_pattern_6(),
                     set_strain_pattern_7(), set_strain_pattern_8(), set_strain_pattern_9(),
                     set_strain_pattern_10(), set_strain_pattern_11(), set_strain_pattern_12())
    strain_ID <- extract_strain_ID(patterns, species_description)
    
    # Clean up formatting
    strain_ID = clean_strain_ID(strain_ID)
    
    return(strain_ID)
  }
  
  #' List Files in a Directory Matching a Pattern
  #'
  #' Lists files in a specified directory that match a given pattern.
  #' If no directory is specified, it uses the current working directory.
  #' @param directory The directory to search in. Defaults to the working directory.
  #' @param pattern The pattern to match file names.
  #' @return A character vector of file paths that match the pattern.
  #' @export
  get_matching_files <- function(directory = getwd(), pattern) {
    matching_files <- list.files(path = directory, pattern = pattern, full.names = TRUE)
    
    return(matching_files)
  }
  
  #' Remove Extra Species Entries
  #'
  #' Filters a data frame to remove entries with no subspecies name when a subspecies is present in other entries with the same genus and species.
  #'
  #' @param data A data frame containing taxonomic information, including columns `Genus`, `Species`, and `Subspecies`.
  #' @return A filtered data frame with extra species entries removed.
  #' @importFrom dplyr group_by filter ungroup
  #' @export
  remove_extra_species <- function(data) {
    data = data %>%
      dplyr::group_by(Genus, Species) %>%
      dplyr::filter(!(is.na(Subspecies) & any(!is.na(Subspecies)))) %>%
      dplyr::ungroup()
    
    return(data)
  }
