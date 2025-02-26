# Download Data from Bergey's Manual for App
# This script automates navigation and downloading of HTML files from Bergey's Manual
# It defines functions to control browser actions using keyboard commands, extract URLs, filter
# them based on specific patterns, and save files to local storage.
# Requirements:
# - Packages in install/installPackages.R
# - Programs must be pinned to the Windows taskbar.
# - Browser must be configured with default download settings for selected file types.
# - Access (license) to Bergey's Manual (https://onlinelibrary.wiley.com/doi/book/10.1002/9781118960608)
# Author: Timothy Hackmann
# Date: 13 February 2025

# === Define Functions ===
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

# === Get database directory ===
  database_directory <- FileLocator::getCurrentFileLocation()
  subdirectory <- "/Bergey"
  database_directory <- gsub(paste0(subdirectory, "$"), "", database_directory)

# === Load external R files ===
  setwd(database_directory)
  source("utils\\databaseUtils.R", local = TRUE)

# === Download table of contents ===
  # Set urls
  urls <- c(
    "https://onlinelibrary.wiley.com/browse/book/10.1002/9781118960608/title?startPage=0&pageSize=1000",
    "https://onlinelibrary.wiley.com/browse/book/10.1002/9781118960608/title?startPage=1&pageSize=1000",
    "https://onlinelibrary.wiley.com/browse/book/10.1002/9781118960608/title?startPage=2&pageSize=1000"
    )

  # Loop over urls
  for(i in 1:length(urls))
  {
    #Set sleep
    switch_sleep <- runif(1, min = 1, max = 2)
    navigation_bar_sleep <- runif(1, min = 2, max = 3)
    navigate_sleep <- runif(1, min = 120, max = 130)
    download_sleep <- runif(1, min = 30, max = 40)
    fp = paste0("toc_", i, ".html")

    # Navigate and download
    navigate_and_download(url = urls[i], switch_sleep = switch_sleep, fp = fp, navigation_bar_sleep = navigation_bar_sleep, navigate_sleep = navigate_sleep)

    # Show progress
    svMisc::progress(value = i, max = length(urls))
  }

# === Get urls for articles ===
  # Get urls from table of contents
  article_directory <- paste0(database_directory, "/articles")
  setwd(article_directory)

  # Extract urls and filter out those for genera
  urls <- lapply(html_files, extract_urls)
  urls = unlist(urls)
  urls <- filter_urls(urls = urls, pattern = "^/doi/full/10\\.1002/9781118960608\\.gbm[0-9]{5}(\\.pub[0-9]+[a-z]*)?$")
  urls <- paste0("https:/onlinelibrary.wiley.com", urls)

# === Download articles ===
  # Loop over urls
  for(i in 1:length(urls))
  {
    #Set sleep
    switch_sleep <- runif(1, min = 1, max = 2)
    navigation_bar_sleep <- runif(1, min = 2, max = 3)
    navigate_sleep <- runif(1, min = 10, max = 20)
    download_sleep <- runif(1, min = 10, max = 20)
    fp = paste0(str_extract(urls[i], "gbm[0-9]{5}(\\.pub[0-9]+)?$"), ".html")

    # Navigate and download
    navigate_and_download(url = urls[i], switch_sleep = switch_sleep, fp = fp, navigation_bar_sleep = navigation_bar_sleep, navigate_sleep = navigate_sleep)

    # Show progress
    svMisc::progress(value = i, max = length(urls))
  }
