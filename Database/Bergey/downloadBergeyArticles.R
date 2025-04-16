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

# === Get database directory ===
  database_directory <- FileLocator::getCurrentFileLocation()
  subdirectory <- "/Bergey"
  database_directory <- gsub(paste0(subdirectory, "$"), "", database_directory)

# === Load external R files ===
  setwd(database_directory)
  source("functions\\helperFunctions.R", local = TRUE)
  source("Bergey\\functions.R", local = TRUE)

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
