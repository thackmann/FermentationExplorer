# Helper Functions for App
# This script contains various helper functions for use in the Shiny app,
# including custom operators, file input handling, validation, and data cleaning.
# Author: Timothy Hackmann
# Date: 9 Mar 2025

# === General ===
  #' Pipe Operator
  #'
  #' This operator is imported from the magrittr package and is used to chain operations together.
  #'
  import::from(magrittr, "%>%")

  #' Null Defaults Operator
  #'
  #' This custom operator returns the first argument if it is not NULL, otherwise returns the second argument.
  #'
  #' @param x The value to check for NULL.
  #' @param y The default value to return if `x` is NULL.
  #' @return Returns `x` if it is not NULL; otherwise, returns `y`.
  #' @export
  `%||%` <- function(x, y) {
    if (!is.null(x)) x else y
  }
  
  #' Negation of In Operator
  #'
  #' This custom operator returns the negation of the `%in%` operator, 
  #' checking if elements are not present in a vector or list.
  #'
  #' @param x The values to check.
  #' @param table The vector or list to check against.
  #' @return A logical vector indicating if elements of `x` are not in `table`.
  #' @export
  #' @importFrom base Negate
  `%nin%` = Negate(`%in%`)
  
  
  #' Assign a Default Value if Input is Invalid
  #'
  #' This function checks whether an input value (`x`) is `NULL`, `NA`, or an empty string (`""`). 
  #' If so, it assigns and returns the specified `default` value. Otherwise, it returns `x` unchanged.
  #'
  #' @param x The input value to check.
  #' @param default The default value to return if `x` is invalid (`NULL`, `NA`, or `""`).
  #'
  #' @return The original `x` if valid, or `default` if `x` is `NULL`, `NA`, or an empty string.
  #' 
  #' @examples
  #' assign_if_invalid(NULL, "LPSN")  # Returns "LPSN"
  #' assign_if_invalid(NA, "LPSN")    # Returns "LPSN"
  #' assign_if_invalid("", "LPSN")    # Returns "LPSN"
  #' assign_if_invalid("Valid", "LPSN") # Returns "Valid"
  #'
  #' @export
  assign_if_invalid <- function(x, default) {
    if (is.null(x) || is.na(x) || !nzchar(x)) {
      return(default)
    }
    return(x)
  }

  #' Filter and Select Columns Based on Variable Name
  #'
  #' This helper function filters a dataframe by a specified variable name and selects relevant columns.
  #'
  #' @param df A dataframe to filter and select from.
  #' @param var_name A character string specifying the variable name to filter by.
  #' @return A filtered dataframe with relevant columns selected.
  #' @export
  #' @importFrom dplyr filter select
  filter_var <- function(df, var_name) {
    if (!is.null(var_name)) {
      df <- df %>%
        dplyr::filter(var == var_name) %>%
        dplyr::select(-var)
    }
    return(df)
  }

  #' Check if Shiny App is Running Locally
  #'
  #' Determines whether a Shiny application is running locally
  #' (e.g., via `runApp()` or RStudio) by checking the session's hostname.
  #' This can be useful to conditionally show messages, warnings, or behaviors
  #' that should only apply in a deployed environment (e.g., Shiny Server).
  #'
  #' @param session The Shiny session object (typically passed as `session` in server function).
  #'
  #' @return A logical value: `TRUE` if the app is running on `localhost` or `127.0.0.1`, otherwise `FALSE`.
  #'
  #' @examples
  #' \dontrun{
  #'   if (!is_running_locally(session)) {
  #'     showNotification("You are running this on a server.")
  #'   }
  #' }
  #'
  #' @export
  is_running_locally <- function(session = getDefaultReactiveDomain()) { 
    # Tries to detect if running on localhost (127.0.0.1 or localhost)
    hostname <- session$clientData$url_hostname
    grepl("^(localhost|127\\.0\\.0\\.1)$", hostname)
  }
  
# === Navigation of tabs ===
  #' Extract a query string parameter from the URL
  #'
  #' This function safely retrieves a value from the URL's query string, optionally prints it,
  #' and returns `NULL` if not found or empty.
  #'
  #' @param session The Shiny session object.
  #' @param param_name The name of the parameter to extract (default is "job").
  #' @param verbose Logical; if TRUE, prints the query and result.
  #'
  #' @return The value of the query parameter or NULL if missing.
  #' @export
  #'
  #' @examples
  #' job_id <- get_query_param(session, "job")
  get_query_param <- function(session = getDefaultReactiveDomain(), 
                              param_name = "job", verbose = FALSE) {
    query <- shiny::parseQueryString(session$clientData$url_search)
    
    if (verbose) print(query)
    
    if (!is.null(query) && param_name %in% names(query) && nzchar(query[[param_name]])) {
      if (verbose) message("Retrieved ", param_name, ": ", query[[param_name]])
      return(query[[param_name]])
    }
    
    if (verbose) message("No value found for '", param_name, "' in query string.")
    return(NULL)
  }
  
  #' Restore Shiny Tab from URL Query on Initial Load
  #'
  #' Restores the active tab based on the `?tab=` query parameter when the Shiny app first loads.
  #' Also hides the loading screen and initializes session state for tracking navigation.
  #'
  #' @param session The Shiny session object. Defaults to `getDefaultReactiveDomain()`.
  #' @param input The Shiny input object. Defaults to `getDefaultReactiveDomain()$input`.
  #' @param tab_input_id The ID of the tabset input (e.g., `"tabs"`).
  #'
  #' @return `NULL`. Called for side effects.
  #' @export
  #' #' @details
  #' - Checks `session$userData$has_initialized` to ensure it only runs once per session.
  #' - If the URL contains `?tab=xyz`, it programmatically switches to that tab using `shinyjs::runjs("shinyjs.goToTab(...)")`.
  #' - This ensures a seamless experience when reloading or deep-linking into a specific tab.
  #' - It also initializes internal session counters, such as `navigation_count`, which is used to track how many times the user navigates between tabs.
  #' - Finally, it hides the loading screen and shows the app container, ensuring the app appears only after the appropriate tab is displayed.
  restore_tab_from_query <- function(session = getDefaultReactiveDomain(),
                                     input = getDefaultReactiveDomain()$input,
                                     tab_input_id = "tabs") {
    observe({
      if (is.null(session$userData$has_initialized) || !session$userData$has_initialized) {
        session$userData$has_initialized <- TRUE
        
        tab <- get_query_param(session, "tab")
        job <- get_query_param(session, "job")
        
        session$userData$loaded_job_on_init <- !is.null(job) && nzchar(job)
        session$sendCustomMessage("job_loaded_flag", session$userData$loaded_job_on_init)
        
        if (!is.null(tab)) {
          shinyjs::runjs(sprintf("shinyjs.goToTab('%s');", tab))
        }
        
        session$userData$initial_tab <- tab
        session$userData$navigation_count <- 0L
        
        shinyjs::runjs("shinyjs.hide('app-loading-screen'); shinyjs.show('app-wrapper');")
      }
    })
  }
  
  #' Sync Shiny Tab Selection with URL Query on Navigation
  #'
  #' This function updates the `?tab=` query parameter whenever the user switches tabs.
  #' It also clears the `?job=` parameter after the first manual tab change.
  #'
  #' @param session The Shiny session object.
  #' @param input The Shiny input object.
  #' @param tab_input_id The ID of the tabset input.
  #'
  #' @return NULL
  #' @export
  #' #' @details
  #' - Responds to any changes in the tabset input (`input[[tab_input_id]]`).
  #' - Updates the query string in the browser URL to reflect the newly selected tab.
  #' - If the URL contains a `?job=` parameter (used for result sharing or job recovery), it will be **cleared** after the first tab navigation to avoid accidentally applying an old job ID to the wrong tab.
  #' @export
  sync_tab_query_on_navigation <- function(session = getDefaultReactiveDomain(),
                                           input = getDefaultReactiveDomain()$input,
                                           tab_input_id = "tabs") {
    observeEvent(input[[tab_input_id]], {
      tab <- input[[tab_input_id]]
      query <- parseQueryString(session$clientData$url_search)
      
      count <- session$userData$navigation_count %||% 0L
      session$userData$navigation_count <- count + 1L
      
      if (!is.null(query$job) && session$userData$navigation_count > 1L) {
        query$job <- NULL
      }
      
      query$tab <- tab
      
      new_query <- paste0(
        "?",
        paste(
          vapply(names(query), function(name) {
            paste0(URLencode(name), "=", URLencode(query[[name]]))
          }, character(1)),
          collapse = "&"
        )
      )
      
      updateQueryString(new_query, mode = "replace", session = session)
    }, ignoreInit = TRUE)
  }
  
  #' Synchronize Shiny Tabs with URL Query Parameters
  #'
  #' This function keeps the active tab in a Shiny app synchronized with the browser's URL
  #' using the `?tab=` query parameter. It supports both:
  #' - **Restoring a tab on app load** based on the query string (e.g., `?tab=results`)
  #' - **Updating the query string** whenever the user navigates between tabs
  #'
  #' This makes it possible to:
  #' - Link directly to a specific tab via URL
  #' - Preserve the active tab when the user reloads or shares the page
  #' - Avoid stale job IDs when the user navigates away from the tab that created them
  #'
  #' @param session The Shiny session object. Defaults to `getDefaultReactiveDomain()`.
  #' @param input The Shiny input object. Defaults to `getDefaultReactiveDomain()$input`.
  #' @param tab_input_id The ID of the tabset input (e.g., `"tabs"`).
  #'
  #' @return `NULL`. Called for side effects.
  #' @export
  sync_tabs_with_query <- function(session = getDefaultReactiveDomain(),
                                   input = getDefaultReactiveDomain()$input,
                                   tab_input_id = "tabs") {
    restore_tab_from_query(session, input, tab_input_id)
    sync_tab_query_on_navigation(session, input, tab_input_id)
  }
  
# === Job submission and retrieval ===  
  #' Create a simplified job ID (just a UUID or hash)
  #'
  #' @param short Logical. Whether to use a short hash instead of full UUID (default = FALSE).
  #' @return A character string like: abc123
  #' @export
  create_job_id <- function(short = FALSE) {
    if (short) {
      digest::digest(Sys.time(), algo = "xxhash32", serialize = FALSE)
    } else {
      uuid::UUIDgenerate()
    }
  }
  
  #' Construct a URL for a job based on tab name and job ID
  #'
  #' @param session The Shiny session object
  #' @param job_id The job ID (no prefix needed)
  #' @param tab The current tab name (e.g., "predictionsTaxonomy")
  #'
  #' @return A string URL like ?tab=predictionsTaxonomy&job=abc123
  #' @export
  create_job_url <- function(session = getDefaultReactiveDomain(), 
                             job_id, tab) {
    paste0(
      session$clientData$url_protocol, "//",
      session$clientData$url_hostname,
      if (!is.null(session$clientData$url_port)) paste0(":", session$clientData$url_port),
      session$clientData$url_pathname,
      "?tab=", tab,
      "&job=", job_id
    )
  }
  
  #' Get user's IP address from session
  #'
  #' @param session The Shiny session (default = getDefaultReactiveDomain())
  #' @return IP address as a string, sanitized. Returns "unknown" if not available.
  #' @export
  get_user_ip <- function(session = getDefaultReactiveDomain()) {
    tryCatch({
      ip <- session$request$REMOTE_ADDR
      ip <- gsub("[^0-9A-Za-z]", "", ip)
      if (nzchar(ip)) ip else "unknown"
    }, error = function(e) "unknown")
  }
  
  #' Format sanitized IP for display
  #'
  #' @param sanitized_ip A string like "127001" or "19216811"
  #' @return Best-effort formatted IP like "127.0.0.1"
  #' @export
  format_ip_for_display <- function(sanitized_ip) {
    if (!grepl("^[0-9]+$", sanitized_ip)) return(sanitized_ip)  # fallback for unexpected input
    
    # Try to reformat as IPv4 (best-effort)
    if (nchar(sanitized_ip) == 6 || nchar(sanitized_ip) == 7 || nchar(sanitized_ip) == 8) {
      parts <- substring(sanitized_ip, c(1, 2, 4, 6), c(1, 3, 5, nchar(sanitized_ip)))
      return(paste(parts, collapse = "."))
    }
    
    # Just return original if reformatting fails
    sanitized_ip
  }
  
  #' Construct a job directory path
  #'
  #' @param tab Tab name (e.g., "predictionsTaxonomy")
  #' @param ip User's IP address (already sanitized). If NULL, it is auto-detected.
  #' @param base_dir Base job directory (default = "jobs")
  #' @return Full job directory path
  #' @export
  get_job_dir <- function(tab, ip = NULL, base_dir = "jobs") {
    if (is.null(ip)) ip <- get_user_ip()
    file.path(base_dir, ip, tab)
  }
  
  #' Save a result to the job directory
  #'
  #' @param job_id Job ID string
  #' @param result The result to save
  #' @param job_dir Path to job directory
  #' @export
  save_job_result <- function(job_id, result, job_dir) {
    dir.create(job_dir, recursive = TRUE, showWarnings = FALSE)
    saveRDS(result, file = file.path(job_dir, paste0(job_id, ".rds")))
  }
  
  #' Load a result by job ID
  #'
  #' @param job_id Job ID string
  #' @param job_dir Path to job directory
  #' @return The loaded result object, or NULL if not found
  #' @export
  load_job_result <- function(job_id, job_dir) {
    path <- file.path(job_dir, paste0(job_id, ".rds"))
    if (file.exists(path)) readRDS(path) else NULL
  }
  
  #' Check if a job result file exists
  #'
  #' @param job_id Job ID string
  #' @param job_dir Path to job directory
  #' @return TRUE if the job result file exists, FALSE otherwise
  #' @export
  job_result_exists <- function(job_id, job_dir) {
    file.exists(file.path(job_dir, paste0(job_id, ".rds")))
  }
  
  #' List all saved job IDs in a directory
  #'
  #' @param job_dir Path to job directory
  #' @return A character vector of job IDs
  #' @export
  list_saved_jobs <- function(job_dir) {
    job_files <- list.files(job_dir, pattern = "\\.rds$", full.names = FALSE)
    gsub("\\.rds$", "", job_files)
  }
  
  #' Clear Old or Large Job Files
  #'
  #' @inheritParams setup_auto_cleanup
  #' @return A character vector of deleted file paths
  #' @export
  clear_old_jobs <- function(job_dir = "jobs",
                             max_age_days = 30,
                             max_size_MB = 100,
                             verbose = FALSE) {
    if (!dir.exists(job_dir)) return(character(0))
    
    files <- list.files(job_dir, pattern = "\\.rds$", full.names = TRUE, recursive = TRUE)
    if (length(files) == 0) return(character(0))
    
    info <- file.info(files)
    info$age_days <- as.numeric(difftime(Sys.time(), info$mtime, units = "days"))
    info$size_MB <- info$size / (1024^2)
    
    to_delete <- rownames(info)[info$age_days > max_age_days | info$size_MB > max_size_MB]
    
    if (length(to_delete) > 0) {
      file.remove(to_delete)
      if (verbose) message("Deleted files:\n", paste("  -", basename(to_delete), collapse = "\n"))
      return(to_delete)
    } else {
      if (verbose) message("No job files exceeded thresholds.")
      return(character(0))
    }
  }
  
  #' Set Up Automatic Job Cleanup
  #'
  #' @inheritParams clear_old_jobs
  #' @param interval_hours Cleanup interval (default = 6)
  #' @return A reactive observer that performs cleanup
  #' @export
  setup_auto_cleanup <- function(session = getDefaultReactiveDomain(),
                                 job_dir = "jobs",
                                 max_age_days = 30,
                                 max_size_MB = 100,
                                 interval_hours = 6,
                                 verbose = FALSE) {
    auto_cleanup_timer <- reactiveTimer(interval_hours * 60 * 60 * 1000, session = session)
    
    observe({
      auto_cleanup_timer()
      all_dirs <- list.dirs(job_dir, full.names = TRUE, recursive = TRUE)
      lapply(all_dirs, function(dir) {
        clear_old_jobs(
          job_dir = dir,
          max_age_days = max_age_days,
          max_size_MB = max_size_MB,
          verbose = verbose
        )
      })
    })
  }
  
# === Download handlers ===
	#' Create a Shiny Download Handler for Multiple File Types
	#'
	#' This function generates a Shiny download handler for downloading either `.csv` or `.rds` files.
	#' The handler dynamically creates filenames based on a provided prefix, loads the data using a function or object, 
	#' and saves the data in the specified file format.
	#'
	#' @param filename_prefix A character string or reactive used as the prefix for the downloaded file's name.
	#' @param data_source An object or function returns the data to be downloaded. 
	#' @param file_type A character string indicating the file type for download. 
	#'        Can be either "csv" or "rds". Default is "csv".
	#' @return A Shiny download handler, ready to be assigned to an output object in a Shiny server function.
	#' @export
	#' @examples
	#' \dontrun{
	#' # In a Shiny server function:
	#' output$downloadData <- create_download_handler("gene_functions", function() gene_functions_data)
	#'
	#' # For downloading an RDS file:
	#' output$downloadModel <- create_download_handler("model_methanogenesis", load_model_methanogenesis, file_type = "rds")
	#' }
	create_download_handler <- function(filename_prefix, data_source, file_type = "csv") {
	  shiny::downloadHandler(
		filename = function() {
		  # Dynamically evaluate reactive expressions inside the function
		  prefix <- if (is.reactive(filename_prefix)) filename_prefix() else filename_prefix
		  paste0(prefix, ".", file_type)
		},
		content = function(file) {
		  # Ensure data_source is evaluated dynamically
		  data <- if (is.function(data_source)) data_source() else data_source
		  
		  if (file_type == "csv") {
			write.csv(data, file, row.names = FALSE)
		  } else if (file_type == "rds") {
			saveRDS(data, file)
		  } else if (file_type == "txt") {
			writeLines(data, file)
		  } else {
			stop("Unsupported file type")
		  }
		}
	  )
	}

# === Modals ===
	#' Show a Shiny Modal with Download Links
	#'
	#' @param ns A namespace function (for module compatibility, default is identity).
	#' @param title The title of the modal.
	#' @param downloads A named list where names are output IDs and values are labels for the download links.
	#' @param help_link_id The input ID for the help action link.
	#' @export
	showDownloadModal <- function(ns = identity, title = "Example files", downloads, help_link_id = "go_to_help") {
	  shiny::showModal(shiny::modalDialog(
		shiny::h3(title),
		htmltools::tags$ol(class = "circled-list",
						   lapply(names(downloads), function(output_id) {
							 htmltools::tags$li(shiny::downloadLink(outputId = ns(output_id), label = downloads[[output_id]]))
						   })
		),
		htmltools::div("Click ",
					   shiny::actionLink(ns(help_link_id), "here"),
					   " to see detailed guidelines."
		),
		easyClose = TRUE, footer = NULL
	  ))
	}

	#' Show a Shiny Modal with an Error Message
	#'
	#' This function displays a modal dialog with an error message.
	#'
	#' @param ns A namespace function (for module compatibility, default is identity).
	#' @param title The title of the modal (default: "Download Unavailable").
	#' @param message The error message text to display.
	#' @export
	showErrorModal <- function(ns = identity,
	                           title = "Download Unavailable",
	                           message = "Data is not available to download.") {
	  shiny::showModal(
	    shiny::modalDialog(
	      shiny::h3(title),
	      htmltools::div(
	        HTML(message)
	      ),
	      easyClose = TRUE,
	      footer = NULL
	    )
	  )
	}
	
	#' Stop Process with Condition
	#'
	#' This helper function stops the execution of code with a specified condition and error message.
	#'
	#' @param class A character vector of classes to assign to the error.
	#' @param message A character string specifying the error message.
	#' @return Stops the execution with the specified condition.
	#' @export
	stopWithCondition <- function(class, message) {
	  cond <- structure(
		list(message = message),
		class = c(class, 'error', 'condition')
	  )
	  stop(cond)
	}

	#' Stop Reactive Process with Condition
	#'
	#' This helper function stops a Shiny reactive process with a specified condition.
	#'
	#' @param message A character string specifying the error message.
	#' @param class A character vector of classes to assign to the error.
	#' @return Stops the reactive process with the specified condition.
	#' @export
	reactiveStop <- function(message = "", class = NULL) {
	  stopWithCondition(c("shiny.silent.error", class), message)
	}

	#' Validate Input and Launch Modal on Error
	#'
	#' This function validates Shiny input and launches a modal dialog with an error message if validation fails.
	#' It is modified from shiny::validate() to launch modal rather than returning a simple text output. 
	#' 
	#' @param ... Validation conditions to check.
  #' @param session The Shiny session object.
	#' @param errorClass A character vector of error classes to assign to the error. Default is an empty character vector.
	#' @param delay_time The delay in milliseconds before launching modal. 
	#' @return Invisible if validation passes; otherwise, stops the reactive process with a modal error message.
	#' @export
	#' @importFrom shiny removeModal showModal modalDialog h4 p
	#' @importFrom rlang list2
	#' @importFrom stats na.omit
	runValidationModal <- function(..., session = getDefaultReactiveDomain(), errorClass = character(0), delay_time = 250) {
	  # Test validation conditions
	  results <- sapply(rlang::list2(...), function(x) {
		if (is.null(x)) 
		  return(NA_character_)
		else if (identical(x, FALSE)) 
		  return("")
		else if (is.character(x)) 
		  return(paste(as.character(x), collapse = "\n"))
		else stop("Unexpected validation result: ", as.character(x))
	  })
	  results <- stats::na.omit(results)
	  if (length(results) == 0) 
		return(invisible())
	  results <- results[nzchar(results)]
	  
	  # Show error message
	  shinyjs::delay(delay_time, { 
		# Remove existing modals
		shiny::removeModal()
		
		# Set the modal open state to FALSE
		session$userData$modal_open(FALSE)
		
		# Launch modal
		shiny::showModal(shiny::modalDialog(
		  shiny::h4("Error in input"),
		  shiny::p(paste(results)),
		  easyClose = TRUE, footer = NULL
		))
	  })
	  
	  # Stop reactive process
	  reactiveStop(paste("", collapse = "\n"), c(errorClass,
												 "validation"))
	}

	#' Display Modal with Progress Bar and Optional Link
	#'
	#' This function displays or updates a modal with a progress bar.
	#' If a modal is already open, it updates the message and progress bar.
	#'
	#' @param session The Shiny session object.
	#' @param id The id of the progress bar.
	#' @param message Message to display above the progress bar (default: "Initializing").
	#' @param value Value for the progress bar (default: 0).
	#' @param url Optional URL to display below the message.
	#'
	#' @importFrom shiny showModal modalDialog tags removeModal
	#' @importFrom shinyjs runjs
	#' @importFrom shinyWidgets progressBar updateProgressBar
	#'
	#' @export
	display_modal <- function(session = getDefaultReactiveDomain(), 
	                          id, 
	                          message = "Initializing", 
	                          value = 0, 
	                          url = NULL) {
	  if (isFALSE(session$userData$modal_open())) {
	    
	    
	    # Create the modal content
	    modal_content <- list(
	      shiny::tags$h4(id = "modal-text", message),
	      if (!is.null(url)) shiny::tags$div(
	        id = "modal-link",
	        class = "modal-link-text",
	        "Results will be stored at ",
	        shiny::tags$a(href = url, "this link", target = "_blank"),
	        "."
	      ),
	      shinyWidgets::progressBar(id = id, value = value, display_pct = TRUE)
	    )
	    
	    shiny::showModal(shiny::modalDialog(
	      modal_content,
	      easyClose = FALSE,
	      footer = NULL
	    ))
	    
	    session$userData$modal_open(TRUE)
	  } else {
	    # Update the message text
	    shinyjs::runjs(sprintf(
	      "document.getElementById('modal-text').innerText = '%s';", message
	    ))
	    
	    # Update or insert the job URL with consistent styling
	    if (!is.null(url)) {
	      link_update_js <- sprintf(
	        "
        document.getElementById('modal-link')?.remove();
        const p = document.createElement('p');
        p.id = 'modal-link';
        p.className = 'modal-link-text'; 
        const text = document.createTextNode('Results available at ');
        const a = document.createElement('a');
        a.href = '%s';
        a.target = '_blank';
        a.textContent = 'this link';
        p.appendChild(text);
        p.appendChild(a);
        p.appendChild(document.createTextNode('.'));
        const modalBody = document.querySelector('.modal-body');
        const progress = modalBody.querySelector('.progress');
        modalBody.insertBefore(p, progress);
        ",
	        url
	      )
	      shinyjs::runjs(link_update_js)
	    }
	    
	    # Update progress bar
	    shinyWidgets::updateProgressBar(session = session, id = id, value = value)
	  }
	}
	
	#' Hide Modal with Progress Bar
	#'
	#' This function hides a modal with a progress bar.  It includes a brief delay 
	#' to ensure UI elements are fully updated before the modal is removed (otherwise
	#' the modal may not close).
	#' 
	#' @param session The Shiny session object.
	#' @param id The id of the progress bar to be updated.
	#' @param delay_time The delay in milliseconds before hiding the modal (default: 250).
	#' @export 
	#' @importFrom shinyjs delay
	#' @importFrom shinyWidgets updateProgressBar
	#' @importFrom shiny removeModal
	hide_modal_with_progress <- function(session = getDefaultReactiveDomain(), id, delay_time = 250) {
	  shinyjs::delay(delay_time, {
		shinyWidgets::updateProgressBar(session = session, id = id, value = 100)
		shiny::removeModal()
	  })
	  
	  # Set the modal open state to FALSE
	  session$userData$modal_open(FALSE)
	}

# === File reading ===
	#' Concatenate a Vector into Noun Phrase
	#'
	#' This function takes a character vector and formats it into a a non, 
	#' using commas and a conjunction (e.g., "or" or "and") before the last element.
	#'
	#' @param vec A character vector of words or phrases to concatenate.
	#' @param conjunction A string specifying the conjunction to use before the last element (default is "or").
	#'
	#' @return A string representing the concatenated list.
	#' @examples
	#' format_list(c("apple", "banana", "cherry")) # "apple, banana, or cherry"
	#' format_list(c("apple", "banana"), "and")   # "apple and banana"
	#' format_list("apple")                        # "apple"
	#' format_list(character(0))                   # ""
	#'
	#' @export
	vector_to_phrase <- function(vec, conjunction = "or") {
	  n <- length(vec)
	  if (n == 0) {
	    return("")
	  } else if (n == 1) {
	    return(vec)
	  } else if (n == 2) {
	    return(paste0(vec, collapse = paste0(" ", conjunction, " ")))
	  } else {
	    return(paste0(paste0(vec[-n], collapse = ", "), conjunction, vec[n]))
	  }
	}
	
	#' Validate and Read a File
	#'
	#' This function validates that the file is of an accepted type, ensures it exists, and reads it accordingly.
	#'
	#' @param session The Shiny session object.
	#' @param file_path Character. The path to the file.
	#' @param accepted_extensions Character vector. The accepted file extensions (default: c("csv", "txt", "rds", "ko", "xls", "xlsx")).
	#' @return A dataframe (for CSV/TXT/KO/Excel) or an R object (for RDS), or NULL if an error occurs.
	#' @export
	#' @importFrom tools file_ext
	#' @importFrom shiny req
	#' @importFrom utils read.csv
	#' @importFrom readr read_csv read_delim
	#' @importFrom base readRDS
	#' @importFrom readxl read_excel
	validate_and_read_file <- function(session = getDefaultReactiveDomain(), 
	                                   file_path, 
	                                   accepted_extensions = c("csv", "txt", "rds", "ko", "xls", "xlsx")) {
	  if (is.null(file_path) || file_path == "") {
	    runValidationModal(session = session, "No file selected. Please upload a file.")
	    return(NULL)
	  }
	  
	  file_ext <- tolower(tools::file_ext(file_path))
	  
	  if (!file_ext %in% accepted_extensions) {
	    runValidationModal(session = session, paste("Invalid file type. Please upload a", vector_to_phrase(accepted_extensions)))
	    return(NULL)
	  }
	  
	  # Determine the appropriate function to read the file
	  load_function <- switch(file_ext,
	                          "rds"  = readRDS,
	                          "csv"  = function(file, ...) readr::read_csv(file, show_col_types = FALSE, ...),
	                          "txt"  = function(file, ...) readr::read_delim(file, delim = "\t", show_col_types = FALSE, ...),
	                          "ko"   = function(file, ...) read.table(file, sep = "\t", header = FALSE, fill = TRUE, ...),
	                          "xls"  = function(file, ...) readxl::read_excel(file, ...),
	                          "xlsx" = function(file, ...) readxl::read_excel(file, ...),
	                          stop("Unsupported file extension"))
	  
	  data <- tryCatch({
	    do.call(load_function, list(file_path))
	  }, error = function(e) {
	    runValidationModal(session = session, "Error reading the file. Please check the file format.")
	    return(NULL)
	  })
	  
	  return(data)
	}

# === Processing gene functions ===
	#' Detect Columns Matching a Pattern
	#'
	#' This function detects which columns in a dataframe contain values matching a specified pattern.
	#'
	#' @param data A dataframe to search for matching patterns.
	#' @param pattern A character string specifying the regular expression pattern to match.
	#' @return A character vector of column names that match the pattern, or NA if none are found.
	#' @export
	#' @importFrom base grepl
	detect_pattern_column <- function(data, pattern) {
	  matching_columns <- vector("character")
	  
	  for (col_name in colnames(data)) {
	    if (any(grepl(pattern, data[[col_name]], perl = TRUE))) {
	      matching_columns <- c(matching_columns, col_name)
	    }
	  }
	  
	  if (length(matching_columns) == 0) {
	    return(NA)
	  } else {
	    return(matching_columns)
	  }
	}
	
	#' Check if a column name matches genome or organism identifiers
	#'
	#' This helper function checks if a column name is related to genome or organism identifiers.
	#' It matches case-insensitively against a set of known headers.
	#'
	#' @param column_name Character. A column name.
	#' 
	#' @return Logical. TRUE if the column name matches, FALSE otherwise.
	detect_genome_column <- function(column_name) {
	  valid_genome_headers <- c("Genome", "Genome ID", "Genome Name", 
	                            "Organism", "Organism ID", "Organism Name")
	  return(tolower(column_name) %in% tolower(valid_genome_headers))
	}
	
	#' Check if a column contains KO identifiers
	#'
	#' This helper function checks if a column contains KO (KEGG Orthology) identifiers.
	#' It does this by applying a regex pattern to the column's values.
	#'
	#' @param column Vector. A column from a dataframe.
	#' 
	#' @return Logical. TRUE if the column contains KO identifiers, FALSE otherwise.
	detect_ko_column <- function(column) {
	  ko_pattern <- "^K\\d{5}$"
	  
	  # Remove NA values before checking
	  return(any(grepl(ko_pattern, column[!is.na(column)])))
	}
	
	#' Check if Data Follows IMG Gene Cart Format
	#'
	#' This function checks if the data contains columns in IMG gene carts.
	#' It does this by checking if it has columns with headers 
	#' "gene_oid", "Genome ID", "Genome Name", and "KO". These headers are rare
	#' in other files.
	#' 
	#' @param data Dataframe. The data to check.
	#' 
	#' @return Logical. TRUE if the data follows IMG format, FALSE otherwise.
	#' @export
	is_img_gene_format <- function(data) {
	  if (!is.data.frame(data)) return(FALSE)
	  
	  required_columns <- c("gene_oid", "Genome ID", "Genome Name", "KO")
	  
	  return(all(required_columns %in% colnames(data)))
	}
	
	#' Check if Data Follows KAAS Format
	#'
	#' This function checks if the data contains columns in KAAS (.ko) files.
	#' It does this by checking if there are two columns.  
	#' The second column contains KO IDs, and the first column does not contain the 
	#' header "Genome" or "Organism". This rules out most files where 
	#' genome or organism identifiers are present as the first column.
	#'
	#' @param data Dataframe. The data to check.
	#' 
	#' @return Logical. TRUE if the data follows KAAS format, FALSE otherwise.
	#' @export
	is_kaas_format <- function(data) {
	  if (!is.data.frame(data)) return(FALSE)
	  
	  # Check if there are exactly two columns
	  if (ncol(data) != 2) return(FALSE)
	  
	  # Ensure that the second column contains KO IDs
	  if (!detect_ko_column(data[[2]])) return(FALSE)
	  
	  # Ensure the first column does not have a genome/organism identifier as a header
	  has_invalid_first_column <- detect_genome_column(colnames(data)[1])
	  
	  return(!has_invalid_first_column)
	}
	
	#' Check if Data Has One Column with KO IDs
	#'
	#' This function checks if the data contains at least one column of KO IDs.
	#'
	#' @param data Dataframe. The data to check.
	#'
	#' @return Logical. TRUE if the data has exactly one KO ID column, FALSE otherwise.
	#' @export
	is_single_ko_format <- function(data) {
	  if (!is.data.frame(data)) return(FALSE)
	  
	  # Detect all KO columns by checking their values
	  ko_columns <- sapply(data, detect_ko_column)
	  
	  # Check if there is exactly one KO column
	  return(sum(ko_columns) == 1)
	}
	
	#' Check if Data Has Multiple Columns with KO IDs
	#'
	#' This function checks if the data has multiple columns of KO IDs.
	#'
	#' @param data Dataframe. The data to check.
	#'
	#' @return Logical. TRUE if the data has multiple KO ID columns, FALSE otherwise.
	#' @export
	is_multi_ko_format <- function(data) {
	  if (!is.data.frame(data)) return(FALSE)
	  
	  # Detect all KO columns by checking their values
	  ko_columns <- sapply(data, detect_ko_column)
	  
	  # Check if there are multiple KO columns
	  return(sum(ko_columns) > 1)
	}
	
	#' Process Data Following IMG Format
	#'
	#' This function extracts the "Genome Name" and "KO" columns from an IMG gene cart dataset,
	#' then pivots the data so that each genome name becomes a column header with KO IDs as values.
	#' It also extracts clean KO IDs from strings like "KO:K00001".
	#'
	#' @param data Dataframe. The data to process.
	#'
	#' @return A dataframe where genome names are column headers, and KO IDs are values.
	#' @export
	process_img_gene_format <- function(data) {
	  # Select relevant columns and remove missing values
	  processed_data <- data %>%
	    dplyr::select(`Genome Name`, KO) %>%
	    dplyr::filter(!is.na(KO) & KO != "")
	  
	  # Extract only KO IDs
	  processed_data <- processed_data %>%
	    dplyr::mutate(KO = stringr::str_extract(KO, "(?<=KO:)K\\d{5}")) %>%
	    dplyr::filter(!is.na(KO))
	  
	  # Convert to tibble for tidyr functions
	  processed_data <- tibble::as_tibble(processed_data)
	  
	  # Pivot wider: Genome Names become column headers, KO IDs as values
	  processed_data <- processed_data %>%
	    dplyr::group_by(`Genome Name`) %>%
	    dplyr::mutate(row_id = dplyr::row_number()) %>%  # Create row index to prevent duplication collapse
	    tidyr::pivot_wider(names_from = `Genome Name`, values_from = KO) %>%
	    dplyr::select(-row_id)  # Remove the row index column
	  
	  return(processed_data)
	}
	
	#' Process Data Following KAAS Format
	#'
	#' This function extracts KO identifiers from a KAAS-formatted dataset,
	#' assigns "Organism" as the genome column header, and removes rows without KO IDs.
	#'
	#' @param data Dataframe. The data to process.
	#'
	#' @return A dataframe containing the standard "Genome" and "KO" columns.
	#' @export
	process_kaas_format <- function(data) {
	  # Extract only the KO ID column (second column)
	  processed_data <- data[, 2, drop = FALSE]
	  
	  # Remove rows where KO is missing or empty
	  processed_data <- processed_data[processed_data[[1]] != "", , drop = FALSE]
	  processed_data <- processed_data[!is.na(processed_data[[1]]), , drop = FALSE]
	  
	  # Assign standard column names
	  colnames(processed_data) <- c("KO")
	  
	  # Add "Organism" as the first column
	  processed_data <- cbind(Genome = "Organism", processed_data)
	  
	  # Pivot wider: "Organism" as the column header, KO IDs as values
	  processed_data <- processed_data %>%
	    dplyr::group_by(Genome) %>%
	    dplyr::mutate(row_id = dplyr::row_number()) %>%
	    tidyr::pivot_wider(names_from = Genome, values_from = KO) %>%
	    dplyr::select(-row_id)  # Remove the row index column
	  
	  return(processed_data)
	}
	
	
	#' Process Data Following Single-KO Format
	#'
	#' This function extracts a single KO column from a dataset and assigns a default
	#' genome name ("Organism") if no genome column is found. The data is then pivoted
	#' wider so that each genome becomes a column header with KO IDs as values.
	#'
	#' @param data Dataframe. The data to process.
	#'
	#' @return A dataframe where genome names are column headers, and KO IDs are values.
	#' @export
	process_single_ko_format <- function(data) {
	  # Detect KO-related columns
	  ko_columns <- sapply(data, detect_ko_column)
	  
	  # Detect genome-related columns
	  genome_columns <- sapply(colnames(data), detect_genome_column)
	  
	  if (any(genome_columns)) {
	    # There is a genome column, keep it
	    processed_data <- data[, genome_columns | ko_columns, drop = FALSE]
	  } else {
	    # No genome column detected, assume a single organism
	    processed_data <- data[, ko_columns, drop = FALSE]
	    processed_data <- cbind(Genome = "Organism", processed_data)
	  }
	  
	  colnames(processed_data) <- c("Genome", "KO")  # Standardize column names
	  
	  # Convert to tibble for tidyr functions
	  processed_data <- tibble::as_tibble(processed_data)
	  
	  # Pivot wider: Genome Names become column headers, KO IDs as values
	  processed_data <- processed_data %>%
	    dplyr::group_by(Genome) %>%
	    dplyr::mutate(row_id = dplyr::row_number()) %>%
	    tidyr::pivot_wider(names_from = Genome, values_from = KO) %>%
	    dplyr::select(-row_id)  # Remove the row index column
	  
	  return(processed_data)
	}
	
	#' Process Data Following Multi-KO Format
	#'
	#' This function extracts columns that contain KO identifiers from a dataset
	#' that follows the multi-KO format.
	#'
	#' @param data Dataframe. The data to process.
	#'
	#' @return A dataframe containing only columns with KO identifiers.
	#' @export
	process_multi_ko_format <- function(data) {
	  if (!is.data.frame(data)) return(NULL)
	  
	  # Identify columns that contain KO IDs
	  ko_columns <- sapply(data, detect_ko_column)
	  
	  # Subset data to keep only KO ID columns
	  return(data[, ko_columns, drop = FALSE])
	}
	
	#' Process Uploaded Gene Functions File
	#'
	#' This function processes an uploaded gene functions file.  
	#' The type of processing depends on the file type detected.
	#' 
	#' gene_functions Dataframe. The gene functions data.
	#' 
	#' @return A processed dataframe or NULL if the file structure is unrecognized.
	#' @export
	process_uploaded_gene_functions <- function(gene_functions) {
	  if (!is.data.frame(gene_functions)) return(NULL)
	  
	  if(is_img_gene_format(gene_functions))
	  {
	    gene_functions <- process_img_gene_format(gene_functions)
	  }else if(is_kaas_format(gene_functions)){
	    gene_functions <- process_kaas_format(gene_functions)
	  }else if(is_single_ko_format(gene_functions)){
	    gene_functions <- process_single_ko_format(gene_functions)
	  }else if(is_multi_ko_format(gene_functions)){
	    gene_functions <- process_multi_ko_format(gene_functions)
	  }else{
	    return(NULL)
	  }
	  
	  return(gene_functions)
	}
  
  #' Process Gene Functions from the Database
  #'
  #' This function processes gene functions for selected organisms, filtering and reshaping the data accordingly.
  #'
  #' @param gene_functions Dataframe. The gene functions data.
  #' @param organism_by_genome Dataframe. The dataframe mapping organisms to genomes.
  #' @param selected_organisms Character vector. The organisms selected by the user.
  #'
  #' @return A dataframe with gene functions for the selected organisms, with renamed columns to use organism names.
  #' @export
  #' @importFrom dplyr filter group_by mutate select
  #' @importFrom tidyr pivot_wider
  process_database_gene_functions <- function(gene_functions, organism_by_genome, selected_organisms) {
    filtered_data <- organism_by_genome %>%
      dplyr::filter(Organism %in% selected_organisms)

    filtered_data$Genome <- as.character(filtered_data$Genome)

    gene_functions <- gene_functions %>%
      dplyr::filter(Genome %in% filtered_data$Genome) %>%
      dplyr::group_by(Genome) %>%
      dplyr::mutate(row_id = dplyr::row_number()) %>%
      tidyr::pivot_wider(names_from = Genome, values_from = Database_ID) %>%
      dplyr::select(-row_id) %>%
      dplyr::select(all_of(filtered_data$Genome))

    colnames(gene_functions) <- filtered_data$Organism

    gene_functions <- as.data.frame(gene_functions)
    
    return(gene_functions)
  }
  
# === Processing query strings ===  
  #' Process a Query String for More Precise Matching
  #'
  #' This function updates a query string generated by a query builder to ensure 
  #' that all `grepl()` expressions match only standalone values or values separated by semicolons.
  #' It modifies **all** occurrences of `grepl()` in the input query by adding regex lookarounds 
  #' (`(?<=^|;)` and `(?=;|$)`) and enables Perl-compatible regular expressions (`perl = TRUE`).
  #'
  #' ## Effect of the Modification
  #' - The modified pattern will now **only match** if the search term:
  #'   - Appears at the **start of the string**.
  #'   - Appears at the **end of the string**.
  #'   - Is **preceded and/or followed by a semicolon (`;`)**.
  #'   (e.g., `"butyrate"` will match `"acetate;butyrate;isobutyrate"`).
  #' - **Partial matches within words or substrings will not occur**.
  #' - **Does not match** if the search term is part of a longer word 
  #'   (e.g., `"butyrate"` will not match `"isobutyrate"`).
  #' - **Handles multiple `grepl()` statements** in a complex query while preserving logical operators (`&`, `|`).
  #'
  #' @param query_string A character string representing a complex query with one or more `grepl()` expressions.
  #'                     Each `grepl()` should be in the format `"grepl(\"search_term\", column_name)"`.
  #'
  #' @return A modified query string where **all `grepl()` calls** include lookarounds to match 
  #'         only standalone occurrences of the search term or those separated by semicolons.
  #'
  #' @examples
  #' query_string <- "grepl(\"butyrate\", `End products`) & grepl(\"acetate\", `End products`)"
  #' new_query_string <- process_query_string(query_string)
  #' print(new_query_string)
  #' # Output: "grepl(\"(?<=^|;)butyrate(?=;|$)\", `End products`, perl = TRUE) & grepl(\"(?<=^|;)acetate(?=;|$)\", `End products`, perl = TRUE)"
  #'
  #' @export
  process_query_string <- function(query_string) {
    gsub(
      pattern = "grepl\\(\"(.*?)\", (.*?)\\)", 
      replacement = "grepl(\"(?<=^|;)\\1(?=;|$)\", \\2, perl = TRUE)", 
      query_string
    )
  }
  
  #' Extract Variables from Query Builder Output
  #'
  #' This function extracts all variables enclosed in backticks from a query builder output string.
  #'
  #' @param query_string A string containing a query with variables enclosed in backticks.
  #' @return A character vector of the extracted variable names.
  #' @export
  extract_query_var <- function(query_string) {
    # Extract variables between backticks
    variables <- stringr::str_extract_all(query_string, "`[^`]+`")[[1]]
    
    # Remove the backticks
    variables <- stringr::str_replace_all(variables, "`", "")
    
    return(variables)
  }
  
  #' Filter Data Based on Query String
  #'
  #' This function dynamically filters data based on a query string from jqbr::queryBuilderInput()
  #' The query string is evaluated and used to filter the data.
  #'
  #' @param data A data frame containing the data to be filtered.
  #' @param query_string A string representing the filtering condition (e.g., "`Gram_stain` == \"positive\"").
  #' @return A filtered data frame based on the query string condition.
  #' @export
  #' @importFrom dplyr filter
  #' @importFrom rlang parse_expr
  filter_data_by_query <- function(data, query_string) {
    data_filtered <- data %>% dplyr::filter(!!rlang::parse_expr(query_string))
    return(data_filtered)
  }
  
  #' Filter Data Excluding NA Values
  #'
  #' This function filters data, removing rows that have NA or "NA" values in the variables 
  #' extracted from the query string.  The query string is from jqr::queryBuilderInput().
  #'
  #' @param data A data frame containing the data to be filtered.
  #' @param query_string A string representing the query filter (e.g., "`Gram_stain` == \"positive\"").
  #' @return A filtered data frame that excludes rows where any extracted variable has NA or "NA" values.
  #' @export
  #' @importFrom dplyr filter
  #' @importFrom rlang sym
  filter_data_excluding_na <- function(data, query_string) {
    vars <- extract_query_var(query_string)
    data_all <- data
    for (var in vars) {
      data_all <- data_all %>% dplyr::filter(!is.na(!!rlang::sym(var)) | !!rlang::sym(var) != "NA")
    }
    return(data_all)
  }
  
  #' Mutate Data Based on Query String 
  #'
  #' This function dynamically mutates data based on a query string from jqr::queryBuilderInput().
  #' It adds a new column indicating whether the condition is met (1) or not (0), and the name
  #' of this new column can be specified.
  #'
  #' @param data A data frame containing the data to be filtered.
  #' @param query_string A string representing the filtering condition (e.g., "`Gram_stain` == \"positive\"").
  #' @param new_col A string specifying the name of the new column to be added.
  #' @return A mutated data frame based on the query string condition.
  #' @export
  #' @importFrom dplyr mutate
  #' @importFrom rlang parse_expr sym
  mutate_data_by_query <- function(data, query_string, new_col="X") {
    condition <- rlang::parse_expr(query_string)
    
    data <- data %>%
      dplyr::mutate(!!rlang::sym(new_col) := ifelse(!!condition, 1, 0))
    
    return(data)
  }
  
  #' Collapse Dataframe Columns into a Single Column
  #'
  #' This function collapses a set of columns in a dataframe into a single column with 
  #' semi-colon separated values. The rows in the columns can contain user-defined 
  #' positive and negative values (e.g., "1" for presence and "0" for absence).
  #' The names of the columns will be the new values in the new column.
  #' These values will be semi-colon separated (e.g., column1;column2), 
  #' and values absent in the original column will be excluded.
  #'
  #' @param df A dataframe containing the columns to be collapsed.
  #' @param cols A character vector of column names to collapse.
  #' @param new_col_name The name of the new column to store collapsed values.
  #' @param delete A string pattern to remove from column names during the collapse. Default is an empty string.
  #' @param positive_value The value that represents a "positive" presence (default is "1").
  #' @param negative_value The value that represents a "negative" absence (default is "0").
  #' @return A dataframe with the specified columns collapsed into a single column.
  #' @export
  #' @importFrom dplyr rowwise mutate ungroup select
  #' @importFrom stringr str_replace
  collapse_columns <- function(df, cols, new_col_name, delete = "", positive_value = "1", negative_value = "0") {
    df[[new_col_name]] <- sapply(1:nrow(df), function(i) {
      col_values <- gsub(delete, "", cols)
      # Identify positive matches based on the positive value
      values <- col_values[df[i, cols] == positive_value]
      # Exclude NA and empty values
      values <- values[!is.na(values) & values != "NA"]
      if (length(values) > 0) {
        return(paste(values, collapse = ";"))
      } else {
        return(NA)
      }
    })
    df <- df[, !(colnames(df) %in% cols)]
    
    return(df)
  }
  
  #' Simplify Taxonomy Names
  #'
  #' This function simplifies taxonomy names by replacing high-level taxonomic ranks with NA.
  #' The last non-NA rank is preserved, along with the species name.
  #'
  #' @param row A vector representing a row of taxonomy data.
  #' @param col_names A character vector of column names corresponding to the taxonomy ranks.
  #' @return A simplified vector with high-level ranks replaced by NA.
  #' @export
  simplify_names <- function(row, col_names) {
    species_column_idx <- which(col_names == "Species")
    
    if (all(is.na(row[-species_column_idx]))) {
      row
    } else {
      last_non_na_idx <- max(which(!is.na(row[-species_column_idx])))
      row[-c(last_non_na_idx, species_column_idx)] <- NA
    }
    
    return(row)
  }
  
  #' Expand Taxonomy String into Vector
  #'
  #' Converts a Greengenes-compatible taxonomy string into a vector of taxonomic levels.
  #'
  #' @param tax_string A character string formatted as a Greengenes taxonomy, 
  #'   with levels separated by semicolons and prefixed with rank indicators (e.g., `d__`, `p__`).
  #' @param simplify_species Logical. If TRUE, only the species epithet (e.g., "coli" from "Escherichia coli") is returned
  #'   in the species position. Default is TRUE.
  #' @return A character vector containing the taxonomic levels without prefixes. If `simplify_species` is TRUE, the species 
  #'   level contains only the epithet.
  #' @examples
  #' tax_string <- "d__Bacteria;p__Abditibacteriota;c__Abditibacteriia;o__Abditibacteriales;f__Abditibacteriaceae;g__Abditibacterium;s__Abditibacterium utsteinense"
  #' expand_taxonomy(tax_string, simplify_species = TRUE)
  #' @export
  expand_taxonomy <- function(tax_string, simplify_species = TRUE) {
    # Split the taxonomy string by semicolons
    tax_vector <- unlist(strsplit(tax_string, ";"))
    
    # Remove the rank prefixes (d__, p__, etc.) and extract only the names
    tax_vector <- sub("^[a-z]__*", "", tax_vector)
    
    # If simplify_species is TRUE, extract only the species epithet
    if (simplify_species && length(tax_vector) >= 7) {
      tax_vector[7] <- sub(".* ", "", tax_vector[7]) # Remove genus part, leaving only the species epithet
    }
    
    return(tax_vector)
  }
  
  #' Expand and Merge Taxonomy into Dataframe
  #'
  #' Expands taxonomic strings in a specified column of a given dataframe and merges the extracted 
  #' taxonomic levels into that dataframe. If the required taxonomy columns ("Domain", "Phylum", "Class", 
  #' "Order", "Family") already exist, the function skips processing.
  #'
  #' @param data A dataframe containing a column with taxonomic strings 
  #'   formatted as Greengenes-style taxonomy (i.e., levels separated by semicolons and prefixed with rank indicators).
  #' @param col_name A string specifying the name of the column containing the taxonomic strings.
  #' @return A dataframe with additional columns for expanded taxonomy, specifically "Domain", "Phylum", 
  #'   "Class", "Order", and "Family". The "Genus" and "Species" columns are removed before merging.
  #' @export
  expand_and_merge_taxonomy <- function(data, col_name) {
    required_cols <- c("Domain", "Phylum", "Class", "Order", "Family")
    
    if (!all(required_cols %in% colnames(data))) {
      tax <- data[[col_name]] %>%
        purrr::map(expand_taxonomy) %>%
        purrr::map_dfr(~purrr::set_names(as.list(.), c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species")))
  
      tax <- dplyr::select(tax, -Genus, -Species)
      data <- cbind(tax, data)
    }    
    
    return(data)
  }
  
  #' Get Organism and Genome Information
  #'
  #' This function extracts organism names and their corresponding genome IDs
  #' from a cleaned database. The organism names are formatted as 
  #' "Genus Species Subspecies", with missing subspecies values properly handled.
  #'
  #' @param database A dataframe containing organism data, including columns for genome IDs, genus, species, and subspecies.
  #' @param genome_id_col A character string specifying the column name that contains genome IDs. Defaults to `"IMG Genome ID max genes"`.
  #' @param genus_col A character string specifying the column name that contains genus information. Defaults to `"Genus"`.
  #' @param species_col A character string specifying the column name that contains species information. Defaults to `"Species"`.
  #' @param subspecies_col A character string specifying the column name that contains subspecies information. Defaults to `"Subspecies"`.
  #' @param remove_na_genome A logical value indicating whether to remove organisms with NA genome IDs. Defaults to TRUE.
  #'
  #' @return A dataframe with two columns: `Organism` and `Genome`.
  #'   
  #' @examples
  #' # Example usage
  #' get_organism_by_genome(database_clean, 
  #'                        genome_id_col = "Genome_ID", 
  #'                        genus_col = "Genus_Name",
  #'                        species_col = "Species_Name",
  #'                        subspecies_col = "Subspecies_Name",
  #'                        remove_na_genome = TRUE)
  #'
  #' @importFrom dplyr filter mutate rename select
  #' @export
  get_organism_by_genome <- function(database, 
                                     genome_id_col = "IMG Genome ID max genes", 
                                     genus_col = "Genus", 
                                     species_col = "Species", 
                                     subspecies_col = "Subspecies",
                                     remove_na_genome = TRUE) {
    
    result <- database %>%
      dplyr::mutate(Organism = paste(.data[[genus_col]], .data[[species_col]], .data[[subspecies_col]], sep = " ")) %>%
      dplyr::mutate(Organism = gsub(" NA$", "", Organism)) %>%
      dplyr::rename(Genome = .data[[genome_id_col]]) %>%
      dplyr::select(Organism, Genome) 
    
    if (remove_na_genome) {
      result <- result %>% dplyr::filter(!is.na(Genome) & Genome != "NA")
    }
    
    return(result)
  }
  
  #' Get Choices for Organisms
  #'
  #' This function gets names of organisms that have gene functions in
  #' the database. It can optionally filter organisms based on their 
  #' metabolism type and genome ID column.
  #'
  #' @param database A dataframe containing organism data, including columns 
  #'   for genome IDs, genus, species, and subspecies.
  #' @param genome_id_col A character string specifying the column name that 
  #'   contains genome IDs. Defaults to `"IMG Genome ID max genes"`.
  #' @param metabolism_type_col A character string specifying the column name that 
  #'   contains metabolism type information. Defaults to `"Type of metabolism"`.
  #' @param genus_col A character string specifying the column name that contains 
  #'   genus information. Defaults to `"Genus"`.
  #' @param species_col A character string specifying the column name that contains 
  #'   species information. Defaults to `"Species"`.
  #' @param subspecies_col A character string specifying the column name that contains 
  #'   subspecies information. Defaults to `"Subspecies"`.
  #' @param metabolism_type A character string specifying the metabolism type to filter 
  #'   the organisms by (e.g., "Fermentation", "Methanogenesis"). Defaults to `NULL`, 
  #'   meaning no metabolism filtering is applied.
  #' @param filter_by_genome_id A logical indicating whether to filter out rows with 
  #'   missing or "NA" values in the `genome_id_col`. Defaults to `TRUE`.
  #'
  #' @return A character vector of organism names formatted as "Genus Species Subspecies".
  #'   Entries with missing values in `Subspecies` are cleaned to remove trailing "NA".
  #'   
  #' @examples
  #' # Example usage
  #' get_organism_choices(database_clean, 
  #'                      genome_id_col = "Genome_ID", 
  #'                      metabolism_type_col = "Metabolism",
  #'                      genus_col = "Genus_Name",
  #'                      species_col = "Species_Name",
  #'                      subspecies_col = "Subspecies_Name",
  #'                      metabolism_type = "Fermentation",
  #'                      filter_by_genome_id = FALSE)
  #'
  #' @importFrom dplyr filter mutate pull
  #' @export
  get_organism_choices <- function(database, 
                                   genome_id_col = "IMG Genome ID max genes", 
                                   metabolism_type_col = "Type of metabolism", 
                                   genus_col = "Genus", 
                                   species_col = "Species", 
                                   subspecies_col = "Subspecies", 
                                   metabolism_type = NULL,
                                   filter_by_genome_id = TRUE) {
    
    if (filter_by_genome_id) {
      database <- database %>%
        dplyr::filter(.data[[genome_id_col]] != "NA" & !is.na(.data[[genome_id_col]]))
    }
    
    if (!is.null(metabolism_type)) {
      database <- database %>%
        dplyr::filter(.data[[metabolism_type_col]] == metabolism_type)
    }
    
    database %>%
      dplyr::mutate(choices = paste(.data[[genus_col]], 
                                    .data[[species_col]], 
                                    .data[[subspecies_col]], sep = " ")) %>%
      dplyr::mutate(choices = gsub(" NA$", "", choices)) %>%
      dplyr::pull(choices)
  }
  
  #' Get Choices for Taxa
  #'
  #' This function extracts unique taxonomic names from a dataframe and appends 
  #' the corresponding rank in parentheses.
  #'
  #' @param df A dataframe containing taxonomic ranks as columns (e.g., "Phylum", "Class", 
  #'   "Order", "Family", "Genus", "Species"), with taxon names as values.
  #'
  #' @return A character vector of unique taxon names formatted as "Taxon_Name (Rank)".
  #'
  #' @examples
  #' taxon_df <- data.frame(
  #'   Phylum = c("Abditibacteriota", "Bacillota", "Bacillota"),
  #'   Class = c("Abditibacteriia", "Bacilli", "Erysipelotrichia"),
  #'   Order = c("Abditibacteriales", "Lactobacillales", "Erysipelotrichales")
  #' )
  #' 
  #' get_taxon_choices(taxon_df)
  #'
  #' @export
  get_taxon_choices <- function(data) {
    # Get column names as taxonomic ranks
    ranks <- c("Phylum", "Class", "Order", "Family", "Genus")
    
    data <- data %>% dplyr::select(dplyr::all_of(ranks))
    
    # Convert the dataframe into a vector with the required format
    choices <- unlist(mapply(function(col, rank) {
      paste(data[[col]], sprintf("(%s)", rank))
    }, colnames(data), ranks, SIMPLIFY = FALSE))
    
    # Ensure unique values and remove names
    choices <- unique(unname(choices))
    
    return(choices)
  }
  
  #' Update Query Builder Filters
  #' 
  #' This function loads query filters, filters them based on the selected variable list, 
  #' and updates the query builder input. It includes a brief delay 
  #' to ensure UI elements are fully updated before the builder is updated (otherwise
  #' the update may not be successful).
  #' 
  #' @param inputId A character string specifying the ID of the query builder input.
  #' @param choices A character vector of variable names to filter the query builder filters.
  #' @param delay_time An optional delay in milliseconds before applying the update (default: 250 ms).
  #' 
  #' @return Updates the query builder input dynamically.
  #' 
  #' @examples
  #' update_query_builder("query_builder", choices_traits_taxonomy)
  update_query_builder <- function(inputId, choices, delay_time = 250) {
    filters <- load_query_filters()
    filters <- purrr::keep(filters, ~ .x$id %in% choices)
    
      shinyjs::delay(delay_time, {
        jqbr::updateQueryBuilder(
          inputId = inputId,
          setFilters = filters
        )
      })
  }
  
  #' Update Selectize Input
  #'
  #' This function updates a Selectize input in a Shiny application. It dynamically sets
  #' the available choices and selects a default value.
  #'
  #' @param session The Shiny session object.
  #' @param inputId A character string specifying the ID of the Selectize input to update.
  #' @param choices A character vector of choices to populate the Selectize input.
  #' @param selected A character vector specifying the default selected choice(s).
  #' @param server A logical indicating whether to use server-side processing (default: TRUE).
  #'
  #' @return Updates the specified Selectize input dynamically.
  #'
  #' @examples
  #' update_select_input(session, "gene_functions_database", choices = c("Option 1", "Option 2"))
  update_select_input <- function(session = getDefaultReactiveDomain(), 
                                  inputId, choices = NULL, selected = NULL, server = TRUE) {
    if (is.null(choices)) choices <- character(0)
    if (is.null(selected)) selected <- head(choices, 1)

    shiny::updateSelectizeInput(session, inputId = inputId, choices = choices, selected = selected, server = server)
  }
  
  #' Update Picker Input
  #'
  #' This function updates a Picker input (shinyWidgets) in a Shiny application. It sets
  #' the available choices and selects a default value.
  #'
  #' @param session The Shiny session object.
  #' @param inputId A character string specifying the ID of the Picker input to update.
  #' @param choices A character vector of choices to populate the Picker input.
  #' @param selected A character vector specifying the default selected choice(s). 
  #'
  #' @return Updates the specified Picker input dynamically.
  #'
  #' @examples
  #' update_picker_input(session, "variable_to_display", choices = c("Phylum", "Genus"))
  update_picker_input <- function(session = getDefaultReactiveDomain(), 
                                  inputId, choices = NULL, selected = NULL) {
    if (is.null(choices)) choices <- character(0)
    if (is.null(selected)) selected <- head(choices, 1)
    
    shinyWidgets::updatePickerInput(session, inputId = inputId, choices = choices, selected = selected)
  }
  
  #' Update Checkbox Group Input
  #'
  #' This function updates a checkbox group input in a Shiny application. It dynamically sets
  #' the available choices and selects default values.
  #'
  #' @param session The Shiny session object.
  #' @param inputId A character string specifying the ID of the checkbox group input to update.
  #' @param choices A named list or character vector of choices to populate the checkbox group.
  #' @param selected A character vector specifying the default selected choices. Defaults to NULL.
  #'
  #' @return Updates the specified checkbox group input dynamically.
  #'
  #' @examples
  #' update_checkbox_group(session, "checkboxes_info_organism", choices = c("Genus", "Species"))
  update_checkbox_group <- function(session = getDefaultReactiveDomain(), 
                                    inputId, choices = NULL, selected = NULL) {
    if (is.null(choices)) choices <- character(0)
    
    shiny::updateCheckboxGroupInput(session, inputId = inputId, choices = choices, selected = selected)
  }
  
  #' Count Traits and Organisms with Predictions
  #'
  #' This function calculates the total number of unique traits and organisms in a dataset, 
  #' as well as the number of traits and organisms with predictions available at or above a specified threshold.
  #'
  #' @param df A dataframe containing trait prediction data.
  #' @param organism_col A string specifying the column name that contains organism identifiers. Default is `"Organism number"`.
  #' @param trait_col A string specifying the column name that contains trait categories. Default is `"Trait category"`.
  #' @param value_col A string specifying the column name that contains the predicted probability values. Default is `"Probability"`.
  #' @param threshold A numeric value specifying the minimum probability required for a prediction to be considered valid.
  #'
  #' @return A list containing four values:
  #'   \item{traits_total}{Total number of unique traits in the dataset.}
  #'   \item{traits_predictions}{Number of unique traits with predictions above the threshold.}
  #'   \item{organisms_total}{Total number of unique organisms in the dataset.}
  #'   \item{organisms_predictions}{Number of unique organisms with predictions above the threshold.}
  #'
  #' @importFrom dplyr filter pull n_distinct
  #' @importFrom rlang sym
  #' @export
  count_predictions <- function(df, organism_col = "Organism number", trait_col = "Trait category", value_col = "Probability", threshold = 0.5) {
    # Get dataframe of all variables
    df_total <- df

    # Get dataframe of predicted variables
    df_predicted <- df %>% dplyr::filter(!!rlang::sym(value_col) >= threshold)
    
    # Count organisms
    organisms_total <- df_total %>% 
      dplyr::pull(organism_col) %>% dplyr::n_distinct()
    organisms_predictions <- df_predicted %>% 
      dplyr::pull(organism_col) %>% dplyr::n_distinct()
    
    # Count traits
    traits_total <- df_total %>% 
      dplyr::pull(trait_col) %>% dplyr::n_distinct()
    traits_predictions <- df_predicted %>% 
      dplyr::pull(trait_col) %>% dplyr::n_distinct()
    
    return(
      list(
        traits_total = traits_total, 
        traits_predictions = traits_predictions, 
        organisms_total = organisms_total, 
        organisms_predictions = organisms_predictions)
    )
  }
    
  #' Format Summary Text for Predictions
  #'
  #' Generates a summary statement about the number of traits or end products predicted 
  #' for a given number of organisms.
  #'
  #' @param count1 Integer. The number of predicted traits or end products.
  #' @param count2 Integer. The number of organisms with predictions.
  #' @param label1 Character. The label for the predicted items (e.g., "traits", "end products").
  #' @param label2 Character. The label for the entities being predicted for (e.g., "organisms").
  #' @param total2 Integer (optional). The total number of organisms in the dataset.
  #' 
  #' @return A character string summarizing the predictions.
  #' @examples
  #' format_summary_text(5, 2, "traits", "organisms", 3)
  #' # Returns: "5 traits predicted for 2 of 3 organisms"
  #'
  #' format_summary_text(0, 3, "traits", "organisms", 3)
  #' # Returns: "No traits predicted for 3 organisms"
  #'
  #' @export
  format_summary_text <- function(count1, count2, label1, label2, total2 = NULL) {
    label1_singular <- sub("s$", "", label1)  # Convert to singular form if needed
    label2_singular <- sub("s$", "", label2)  # Convert to singular form if needed
    
    string <- NULL
    
    # First part (e.g., "5 traits predicted for ")
    if(count1 > 1){
      string <- paste0(count1, " ", label1, " predicted for ")
    }else if(count1 == 1){
      string <- paste0(count1, " ", label1_singular, " predicted for ")
    }else if(count1 == 0){
      string <- paste0("No ", label1, " predicted for ")
    }
 
    # Second part (e.g., "2 organisms")
    if(count2 > 1){
      string <- paste0(string, count2, " ", label2)
    } else if(count2 == 1){
      string <- paste0(string, count2, " ", label2_singular)
    } else if(count2 == 0 & total2 > 1){
      string <- paste0(string, total2, " ", label2)
    } else if(count2 == 0 & total2 == 1){
      string <- paste0(string, total2, " ", label2_singular)
    } 
    
    # Third part (e.g., "(No predictions for 1 more organism"))
    if(count1 > 0 & total2 > 1 & (total2 - count2)>1){
      string <- paste0(string, " (", total2 - count2, " more ", label2, " had no predictions)")
    }
    
    if(count1 > 0 & total2 > 1 & (total2 - count2)==1){
      string <- paste0(string, " (", total2 - count2, " more ", label2_singular, " had no predictions)")
    }
    
    return(string)
  }
  
  #' Save Data in Original Format and Zip It
  #'
  #' This function saves a dataframe in its original format (e.g., CSV, RDS), 
  #' compresses it into a ZIP archive, and optionally removes the original file.
  #'
  #' @param data A dataframe to be saved.
  #' @param fp A character string specifying the full file path, including the original extension (e.g., "data/file.csv").
  #' @param remove_original A logical value indicating whether to delete the original file after zipping. Default is `TRUE`.
  #' @param overwrite A logical value indicating whether to overwrite an existing ZIP file. Default is `TRUE`.
  #' 
  #' @return The function does not return anything. It writes the file to disk.
  #' 
  #' @export
  save_as_zip <- function(data, fp, remove_original = TRUE, overwrite = TRUE) {
    # Ensure the zip package is available
    if (!requireNamespace("zip", quietly = TRUE)) stop("Package 'zip' is required. Install it with install.packages('zip')")
    
    # Normalize the file path
    fp <- normalizePath(fp, winslash = "/", mustWork = FALSE)
    
    # Extract file name, extension, and directory
    ext <- tools::file_ext(fp)
    if (ext == "") stop("File path must include an extension (e.g., .csv, .rds).")
    
    file_name <- basename(fp)  # e.g., "database_clean.csv"
    dir_name <- dirname(fp)    # e.g., "data"
    
    # Create a folder with the same name as the object
    object_name <- tools::file_path_sans_ext(file_name)  # e.g., "database_clean"
    zip_folder <- file.path(dir_name, object_name)
    
    # Ensure the folder is clean
    if (dir.exists(zip_folder)) unlink(zip_folder, recursive = TRUE)
    dir.create(zip_folder)
    
    # Define paths
    save_fp <- file.path(zip_folder, file_name)  # Save inside the new folder
    zip_fp <- file.path(dir_name, paste0(object_name, ".zip"))  # ZIP file in same directory
    
    # Save file based on extension
    switch(ext,
           "csv" = write.csv(data, save_fp, row.names = FALSE),
           "rds" = saveRDS(data, save_fp),
           stop("Unsupported file format: ", ext)
    )
    
    # Overwrite existing ZIP file if specified
    if (overwrite && file.exists(zip_fp)) file.remove(zip_fp)
    
    # Create ZIP file using zip::zipr()
    zip::zipr(zip_fp, files = save_fp, recurse = FALSE, compression_level = 9)
    
    # Ensure the ZIP file was created before deleting the original
    if (file.exists(zip_fp) && remove_original) unlink(zip_folder, recursive = TRUE)
  }
 
# === Format organism names in plots === # 
  #' Concatenate selected columns into a single string
  #'
  #' This function merges multiple columns in a dataframe into a single column, 
  #' using a specified separator. It optimizes performance by applying transformations 
  #' only to unique rows and mapping them back to the full dataframe.
  #'
  #' @param df A dataframe containing the columns to be merged.
  #' @param name_col A string specifying the name of the new column where the merged text will be stored.
  #' @param cols A character vector of column names to be concatenated.
  #' @param sep A string separator used to join elements. Default is a space (" ").
  #' @param na.rm Logical; if TRUE, removes NA values before merging. Default is TRUE.
  #' @param remove Logical; if TRUE, removes original columns after merging. Default is FALSE.
  #'
  #' @return A dataframe with a new column containing the merged text.
  #' @export
  concatenate_columns <- function(df, name_col = "Organism name", cols, sep = " ", na.rm = TRUE, remove = FALSE) {
    if (!all(cols %in% names(df))) {
      stop("One or more specified columns do not exist in the dataframe.")
    }
    
    # Create a distinct map of unique rows and their collapsed values
    map <- df %>%
      dplyr::distinct(dplyr::across(all_of(cols))) %>%
      dplyr::mutate(
        !!name_col := tidyr::unite(., col = !!name_col, dplyr::all_of(cols), sep = sep, na.rm = na.rm, remove = FALSE)[[name_col]]
      ) %>%
      dplyr::select(dplyr::all_of(cols), !!rlang::sym(name_col))
    
    # Merge the collapsed values back to the original dataframe
    df <- df %>%
      dplyr::left_join(map, by = cols)
    
    # Remove original columns if requested
    if (remove) {
      df <- df %>%
        dplyr::select(-dplyr::all_of(cols))
    }
    
    return(df)
  }
  
  #' Generate abbreviated organism names
  #'
  #' This function creates an abbreviated name for each unique organism entry in a dataframe.
  #' - If only one word is present, "sp." is appended.
  #' - If multiple words are present and the last word is lowercase, the last two words are returned.
  #' - Otherwise, the last word is returned with "sp." appended.
  #'
  #' This function optimizes performance by applying abbreviations only to unique values
  #' and then mapping them back to the original dataframe.
  #'
  #' @param df A dataframe containing organism names.
  #' @param name_col A string specifying the column with organism names to be abbreviated.
  #'
  #' @return A dataframe with `name_col` replaced by its abbreviated form.
  #' @export
  create_organism_abbreviations <- function(df, name_col) {
    if (!name_col %in% names(df)) {
      stop("The specified name_col does not exist in the dataframe.")
    }
    
    # Create a distinct map of unique names and their abbreviations
    map <- df %>%
      dplyr::distinct(!!rlang::sym(name_col)) %>%
      dplyr::mutate(
        Abbreviated_Name = purrr::map_chr(!!rlang::sym(name_col), function(text) {
          words <- unlist(strsplit(text, "\\s+"))
          n <- length(words)
          
          if (n == 0 || is.na(text)) {
            return(NA_character_)
          } else if (n == 1) {
            return(paste(words, "sp."))
          } else if (grepl("^[a-z]+$", words[n])) {
            return(paste(words[n - 1], words[n]))
          } else {
            return(paste(words[n], "sp."))
          }
        })
      )
    
    # Merge the abbreviation map back to the original dataframe
    df <- df %>%
      dplyr::left_join(map, by = name_col) %>%
      dplyr::select(-!!rlang::sym(name_col)) %>%
      dplyr::rename(!!rlang::sym(name_col) := Abbreviated_Name)
    
    return(df)
  }
  
  
  #' Ensure unique organism names in a dataframe
  #'
  #' This function ensures that names in a specified column are unique by appending numeric suffixes when duplicates exist.
  #' If an index column is provided, names will be made unique only within the same index.
  #'
  #' @param df A dataframe containing organism names.
  #' @param name_col A string specifying the column name containing organism names.
  #' @param index_col Optional; a string specifying the column to group uniqueness checks by. If NULL, names are made unique across the entire dataframe.
  #'
  #' @return A dataframe with unique names in the specified column.
  #' @export
  ensure_unique_names <- function(df, name_col, index_col = NULL) {
    if (!name_col %in% names(df)) {
      stop("The specified name_col does not exist in the dataframe.")
    }
    
    if (!is.null(index_col) && !index_col %in% names(df)) {
      stop("The specified index_col does not exist in the dataframe.")
    }
    
    # Create mapping of unique names within index_col
    map <- df %>%
      dplyr::group_by(!!rlang::sym(index_col),
                      !!rlang::sym(name_col)) %>%
      dplyr::distinct(!!rlang::sym(name_col), .keep_all = TRUE) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(!!rlang::sym(name_col)) %>%
      dplyr::mutate(
        count = dplyr::n(),  # Count occurrences within group
        id = ifelse(count == 1, "", as.character(dplyr::row_number()))  # Assign "" if unique, else sequential number
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(!!rlang::sym(index_col), !!rlang::sym(name_col), id)
    
    # Apply numbers from the map back to the original dataframe
    df <- df %>%
      dplyr::left_join(map, by = c(index_col, name_col)) %>%
      dplyr::mutate(!!name_col := paste0(.data[[name_col]], ifelse(id == "", "", paste0(" ", id)))) %>%
      dplyr::select(-id)  # Remove helper column
    
    return(df)
  }
  
  #' Format organism names from a selection of columns
  #'
  #' This function takes a dataframe and a selection of columns (e.g., `Phylum` to `Species`),
  #' collapses them into a single organism name (if `abbreviate_names = TRUE`), 
  #' creates an abbreviation, and ensures that names are unique **within the same organism number**.
  #'
  #' @param df A dataframe containing taxonomy or organism name columns.
  #' @param cols A character vector of column names to be collapsed into an organism name.
  #' @param name_col A string specifying the new column name for the formatted organism names. Default is "Organism name".
  #' @param index_col A string specifying a column to ensure uniqueness within groups. Default is "Organism number".
  #' @param abbreviate_names Logical; if TRUE (default), collapses taxon columns and generates organism abbreviations. If FALSE, ensures uniqueness only.
  #'
  #' @return A dataframe with a new column containing formatted and unique organism names.
  #' @export
  format_organism_names <- function(df, cols, name_col = "Organism name", index_col = "Organism number", abbreviate_names = TRUE) {
    if (!index_col %in% names(df)) {
      stop("The specified index_col does not exist in the dataframe.")
    }
    
    if (abbreviate_names) {
      if (!all(cols %in% names(df))) {
        stop("One or more specified columns do not exist in the dataframe.")
      }
      
      # Collapse taxon columns into a single string
      df <- concatenate_columns(df, name_col = name_col, cols = cols)
      
      # Create organism abbreviations
      df <- create_organism_abbreviations(df, name_col)
    }
    
    # Ensure names are unique
    df <- ensure_unique_names(df, name_col, index_col)
    
    return(df)
  }