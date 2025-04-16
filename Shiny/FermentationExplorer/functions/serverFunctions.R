#' Server Functions for App
#' 
#' This script defines functions for the server function of
#' of the Shiny app. These functions modularize common elements to improve 
#' readability, maintainability, and consistency across modules.
#' 
#' @author Timothy Hackmann
#' @date 9 Mar 2025

# === Reactive flags ===
  #' Create a reactive output flag that returns TRUE if value is not NULL
  #'
  #' This helper is typically used to define `output$flag_*` values for controlling
  #' visibility of UI elements via `shiny::conditionalPanel()`. It assigns a reactive
  #' expression to `output[[output_id]]` that returns TRUE if `value_fun()` is not NULL.
  #'
  #' @param output The output object from the server function (e.g. `output`)
  #' @param output_id The name of the output (as string) to assign to
  #' @param trigger A reactive expression used to trigger reevaluation
  #' @param value_fun A function that returns the value to test for NULL
  #' @param label Optional label for debugging (used in eventReactive)
  #'
  #' @return None (side effect: assigns to output)
  #'
  #' @examples
  #' flag_if_not_null(output, "flag_results", trigger = make_trigger, value_fun = get_results)
  #'
  #' # In UI:
  #' # conditionalPanel("output.flag_results", ...)
  flag_if_not_null <- function(output, output_id, trigger, value_fun, label = NULL) {
    output[[output_id]] <- shiny::eventReactive(trigger(), {
      !is.null(value_fun())
    }, label = label %||% output_id)
    
    shiny::outputOptions(output, output_id, suspendWhenHidden = FALSE)
  }
  
  #' Create a reactive output flag that returns TRUE if value has multiple elements or columns
  #'
  #' This helper is typically used to define `output$flag_*` values for controlling
  #' visibility of UI elements via `shiny::conditionalPanel()`. It assigns a reactive
  #' expression to `output[[output_id]]` that returns TRUE if `value_fun()` has more
  #' than one element (or more than one column, if a data.frame or matrix).
  #'
  #' @param output The output object from the server function
  #' @param output_id The name of the output (as string) to assign to
  #' @param trigger A reactive expression used to trigger reevaluation
  #' @param value_fun A function that returns a vector, list, or data.frame
  #' @param label Optional label for debugging (used in eventReactive)
  #'
  #' @return None (side effect: assigns to output)
  #'
  #' @examples
  #' flag_if_multiple(output, "flag_multiple_organisms", trigger = make_trigger, value_fun = get_gene_functions)
  #'
  #' # In UI:
  #' # conditionalPanel("output.flag_multiple_organisms", ...)
  flag_if_multiple <- function(output, output_id, trigger, value_fun, label = NULL) {
    output[[output_id]] <- shiny::eventReactive(trigger(), {
      val <- value_fun()
      
      if (length(val) > 1) {
        return(TRUE)
      } else if (length(val) == 1 && (is.data.frame(val) || is.matrix(val))) {
        return(ncol(val) > 1)
      } else {
        return(FALSE)
      }
    }, label = label %||% output_id)
    
    shiny::outputOptions(output, output_id, suspendWhenHidden = FALSE)
  }

# === Reactive triggers ===
  #' Create a reactive trigger based on one or more expressions
  #'
  #' This function is usually used to define a trigger for reactive outputs or observers.
  #'
  #' @param ... Expressions to track. Can be individual inputs or expressions.
  #'
  #' @return A reactive expression (used to trigger updates)
  #' @examples
  #' my_trigger <- make_trigger(input$go_button, input$some_setting)
  make_trigger <- function(...) {
    exprs <- rlang::enquos(...)
    
    reactive({
      lapply(exprs, function(expr) rlang::eval_tidy(expr))
    })
  }
  
  #' Trigger when an action button is clicked (without explicitly passing `input`)
  #'
  #' @param button_id The ID of the action button
  #' @param input The Shiny input object. Defaults to `getDefaultReactiveDomain()$input`.
  #' @return A reactive expression that triggers after clicking the action button
  make_action_button_trigger <- function(button_id, input = getDefaultReactiveDomain()$input) {
    make_trigger(req(input[[button_id]] > 0))
  }
  
  #' Trigger when a specific tab is selected
  #'
  #' This function creates a reactive trigger that activates only when the currently
  #' selected tab matches a specified tab name. It is useful for delaying computations
  #' or outputs until a particular tab is in view.
  #'
  #' @param tab_fn A reactive expression that returns the name of the currently selected tab.
  #' @param tab_name A character string specifying the name of the tab to trigger on.
  #'
  #' @return A reactive expression that returns TRUE when the selected tab matches `tab_name`.
  #'
  #' @examples
  #' # Trigger only when 'resultsTab' is selected
  #' tab_trigger <- make_tab_trigger(selected_tab, "resultsTab")
  #'
  #' @export
  make_tab_trigger <- function(tab_fn = selected_tab, tab_name) {
    reactive({
      if (tab_fn() == tab_name) {
        return(TRUE)
      }
    })
  }
  
  #' Create a reactive trigger based on a query parameter and active tab in the URL
  #'
  #' This creates a reactive expression that returns TRUE when a given query parameter (e.g., ?job=xyz)
  #' is present and the tab in the URL matches the expected tab name.
  #'
  #' @param session The Shiny session object
  #' @param param_name The query parameter to look for (default = "job")
  #' @param tab_name Optional tab name to match against the ?tab=... value in the URL
  #'
  #' @return A reactive expression that returns TRUE when both the query parameter is set and the tab matches
  #' @export
  make_url_trigger <- function(session = getDefaultReactiveDomain(), param_name = "job", tab_name = NULL) {
    reactive({
      query <- parseQueryString(session$clientData$url_search)

      value <- query[[param_name]]
      tab   <- query$tab

      if (is.null(value) || value == "") return(NULL)
      if (!is.null(tab_name) && tab != tab_name) return(NULL)

      return(TRUE)
    })
  }
  
  #' Create a reactive trigger that returns TRUE if a job was loaded at app initialization
  #'
  #' This trigger checks the `?job=` query parameter when the app first loads.
  #' It can be used by other observers or modules to restore previous inputs or state
  #' based on whether the user opened the app with a job preloaded.
  #'
  #' The result is stored in `session$userData$loaded_job_on_init` during tab synchronization
  #' (`sync_tabs_with_query()`), and this trigger reads that value reactively.
  #'
  #' @param session The Shiny session object.
  #'
  #' @return A reactive expression that returns TRUE if a job was loaded at init, else NULL.
  #' @export
  make_restore_trigger <- function(session = getDefaultReactiveDomain()) {
    reactive({
      val <- session$userData$loaded_job_on_init
      if (isTRUE(val)) TRUE else NULL
    })
  }
  
  #' Force re-execution of a reactive expression (trigger)
  #'
  #' This works by wrapping the trigger with a reactiveVal counter.
  #' It assumes the trigger has no side effects and is used only for dependency tracking.
  #'
  #' @param trigger A reactive expression created by make_trigger() or similar.
  #' @return A function that forces the trigger to re-execute.
  #' @export
  make_manual_trigger <- function() {
    counter <- reactiveVal(0)
    list(
      trigger = reactive({ counter(); TRUE }),
      reexecute = function() counter(counter() + 1)
    )
  }
  
  #' Create a general-purpose reactive trigger
  #'
  #' A wrapper around make_trigger() for naming consistency.
  #' Use this when the trigger doesn't fit other named categories (e.g., tab or button triggers).
  #'
  #' @inheritParams make_trigger
  #' @return A reactive expression (used to trigger updates)
  #' @export
  make_other_trigger <- function(...) {
    make_trigger(...)
  }
  
  #' Create a reactive trigger that fires when any of two triggers activate
  #'
  #' This function returns a reactive trigger that invalidates whenever
  #' either `trigger1` or `trigger2` fires. It uses a reactiveVal-based counter
  #' that increments on each event, forcing reactivity.
  #'
  #' @param trigger1 A reactive expression (e.g. an eventReactive or reactive)
  #' @param trigger2 A second reactive expression
  #' @param label Optional label for debugging/logging
  #'
  #' @return A reactive expression that returns an incrementing counter
  #'
  #' @examples
  #' combined_trigger <- make_combined_trigger(tab_selected_trigger, make_predictions_trigger)
  #' observeEvent(combined_trigger(), {
  #'   cat("Triggered by either source\n")
  #' })
  #'
  #' @export
  or_trigger <- function(trigger1, trigger2, label = NULL) {
    counter <- reactiveVal(0)
    
    observeEvent(trigger1(), {
      counter(counter() + 1)
    })
    
    observeEvent(trigger2(), {
      counter(counter() + 1)
    })
    
    reactive({
      counter()
    })
  }
  
# === Other ====
  #' Output Modal with Example Data
  #'
  #' This function outputs a modal with example data.  It creates a Shiny observer 
  #' to detect when the user wants to launch the modal, generates download handlers
  #' for the files, then launches the modal.  
  #'
  #' @param input The Shiny input object. Defaults to `getDefaultReactiveDomain()$input`.
  #' @param output The Shiny output object. Defaults to `getDefaultReactiveDomain()$output`.
  #' @param input_id The ID of the input that triggers the modal (e.g., `gene_functions_modal` in `input$gene_functions_modal`).
  #' @param output_id The ID that appears in the output file (e.g., `file` in `output$file`).
  #' @param object_ids A character vector of object IDs that map to `load_` functions.
  #' @param labels A character vector of human-readable labels corresponding to the `object_ids`.
  #' @param file_types Optional character vector indicating file type for each object (`"csv"` by default).
  #' @param ns A namespace function for module compatibility.
  #' @param title The title to display in the modal.
  #' @param label An optional label for the observer (for debugging).
  #'
  #' @export
  output_download_modal <- function(input = getDefaultReactiveDomain()$input,
                                    output = getDefaultReactiveDomain()$output,
                                    input_id, output_id = "file", object_ids, labels,
                                     file_types = rep("csv", length(object_ids)),
                                     ns = identity, title = "Example files", label = NULL) {
    # Create observer
    shiny::observeEvent(input[[input_id]], {
      stopifnot(length(object_ids) == length(labels))
      stopifnot(length(object_ids) == length(file_types))
      
      # Generate download handlers for files
      for (i in seq_along(object_ids)) {
        local({
          i_local <- i
          output_id <- paste0(output_id, "_", i_local)
          load_func_name <- paste0("load_", object_ids[i_local])
          
          output[[output_id]] <- create_download_handler(
            filename_prefix = object_ids[i_local],
            data_source = function() do.call(load_func_name, list()),
            file_type = file_types[i_local]
          )
        })
      }

      downloads_named_list <- stats::setNames(labels, paste0(output_id, "_", seq_along(labels)))

      # Launch modal
      showDownloadModal(
        ns = ns,
        title = title,
        downloads = downloads_named_list
      )
    }, label = label)
  }
  
  #' Output Modal for Missing Files
  #'
  #' This function creates a Shiny observer that triggers a modal with an error message
  #' when a specific input is activated (e.g., clicking a faux download button).
  #'
  #' @param input The Shiny input object. Defaults to `getDefaultReactiveDomain()$input`.
  #' @param input_id The ID of the input that triggers the modal (e.g., `"null_download"`).
  #' @param title The title to display in the modal dialog. Default is `"Download Unavailable"`.
  #' @param message The body text to show in the modal dialog.
  #' @param ns A namespace function for module compatibility. Use `session$ns` in modules; defaults to `identity` for non-modular use.
  #' @param label An optional label for the observer, useful for debugging.
  #'
  #' @return A Shiny observer that shows an error modal when the input is triggered.
  #' @export
  output_missing_files_modal <- function(input = getDefaultReactiveDomain()$input, 
                                         input_id,
                                         title = "Data Unavailable",
                                         message = "Data is not available to download.",
                                         ns = identity,
                                         label = NULL) {
    observeEvent(input[[input_id]], {
      # Set inputs and outputs
      showErrorModal(ns = ns, title = title, message = message)
    }, label = label)
  }
  
  #' Navigate User to Help
  #'
  #' This function will navigate the user to help when they click on the appropriate link.
  #' It creates an Shiny observer to detect when the link is clicked, navigates the user, 
  #' and then closes any active modals.  
  #'
  #' @param session The session object
  #' @param selected_tab The tab to switch to
  #' @param selected_panel The panel to activate inside navlist
  #' @param label Optional label for the observer
  navigate_to_help <- function(input, session, selected_tab, selected_panel, label = NULL) {
    # Create observer
    observeEvent(getDefaultReactiveDomain()$input$go_to_help, {
      # Navigate user to help
      updateNavbarPage(session = session, inputId = "tabs", selected = selected_tab)
      updateNavlistPanel(session = session, inputId = "navlist_panel", selected = selected_panel)
      
      # Close open modals
      removeModal()
    }, label = label %||% "go_to_help")
  }