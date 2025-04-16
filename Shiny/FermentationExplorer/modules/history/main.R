# Define the History Module in Shiny App
# This script defines the user interface (UI) and server for the 
# prediction history module.
# Author: Timothy Hackmann
# Date: 10 April 25

# === Define user interface (UI) ===
historyUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # Inject Javascript handlers
    tags$head(
      tags$script(HTML(sprintf("registerJobTableHandlers('%s');", ns("job_table"))))
    ),
    
    # --- Loading screen ---
    create_loading_screen("history-loading-screen"), 
    
    # --- Main UI (initially hidden ) ---
    shinyjs::hidden(
      div(id = "history-wrapper",
            
      # Title
      create_title_div("Prediction history"),
      
      # Main content area
      bslib::card(
        bslib::card_header("Past Jobs"),
        
        # Table and buttons
          # Scrollable table
          div(
            create_data_table(inputId = ns("job_table"))
          ),
          
        # Action buttons
        div(
          class = "d-flex align-items-center justify-content-between",  # Space between left and right
          # Left group: Select All and Delete
          div(
            class = "d-flex align-items-center",
            actionButton(ns("select_all_toggle"), "Select All", class = "btn btn-secondary me-2"),
            actionButton(ns("delete_selected"), "Delete selected", class = "btn btn-danger")
          ),
          # div( # uncomment to activate ability to view all users
          #   shiny::conditionalPanel(
          #     condition = "output.flag_localhost",
          #     ns = ns,
          #     actionButton(ns("view_all_users"), "View all users", class = "btn btn-primary")
          #   )
          # )
        )
      )
     )
    )
  )
}


# === Define server ===
historyServer <- function(input, output, session, x, selected_tab, show_all_users = FALSE) {
  # Set namespace
  ns <- session$ns
  
  # --- Define triggers for reactive expressions ---
  tab_selected_trigger <- make_tab_trigger(selected_tab, "history")
  manual_trigger <- make_manual_trigger()
  regenerate_table_trigger <- manual_trigger$trigger
  trigger_reexecute <- manual_trigger$reexecute
  combined_trigger <- or_trigger(tab_selected_trigger, regenerate_table_trigger)
  
  # --- Define reactive values ---
  # Value for toggling checkboxes
  select_all_state <- reactiveVal(TRUE) 
  # show_all <- reactiveVal(FALSE) # uncomment to activate ability to view all users
  
  # --- Process inputs ---
  get_table <- eventReactive(combined_trigger(), {
    # Identify user job directories
    if (show_all_users) {
      ip_dirs <- list.dirs("jobs", recursive = FALSE, full.names = TRUE)
    } else {
      ip <- get_user_ip(session)
      ip_dirs <- file.path("jobs", ip)
    }
    
    # List all job subdirectories and .rds job files
    tool_dirs <- list.dirs(ip_dirs, recursive = TRUE, full.names = TRUE)
    job_files <- list.files(tool_dirs, pattern = "\\.rds$", full.names = TRUE)
    
    # Handle case of no jobs found
    if (length(job_files) == 0) {
      return(data.frame(
        Select = NA,
        Tool = "No jobs found.",
        Time = NA,
        Size = NA,
        Job = NA,
        User = NA,
        stringsAsFactors = FALSE
      ))
    }
    
    # Extract job metadata from file paths
    job_ids <- gsub("\\.rds$", "", basename(job_files))            # Remove .rds extension
    file_info <- file.info(job_files)                              # Get file timestamps and sizes
    subdirs <- dirname(job_files)                                  # Get tool subdirectories
    tabs <- basename(subdirs)                                      # Get tab names (tool IDs)
    users <- basename(dirname(subdirs))                            # Get IP-level folders (user IDs)
    users_display <- vapply(users, format_ip_for_display, character(1))  # Format IP for display
    
    # Map internal tab names to user-friendly tool labels
    tool_labels <- dplyr::recode(tabs,
                                 databaseSearch = "Database search",
                                 predictionsTaxonomy = "Taxonomy",
                                 predictionsMachineLearning = "Machine learning",
                                 predictionsNetwork = "Metabolic networks",
                                 .default = "Unknown"
    )
    
    # Construct job URLs with query parameters
    tab_query <- paste0("?tab=", tabs, "&job=", job_ids)
    urls <- paste0(
      session$clientData$url_protocol, "//",
      session$clientData$url_hostname,
      if (!is.null(session$clientData$url_port)) paste0(":", session$clientData$url_port),
      session$clientData$url_pathname,
      tab_query
    )
    
    # Convert file size to MB and round
    file_size_KB <- round(file_info$size / 1024, 0)
    
    # Create a display table for the UI
    df <- data.frame(
      Select = sprintf("<input type='checkbox' class='row_checkbox' name='row_selected' value='%s'>", job_ids),
      Tool = tool_labels,
      Time = format(file_info$mtime, "%Y-%m-%d %H:%M:%S"),
      `Size (KB)` = file_size_KB,
      User = users_display,
      Job = paste0("<a href='", urls, "' target='_blank'>", job_ids, "</a>"),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    
    # For admin view, show raw user IDs instead of formatted ones
    if (show_all_users) {
      df$User <- users
    }
    
    df
  })
  
  # # Show all users # uncomment to activate ability to view all users
  # observeEvent(input$view_all_users, {
  #   show_all(!show_all())
  #   trigger_reexecute()
  # })
  
  # Disable buttons when no jobs exist
  observe({
    tbl <- get_table()
    has_data <- !all(is.na(tbl$Select)) && nrow(tbl) > 0
    
    shinyjs::toggleState("select_all_toggle", condition = has_data)
    shinyjs::toggleState("delete_selected", condition = has_data)
  })
  
  # --- Update user interface (UI) elements ---
  # Hide loading screen
  observeEvent({tab_selected_trigger},
  {
    # Hide loading screen
    shinyjs::runjs("shinyjs.hide('history-loading-screen'); shinyjs.show('history-wrapper');")
  })
  
  # Handle select/deselect all
  observeEvent(input$select_all_toggle, {
    # Send JS instruction to (de)select checkboxes
    session$sendCustomMessage(ns("job_table_toggle_checkboxes"), select_all_state())
    
    # Flip state
    select_all_state(!select_all_state())
    
    # Update button label
    updateActionButton(session, "select_all_toggle",
                       label = if (select_all_state()) "Select All" else "Deselect All")
  })
  
  # Delete button click
  observeEvent(input$delete_selected, {
    session$sendCustomMessage(ns("job_table_get_selected"), list())
  })
  
  # Handle received selected job IDs
  observeEvent(input$job_table_selected_jobs, {
    selected_ids <- input$job_table_selected_jobs
    
    ip <- get_user_ip(session)
    ip_dir <- file.path("jobs", ip)
    
    for (id in selected_ids) {
      matches <- list.files(ip_dir, recursive = TRUE, full.names = TRUE, pattern = paste0("^", id, "\\.rds$"))
      file.remove(matches)
    }
    
    # Refresh table
    trigger_reexecute()
  })
  
  # --- Generate outputs ---
  # Create output flags
  flag_if_not_null(output, "flag_localhost", trigger = reactive(TRUE), 
                   value_fun = function() is_running_locally())
  
  # observe(print(output$flag_localhost))  # or use reactiveLog()
  
  # Output table with jobs
  output$job_table <- DT::renderDataTable({
    get_table()
  }, escape = FALSE, selection = "none", rownames = FALSE,
  options = list(
    pageLength = 10,
    order = list(list(2, 'desc'))
  ))
}
