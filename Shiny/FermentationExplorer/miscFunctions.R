#Define file input box with link to modal
fileInput_modal=function(inputId, label, multiple = FALSE, accept = NULL, width = NULL, buttonLabel = "Browse...", placeholder = "No file selected", modalId, modalLabel) 
{
  restoredValue <- restoreInput(id = inputId, default = NULL)
  if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
    warning("Restored value for ", inputId, " has incorrect format.")
    restoredValue <- NULL
  }
  if (!is.null(restoredValue)) {
    restoredValue <- toJSON(restoredValue, strict_atomic = FALSE)
  }
  inputTag <- tags$input(id = inputId, name = inputId, type = "file", 
                         style = "display: none;", `data-restore` = restoredValue)
  if (multiple) 
    inputTag$attribs$multiple <- "multiple"
  if (length(accept) > 0) 
    inputTag$attribs$accept <- paste(accept, collapse = ",")
  div(
    class = "form-group shiny-input-container", style = if (!is.null(width)) 
      paste0("width: ", validateCssUnit(width), ";"),
    shinyInputLabel(inputId, label), 
    div(
      class = "input-group", 
      tags$label(class = "input-group-btn input-group-prepend", 
                 span(class = "btn btn-default btn-file", 
                      buttonLabel, inputTag)), 
      tags$input(type = "text", 
                 class = "form-control", placeholder = placeholder, 
                 readonly = "readonly")),
    actionLink(inputId = modalId, label = modalLabel),
    tags$div(
      id = paste(inputId, "_progress", sep = ""), class = "progress active shiny-file-input-progress", 
      tags$div(class = "progress-bar")),
  )
}

#Define helper function for upload file box function
shinyInputLabel <- function(inputId, label = NULL) {
  tags$label(
    label,
    class = "control-label",
    class = if (is.null(label)) "shiny-label-null",
    `for` = inputId
  )
}

#Validate an input and launch a modal if an error
##Modified from shiny::validate() to launch modal rather than returning a simple text output
validateShowModal <- function(..., errorClass = character(0)) 
{
  results <- sapply(list2(...), function(x) {
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
  
  #Launches modal
  showModal(modalDialog(
    h4("Error in input"),
    p(paste(results)),
    easyClose = TRUE, footer = NULL
  )
  )
  
  reactiveStop(paste("", collapse = "\n"), c(errorClass, 
                                             "validation"))
}

#Helper function for validateShowModal
reactiveStop <- function(message = "", class = NULL) {
  stopWithCondition(c("shiny.silent.error", class), message)
}

#Helper function for validateShowModal
stopWithCondition <- function(class, message) {
  cond <- structure(
    list(message = message),
    class = c(class, 'error', 'condition')
  )
  stop(cond)
}