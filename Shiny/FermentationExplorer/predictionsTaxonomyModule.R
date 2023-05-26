#################
#Define functions
#################
#Filter organisms by preferred taxonomy
filter_by_taxonomy = function(data, taxonomy_type)
{
  if(taxonomy_type=="NCBI")
  {
    data = data %>% filter(NCBI_Phylum!="NA")
    data$Phylum = data$NCBI_Phylum
    data$Class = data$NCBI_Class
    data$Order = data$NCBI_Order
    data$Family = data$NCBI_Family
    data$Genus = data$NCBI_Genus
    data$Species = data$NCBI_Species
  }else if(taxonomy_type=="Bergey")
  {
    data = data %>% filter(Phylum!="NA")
  }

  #Get last word in species name (removes genus name if included)
  data$Species = word(data$Species,-1)

  return(data)
}

#Take semi-colon separated values in a column in a dataframe, then expand them into multiple columns
expand_column <- function(df, col_name) {
  # Split the values in the given column by the separator
  split_values <- strsplit(as.character(df[[col_name]]), ";")

  # Get the unique values in the specified column
  unique_values <- unique(unlist(split_values))

  # Remove NA values
  unique_values <- unique_values[!is.na(unique_values)]
  unique_values <- unique_values[unique_values!="NA"]

  # Iterate over unique values and create new binary columns
  for (value in unique_values) {
    new_col <- sapply(split_values, function(x) {
      if (any(is.na(x))) {
        return(NA)
      }
      if (value %in% x) {
        return(1)
      } else {
        return(0)
      }
    })
    df[[value]] <- new_col
  }

  # Drop the original column
  df[[col_name]] <- NULL

  return(df)
}

#Take a set of columns in a dataframe, collapse values into a semi-colon separated elements, and then place them in a new column
collapse_columns <- function(df, cols, new_col_name) {
  # Create a new column with concatenated values from the specified columns
  df[[new_col_name]] <- sapply(1:nrow(df), function(i) {
    row_values <- df[i, cols]

    values <- cols[row_values == 1]
    # Remove NA values from the values vector
    values <- values[!is.na(values) & values != "NA"]

    if (length(values) > 0) {
      return(paste(values, collapse = ";"))
    } else {
      return(NA)
    }
  })

  # Drop the specified columns
  df <- df[, !(colnames(df) %in% cols)]

  return(df)
}

# Clean data from external sources (BacDive and FAPROTAX)
clean_external_data = function(x, is_numeric=FALSE, to_lower=FALSE)
{
  if(is_numeric==TRUE)
  {
    x =  as.character(x)

    # Replace all patterns at once
    x = gsub(pattern="<|>|day[s]*|_| |\\.$|\\.-", replacement="", x=x)

    # Calculate means of split values
    z = strsplit(x, "-")
    z = lapply(z, function(y) {
      num_values <- y[!grepl("\\D", y)]  # Filter out non-numeric values
      if(length(num_values) > 0) as.numeric(num_values) else NA
    })
    x = sapply(z, mean, na.rm = TRUE)

    x = as.numeric(x)

  } else if(is_numeric==FALSE) {
    x = gsub(pattern="^0$", replacement="-", x = x)
    x = gsub(pattern="^1$", replacement="+", x = x)
    x = gsub(pattern="#", replacement="", x = x)
    x = gsub(pattern="_", replacement=" ", x = x)
  }

  if(to_lower==TRUE)
  {
    x = tolower(x)
  }

  return(x)
}

###########################
#Define user interface (UI)
###########################
predictionsTaxonomyUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width=12,
             h3("Predict traits from taxonomy")),
    ),
    sidebarPanel(width = 3,
                 fileInput_modal(ns("file_query_taxonomy"), "Upload names of taxa", multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"), modalId = ns("taxonomy_file_modal"), modalLabel = "Download example"),
                 sliderInput(ns("threshold"), "Prediction threshold", min = 0, max = 1, value = 0.5),
                 div(class = "vertical-container",
                     tags$b("Simplify names of taxa"),
                     switchInput(inputId = ns("simplify_names"), value = TRUE,  size = "small", inline=TRUE)
                 ),
                 radioGroupButtons(inputId=ns("predictionsTaxonomy_filter"), label="Taxonomy", choices=list("NCBI", "Bergey"), selected=list("NCBI")),
                 actionButton(ns("make_predictions"), "Make predictions", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    ),
    conditionalPanel(
      condition = "input.make_predictions == 0 | output.check_file_taxonomy", 
      ns = ns,
      h4("Please upload files and make selections at left")
    ),
    conditionalPanel(
      condition = "input.make_predictions > 0 & !output.check_file_taxonomy", 
      ns = ns,
      mainPanel(width = 9,
                div(
                  id = "results_page",
                  fluidRow(
                    box(
                      title = textOutput(ns("n_match")),
                      downloadButton(ns('download_data'), 'Download results') %>% withSpinner(color="#3C8DBC"),
                      status = "primary", solidHeader = TRUE
                    )
                  ),
                  fluidRow(
                    box(
                      title = "Overview",
                      column(width=4, plotOutput(ns("plot_1"), height=250) %>% withSpinner(color="#3C8DBC")),
                      column(width=4, plotOutput(ns("plot_2"), height=250) %>% withSpinner(color="#3C8DBC")),
                      column(width=4, plotOutput(ns("plot_3"), height=250) %>% withSpinner(color="#3C8DBC")),
                      width=12, status = "primary", solidHeader = TRUE, collapsible = TRUE, style = 'overflow-x: scroll;'
                    )
                  ),
                  fluidRow(
                    box(
                      title = "Detailed results",  dataTableOutput(ns("table")) %>% withSpinner(color="#3C8DBC"),  width=12, status = "primary", solidHeader = TRUE, collapsible = TRUE
                    )
                  )
                )
      )
    )
  )
}

###################
#Load internal data
###################
data_fp = "data/taxa_simple.csv"
taxa_simple = read.csv(data_fp)

data_fp = "data/taxa_Hungate.csv"
taxa_Hungate = read.csv(data_fp)

data_fp = "data/taxa_RUG.csv"
taxa_RUG = read.csv(data_fp)

data_fp = "data/taxa_infant.csv"
taxa_infant = read.csv(data_fp)

##############
#Define server
##############
predictionsTaxonomyServer <- function(input, output, session, x, selected_section) {
  #Set namespace
  ns <- session$ns
  
  #***********************
  #Get user input (events)
  #***********************
  #Read in taxonomy of query organisms
  get_query_taxonomy <- reactive({
    req(input$file_query_taxonomy)
    query_taxonomy = read.csv(input$file_query_taxonomy$datapath)
    return(query_taxonomy)
  })
  
  #Get raw data and filter
  get_data = reactive({
    #Get data
    data = raw_data
    
    #Filter according to preferred taxonomy
    data = filter_by_taxonomy(data=data, taxonomy_type=input$predictionsTaxonomy_filter)
    
    return(data)
  })
  
  #Get prediction threshold
  get_prediction_threshold = reactive({
    threshold = input$threshold
    
    return(threshold)
  })
  
  #*************
  #Process input
  #*************
  format_query <- eventReactive({input$make_predictions}, {
    #Get data
    data = get_query_taxonomy()
    
    #Detect file type, then format
    ##Column-separated values (DADA2 format)
    if(sum(colnames(data) %in% c("Phylum", "Class", "Order", "Family", "Genus", "Species")) == 6)
    {
      x = data.frame(matrix(NA, nrow = nrow(data), ncol = 6))
      colnames(x) = c("Phylum", "Class", "Order", "Family", "Genus", "Species")
      
      x = data[, colnames(data) %in% colnames(x)]
    }else if(any(sapply(data, function(x) grepl(";", data))))
    {
      ##Semicolon-separated values (QIIME2/Greengenes format)
      x = data.frame(matrix(NA, nrow = nrow(data), ncol = 6))
      colnames(x) = c("Phylum", "Class", "Order", "Family", "Genus", "Species")
      prefix = c("p__", "c__", "o__", "f__", "g__", "s__")
      
      for(i in 1:nrow(data))
      {
        for(j in 1:ncol(x))
        {
          pattern = paste0(prefix[j],".*?(;|$)")
          x[i,j] = str_extract(string=data[i,], pattern=pattern)
          x[i,j] = gsub(pattern=".__", replacement="", x=x[i,j])
          x[i,j] = gsub(pattern=";", replacement="", x=x[i,j])
        }
      }
    }else
    {
      x=NULL
    }
    
    #Do additional formatting
    if(!is.null(x))
    {
      #Replace missing values with NA
      x[x == ""] = NA
      x[x == "?"] = NA
      x[x == "unclassified"] = NA
      
      #Remove brackets
      x = data.frame(lapply(x, function(x) gsub("\\[|\\]", "", x)))
      
      #Simplify names
      if(input$simplify_names==TRUE)
      {
        x = as.data.frame(t(apply(x, 1, simplify_names, colnames(x))))
      }
    }
    
    validateShowModal(need(!is.null(x), "Please check the format of your uploaded file and try again."))
    
    return(x)
  })
  
  #Get predictions
  predict_traits <-  eventReactive({input$make_predictions}, {
    #Get inputs
    table = get_data()
    query = format_query()
    threshold=get_prediction_threshold()
    
    #Format table
    table = table %>% dplyr::select(Phylum, Class, Order, Family, Genus, Species, Fermentative_ability, Substrates_for_end_products, Major_end_products, Minor_end_products)
    
    #Expand endproducts into multiple columns
    table$Major_end_products[table$Major_end_products == "NA"] <- NA
    table$Minor_end_products[table$Minor_end_products == "NA"] <- NA
    table$End_products <- ifelse(is.na(table$Major_end_products), table$Minor_end_products, ifelse(is.na(table$Minor_end_products), table$Major_end_products, paste(table$Major_end_products, table$Minor_end_products, sep=";")))
    table = table %>% dplyr::select(-Major_end_products, -Minor_end_products)
    table = expand_column(df=table, col_name="End_products")
    
    #Format query 
    #Replace elements with regular expression (for search table)
    query_regex = query
    for(i in 1:nrow(query))
    {
      for(j in 1:ncol(query))
      {
        query_regex[i,j] = if_else(!is.na(query[i,j]), paste0("^", query[i,j], "$"), "^.*$")
      }
    }
    
    #Launch progress bar
    updateProgressBar(session = session, id = ns("pb"), value = 0)
    showModal(modalDialog(
      h4("Prediction in progress"),
      progressBar(id = ns("pb"), value = 0, display_pct = TRUE),
      easyClose = TRUE, footer = NULL
    )
    )
    
    #Print status to log
    cat(file=stderr(), paste0("Started prediction at ", Sys.time(), "\n"))
    
    #Search table for matches to query
    #Initialize values
    traits = data.frame(matrix(NA, nrow=nrow(query), ncol=ncol(table)-6)) 
    colnames(traits) = colnames(table)[7:(ncol(table))]
    for(i in 1:nrow(query))
    {
      #Get matching taxa and traits
      match = table %>% dplyr::filter(grepl(pattern=query_regex[i,1], x=Phylum)) %>% 
        dplyr::filter(grepl(pattern=query_regex[i,2], x=Class)) %>% 
        dplyr::filter(grepl(pattern=query_regex[i,3], x=Order)) %>% 
        dplyr::filter(grepl(pattern=query_regex[i,4], x=Family)) %>% 
        dplyr::filter(grepl(pattern=query_regex[i,5], x=Genus)) %>%
        dplyr::filter(grepl(pattern=query_regex[i,6], x=Species))
      
      match = as.data.frame(match)
      match = match %>% dplyr::select(-Phylum, -Class, -Order, -Family, -Genus, -Species)
      
      #Get traits shared by a threshold of taxa
      #Exclude NA values
      if(nrow(match)>0)
      {
        for(j in 1:ncol(traits))
        {
          x = table(match[,j], exclude=NA)
          
          if (any(!is.na(x)) && (max(x)/sum(x)) >= threshold) {
            traits[i,j] = attributes(x[which(x==max(x))[1]])$names[1]
          } else {
            traits[i,j] = NA
          }
          
        }
      }

      # Increment the progress bar
      updateProgressBar(session = session, id = ns("pb"), value = 1/nrow(query)*100*i)
    }
    
    #Put end products back into a single column
    traits = collapse_columns(df=traits, cols=colnames(traits)[3:ncol(traits)], new_col_name="End_products")
    
    #Make other traits NA if fermentative ability is NA or -
    if (!all(is.na(traits))) {
      traits <- traits %>% mutate(across(Substrates_for_end_products:End_products, ~ if_else(Fermentative_ability == "-" | is.na(as.character(Fermentative_ability)), NA_character_, .)))
    }
    
    #Add taxonomy of query organism
    traits = cbind(query, traits)
    
    #Hide progress bar
    removeModal()
    
    #Print status to log
    cat(file=stderr(), paste0("Ended prediction at ", Sys.time(), "\n"))
    
    return(traits)
  }
  , ignoreNULL = FALSE,  ignoreInit=FALSE)
  
  #****************
  #Generate outputs
  #****************
  #Output file upload status
  output$check_file_taxonomy = reactive({
    is.null(input$file_query_taxonomy$datapath)
  })
  outputOptions(output, "check_file_taxonomy", suspendWhenHidden = FALSE)
  
  #Output downloadable csv with example taxa
  output$downloadTaxa_1 <- downloadHandler(
    filename = function() {
      paste("taxa_simple", "csv", sep = ".")
    },
    content = function(file) {
      table=taxa_simple
      write.csv(table, file, row.names = FALSE)
    }
  )   
  
  output$downloadTaxa_2 <- downloadHandler(
    filename = function() {
      paste("taxa_rumen_cultured", "csv", sep = ".")
    },
    content = function(file) {
      table=taxa_Hungate
      write.csv(table, file, row.names = FALSE)
    }
  )  
  
  output$downloadTaxa_3 <- downloadHandler(
    filename = function() {
      paste("taxa_rumen_MAGs", "csv", sep = ".")
    },
    content = function(file) {
      table=taxa_RUG
      write.csv(table, file, row.names = FALSE)
    }
  )  
  
  output$downloadTaxa_4 <- downloadHandler(
    filename = function() {
      paste("taxa_infant", "csv", sep = ".")
    },
    content = function(file) {
      table=taxa_infant
      write.csv(table, file, row.names = FALSE)
    }
  )  
  
  #Output modal with example files
  # Create modal
  observeEvent(input$taxonomy_file_modal, ignoreInit = TRUE, {
    showModal(modalDialog(
      h3("Example files"),
      tags$ol(class = "circled-list",
              tags$li(downloadLink(outputId = ns("downloadTaxa_1"), label = "Toy dataset")),
              tags$li(downloadLink(outputId = ns("downloadTaxa_2"), label = "Cultured prokaryotes from rumen")),
              tags$li(downloadLink(outputId = ns("downloadTaxa_3"), label = "MAGs from rumen")),
              tags$li(downloadLink(outputId = ns("downloadTaxa_4"), label = "OTUs from infant gut"))
      ),
      div("Click ",
          actionLink(ns("go_to_help"), "here"),
          " to see detailed guidelines."),
      easyClose = TRUE, footer = NULL
    ))
  })
  
  observeEvent(input$go_to_help, {
    updateNavbarPage(session=x, inputId="tabs", selected = "help")
    updateNavlistPanel(session=x, inputId="navlist_panel", selected = "Predict traits from taxonomy")
    removeModal()
  })
  
  #Output number of matching organisms and traits
  output$n_match=renderText(paste0("Traits predicted for ", 
                                   nrow(predict_traits() %>% dplyr::filter(!is.na(Fermentative_ability)|!is.na(Substrates_for_end_products)|!is.na(End_products))),
                                   " out of ", nrow(get_query_taxonomy()), " query taxa"))
  
  #Output downloadable csv with matching results
  output$download_data <- downloadHandler(
    filename = function() {
      paste("results", "csv", sep = ".")
    },
    content = function(file) {
      sep <- switch("csv", "csv" = ",", "tsv" = "\t")
      
      # Write to a file specified by the 'file' argument
      write.table(predict_traits(), file, sep = sep, row.names = FALSE)
    }
  ) 
  
  #Output table with predicted traits
  output$table <- renderDataTable({
    predict_traits() %>% replace(is.na(.), "NA")
  }, 
  escape = FALSE, options = list(scrollX = TRUE)) 
  
  #Output plots
  output$plot_1 = renderPlot(exp={
    plot_treemap(data=predict_traits(), var="Fermentative_ability")
  }, width=225, height=250)
  
  output$plot_2 = renderPlot(exp={
    plot_treemap(data=predict_traits(), var="Substrates_for_end_products")
  }, width=225, height=250)
  
  output$plot_3 = renderPlot(exp={
    plot_treemap(data=predict_traits(), var="End_products", sep=";")
  }, width=225, height=250)
}