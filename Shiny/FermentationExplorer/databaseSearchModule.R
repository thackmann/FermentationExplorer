#################
#Define functions
#################
#Plot phylogenetic tree with matching organisms
plot_tree = function(tree, grp)
{
  p = ggtree(tree, layout="rectangular", size=0.1, alpha=0.5)
  
  return(p) 
}

#Format phylogenetic tree
format_tree = function(p, grp, data, filter_data)
{
  #Add trait
  filter_data = filter_data %>% filter(!is.na(`IMG Genome ID max genes`))
  p$data$trait = match(x=p$data$label, table=filter_data$`IMG Genome ID max genes`)
  p$data$trait = if_else(!is.na(p$data$trait),"0","1")
  
  #Add organism name
  row_match = match(x=p$data$label, table=data$IMG_Genome_ID_max_genes)
  p$data$Phylum =  data$Phylum[row_match]
  p$data$Class =  data$Class[row_match]
  p$data$Order =  data$Order[row_match]
  p$data$Family =  data$Family[row_match]
  p$data$Genus =  data$Genus[row_match]
  p$data$Species =  data$Species[row_match]
  
  #Get data for points
  p_data = subset(p$data, isTip == TRUE)
  
  #Split into layers (to place points for positive traits on top)
  p_data_layer_1 <- p_data[p_data$trait=="0",]
  p_data_layer_2 <- p_data[p_data$trait=="1",]
  
  #Set colors
  color_palette = c(green_color, "grey90")
  color_palette_whitened = c(whiten_colors(color_palette[1], alpha_no_transparency=0.2),"white")
  
  #Make plot
  p = p +
    #Plot points
    geom_point(data=p_data_layer_2, aes(x=x,y=y, 
                Phylum=Phylum, Class=Class, Order=Order, Family=Family, Genus=Genus, Species=Species), 
               color="grey90", fill="white", shape=21, stroke=0.25, size=1, alpha=0)+ 
    geom_point(data=p_data_layer_1, aes(x=x,y=y,
                Phylum=Phylum, Class=Class, Order=Order, Family=Family, Genus=Genus, Species=Species),
               color=green_color, fill=whiten_colors(green_color, alpha_no_transparency=0.2), shape=21, stroke=0.25, size=1)+

    #Set limits (to set plot margins)
    ylim(min(p_data$y)-15,max(p_data$y)+15)+
    
    #Add title
    ggtitle("Phylogenetic tree") +
    
    #Set theme
    #Remove legend
    theme(
      plot.title = element_text(angle = 0, vjust = 0.5, hjust=0.5, colour="black", size=10),
      legend.position = "none"
    )

  p = ggplotly(p, tooltip=c("Phylum", "Class", "Order", "Family", "Genus", "Species"))
  
  return(p)
}

#Make t-SNE plot
plot_tsne = function(tsne, data, filter_data, show_legend=FALSE)
{
  #Add trait
  filter_data = filter_data %>% filter(!is.na(`IMG Genome ID max genes`))
  tsne$trait = match(x=tsne$IMG_Genome_ID_max_genes, table=filter_data$`IMG Genome ID max genes`)
  tsne$trait = if_else(!is.na(tsne$trait),"0","1")
  
  #Add organism name
  row_match = match(x=tsne$IMG_Genome_ID_max_genes, table=data$IMG_Genome_ID_max_genes)
    tsne$Phylum = data$Phylum[row_match]
    tsne$Class = data$Class[row_match]
    tsne$Order = data$Order[row_match]
    tsne$Family = data$Family[row_match]
    tsne$Genus = data$Genus[row_match]
    tsne$Species = data$Species[row_match]
  
  #Split into layers (to place points for positive traits on top)
  tsne_layer_1 <- tsne[tsne$trait=="0",]
  tsne_layer_2 <- tsne[tsne$trait=="1",]
  
  #Make plot
  p = ggplot()+
    #Plot points
    geom_point(data=tsne_layer_2, aes(x=x,y=y, 
                Phylum=Phylum, Class=Class, Order=Order, Family=Family, Genus=Genus, Species=Species), 
               color="grey90", fill="white", shape=21, stroke=0.25, size=1)+ 
    geom_point(data=tsne_layer_1, aes(x=x,y=y, 
                Phylum=Phylum, Class=Class, Order=Order, Family=Family, Genus=Genus, Species=Species), 
               color=green_color, fill=whiten_colors(green_color, alpha_no_transparency=0.2), shape=21, stroke=0.25, size=1)+ 
    
    #Add title
    ggtitle("t-SNE plot of gene functions") +
    
    #Set theme
    theme(
      plot.title = element_text(angle = 0, vjust = 0.5, hjust=0.5, colour="black", size=10),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title.x = element_text(angle = 0, vjust = 0.5, hjust=0.5, colour="black", size=6),
      axis.title.y = element_text(angle = 0, vjust = 0.5, hjust=0.5, colour="black", size=6),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank()
    )
  
  p = ggplotly(p, tooltip=c("Phylum", "Class", "Order", "Family", "Genus", "Species"))

  return(p)
}

#Whiten colors
whiten_colors = function(color_palette, alpha_no_transparency){
  rgb2hex = function(r,g,b) rgb(r, g, b, maxColorValue = 255)
  color_palette_whitened = vector()
  for(i in 1:length(color_palette))
  {
    color_whitened = col2rgb(color_palette[i])
    color_whitened = 255-((255-color_whitened)*alpha_no_transparency)
    color_whitened = round(color_whitened,0)
    color_whitened = t(color_whitened)
    color_whitened = rgb2hex(color_whitened[1,1],color_whitened[1,2],color_whitened[1,3])
    color_palette_whitened[i] = color_whitened
  }
  
  return(color_palette_whitened)
}	

# Make html link to external website with comma-separated IDs
createLink <- function(IDs, url) {
  IDs <- as.character(IDs)
  IDs[IDs == "NA"] <- NA
  
  links <- lapply(IDs, function(ID) {
    if (!is.na(ID)) {
      ID_split <- strsplit(x = ID, split = ", ")[[1]]
      individual_links <- sprintf('<a href="%s" target="_blank">%s</a>', URLdecode(paste0(url, ID_split)), ID_split)
      paste(individual_links, collapse = ",")
    } else {
      "NA"
    }
  })
  
  return(unlist(links))
}

# Make html links with button
createLinkButton <- function(urls) {
  urls <- as.character(urls)
  urls[urls == "NA"] <- NA
  links <- sprintf('<a href="%s" target="_blank" class="btn btn-primary">Link</a>', URLdecode(urls))
  links[is.na(links)] <- "NA"
  return(links)
}  

#Simplify input taxonomy (replace high-level ranks with NA)
simplify_names <- function(row, col_names) {
  species_column_idx <- which(col_names == "Species")
  
  if (all(is.na(row[-species_column_idx]))) {
    row
  }else{
    last_non_na_idx <- max(which(!is.na(row[-species_column_idx])))
    row[-c(last_non_na_idx, species_column_idx)] <- NA
  }
  
  return(row)
}

###################
#Load internal data
###################
data_fp = "data/tree.tre"
tree = read.tree(data_fp)

data_fp = "data/tsne.csv"
tsne = read.csv(data_fp)

##############
#Set variables
##############
#Choices for columns of data to display
choices_info_organism = vector()
names_info_organism = colnames(raw_data %>% dplyr::select(Phylum:Article_link))
choices_info_organism = 1:length(names_info_organism)
names(choices_info_organism) = names_info_organism     
names(choices_info_organism) = gsub(pattern="_", replacement=" ", x=names(choices_info_organism))

choices_info_fermentation = vector()
names_info_fermentation = colnames(raw_data %>% dplyr::select(Fermentative_ability:Substrates_for_end_products))
choices_info_fermentation = 1:length(names_info_fermentation)
names(choices_info_fermentation) = names_info_fermentation    
names(choices_info_fermentation) = gsub(pattern="_", replacement=" ", x=names(choices_info_fermentation))

choices_info_JGI = vector()
names_info_JGI = colnames(raw_data %>% dplyr::select(GOLD_Organism_ID:IMG_Genome_ID_max_genes))
choices_info_JGI = 1:length(names_info_JGI)
names(choices_info_JGI) = names_info_JGI    
names(choices_info_JGI) = gsub(pattern="_", replacement=" ", x=names(choices_info_JGI))

choices_info_NCBI = vector()
names_info_NCBI = colnames(raw_data %>% dplyr::select(NCBI_Taxonomy_ID:NCBI_Species))
choices_info_NCBI = 1:length(names_info_NCBI)
names(choices_info_NCBI) = names_info_NCBI    
names(choices_info_NCBI) = gsub(pattern="_", replacement=" ", x=names(choices_info_NCBI))

choices_info_BacDive = vector()
names_info_BacDive = colnames(raw_data %>% dplyr::select(BacDive_Organism_ID:Salt_concentration_unit))
choices_info_BacDive = 1:length(names_info_BacDive)
names(choices_info_BacDive) = names_info_BacDive    
names(choices_info_BacDive) = gsub(pattern="_", replacement=" ", x=names(choices_info_BacDive))  
names(choices_info_BacDive) = gsub(pattern ="Cell length", replacement="Cell length in microns", x=names(choices_info_BacDive))  
names(choices_info_BacDive) = gsub(pattern ="Cell width", replacement="Cell width in microns", x=names(choices_info_BacDive))  
names(choices_info_BacDive) = gsub(pattern ="Incubation period", replacement="Incubation period in days", x=names(choices_info_BacDive))  

choices_info_FAPROTAX = vector()
names_info_FAPROTAX = colnames(raw_data %>% dplyr::select(FAPROTAX_predicted_metabolism))
choices_info_FAPROTAX = 1:length(names_info_FAPROTAX)
names(choices_info_FAPROTAX) = names_info_FAPROTAX
names(choices_info_FAPROTAX) = gsub(pattern="_", replacement=" ", x=names(choices_info_FAPROTAX))  

###########################
#Define user interface (UI)
###########################
#Search database tab
databaseSearchUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width=12, h3("Search database"))
    ),
    sidebarPanel(
      width=4,
      div(actionButton(ns("perform_search"), "Perform search", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
      queryBuilderOutput(ns('querybuilder')) %>% withSpinner(color="#3C8DBC")
    ),
    mainPanel(
      width=8,
      div(
        id = ns("results_page"),
        conditionalPanel(
          condition = "input.perform_search== 0",
          ns = ns,
          h4("Please make selections at left")
        ),
        conditionalPanel(
          condition = "input.perform_search > 0",
          ns = ns,
          fluidRow(
            box(
              title = textOutput(ns("n_match")), downloadButton(ns('download_data'), 'Download results'), status = "primary", solidHeader = TRUE
            )
          ),
          fluidRow(
            box(
              title = "Plots",
              column(
                column(
                  width=5, 
                  style = "min-width: 300px;",
                  style = "max-width: 400px;",
                  plotlyOutput(ns("plot_1"), height=350) %>% withSpinner(color="#3C8DBC")
                ), 
                column(
                  width=5, 
                  style = "min-width: 300px;",
                  style = "max-width: 400px;",
                  plotlyOutput(ns("plot_2"), height=350) %>% withSpinner(color="#3C8DBC")
                ),
                column(
                  width=2
                  ),
                width = 12
              ),
              div(
                "Matching organisms are in green.  Only organisms with genome sequences are shown."
              ),
              width=12, status = "primary", solidHeader = TRUE, collapsible = TRUE, style = 'overflow-x: scroll;'
            )
          ),
          fluidRow(
            box(
              title = "Detailed results",  
              fluidRow(
                column(width=12, offset = 0, dataTableOutput(ns("table")) %>% withSpinner(color="#3C8DBC"))
              ),
              fluidRow(
                column(width=3, 
                       div(
                         checkboxGroupInput(ns("checkboxes_info_organism"), "Organism", choices=choices_info_organism, selected=c(which(names(choices_info_organism)=="Genus"), which(names(choices_info_organism)=="Species"), which(names(choices_info_organism)=="Article link"))),
                         checkboxGroupInput(ns("checkboxes_info_fermentation"), "Fermentation", choices=choices_info_fermentation)
                       )
                ),
                column(width=3, 
                       div(
                         checkboxGroupInput(ns("checkboxes_info_JGI"), "JGI", choices=choices_info_JGI, selected=c(which(names(choices_info_JGI)=="GOLD Organism ID"), which(names(choices_info_JGI)=="IMG Genome ID"))), 
                         checkboxGroupInput(ns("checkboxes_info_NCBI"), "NCBI", choices=choices_info_NCBI, selected=c(which(names(choices_info_NCBI)=="NCBI Taxonomy ID")))
                       )
                ),
                column(width=3, 
                       checkboxGroupInput(ns("checkboxes_info_BacDive"), "BacDive", choices=choices_info_BacDive, selected=c(which(names(choices_info_BacDive)=="BacDive Organism ID")))
                ),
                column(width=3, 
                       checkboxGroupInput(ns("checkboxes_info_FAPROTAX"), "FAPROTAX", choices=choices_info_FAPROTAX)
                )
              ),
              width=12, status = "primary", solidHeader = TRUE, collapsible = TRUE
            )
          )
        )
      )
    )
  )
}

##############
#Define server
##############
databaseSearchServer <- function(input, output, session) {
  #Set namespace
  ns <- session$ns
  
  #***********************
  #Get user input (events)
  #***********************
  #Get raw data
  data = raw_data
  
  #*************
  #Process input
  #*************
  #Clean data
  clean_data <- reactive({
    #Launch modal with progress bar
    updateProgressBar(session = session, id = ns("pb"), value = 0)
    showModal(modalDialog(
      h4("Loading page"),
      progressBar(id = ns("pb"), value = 0, display_pct = TRUE),
      easyClose = TRUE, footer = NULL
    )
    )
    
    data = raw_data
    
    data[] <- lapply(raw_data, as.character)
    
    data_vars <- c("Cell_length", "Cell_shape", "Cell_width", "Colony_size",
                   "Flagellum_arrangement", "Growth_temperature", "Gram_stain",
                   "Incubation_period", "Indole_test", "Isolation_source_category_1",
                   "Isolation_source_category_2", "Isolation_source_category_3",
                   "Oxygen_tolerance", "pH_for_growth", "Spore_formation",
                   "Salt_concentration_amount", "Salt_concentration_unit", "FAPROTAX_predicted_metabolism")
    
    is_numeric_vars <- c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE,
                         FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE)
    
    data[data_vars] <- mapply(clean_external_data, x=data[data_vars], is_numeric=is_numeric_vars, SIMPLIFY=FALSE)
    
    # Rename variables for clarity
    data <- data %>%
      dplyr::rename(Cell_length_in_microns = "Cell_length",
                    Cell_width_in_microns = "Cell_width",
                    Incubation_period_in_days = "Incubation_period")
    
    # Remove underscores in variable and phylum names
    colnames(data) <- gsub(pattern="_", replacement=" ", x=colnames(data))
    data$Phylum <- gsub(pattern="[_\\.]", replacement=" ", x=data$Phylum)
    
    # Remove row index
    data <- data %>% dplyr::select(-X)
    
    # Convert character columns to factor
    data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)
    
    #Hide modal with progress bar
    updateProgressBar(session = session, id = ns("pb"), value = 100)
    removeModal()
    
    return(data)
  })
  
  #Filter data according to query
  filter_data = eventReactive({input$perform_search}, {
    #Launch modal with progress bar
    updateProgressBar(session = session, id = ns("pb"), value = 0)
    
    showModal(modalDialog(
      h4("Search in progress"),
      progressBar(id = ns("pb"), value = 0, display_pct = TRUE),
      easyClose = TRUE, footer = NULL
    ))
    data = clean_data()
    data = filterTable(input$querybuilder_out, data, 'table')
    
    return(data)
  })
  
  # Get links to external websites
  get_links <- reactive({
    #Update modal with progress bar
    updateProgressBar(session = session, id = ns("pb"), value = 50)
    
    # Get table with matching organisms
    data = clean_data()
    
    # Add links
    data$`NCBI Taxonomy ID` <- createLink(data$`NCBI Taxonomy ID`, "https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?mode=Info&id=")
    data$`GOLD Organism ID` <- createLink(data$`GOLD Organism ID`, "https://gold.jgi.doe.gov/organism?id=")
    data$`GOLD Project ID` <- createLink(data$`GOLD Project ID`, "https://gold.jgi.doe.gov/project?id=")
    data$`IMG Genome ID` <- createLink(data$`IMG Genome ID`, "https://img.jgi.doe.gov/cgi-bin/m/main.cgi?section=TaxonDetail&page=taxonDetail&taxon_oid=")
    data$`IMG Genome ID max genes` <- createLink(data$`IMG Genome ID max genes`, "https://img.jgi.doe.gov/cgi-bin/m/main.cgi?section=TaxonDetail&page=taxonDetail&taxon_oid=")
    data$`BacDive Organism ID` <- createLink(data$`BacDive Organism ID`, "https://bacdive.dsmz.de/strain/")
    data$`Article link` <- createLinkButton(data$`Article link`)
    
    return(data)
  })  
  
  #Add links 
  add_links <- reactive({
    #Update modal with progress bar
    updateProgressBar(session = session, id = ns("pb"), value = 90)
    
    #Get table with matching organisms
    data = filter_data()
    
    #Get links
    links = get_links()
    
    #Add links to table
    x = data %>% select(-`NCBI Taxonomy ID`,-`GOLD Organism ID`,-`GOLD Project ID`,-`IMG Genome ID`,-`IMG Genome ID max genes`,-`BacDive Organism ID`,-`Article link`)
    y = links %>% select(Genus, Species, Subspecies, `Strain ID`, `NCBI Taxonomy ID`,`GOLD Organism ID`,`GOLD Project ID`,`IMG Genome ID`,`IMG Genome ID max genes`,`BacDive Organism ID`,`Article link`)
    data = left_join(x=x, y=y, by=c("Genus", "Species", "Subspecies", "Strain ID"))
    
    return(data)
  })
  
  #Get selected columns
  get_columns <- reactive({
    #Update modal with progress bar
    updateProgressBar(session = session, id = ns("pb"), value = 50)
    
    #Get table with matching organisms and links
    data = add_links()
    
    #Get names of columns selected via checkbox
    columns_info_organism = names(choices_info_organism[as.numeric(input$checkboxes_info_organism)])
    columns_info_fermentation = names(choices_info_fermentation[as.numeric(input$checkboxes_info_fermentation)])
    columns_info_JGI = names(choices_info_JGI[as.numeric(input$checkboxes_info_JGI)])
    columns_info_NCBI = names(choices_info_NCBI[as.numeric(input$checkboxes_info_NCBI)])
    columns_info_BacDive = names(choices_info_BacDive[as.numeric(input$checkboxes_info_BacDive)])
    columns_info_FAPROTAX = names(choices_info_FAPROTAX[as.numeric(input$checkboxes_info_FAPROTAX)])
    
    #Keep only selected columns
    columns=c(columns_info_organism, columns_info_fermentation, columns_info_JGI,columns_info_NCBI, columns_info_BacDive,columns_info_FAPROTAX)
    data = data %>% dplyr::select(all_of(columns))
    
    #Hide modal with progress bar
    updateProgressBar(session = session, id = ns("pb"), value = 100)
    removeModal()

    return(data)
  })
  
  #Get initial phylogenetic tree
  get_initial_tree <- reactive({
    
    #Update modal with progress bar
    updateProgressBar(session = session, id = ns("pb"), value = 25)
    
    p = plot_tree(tree=tree, grp=grp)
    
    return(p)
  })
  
  #****************
  # Generate outputs
  #****************
  # Output widget for query builder
  query_filters <- list(
    list(name = 'Phylum',  type = 'string', input = 'selectize'),
    list(name = 'Class',  type = 'string', input = 'selectize'),
    list(name = 'Order',  type = 'string', input = 'selectize'),
    list(name = 'Family',  type = 'string'),
    list(name = 'Genus',  type = 'string'),
    list(name = 'Species',  type = 'string'),
    list(name = 'Subspecies',  type = 'string'),
    list(name = 'Strain ID',  type = 'string'),
    list(name = 'Fermentative ability',  type = 'string', input = 'selectize'),
    list(name = 'Major end products',  type = 'string'),
    list(name = 'Minor end products',  type = 'string'),
    list(name = 'Substrates for end products',  type = 'string', input = 'selectize'),
    list(name = 'GOLD Organism ID',  type = 'string'),
    list(name = 'GOLD Project ID',  type = 'string'),
    list(name = 'IMG Genome ID',  type = 'string'),
    list(name = 'IMG Genome ID max genes',  type = 'string'),
    list(name = 'NCBI Taxonomy ID',  type = 'string'),
    list(name = 'NCBI Phylum', type = 'string', input = 'selectize'),
    list(name = 'NCBI Class',  type = 'string', input = 'selectize'),
    list(name = 'NCBI Order',  type = 'string', input = 'selectize'),
    list(name = 'NCBI Family',  type = 'string'),
    list(name = 'NCBI Genus',  type = 'string'),
    list(name = 'NCBI Species',  type = 'string'),
    list(name = 'BacDive Organism ID',  type = 'string'),
    list(name = 'Cell length in microns',  type = 'double'),
    list(name = 'Cell shape',  type = 'string', input = 'selectize'),
    list(name = 'Cell width in microns',  type = 'double'),
    list(name = 'Colony size',  type = 'double'),
    list(name = 'Flagellum arrangement',  type = 'string', input = 'selectize'),
    list(name = 'Growth temperature',  type = 'string', input = 'selectize'),
    list(name = 'Gram stain',  type = 'string', input = 'selectize'),
    list(name = 'Incubation period in days',  type = 'double'),
    list(name = 'Indole test',  type = 'string', input = 'selectize'),
    list(name = 'Isolation source category 1',  type = 'string', input = 'selectize'),
    list(name = 'Isolation source category 2',  type = 'string', input = 'selectize'),
    list(name = 'Isolation source category 3',  type = 'string', input = 'selectize'),
    list(name = 'Oxygen tolerance',  type = 'string', input = 'selectize'),
    list(name = 'pH for growth',  type = 'double'),
    list(name = 'Spore formation',  type = 'string', input = 'selectize'),
    list(name = 'Salt concentration amount',  type = 'double'),
    list(name = 'Salt concentration unit',  type = 'string', input = 'selectize'),
    list(name = 'FAPROTAX predicted metabolism',  type = 'string')
  )
  
  output$querybuilder <- renderQueryBuilder({
    queryBuilder(
      data = clean_data(),
      filters = query_filters,
      autoassign = FALSE,
      default_condition = 'AND',
      allow_empty = FALSE,
      display_errors = FALSE,
      display_empty_filter = FALSE
    )
  })
  
  # Output number of matching organisms
  output$n_match <- renderText(sprintf("Query matched %d organisms", nrow(filter_data())))
  
  #Output plots
  #Tree
  output$plot_1 = renderPlotly(exp={
    format_tree(p=get_initial_tree(), grp=grp, data=raw_data, filter_data=filter_data())
  })
  
  #t-SNE
  output$plot_2 = renderPlotly(exp={
    plot_tsne(data=raw_data, filter_data=filter_data(), tsne=tsne)
  })
  
  # Output table with matching organisms and columns
  output$table <- renderDataTable({
    get_columns()
  }, escape = FALSE, options = list(scrollX = TRUE))
  
  # Output downloadable csv with matching results
  output$download_data <- downloadHandler(
    filename = function() {
      paste("results", "csv", sep = ".")
    },
    content = function(file) {
      sep <- switch("csv", "csv" = ",", "tsv" = "\t")
      
      # Write to a file specified by the 'file' argument
      write.table(filter_data(), file, sep = sep, row.names = FALSE)
    }
  )
}