###########################
#Define user interface (UI)
###########################
helpUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width=12, h3("Help"))
    ),
    navlistPanel(
      widths = c(3,9),
      well = FALSE,
      # id = ns("navlist_panel"),
      id = "navlist_panel",
      tabPanel("Video tutorials",
               p(h3("Video tutorials")),
               p(h5("How to predict traits from taxonomy:")),
               p(uiOutput(ns("video_predictionsTaxonomy"))),
               p(h5("How to predict traits from genome:")),
               p(uiOutput(ns("video_predictionsGenome")))
      ),
      tabPanel("Search database",
               id = "Search database",
               p(h3("Search database")),
               p("This tool allows the user to search the internal database."),
               p(h4("Query builder")),
               p("The user can generate simple or complex queries with the query builder at the left.  Note that capitalization and spaces matter."),
               p(h4("Output format")),
               p("The *csv for matching organisms can be downloaded."),
               p("Matching organisms are also shown in a phylogenetic tree and t-SNE plot of gene functions.  The phylogenetic tree is of n = 14 ribosomal genes from n = 3,822 prokaryotes.  The t-SNE plot is of n = 30,805 gene functions from n = 4,301 prokaryotes.  Organisms that cluster together in this plot have similar functions.")
      ),   
      tabPanel("Download database",
               id = "Download database",
               p(h3("Download database")),
               p("This tool allows the user to download the internal database. The *csv includes all organisms and all information available.")
      ),
      tabPanel("Predict traits from taxonomy",
               id = "Predict traits from taxonomy",
               p(h3("Predict traits from taxonomy")),
               p("This tool predicts traits for organisms given its taxonomy. After the user uploads the taxonomy, the tool finds matching organisms in the internal database. If the matching organisms share a trait, the trait is predicted for the user's organisms. This approach is similar to that used by FAPROTAX."),
               p(h4("Data format")),
               p("The input taxonomy should be uploaded as a *csv file and follow one of two formats."),
               tags$i("Column-separated format"),
               p(tagList("This format follows the output of ", url_DADA2, ". Taxonomic ranks are in columns and organisms in rows. Column names should include \"Phylum\", \"Class\", \"Order\", \"Family\", \"Genus\", \"Species\". Additional columns (e.g., kingdom) can be provided but will be ignored. If a taxonomic rank (e.g., species) is not known for an organism, it can be reported as \"NA\".")),
               p("Example files:"),
               tags$ol(class = "circled-list",
                       tags$li(downloadLink(outputId = ns("downloadTaxa_1"), label = "Previously uncharacterized bacteria")),
                       tags$li(downloadLink(outputId = ns("downloadTaxa_2"), label = "Cultured prokaryotes from rumen")),
                       tags$li(downloadLink(outputId = ns("downloadTaxa_3"), label = "MAGs from rumen")),
               ),
               tags$i("Semicolon-separated format"),
               p(tagList("This format follows the output of ", url_QIIME2, ". Taxonomic ranks should be separated by semi-colons. The format should be \"p__name; c__name; o__name; f__name; g__name; s__name\". Additional values (e.g., \"k__name\") can be provided but will be ignored. If the species is not known for an organism, it can be reported as \"s__unclassified\" or \"s__?\"; the same principle holds for other taxonomic ranks.")),
               p("Example file:"),
               tags$ol(class = "circled-list",
                       tags$li(downloadLink(outputId = ns("downloadTaxa_4"), label = "OTUs from infant gut"))
               ),
               p(h4("Prediction threshold")),
               p("When this slider is set to 1, all matching organisms must share the trait for it to be predicted. When it is set to 0.5, only half (0.5) of the matching organisms must have it."),
               p(h4("Simplify names of taxa")),
               p("When this box is checked, only genus and species names are kept, and others are replaced with \"NA\". Because \"NA\" are not matched, this simplifies matching and usually leads to more matches."),
               p("If genus and species are \"NA\", then family (or next highest taxonomic rank) is kept."),
               p(h4("Taxonomy")),
               p("This switch controls the taxonomy in the internal database  used for matching."),
               p(h4("Output format")),
               p("The downloadable *csv reports traits for the input taxonomy. A value of \"NA\" represents no trait being predicted."),
      ),
      tabPanel("Predict traits from genome",
               id = "Predict traits from genome",
               p(h3("Predict traits from genome")),
               p("This tool predicts traits for an organism from its genome (predicted gene functions). It builds a metabolic model of the organism, then it predicts if a chosen substrate can be fermented to end products. The user either chooses an organism from the database or uploads data files."),
               p(h4("Data format")),
               p("If the database tab is chosen, the user only needs to select an organism from the drop down menu."),
               p(tagList("If the file upload tab is chosen, the user needs to upload *csv files containing the gene functions and reference reactions.  For gene functions, *ko files (from ", url_KAAS, ") are also accepted.")),
               p(tagList("The file for gene functions follows the output of KEGG Automatic Annotation Server (", url_KAAS, ").  The rows are KO IDs for the gene functions.  To analyze more than one genome, include a column named \"Genome\" with rows containing genome IDs.")),
               p("Example files:"),
               tags$ol(class = "circled-list",
                       tags$li(downloadLink(outputId = ns("downloadFunctions_1"), label = "E. coli")),
                       tags$li(downloadLink(outputId = ns("downloadFunctions_2"), label = "Previously uncharacterized bacteria")),
                       tags$li(downloadLink(outputId = ns("downloadFunctions_3"), label = "Cultured prokaryotes of rumen")),
                       tags$li(downloadLink(outputId = ns("downloadFunctions_4"), label = "MAGs from rumen")),
               ),
               p(tagList("The *csv of reference reactions follows the format of the ", url_fbar, " package of R. All reactions needed to ferment a chosen substrate to end products should be included.")),
               p("Example files:"),
               tags$ol(class = "circled-list",
                       tags$li(downloadLink(outputId = ns("downloadReference_1"), label = "Glucose fermentation")),
                       tags$li(downloadLink(outputId = ns("downloadReference_2"), label = "Fructose fermentation")),
               ),
               p(h4("Substrate")),
               p("The user specifies the substrate for the metabolic model here. Any metabolite in the reference model can be chosen."),
               p(h4("End products")),
               p("The user specifies end products to check here. Any metabolite in the reference model can be chosen."),
               p(h4("Unbalanced intermediates")),
               p("Metabolites chosen here are allowed to be produced (or consumed) in infinite quantities.  NADH and ATP are examples of metabolites usually chosen to be unbalanced.  In the metabolic model, these can accumulate without needing to be regenerated to NAD+ or ADP.  This simplifies the model, as reactions for consuming NADH and ATP don't have to be included."),
               p(h4("Output format")),
               p("The *csv for fluxes and for the model can be downloaded. The higher the fluxes, the faster the reaction or more product that is formed. The flux of substrate is set to -1000. Thus, if an end product has a flux of +2000, 2 moles are formed per mol of substrate. If an end product has a flux <1, we consider it not to be formed.")
      ),
      tabPanel("FAQ",
               id = "FAQ",
               p(h3("FAQ")),
               p(h4("General")),
               p(HTML("<b>Q.  When running the tool online, can I use multiple browser windows?</b>")),
               p("A.  Yes, but you cannot run multiple processes (e.g., predictions) simultaneously.  Also, some users report an error (in DataTables) when opening a new tab."),
               p(h4("Predict traits from genome")),      
               p(HTML("<b>Q.  The graph of the metabolic model does not show hydrogen.  How do I show it?</b>")),
               p("A.  To show hydrogen, go to Unbalanced intermediates, and unclick its substrates (e.g., Reduced_ferredoxin). By default, all substrates for hydrogen are unbalanced intermediates, which are not shown in the graph."),
               p(HTML("<b>Q.  Your metabolic models are for fermentation.  Can I build other types of metabolic models?</b>")),
               p("A.  Yes, any set reference reactions of reference reactions can be uploaded."),
      )        
    )
  )
}

##############
#Define server
##############
helpServer <- function(input, output, session, selected_section) {
  #Set namespace
  ns <- session$ns
  
  #Output videos for tutorial
  output$video_predictionsTaxonomy <- renderUI({
    HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/Lhlk-4vRmL4" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
  })
  
  output$video_predictionsGenome <- renderUI({
    HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/MOubZwqIW4I" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
  })
  
  #Output downloadable csv with example taxa
  output$downloadTaxa_1 <- downloadHandler(
    filename = function() {
      paste("taxa_uncharacterized", "csv", sep = ".")
    },
    content = function(file) {
      table=taxa_uncharacterized
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
  
  #Output downloadable csv with example gene functions
  output$downloadFunctions_1 <- downloadHandler(
    filename = function() {
      paste("gene_functions_e_coli", "csv", sep = ".")
    },
    content = function(file) {
      table=gene_functions_e_coli
      write.csv(table, file, row.names = FALSE)
    }
  )
  
  #Output downloadable csv with example gene functions
  output$downloadFunctions_2 <- downloadHandler(
    filename = function() {
      paste("gene_functions_uncharacterized", "csv", sep = ".")
    },
    content = function(file) {
      table=gene_functions_uncharacterized
      write.csv(table, file, row.names = FALSE)
    }
  )
  
  output$downloadFunctions_3 <- downloadHandler(
    filename = function() {
      paste("gene_functions_rumen_cultured", "csv", sep = ".")
    },
    content = function(file) {
      table=gene_functions_Hungate
      write.csv(table, file, row.names = FALSE)
    }
  )
  
  output$downloadFunctions_4 <- downloadHandler(
    filename = function() {
      paste("gene_functions_rumen_MAGs", "csv", sep = ".")
    },
    content = function(file) {
      table=gene_functions_RUG
      write.csv(table, file, row.names = FALSE)
    }
  )
  
  #Output downloadable csv with example reference model
  output$downloadReference_1 <- downloadHandler(
    filename = function() {
      paste("reference_reactions_glucose_fermentation", "csv", sep = ".")
    },
    content = function(file) {
      table=reference_reactions
      write.csv(table, file, row.names = FALSE)
    }
  )
  
  output$downloadReference_2 <- downloadHandler(
    filename = function() {
      paste("reference_reactions_fructose_fermentation", "csv", sep = ".")
    },
    content = function(file) {
      table=reference_reactions_fructose_fermentation
      write.csv(table, file, row.names = FALSE)
    }
  )
}