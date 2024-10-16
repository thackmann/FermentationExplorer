# Define the Help Module in Shiny App
# This script defines the user interface (UI) and server for the help module.  
# Author: Timothy Hackmann
# Date: 14 October 2024

# === Define user interface (UI) ===
helpUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # Title
    div(
      shiny::h3("Help")
    ),
    bslib::layout_sidebar(
      
    #Sidebar
    sidebar = bslib::navset_pill(
                          id = ns("subtabs"),
                          bslib::nav_panel( 
                                    title = "Video tutorials",
                                    value = "Video tutorials",
                                    id = "Video tutorials"
                          ),
                          bslib::nav_panel(
                                    title = "Search database",
                                    value = "Search database",
                                    id = "Search database"
                          ),
                          bslib::nav_panel(
                                    title = "Download database",
                                    value = "Download database",
                                    id = "Download database"
                          ),
                          bslib::nav_panel(
                                    title = "Predict traits from taxonomy",
                                    value = "Predict traits from taxonomy",
                                    id = "Predict traits from taxonomy"
                          ),
                          bslib::nav_panel(
                                    title = "Predict traits with metabolic networks",
                                    value = "Predict traits with metabolic networks",
                                    id = "Predict traits with metabolic networks"
                          ),
                          bslib::nav_panel(
                                    title = "Predict traits with machine learning",
                                    value = "Predict traits with machine learning",
                                    id = "Predict traits with machine learning"
                          )
                        ),
    #Main content area
    div(
        shiny::uiOutput(ns("main_content"))
      )
    )
  )
}

# === Define server ===
helpServer <- function(input, output, session, selected_section) {
  ns <- session$ns
  
  # Render main content based on selected tab
  output$main_content <- shiny::renderUI({
    switch(input$subtabs,
           "Video tutorials" = div(
             p(h3("Video tutorials")),
             p(h5("How to predict traits from taxonomy:")),
             p(shiny::uiOutput(ns("video_predictionsTaxonomy"))),
             p(h5("How to predict traits with metabolic networks:")),
             p(shiny::uiOutput(ns("video_predictionsNetwork")))
           ),
           "Search database" = div(
             p(h3("Search database")),
             p("This tool allows the user to search the internal database."),
             p(h4("Query builder")),
             p("The user can generate simple or complex queries with the query builder at the left. Note that capitalization and spaces matter."),
             p(h4("Output format")),
             p("The *csv for matching organisms can be downloaded."),
             p("Matching organisms are also shown in a phylogenetic tree and t-SNE plot of gene functions. The phylogenetic tree is of n = 14 ribosomal genes from n = 3,822 prokaryotes. The t-SNE plot is of n = 30,805 gene functions from n = 4,301 prokaryotes. Organisms that cluster together in this plot have similar functions.")
           ),
           "Download database" = div(
             p(h3("Download database")),
             p("This tool allows the user to download the internal database. The *csv includes all organisms and all information available.")
           ),
           "Predict traits from taxonomy" = div(
             p(h3("Predict traits from taxonomy")),
             p("This tool predicts traits for organisms given its taxonomy. After the user uploads the taxonomy, the tool finds matching organisms in the internal database. If the matching organisms share a trait, the trait is predicted for the user's organisms. This approach is similar to that used by FAPROTAX."),
             p(h4("Data format")),
             p("The input taxonomy should be uploaded as a *csv file and follow one of two formats."),
             tags$i("Column-separated format"),
             p(shiny::tagList("This format follows the output of ", url_DADA2, ". Taxonomic ranks are in columns and organisms in rows. Column names should include \"Phylum\", \"Class\", \"Order\", \"Family\", \"Genus\", \"Species\". Additional columns (e.g., kingdom) can be provided but will be ignored. If a taxonomic rank (e.g., species) is not known for an organism, it can be reported as \"NA\".")),
             p("Example files:"),
             tags$ol(class = "circled-list",
                     tags$li(shiny::downloadLink(outputId = ns("downloadTaxa_1"), label = "Previously uncharacterized bacteria")),
                     tags$li(shiny::downloadLink(outputId = ns("downloadTaxa_2"), label = "Cultured prokaryotes from rumen")),
                     tags$li(shiny::downloadLink(outputId = ns("downloadTaxa_3"), label = "MAGs from rumen"))
             ),
             tags$i("Semicolon-separated format"),
             p(shiny::tagList("This format follows the output of ", url_QIIME2, ". Taxonomic ranks should be separated by semi-colons. The format should be \"p__name; c__name; o__name; f__name; g__name; s__name\". Additional values (e.g., \"k__name\") can be provided but will be ignored. If the species is not known for an organism, it can be reported as \"s__unclassified\" or \"s__?\"; the same principle holds for other taxonomic ranks.")),
             p("Example file:"),
             tags$ol(class = "circled-list",
                     tags$li(shiny::downloadLink(outputId = ns("downloadTaxa_4"), label = "OTUs from infant gut"))
             ),
             p(h4("Prediction threshold")),
             p("When this slider is set to 1, all matching organisms must share the trait for it to be predicted. When it is set to 0.5, only half (0.5) of the matching organisms must have it."),
             p(h4("Simplify names of taxa")),
             p("When turned on, only genus and species names are kept, and others are replaced with \"NA\". Because \"NA\" are not matched, this simplifies matching and usually leads to more matches."),
             p("If genus and species are \"NA\", then family (or next highest taxonomic rank) is kept."),
             p(h4("Ignore missing values in database")),
             p("When turned on, matching organisms with \"NA\" for a trait are ignored.  This leads to more traits being predicted."),
             p(h4("Taxonomy")),
             p("This switch controls the taxonomy in the internal database used for matching."),
             p(h4("Output format")),
             p("The downloadable *csv reports traits for the input taxonomy. A value of \"NA\" represents no trait being predicted.")
           ),
           "Predict traits with metabolic networks" = div(
             p(h3("Predict traits with metabolic networks")),
             p("This tool predicts traits for an organism by building a metabolic network from the genome. The user uploads gene functions from the organism???s genome, then the tool builds a network of biochemical reactions.  It then uses flux balance analysis (FBA) to determine if the network is complete and can transform a chosen substrate to end products."),
             p("In addition to uploading gene functions, the user can also choose an organism from the database."),
             p(h4("Data format")),
             p("If the database tab is chosen, the user only needs to select an organism from the drop down menu."),
             p(shiny::tagList("If the file upload tab is chosen, the user needs to upload *csv files containing the gene functions and reference reactions. For gene functions, *ko files (from ", url_KAAS, ") are also accepted.")),
             p(shiny::tagList("The file for gene functions follows the output of KEGG Automatic Annotation Server (", url_KAAS, "). The rows are KO IDs for the gene functions. To analyze more than one genome, include a column named \"Genome\" with rows containing genome IDs.")),
             p("Example files:"),
             tags$ol(class = "circled-list",
                     tags$li(shiny::downloadLink(outputId = ns("downloadFunctions_1"), label = "E. coli")),
                     tags$li(shiny::downloadLink(outputId = ns("downloadFunctions_2"), label = "Previously uncharacterized bacteria")),
                     tags$li(shiny::downloadLink(outputId = ns("downloadFunctions_3"), label = "Cultured prokaryotes of rumen")),
                     tags$li(shiny::downloadLink(outputId = ns("downloadFunctions_4"), label = "MAGs from rumen"))
             ),
             p(shiny::tagList("The *csv of reference reactions follows the format of the ", url_fbar, " package of R. All reactions needed to ferment a chosen substrate to end products should be included.")),
             p("Example files:"),
             tags$ol(class = "circled-list",
                     tags$li(shiny::downloadLink(outputId = ns("downloadReference_1"), label = "Glucose fermentation")),
                     tags$li(shiny::downloadLink(outputId = ns("downloadReference_2"), label = "Fructose fermentation"))
             ),
             p(h4("Substrate")),
             p("The user specifies the substrate for the metabolic model here. Any metabolite in the reference model can be chosen."),
             p(h4("End products")),
             p("The user specifies end products to check here. Any metabolite in the reference model can be chosen."),
             p(h4("Unbalanced intermediates")),
             p("Metabolites chosen here are allowed to be produced (or consumed) in infinite quantities. NADH and ATP are examples of metabolites usually chosen to be unbalanced. In the metabolic model, these can accumulate without needing to be regenerated to NAD+ or ADP. This simplifies the model, as reactions for consuming NADH and ATP don't have to be included."),
             p(h4("Output format")),
             p("The *csv for fluxes and for network model can be downloaded. The higher the fluxes, the faster the reaction or more product that is formed. The flux of substrate is initially set to -1000. If an end product has a flux <1 (<0.1% of initial value of flux of the substrate), we consider it not to be formed.")
           ),
           "Predict traits with machine learning" = div(
             p(h3("Predict traits with machine learning")),
             p("This tool predicts traits for an organism from its genome using machine learning.  The user uploads gene functions from the genome, the tool uses a machine learning algorithm (random forest classifier) to predict traits."),
             p("The user can predict simple traits using pre-trained models.  They can also train their own models to predict more complex traits."),
             p(h4("Data format")),
             tags$i("Gene functions"),
             p(shiny::tagList("For all tabs, the user needs to upload a file containing gene functions.  The file can be in *csv or *ko format (the latter from ", url_KAAS, ").")),
             p(shiny::tagList("The format follows the output of KEGG Automatic Annotation Server (", url_KAAS, "). The rows are KO IDs for the gene functions. To analyze more than one genome, include a column named \"Genome\" with rows containing genome IDs.")),
             p("Example files:"),
             tags$ol(class = "circled-list",
                     tags$li(shiny::downloadLink(outputId = ns("downloadFunctions_1"), label = "E. coli")),
                     tags$li(shiny::downloadLink(outputId = ns("downloadFunctions_2"), label = "Previously uncharacterized bacteria")),
                     tags$li(shiny::downloadLink(outputId = ns("downloadFunctions_3"), label = "Cultured prokaryotes from rumen")),
                     tags$li(shiny::downloadLink(outputId = ns("downloadFunctions_4"), label = "MAGs from rumen"))
             ),
             tags$i("Response variable"),
             p(shiny::tagList("For the Data upload tab, the user needs to upload a *csv file for the response variable.  See the example files for the format.  Under the Response column, only two values (classes) are allowed (e.g., 0 and 1).")),
             p("Example files:"),
             tags$ol(class = "circled-list",
                     tags$li(shiny::downloadLink(outputId = ns("downloadResponse_1"), label = "Fermentation")),
                     tags$li(shiny::downloadLink(outputId = ns("downloadResponse_2"), label = "Methanogenesis")),
             ),
             tags$i("Predictor variables"),
             p(shiny::tagList("For the Data upload tab, the user also needs to upload a *csv file for the predictor variables.  See the example files for the format.")),
             p("Example files:"),
             tags$ol(class = "circled-list",
                     tags$li(shiny::downloadLink(outputId = ns("downloadPredictors_1"), label = "Fermentation")),
                     tags$li(shiny::downloadLink(outputId = ns("downloadPredictors_2"), label = "Methanogenesis")),
             ),
             tags$i("Random forest models"),
             p(shiny::tagList("For the Model upload tab, the user also needs to upload one or more *rds files of random forest models.  These files typically come from other tabs.")),
             p("Example files:"),
             tags$ol(class = "circled-list",
                     tags$li(shiny::downloadLink(outputId = ns("downloadModel_1"), label = "Fermentation")),
                     tags$li(shiny::downloadLink(outputId = ns("downloadModel_2"), label = "Methanogenesis")),
             ),
             p(h4("Choose traits")),
             p("The user specifies the traits to predict here. For the Standard traits tab, this will load a pre-trained random forest classifier for the trait.  For the Other traits tab, this will train a random forest model given the input in the query builder."),
             p(h4("Advanced settings")),
             p("These settings are for model training."),
             tags$i("Ignore missing values in database"),
             p("When turned on, matching organisms with \"NA\" for a trait are ignored.  This leads to more traits being predicted."),
             tags$i("Proportion of predictors to keep."),
             p("When this slider is set to 0.1, a random subsample of 10% of the predictors is kept for model training.  Higher values will increase training time but may improve predictive performance."),
             tags$i("Proportion of data for model training."),
             p("When this slider is set to 0.7, a random subsample of 70% of data is used for training and 30% for evaluation.  The data include both responses and predictors."),
             tags$i("Set seed for subsampling."),
             p("This sets the seed for randomly subsampling predictors and responses.  If kept at the default (123), subsampling will be identical each time the model is trained."),
             tags$i("Set number of trees"),
             p("This sets the number of trees in the random forest model.  Higher values will increase training time but may improve predictive performance."),
             tags$i("Set number of nodes"),
             p("This sets the number of nodes in the random forest model.  Higher values will increase training time but may improve predictive performance."),
             tags$i("Weight for positive classes of responses."),
             p("When this slider is set to 0.5, positive and negative responses receive equal weight during training.  Increasing it will give more weight to positive responses."),
             tags$i("Name of trait"),
             p("This sets the name of trait in the output, and it does not affect predictive performance.  Only alphanumeric characters are allowed."),
             p(h4("Output format")),
             p("The *csv for probabilities of predicted traits can be downloaded. The probabilities range from 0 to 1."),
             p("Additionally, an *rds file for the random forest can be downloaded. It can be re-uploaded using the Model upload tab.")
           )
    )
  })
  
  # Output videos for tutorial
  output$video_predictionsTaxonomy <- shiny::renderUI({
    shiny::HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/Lhlk-4vRmL4" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
  })
  
  output$video_predictionsNetwork <- shiny::renderUI({
    shiny::HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/MOubZwqIW4I" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
  })
  
  # Output example data for download
  output$downloadTaxa_1 <- create_download_handler("taxa_uncharacterized", function() taxa_uncharacterized)
  output$downloadTaxa_2 <- create_download_handler("taxa_rumen_cultured", function() taxa_rumen_cultured)
  output$downloadTaxa_3 <- create_download_handler("taxa_rumen_MAGs", function() taxa_rumen_MAGs)
  output$downloadTaxa_4 <- create_download_handler("taxa infant", function() taxa_infant)
  
  output$downloadFunctions_1 <- create_download_handler("gene_functions_e_coli", function() gene_functions_e_coli)
  output$downloadFunctions_2 <- create_download_handler("gene_functions_uncharacterized", function() gene_functions_uncharacterized)
  output$downloadFunctions_3 <- create_download_handler("gene_functions_rumen_cultured", function() gene_functions_rumen_cultured)
  output$downloadFunctions_4 <- create_download_handler("gene_functions_rumen_MAGs", function() gene_functions_rumen_MAGs)
  
  output$downloadReference_1 <- create_download_handler("reference_reactions_glucose_fermentation", function() reference_reactions_glucose_fermentation)
  output$downloadReference_2 <- create_download_handler("reference_reactions_fructose_fermentation", function() reference_reactions_fructose_fermentation)
  
  output$downloadResponse_1 <- create_download_handler("response_fermentation", load_response_fermentation)
  output$downloadResponse_2 <- create_download_handler("response_methanogenesis", load_response_methanogenesis)
  
  output$downloadPredictors_1 <- create_download_handler("predictors_fermentation", load_predictors_fermentation)
  output$downloadPredictors_2 <- create_download_handler("predictors_methanogenesis", load_predictors_methanogenesis)
  
  output$downloadModel_1 <- create_download_handler("random_forest_fermentation", load_model_fermentation, file_type = "rds")
  output$downloadModel_2 <- create_download_handler("random_forest_methanogenesis", load_model_methanogenesis, file_type = "rds")
}
