##############
#Load packages
##############
library(dplyr)
library(fbar)
library(ggplot2)
library(htmltools)
library(igraph)
library(queryBuilder)
library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(stringr)
library(tidyr)
library(treemap)

###################
#Load internal data
###################
#Database
data_fp = "data/database.csv"
raw_data = read.csv(data_fp)
raw_data[is.na(raw_data)] = "NA"

#Predict from taxonomy
data_fp = "data/query.csv"
example_query = read.csv(data_fp)

#Predict from genome
data_fp = "data/gene_functions.csv"
gene_functions = read.csv(data_fp)

data_fp = "data/e_coli.csv"
e_coli = read.csv(data_fp, header=FALSE)

data_fp = "data/reference_reactions.csv"
reference_reactions = read.csv(data_fp)


data_fp = "data/reference_reactions_with_fructose.csv"
reference_reactions_with_fructose = read.csv(data_fp)

#################
#Define functions
#################
  #Define navigation buttons found on home page
    home_button <- function(image_name, title, subtitle, button_name, icon_background_color="red", position="left") {
      div(  
        class = "home-button-box",
        div(
          class = "home-button-grid",
            div(
              class = "home-button-icon", 
              tags$img(src=paste0(image_name, ".svg"), 
              style=paste0('background-color:',icon_background_color, ';', 'border-radius: 5px; display: block; max-width: 100%; max-height: 60px; height: auto; margin: 0 auto;')
              )
            ),
            div(
              div(
                class = "home-button-title", 
                title
              ),
              div(
                class = "home-button-subtitle", 
                subtitle
              )
            )
        ),
        actionButton(button_name, NULL, class=paste0("home-action-button-", position))
      )
    }
  
  
  #Define file input box with added download link
    fileInput_custom=function(inputId, label, multiple = FALSE, accept = NULL, width = NULL, buttonLabel = "Browse...", placeholder = "No file selected", downloadId, downloadlabel) 
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
        downloadLink(downloadId, downloadlabel),
        tags$div(
          id = paste(inputId, "_progress", sep = ""), class = "progress active shiny-file-input-progress", 
          tags$div(class = "progress-bar")),
      )
    }
  
  #Define file input box with added download link (2 files)
    fileInput_custom_2_files=function(inputId, label, multiple = FALSE, accept = NULL, width = NULL, buttonLabel = "Browse...", placeholder = "No file selected", downloadId1, downloadlabel1, downloadId2, downloadlabel2) 
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
        div(
          downloadLink(downloadId1, downloadlabel1),
          br(),
          downloadLink(downloadId2, downloadlabel2)
        ),
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
  
  #Make html link to external website
    createLink <- function(ID, url) {
        if(!is.na(ID)&ID!="NA")
        {
          ID = as.character(ID)
          
           #Separate multiple IDs  by ","
           ID_split=unlist(strsplit(x=ID, split=", "))
           
          #Create one link per ID, with commas separating multiple links
          for(i in 1:length(ID_split))
          {
            temp=sprintf(paste0('<a href="', URLdecode(paste0(url, ID_split[i])),'" target="_blank">', ID_split[i],'</a>'))
            if(i==1)
            {
              link=temp
            }else{
              link=paste(link,temp, sep=",")
            }

          }
        }else
        {
          link="NA"
        }
        return(link)
    }
  
  #Make html links with button
    createLinkButton <- function(url) {
      if(!is.na(url))
      {
        sprintf(paste0('<a href="', URLdecode(paste0(url)),'" target="_blank" class="btn btn-primary">Link</a>'))
      }else
      {
        "NA"
      }
    }
    
  #Filter organisms by preferred taxonomy
    filter_by_taxonomy = function(data, input_taxonomy)
    {
      if(is.null(input_taxonomy[1]))
      {
        data = data[0,]
      }else if(length(input_taxonomy)==2&input_taxonomy[1]=="Bergey"&input_taxonomy[2]=="NCBI")
      {
        data = data
        data$Phylum = ifelse(data$Phylum=="NA",data$NCBI_Phylum,data$Phylum)
        data$Class = ifelse(data$Class=="NA",data$NCBI_Class,data$Class)
        data$Order = ifelse(data$Order=="NA",data$NCBI_Order,data$Order)
        data$Family = ifelse(data$Family=="NA",data$NCBI_Family,data$Family)
        data$Genus = ifelse(data$Genus=="NA",data$NCBI_Genus,data$Genus)
        data$Species = ifelse(data$Species=="NA",data$NCBI_Phylum,data$Species)
      }else if(input_taxonomy[1]=="Bergey")
      {
        data = data %>% filter(Phylum!="NA")
        data$Phylum = data$Phylum
        data$Class = data$Class
        data$Order = data$Order
        data$Family = data$Family
        data$Genus = data$Genus
        data$Species = data$Species
      }else if(input_taxonomy[1]=="NCBI") 
      {
        data = data %>% filter(NCBI_Phylum!="NA")
        data$Phylum = data$NCBI_Phylum
        data$Class = data$NCBI_Class
        data$Order = data$NCBI_Order
        data$Family = data$NCBI_Family
        data$Genus = data$NCBI_Genus
        data$Species = data$NCBI_Species
      }
      
      return(data)
    }
  
  #' Clean data from BacDive
    #' Clean data from BacDive
    clean_BacDive_data = function(x, is_numeric=FALSE, to_lower=FALSE)
    {
      if(is_numeric==TRUE)
      {
        x =  as.character(x)
        
        for(i in 1:length(x))
        {
          z = x[i]
          z = gsub(pattern="<", replacement="", x=z)
          z = gsub(pattern=">", replacement="", x=z)
          z = gsub(pattern="day[s]*", replacement="", x=z)
          z = gsub(pattern="_", replacement="", x=z)
          z = gsub(pattern=" ", replacement="", x=z)
          z = gsub(pattern="\\.$", replacement="", x=z)
          z = gsub(pattern="\\.-", replacement="-", x=z)
          
          z = str_split(z, pattern="-", simplify=TRUE)
          if(z[1]!="NA")
          {  
            z = as.numeric(z)
          }else
          {
            z = NA
          }
          
          x[i] = mean(z)
        }
        
        x = as.numeric(x)
        
      }else if(is_numeric==FALSE)
      {
        x = gsub(pattern="^0$", replacement="-", x = x)
        x = gsub(pattern="^1$", replacement="+", x = x)
        x = gsub(pattern="#", replacement="", x = x)
      }
      
      if(to_lower==TRUE)
      {
        x = tolower(x)
      }
      
      return(x)
    }
    
    
  #Negation of in 
    `%nin%` = Negate(`%in%`)
      
  #Rescale values between -1 and 1
    rescale = function(x){
      (((x-min(x))/(max(x)-min(x)))-0.5)*2
    }
  
  #Build Metabolic Model
    build_model = function(equation=NULL, direction="Bidirectional", abbreviation=NA, officialName=NA, geneAssociation=NA, subsystem=NA, starting_metabolite="D-Glucose", ending_metabolite="Pyruvate", unbalanced_intermediate=c("NAD+", "NADH", "ATP", "ADP", "Orthophosphate", "H2O", "H+", "CO2"), unbalanced_product=NULL, remove_redundant_reactions=TRUE, add_glycolysis=FALSE)
    {
      if(is.null(equation))
      {
        equation=add_glycolysis()$equation
      }
    
  #Format Data For Metabolic Model
    uppbnd = vector(length=length(equation))
    lowbnd = vector(length=length(equation))
    uppbnd = if_else(direction=="Reverse", 0, 1000)
    lowbnd = if_else(direction=="Forward", 0, -1000)
    df = format_data(abbreviation, lowbnd=lowbnd, uppbnd=uppbnd, obj_coef=0, equation=equation, officialName=officialName, geneAssociation=geneAssociation, subsystem=subsystem)
    df$abbreviation =paste0("Enzyme_", seq(1:nrow(df)))
      
  #Add Enzymes of Glycolysis
    if(add_glycolysis==TRUE)
    {
      df = add_glycolysis(df = df)
    }
    
  #Remove Redundant Reactions
    if(remove_redundant_reactions==TRUE)
    {
      df = remove_redundant_reactions(df)
    }
    
  #Add Target Metabolites
    if(!is.null(starting_metabolite)&!is.null(ending_metabolite))
    {
      lowbnd = -1000
      uppbnd = 10^6
      starting_metabolite = format_name(starting_metabolite)
      ending_metabolite = format_name(ending_metabolite)
      df = add_target_metabolites(df=df, starting_metabolite=starting_metabolite, ending_metabolite=ending_metabolite, lowbnd=lowbnd, uppbnd=uppbnd)
    }
  
  #Add Unbalanced Metabolites
      if(!is.null(unbalanced_intermediate)&!is.null(unbalanced_product))
      {
        unbalanced_intermediate = format_name(unbalanced_intermediate)
        unbalanced_product = format_name(unbalanced_product)
        unbalanced_intermediate = unbalanced_intermediate[!unbalanced_intermediate %in% ending_metabolite]
        unbalanced_product = unbalanced_product[!unbalanced_product %in% ending_metabolite]
        names = c(unbalanced_intermediate,unbalanced_product)
        lowbnd = c(rep(-10^6, times=length(unbalanced_intermediate)), rep(0, times=length(unbalanced_product)))
        uppbnd = c(rep(10^6, times=length(unbalanced_intermediate)), rep(10^6, times=length(unbalanced_product)))
        df = add_unbalanced_metabolites(df=df, names=names, lowbnd=lowbnd, uppbnd=uppbnd)
      }
      
      return(df)
    }
  
  #Format Data For Metabolic Model
    format_data = function(abbreviation=NA, lowbnd=-1000, uppbnd=1000, obj_coef=0, equation, officialName=NA, geneAssociation=NA, subsystem=NA){
      
      #Create dataframe in format
      df = data.frame(abbreviation, lowbnd, uppbnd, obj_coef, equation, officialName, geneAssociation, subsystem)
      
      #Format equations  
      df$equation = format_name(name = df$equation, remove_coefficient=FALSE)
      
      #Remove extra rows
      df = df %>% distinct()
      df = df[which(df$equation!=""),]
      
      return(df)
    }
    
  #Format Names of Metabolites and Equations
    format_name = function(name, remove_coefficient=TRUE, add_underscore=TRUE, remove_charge=TRUE)
    {
      name = gsub(pattern="alpha-D", replacement="D", x = name)
      name = gsub(pattern="beta-D", replacement="D", x = name)
      name = gsub(pattern="^ ", replacement="", x = name)
      name = gsub(pattern=" $", replacement="", x = name)
      name = gsub(pattern="^n ", replacement="", x = name)
      
      if(add_underscore==TRUE)
      {
        name = gsub(pattern="-", replacement="_", x=name)  
        name = gsub(pattern="([aA-zZ])( )([aA-zZ0-9])", replacement="\\1_\\3", x=name)
        name = gsub(pattern="(\\))( )", replacement="\\1_", x=name)
        name = gsub(pattern="(,)", replacement="_", x=name)
      }
      
      if(remove_charge==TRUE)
      {
        name = gsub(pattern="([aA-zZ0-9])(\\+)", replacement="\\1", x=name)
      }
      
      if(remove_coefficient==TRUE){
        name = gsub(pattern="^[1-9] ", replacement="", x = name)
      }
      
      return(name)
    }
    
  #Remove Redundant Enzymatic Reactions
    remove_redundant_reactions = function(df)
    {
      df = df %>% distinct(equation, .keep_all=TRUE)
      
      return(df)
    }
    
  #Add Enzymes of Glycolysis
    add_glycolysis=function(df=NULL)
    {
      df = add_one_reaction(df, abbreviation="Extra_reaction_1", lowbnd= 0, uppbnd=1000, obj_coef=0, equation="ATP + D-Glucose <=> ADP + D-Glucose 6-phosphate", officialName=NA, geneAssociation=NA, subsystem=NA)
      df = add_one_reaction(df, abbreviation="Extra_reaction_2", lowbnd=-1000, uppbnd=1000, obj_coef=0, equation="D-Glucose 6-phosphate <=> D-Fructose 6-phosphate", officialName=NA, geneAssociation=NA, subsystem=NA)
      df = add_one_reaction(df, abbreviation="Extra_reaction_3", lowbnd=-1000, uppbnd=1000, obj_coef=0, equation="ATP + D-Fructose 6-phosphate <=> ADP + D-Fructose 1,6-bisphosphate", officialName=NA, geneAssociation=NA, subsystem=NA)
      df = add_one_reaction(df, abbreviation="Extra_reaction_4", lowbnd= 0, uppbnd=1000, obj_coef=0, equation="D-Fructose 1,6-bisphosphate <=> Glycerone phosphate + D-Glyceraldehyde 3-phosphate", officialName=NA, geneAssociation=NA, subsystem=NA)
      df = add_one_reaction(df, abbreviation="Extra_reaction_5", lowbnd=-1000, uppbnd=1000, obj_coef=0, equation="D-Glyceraldehyde 3-phosphate <=> Glycerone phosphate", officialName=NA, geneAssociation=NA, subsystem=NA)
      df = add_one_reaction(df, abbreviation="Extra_reaction_6", lowbnd=-1000, uppbnd=1000, obj_coef=0, equation="D-Glyceraldehyde 3-phosphate + Orthophosphate + NAD+ <=> 3-Phospho-D-glyceroyl phosphate + NADH + H+", officialName=NA, geneAssociation=NA, subsystem=NA)
      df = add_one_reaction(df, abbreviation="Extra_reaction_7", lowbnd=-1000, uppbnd=1000, obj_coef=0, equation="ATP + 3-Phospho-D-glycerate <=> ADP + 3-Phospho-D-glyceroyl phosphate", officialName=NA, geneAssociation=NA, subsystem=NA)
      df = add_one_reaction(df, abbreviation="Extra_reaction_8", lowbnd=-1000, uppbnd=1000, obj_coef=0, equation="2-Phospho-D-glycerate <=> 3-Phospho-D-glycerate", officialName=NA, geneAssociation=NA, subsystem=NA)
      df = add_one_reaction(df, abbreviation="Extra_reaction_9", lowbnd=-1000, uppbnd=1000, obj_coef=0, equation="2-Phospho-D-glycerate <=> Phosphoenolpyruvate + H2O", officialName=NA, geneAssociation=NA, subsystem=NA)
      df = add_one_reaction(df, abbreviation="Extra_reaction_10", lowbnd=-1000, uppbnd=0, obj_coef=0, equation="ATP + Pyruvate <=> ADP + Phosphoenolpyruvate", officialName=NA, geneAssociation=NA, subsystem=NA)
      
      df$equation = format_name(name = df$equation, remove_coefficient=FALSE)
      
      return(df)
    }
    
  #Specify Which Metabolites Are Unbalanced
  ##This function specifies which metabolites are unbalanced
  ##Unbalanced metabolites which can accumulate (or be consumed) in unlimited quantities
  ##NADH and ATP are examples of metabolites assumed to be unbalanced
  ##In the model, these can accumulate without needing to be regenerated to NAD+ or ADP
  ##This simplifies the metabolic model, as reactions for consuming NADH and ATP don't have to be included
    add_unbalanced_metabolites = function(df, names, lowbnd=-10^6, uppbnd=10^6)
    {
      #Get names of all metabolites
      metabolites = df
      metabolites = str_split(string = metabolites$equation, pattern="<=>", simplify=TRUE)
      metabolites = as.character(metabolites)
      metabolites = str_split(string = metabolites, pattern=" \\+", simplify = TRUE)
      metabolites = as.character(metabolites)
      metabolites = format_name(metabolites)
      metabolites = unique(metabolites)
      
      #Get names of unbalanced metabolites
      index = names %in% metabolites
      names = names[index]
      
      #Format equations for unbalanced metabolites
      abbreviation = paste0("Unbalanced_metabolite_",seq(1:length(names)))
      lowbnd = lowbnd[index]
      uppbnd = uppbnd[index]
      obj_coef = 0
      equation = paste0(names, " <=>")
      officialName=NA
      geneAssociation= NA
      subsystem = NA
      append = data.frame(abbreviation, lowbnd, uppbnd, obj_coef, equation, officialName, geneAssociation, subsystem)
      
      df = rbind(df,append)
      
      return(df)
    }
    
  #Specify Starting and Ending Metabolites
    add_target_metabolites = function(df, starting_metabolite, ending_metabolite, lowbnd=-1000, uppbnd=10^6)
    {
      abbreviation = c("Starting_metabolite","Ending_metabolite")
      lowbnd = c(lowbnd,0)
      uppbnd = c(0,uppbnd)
      obj_coef = c(0,1)
      equation = c(paste0(starting_metabolite, " <=>"), paste0(ending_metabolite, " <=>"))
      officialName=NA
      geneAssociation= NA
      subsystem = NA
      
      match = match(x=equation, table=df$equation)
      match = match[!is.na(match)]
      if(length(match>0))
      {
        df = df[-match,]
      }
      
      
      append = data.frame(abbreviation, lowbnd, uppbnd, obj_coef, equation, officialName, geneAssociation, subsystem)
      df = rbind(df,append)
      
      return(df)
    }
    
  #Add One Enzymatic Reaction
    add_one_reaction = function(df=NULL, abbreviation=NA, lowbnd=-1000, uppbnd=1000, obj_coef=0, equation, officialName=NA, geneAssociation=NA, subsystem=NA)
    {
      #Add a reaction
      append = data.frame(abbreviation, lowbnd, uppbnd, obj_coef, equation, officialName, geneAssociation, subsystem)
      
      #Format equations
      append$equation = format_name(name = append$equation, remove_coefficient=FALSE)
      
      if(is.null(df)|is.function(df)|length(df)==0)
      {
        df = append
      }else{
        df = rbind(df,append)
      }
      
      return(df)
    }
    
  #Delete One Enzymatic Reaction
    delete_one_reaction = function(df, officialName)
    {
      #Delete a reaction
      df = df[-which(df$officialName==officialName),]
      
      return(df)
    }
    
  #' Find Enzymes in Genome
  #' 
  #' This function finds which enzyme reactions in a model are in an organism's genome
  #' The user provides a model along with database IDs for the organism's genes
    find_enzymes = function(df, database_ID)
    {
      x = str_split(df$geneAssociation, pattern=", ")
      for(i in 1:length(x))
      {
        for(j in 1:length(x[[i]]))
        {
          x[[i]][j] = x[[i]][j] %in% database_ID
        }
      }  
      
      for(i in 1:length(x))
      {
        x[[i]] = all(x[[i]]==TRUE)
      }
      
      x = unlist(x)
      
      enzymes = df$officialName[x]
      
      return(enzymes)
    }
    
  #' Simplify Model
  #' 
  #' This function removes extra reactions from a model
  #' The user specifies which reactions to keep
  #' The rest are removed (or have flux is constrained to 0)
    simplify_model = function(df, officialName=NULL, equation=NULL, geneAssociation=NULL, constrain_flux=TRUE, remove_reactions = FALSE)
    {
      `%nin%` = Negate(`%in%`)
      
      to_remove = df %>% filter(!grepl(pattern="Unbalanced_metabolite", abbreviation))
      to_remove = which(to_remove$officialName %nin% officialName)
      
      if(constrain_flux==TRUE)
      {
        df$lowbnd[to_remove] = 0
        df$uppbnd[to_remove] = 0
      }
      
      if(remove_reactions==TRUE)
      {
        df = df[-to_remove,]
      }
      
      return(df)
    }
    
    #Get Metabolite Names
    get_metabolite_names = function(equation, to_remove=NULL)
    {
      name = str_split(string = equation, pattern="<=>", simplify=TRUE)
      name = as.character(name)
      name = str_split(string = name, pattern="[ ]\\+", simplify = TRUE)
      name = as.character(name)
      name = format_name(name, add_underscore=FALSE, remove_charge=FALSE)
      
      if(!is.null(to_remove))
      {
        name = name[!name %in% format_name(to_remove, add_underscore=FALSE, remove_charge=FALSE)]
      }      
      
      name = unique(name)
      name[name != ""]
      name = sort(name)
      
      return(name)
    }
  
    #Draw Enzymatic Reactions in Graph
    graph_enzymes = function(s, name=c("NAD+", "NADH", "ATP", "ADP", "Orthophosphate", "H2O", "H+", "CO2"), show_flux=FALSE, color_vertices=FALSE){
      #Get data
      df = s
      
      #Get names of reactants and products 
      df = df %>% separate(col=equation, into=c("reactant", "product"), sep = "<=>")
      df = df[which(df$reactant!=""),] #Remove reactions with no reactants
      df = df[which(df$product!=""),] #Remove reactions with no products
      df = df %>% separate_rows(reactant,sep = " \\+ ")
      df = df %>% separate_rows(product,sep = " \\+ ")
      
      #Format names of reactants and products
      df$reactant = format_name(name = df$reactant, add_underscore=FALSE)
      df$product = format_name(name = df$product, add_underscore=FALSE)
      df = df %>% distinct()
      
      #Remove any names (usually unbalanced metabolites) not to be displayed in graph
      name = format_name(name)
      df = df[!(df$reactant %in% name), ]
      df = df[!(df$product %in% name), ]
      
      if(show_flux==TRUE)
      {
        #Add fluxes
        df = df[!is.na(df$officialName),]
        match = match(x = paste0(df$officialName,df$geneAssociation), table = paste0(s$officialName,s$geneAssociation))
        df$flux = NA
        df$flux = s$flux[match]
        
        #Get graph
        df = df %>% select(reactant, product, officialName, subsystem, flux, lowbnd, uppbnd)
        g = graph_from_data_frame(d = df, directed = FALSE)
      }else
      {  
        #Get graph
        df = df %>% select(reactant, product, officialName, subsystem)
        g = graph_from_data_frame(d = df, directed = FALSE)
      }
      
      if(show_flux==TRUE)
      {
        green_color = rgb(red=0, green=176, blue=80, maxColorValue = 255) 
        
        #Set colors for edges
        E(g)$color = if_else(E(g)$uppbnd==0&E(g)$lowbnd==0, adjustcolor("black", alpha.f = 0.1),  if_else(abs(E(g)$flux)>1, green_color, "grey50"))
        
        #Set width for edges
        E(g)$width = abs(E(g)$flux)
        E(g)$width = E(g)$width/100+1
        E(g)$width = if_else(E(g)$uppbnd==0&E(g)$lowbnd==0, 1,  E(g)$width)
        
        #Set line type for edges
        E(g)$lty = if_else(E(g)$uppbnd==0&E(g)$lowbnd==0, 3,  1)
        
        #Set colors for vertices
        match = df %>% filter(uppbnd!=0|lowbnd!=0)
        match = unique(c(match$reactant, match$product))
        `%nin%` = Negate(`%in%`)
        V(g)$color = if_else(V(g)$name %in% match, "grey90", adjustcolor("grey90", alpha.f = 0))
        
        #Set line color for vertices
        V(g)$frame.color = if_else(V(g)$name %in% match, "black", adjustcolor("black", alpha.f = 0.1))
        
        #Set label colors for vertices
        V(g)$label.color = if_else(V(g)$name %in% match, "black", adjustcolor("black", alpha.f = 0.1))
      }
      
      if(color_vertices==TRUE)
      {
        #Set colors for vertices
        color = V(g)$name
        df = df %>% pivot_longer(cols=c(reactant,product))
        for(i in 1:length(color))
        {
          color[i] = tail(names(sort(table(df$subsystem[which(df$value==color[i])]))), 1)
        }
        
        n = length(unique(color)) 
        color_pallette = qualitative_hcl(n, h = c(15, 375 * (n - 1)/n), c = 100, l = 65, fixup = TRUE, alpha = 1)
        
        color=as.factor(color)
        levels(color) = color_pallette
        color = as.character(color)
        
        g = set_vertex_attr(graph=g, name="color", value=color)  
        
        #Set label colors for vertices
        V(g)$label.color = "white"
        
        #Set colors for edges
        E(g)$color = "grey50"
        
      }  
      
      #Set names for vertices 
      #V(g)$name = seq(1:length(V(g)$name))
      
      return(g)
    }
  
      
  #Plot treemap
    plot_treemap = function(data, var, sep=NULL, remove_underscore="TRUE")
    {

      if(all(is.na(data[[sym(var)]])))
      {
        return(NULL)
      }else{
        #Prepare data
        if(!is.null(sep))
        {
          data = data %>% separate_rows(!!sym(var), sep=";")
        }
        
        data = data %>% group_by(!!sym(var)) %>% count()
        data = data[!is.na(data[[sym(var)]]),]
        data = data[data[[sym(var)]]!="NA",]
        data =  as.data.frame(data)
        data$n = as.numeric(data$n)
        data[2] = as.numeric(data$n)
        
        title=paste(var)
        if(remove_underscore=="TRUE")
        {
          title = gsub(pattern="_", replacement=" ", x=title)
          title = paste(title)
        }
        
        #Plot tree map
        p = treemap(dtf=data, index=colnames(data[1]), vSize="n", type="index", title=title)
      }
      
      return(p)
    }
    
  #Plot Heatmap of Fermentation Products
    plot_heatmap = function(data, max_char=13)
    {
      min_flux = 1
      
      #Format data
      df = data
      df = df %>% pivot_longer(cols=colnames(df), names_to="End_products", values_to="value")
      df$value = as.numeric(df$value)
      df$value = if_else(df$value>min_flux, 1, 0)
      df$value = as.character(df$value)
      df$value[df$value=="0"]="(-)"
      df$value[df$value=="1"]="(+)"
      to_truncate=which(nchar(df$End_products)>max_char)
      df$End_products[to_truncate]=substr(x=df$End_products[to_truncate], start=0, stop=(max_char-3))
      df$End_products[to_truncate]=paste0(df$End_products[to_truncate],"...")
      
      #Specify colors 
      #Set colors to match those in Powerpoint
      red_color = rgb(red=255, green=0, blue=0, maxColorValue = 255) 
      green_color = rgb(red=0, green=176, blue=80, maxColorValue = 255) 
      grey_color = rgb(red=127, green=127, blue=127, maxColorValue = 255) 
      
      #Construct plot
      #Call ggplot2
      p = ggplot()+
        
        #Plot heat map
        geom_tile(data = df, aes(x=End_products, y=1), size=1, color="black", fill=if_else(df$value=="(-)", "grey", green_color)) +
        geom_label(data = df, aes(x=End_products, y=1, label = value), color="white", size=6, label.size = NA, fill = NA, fontface = "bold")+
        
        #Set colors
        #scale_fill_manual(c(red_color, green_color)) +
        
        #Set theme
        theme(legend.position = "none",
              axis.title.y=element_blank(),
              axis.title.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, colour="black"),
              axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5, colour="black", size=12),
              #text = element_text(size=12),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              #plot.background = element_blank()
        )
      
      p
      
      return(p)
    }
    
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
  
#Choices for metabolites for metabolic models
  choices_unbalanced_intermediates = get_metabolite_names(equation = reference_reactions$equation)  
    
  to_remove = c("NAD+", "NADH", "NADP+", "NADPH", "FAD+", "FADH2", "Reduced ferredoxin", "Oxidized ferredoxin", "Ubiquinone", "Ubiquinol", "Quinone", "Hydroquinone", "Menaquinone", "Menaquinol", "Oxidized hydrogenase", "Reduced hydrogenase",  "ATP", "ADP", "AMP", "GTP", "GDP", "Orthophosphate", "Diphosphate", "Polyphosphate", "H2O", "H+", "H+[side 1]", "H+[side 2]", "Na+[side 1]", "Na+[side 2]", "Sodium cation", "HCO3-")
  choices_metabolites = c(get_metabolite_names(equation = reference_reactions$equation, to_remove = to_remove)) 
    
#Choices for organisms for metabolic models 
  row_match = which(raw_data$IMG_Genome_ID_max_genes %in% gene_functions$Genome_ID)
  choices_organism = paste(raw_data$Genus[row_match], raw_data$Species[row_match], raw_data$Subspecies[row_match], sep=" ")
  choices_organism = gsub(pattern=" NA$", replacement="", x=choices_organism)
  
#Links to external websites
  url_GOLD <- a("GOLD", href="https://gold.jgi.doe.gov/", target="_blank")
  url_IMG <- a("IMG/M", href="https://img.jgi.doe.gov/m/", target="_blank")
  url_JGI <- a("this notice.", href="https://jgi.doe.gov/disclaimer/", target="_blank")
  url_BacDive <- a("BacDive", href="https://bacdive.dsmz.de/", target="_blank")
  url_CC = a("CC by 4.0 license", href="https://creativecommons.org/licenses/by/4.0/", target="_blank")
  url_Bergey = a("Bergey's Manual of Systematics of Archaea and Bacteria", href="https://onlinelibrary.wiley.com/doi/book/10.1002/9781118960608", target="_blank")
  url_fairuse = a("fair use", href="https://www.copyright.gov/fair-use/", target="_blank")
  url_KAAS =  a("KAAS", href="https://www.genome.jp/kegg/kaas/", target="_blank")
  url_QIIME = a("QIIME", href=" http://qiime.org/", target="_blank")
  
###########################
#Define user interface (UI)
###########################
ui <- 
  tagList(
    fluidPage(
      
      #*********
      #Set style
      #*********
      useShinydashboard(), 
      #Get style from style.css (/www folder)
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
        tags$link(rel = "shortcut icon", href = "favicon.svg")
      ),
      
      #*********************
      #Define layout of tabs
      #*********************
      fluidRow(
        #Navigation bar
        navbarPage(
          id="tabs", windowTitle="Fermentation Explorer", 
          title="",
          
        #Home
          tabPanel(
            value="home",
            title=div(icon("home"), "Home"),
            
            fluidRow(
              tags$img(src='FermentationExplorer_logo.svg', width=75),
              br(),
              tags$img(src='FermentationExplorer_logo_text.svg', width=400),
              p(
                "Get started by choosing an option below",
                style = "color: grey; font-size: 20px;"
              ),
              align="center"
            ),
            
            fluidRow(
              p(""),
              p("")
            ),
              
            fluidRow(
              column(
                div(
                  home_button(button_name = 'jump_search_database', position="left", icon_background_color="#6d54a3", image_name="search_database", title = "Search database", subtitle="Find data for thousands of organisms")
                ),
                width=6, align= "right"
              ),
              column(
                div(
                  home_button(button_name = 'jump_download_database', position="right", icon_background_color="#ef4146", image_name="download_database", title = "Download database", subtitle="For use in Excel or other programs")
                ),
                width=6, align= "left"
              )
            ),
              
            fluidRow(
              p("")
            ),
            
            fluidRow(
              column(
                  div(
                    home_button(button_name = 'jump_predict_from_taxonomy', position="left", icon_background_color="#26b784", image_name="predict_from_taxonomy", title = "Predict traits from taxonomy", subtitle="Just provide names of taxa")
                  ),
                  width=6, align= "right"
                ),
              column(
                div(
                  home_button(button_name = 'jump_predict_from_genomes', position="right", icon_background_color="#bb65a8", image_name="predict_from_genomes", title = "Predict traits from genome", subtitle="Use our library of genomes or BYO")
                ),
                width=6, align= "left"
              )
            ),
            
            fluidRow(
              p("")
            ),
            
            fluidRow(
              column(
                div(
                    home_button(button_name = 'jump_help', position="left", icon_background_color="#f3a73f", image_name="help", title = "Help", subtitle="FAQ and tutorials")
                ),
                width=6, align= "right"
              ),
              column(
                div(
                  home_button(button_name = 'jump_about', position="right", icon_background_color="#808285", image_name="about", title = "About", subtitle="How this resource was developed")
                ),
                width=6, align= "left"
              )
            )
          ),
          
          #Database
          navbarMenu(title=div(icon("database"), "Database"),
            #Search database
             tabPanel(
               value="search_database",
               title="Search",
               fluidRow(
                 column(width=12, h3("Search database"))
               ),
               sidebarPanel(
                 width=4,
                 queryBuilderOutput('querybuilder')
               ),
               mainPanel(
                 width=8,
                   div(
                     id = "results_page_database_taxa",
                     fluidRow(
                       box(
                         title = textOutput("n_match_database_taxa"), downloadButton('download_data_search_database', 'Download results'), status = "primary", solidHeader = TRUE
                       )
                     ),
                     fluidRow(
                       box(
                         title = "Detailed results",  
                         fluidRow(
                          column(width=12, offset = 0, dataTableOutput("table_search_database") %>% withSpinner(color="#3C8DBC"))
                         ),
                         fluidRow(
                           column(width=4, 
                                  div(
                                    checkboxGroupInput("checkboxes_info_organism", "Organism", choices=choices_info_organism, selected=c(which(names(choices_info_organism)=="Genus"), which(names(choices_info_organism)=="Species"), which(names(choices_info_organism)=="Article link"))),
                                    checkboxGroupInput("checkboxes_info_fermentation", "Fermentation", choices=choices_info_fermentation)
                                  )
                            ),
                           column(width=4, 
                                  div(
                                    checkboxGroupInput("checkboxes_info_JGI", "JGI", choices=choices_info_JGI, selected=c(which(names(choices_info_JGI)=="GOLD Organism ID"), which(names(choices_info_JGI)=="IMG Genome ID"))), 
                                    checkboxGroupInput("checkboxes_info_NCBI", "NCBI", choices=choices_info_NCBI, selected=c(which(names(choices_info_NCBI)=="NCBI Taxonomy ID")))
                                  )
                           ),
                           column(width=4, 
                                  checkboxGroupInput("checkboxes_info_BacDive", "BacDive", choices=choices_info_BacDive, selected=c(which(names(choices_info_BacDive)=="BacDive Organism ID")))
                          )
                         ),
                         
                         width=12, status = "primary", solidHeader = TRUE, collapsible = TRUE
                       )
                      )
                  )
                )
              ),
            #Download database     
              tabPanel(
                value="download_database", 
                title="Download",
                fluidRow(
                  column(width=2),
                  column(
                    p(h3("Download database")),
                    p(""),
                    p(""),
                    p(h5("Click below to download the full database in csv format.")),
                    p(downloadButton('download_database_full', 'Download')),
                    width=8),
                  column(width=2),
                )
              )
            ),
            
          #Predict 
          navbarMenu(title=div(icon("desktop"), "Predict"),
                     #Predict from taxonomy
                     tabPanel(
                       value="predict_from_taxonomy",
                       title="From taxonomy",
                       fluidRow(
                         column(width=12, 
                                h3("Predict traits from taxonomy")),
                       ),
                       sidebarPanel(width = 3, 
                                    fileInput_custom("file1", "Upload names of taxa",multiple = TRUE,accept = c("text/csv","text/comma-separated-values,text/plain",".csv"), downloadId="downloadTaxa", downloadlabel="Download example"),
                                    sliderInput("threshold", "Prediction threshold:",min = 0, max = 1,value = 1),
                                    checkboxGroupInput(inputId="predict_from_taxonomy_filter", label="Taxonomy", choices=list("Bergey","NCBI"), selected=list("Bergey", "NCBI"), inline=TRUE)
                        ),
                       mainPanel(width = 9,
                                 useShinyjs(),
                                 div(
                                   id = "loading_page_predict_from_taxonomy",
                                   h4("Please upload a file at the left")
                                 ),
                                 hidden(
                                   div(
                                     id = "results_page_predict_from_taxonomy",
                                     fluidRow(
                                       box(
                                         title = textOutput("n_match_predict_from_taxonomy"), 
                                         downloadButton('download_data_predict_from_taxonomy', 'Download results') %>% withSpinner(color="#3C8DBC"),
                                         status = "primary", solidHeader = TRUE
                                       )
                                     ),
                                     fluidRow(
                                       box(
                                         title = "Overview", 
                                         column(width=4, plotOutput("plot_predict_from_taxonomy_1", height=250) %>% withSpinner(color="#3C8DBC")), 
                                         column(width=4, plotOutput("plot_predict_from_taxonomy_2", height=250) %>% withSpinner(color="#3C8DBC")),
                                         column(width=4, plotOutput("plot_predict_from_taxonomy_3", height=250) %>% withSpinner(color="#3C8DBC")),
                                            width=12, status = "primary", solidHeader = TRUE, collapsible = TRUE, style = 'overflow-x: scroll;'
                                       )
                                     ),
                                     fluidRow(
                                       box(
                                         title = "Detailed results",  dataTableOutput("table_predict_from_taxonomy") %>% withSpinner(color="#3C8DBC"),  width=12, status = "primary", solidHeader = TRUE, collapsible = TRUE
                                       )
                                     )
                                   )
                                 )
                            )
                       
                     ),
                     
                     #Predict from genome
                     tabPanel(
                         value="predict_from_genomes",
                         title="From genome",
                         fluidRow(
                           column(width=12, 
                                  h3("Predict traits from genome")),
                         ),
                         sidebarPanel(width = 3, 
                                      tabsetPanel(id = "subtabs",               
                                          tabPanel("Database",
                                                   pickerInput(inputId="organism", label = "Organism", choices=choices_organism, selected = "", multiple = FALSE, options = list(`actions-box` = TRUE)),
                                                   pickerInput(inputId="substrate_1", label = "Substrate", choices=choices_metabolites, selected = "D-Glucose", multiple = FALSE, options = list(`actions-box` = TRUE)),
                                                   pickerInput(inputId="products_1", label = "End products", choices=choices_metabolites, selected = c("Acetate", "(S)-Lactate", "(R)-Lactate", "Ethanol", "Succinate", "Propanoate", "Butanoic acid"), multiple = TRUE, options = list(`actions-box` = TRUE)),
                                                   pickerInput(inputId="unbalanced_intermediate_1", label = "Unbalanced intermediates", choices=choices_unbalanced_intermediates, selected = c("NAD+", "NADH", "NADP+", "NADPH", "FAD", "FADH2", "Reduced ferredoxin", "Oxidized ferredoxin", "Ubiquinone", "Ubiquinol", "Quinone", "Hydroquinone", "Menaquinone", "Menaquinol", "Oxidized hydrogenase", "Reduced hydrogenase",  "ATP", "ADP", "AMP", "GTP", "GDP", "Orthophosphate", "Diphosphate", "Polyphosphate", "H2O", "H+", "H+[side 1]", "H+[side 2]", "Na+[side 1]", "Na+[side 2]", "Sodium cation", "HCO3-"), multiple = TRUE, options = list(`actions-box` = TRUE))
                                         ),
                                         
                                          tabPanel("File upload",
                                                   fileInput_custom("file2", "Upload predicted gene functions",multiple = TRUE,accept = c("text/csv","text/comma-separated-values,text/plain",".csv"), downloadId="downloadFunctions", downloadlabel="Download example"),
                                                   fileInput_custom_2_files("file3", "Upload reference reactions",multiple = TRUE,accept = c("text/csv","text/comma-separated-values,text/plain",".csv"), downloadId1="downloadReference", downloadlabel1="Download example 1 (for glucose)", downloadId2="downloadReferenceFructose", downloadlabel2="Download example 2 (for fructose)"),
                                                   pickerInput(inputId="substrate_2", label = "Substrate", choices=choices_metabolites, selected = "D-Glucose", multiple = FALSE, options = list(`actions-box` = TRUE)),
                                                   pickerInput(inputId="products_2", label = "End products", choices=choices_metabolites, selected = c("Acetate", "(S)-Lactate", "(R)-Lactate", "Ethanol", "Succinate", "Propanoate", "Butanoic acid"), multiple = TRUE, options = list(`actions-box` = TRUE)),
                                                   pickerInput(inputId="unbalanced_intermediate_2", label = "Unbalanced intermediates", choices=choices_unbalanced_intermediates, selected = c("NAD+", "NADH", "NADP+", "NADPH", "FAD", "FADH2", "Reduced ferredoxin", "Oxidized ferredoxin", "Ubiquinone", "Ubiquinol", "Quinone", "Hydroquinone", "Menaquinone", "Menaquinol", "Oxidized hydrogenase", "Reduced hydrogenase",  "ATP", "ADP", "AMP", "GTP", "GDP", "Orthophosphate", "Diphosphate", "Polyphosphate", "H2O", "H+", "H+[side 1]", "H+[side 2]", "Na+[side 1]", "Na+[side 2]", "Sodium cation", "HCO3-"), multiple = TRUE, options = list(`actions-box` = TRUE))
                                          )
                                      )
                                   ),
                         mainPanel(width = 9,
                                     div(
                                       id = "results_page_predict_from_genomes",
                                       fluidRow(
                                           box(
                                             title = textOutput("text_predict_from_genomes"), 
                                             column(
                                              column(
                                                plotOutput("plot_predict_from_genomes_heatmap", height="100px", width="100%") %>% withSpinner(color="#3C8DBC"),
                                                width=12, align="left"
                                              ),
                                             width=12, align="left"),
                                             downloadButton('download_data_predict_from_genomes', 'Download results') %>% withSpinner(color="#3C8DBC"), 
                                             width=12, status = "primary", solidHeader = TRUE, collapsible = TRUE, style = 'overflow-x: scroll;'
                                           )
                                       ),
                                       fluidRow(
                                           box(
                                            title = "Metabolic model",
                                            
                                            selectInput(inputId="product_to_display", label = "End product", choices=choices_metabolites, selected=NULL, multiple = FALSE, selectize=TRUE, width="200px"),
                                            column(
                                              column(width=1),
                                              column(
                                                plotOutput("plot_predict_from_genomes_flux", hover=hoverOpts(id = "plot_hover", delayType = "throttle"))%>% withSpinner(color="#3C8DBC"),
                                                htmlOutput("plot_predict_from_genomes_tooltip"),
                                                width=11, height=400, align = "left"
                                              ),
                                              width=12, align="left"
                                            ),
                                            div("Hover over vertices for names of metabolites"),
                                            div("Positive fluxes are in green"),
                                            width=12, align = "left", status = "primary", solidHeader = TRUE, collapsible = TRUE, style = 'overflow-x: scroll;'
                                            )
                                       )
                                     )
                                   )
                          )
              ),
          
          #Help     
          tabPanel(
            value="help", 
            title=div(icon("question-circle"), "Help"),
            fluidRow(
              column(width=2),
              column(
                p(h3("Help")),
                p(""),
                p(""),
                p(h4("Tutorials")),
                p(""),
                p(h5("How to predict traits from taxonomy:")),                
                p(uiOutput("video_help_predict_from_taxonomy")),
                p(""),
                p(""),
                p(h5("How to predict traits from genomes:")),                
                p(uiOutput("video_help_predict_from_genomes")),
                p(""),
                p(""),
                p(h4("FAQ")),
                p(""),
                div(class="question", "When predicting traits from taxonomy, how do I get names of taxa?"),
                p(h5(tagList("There are no special rules.  If you did sequencing of 16S rRNA gene, you can use names from the output of ", url_QIIME, "."))),          
                p(""),
                div(class="question", "When predicting traits from taxonomy, how do I get predicted gene functions?"),
                p(h5(tagList("You can get these from the output of ", url_KAAS, "."))),          
                p(""),
                div(class="question", "Your metabolic model is for fermentation.  Can I build other types of metabolic models?"),
                p(h5("Yes, the sky is the limit. Go to Predict traits from genome -> File upload -> Upload reference reactions.")),          
                p(""),
                div(class="question", ""),
                p(h5("")),          
                width=8),
              column(width=2),
            )
          ),
        
          #About     
          tabPanel(
            value="about", 
            title=div(icon("circle-info"), "About"),
            fluidRow(
              column(width=2),
              column(
                p(h3("About")),
                p(""),
                p(""),
                p(h5("Development of this resource is described in a manuscript currently under review.  Citation to the manuscript will be provided here upon acceptance.")),
                p(""),
                p(h5(tagList("We acknowledge ",url_BacDive,", ",url_GOLD,", and ",url_IMG," for use of their data.  Data from ",url_BacDive," appear under the terms of a ", url_CC,".  Data from ", url_GOLD," and ",url_IMG," appear under the terms of ",url_JGI))),
                p(""),
                p(h5("This work was supported by an Agriculture and Food Research Initiative Competitive Grant [grant no. 2018-67015-27495] and Hatch Project [accession no. 1019985] from the United States Department of Agriculture National Institute of Food and Agriculture.")),
                p(""),
                width=8),
              column(width=2),
            )
          ),
          
          #Navigation bar options
          selected="home"
        
        )
      )
    ),
    
    #Footer
    tags$footer(
      div(class="footer", "This work is free to use under the terms of a license from", a(img(src="CC_logo.svg", width = 100), href="https://creativecommons.org/licenses/by-nc/4.0/"))
    )
  )


##############
#Define server
##############
server <- function(input, output, session) {
  
  #***********************************
  #Get user input and generate outputs
  #***********************************
  observeEvent(input$tabs, {
    #Home
    if(input$tabs == "home"){
      #Navigate to tab selected by navigation button
      observeEvent(input$jump_search_database, ignoreInit=TRUE, {
        updateNavbarPage(session, inputId="tabs", selected = "search_database")
      })
      observeEvent(input$jump_download_database, ignoreInit=TRUE, {
        updateNavbarPage(session, inputId="tabs", selected = "download_database")
      })
      observeEvent(input$jump_predict_from_taxonomy, ignoreInit=TRUE, {
        updateNavbarPage(session, inputId="tabs", selected = "predict_from_taxonomy")
      })
      observeEvent(input$jump_predict_from_genomes, ignoreInit=TRUE, {
        updateNavbarPage(session, inputId="tabs", selected = "predict_from_genomes")
      })
      observeEvent(input$jump_help, ignoreInit=TRUE, {
        updateNavbarPage(session, inputId="tabs", selected = "help")
      })
      observeEvent(input$jump_about, ignoreInit=TRUE, {
        updateNavbarPage(session, inputId="tabs", selected = "about")
      })
    }    
    
    #Search database
    if(input$tabs == "search_database"){
      
      #***********************
      #Get user input (events)
      #***********************
      #Get raw data
      data = raw_data

      #*************
      #Process input
      #*************
      #Clean data
      clean_data  = reactive({
        data = data
        
        #Convert to character
        data[] = lapply(data, as.character)

        #Clean up data from BacDive
        data$Cell_length = clean_BacDive_data(x=data$Cell_length, is_numeric=TRUE)
        data$Cell_shape = clean_BacDive_data(x=data$Cell_shape)
        data$Cell_width = clean_BacDive_data(x=data$Cell_width, is_numeric=TRUE)
        data$Colony_size = clean_BacDive_data(x=data$Colony_size, is_numeric=TRUE)
        data$Flagellum_arrangement = clean_BacDive_data(x=data$Flagellum_arrangement)
        data$Growth_temperature = clean_BacDive_data(x=data$Growth_temperature)
        data$Gram_stain = clean_BacDive_data(x=data$Gram_stain)
        data$Incubation_period = clean_BacDive_data(x=data$Incubation_period, is_numeric=TRUE)
        data$Indole_test = clean_BacDive_data(x=data$Indole_test)
        data$Isolation_source_category_1 = clean_BacDive_data(x=data$Isolation_source_category_1)
        data$Isolation_source_category_2 = clean_BacDive_data(x=data$Isolation_source_category_2)
        data$Isolation_source_category_3 = clean_BacDive_data(x=data$Isolation_source_category_3)
        data$Oxygen_tolerance = clean_BacDive_data(x=data$Oxygen_tolerance)
        data$pH_for_growth = clean_BacDive_data(x=data$pH_for_growth, is_numeric=TRUE)
        data$Spore_formation = clean_BacDive_data(x=data$Spore_formation)
        data$Salt_concentration_amount = clean_BacDive_data(x=data$Salt_concentration_amount, is_numeric=TRUE)
        data$Salt_concentration_unit = clean_BacDive_data(x=data$Salt_concentration_unit)
        
        #Rename variables to make units clearer
        data = data %>% dplyr::rename(Cell_length_in_microns = "Cell_length", Cell_width_in_microns = "Cell_width", Incubation_period_in_days = "Incubation_period")
        
        #Remove underscores in variable names
        colnames(data)=gsub(pattern="_", replacement=" ", x=colnames(data))
        
        #Remove quotation marks from Firmicutes B (affects sorting of values)
        data$Phylum = gsub(pattern=".Firmicutes B.", replacement="Firmicutes B", x=data$Phylum)
        
        #Remove underscores in phylum names
        data$Phylum = gsub(pattern="_", replacement=" ", x=data$Phylum)
        
        #Remove row index
        data = data %>% dplyr::select(-X)
        
        #Convert to character to factor (to put in alphabetical order)
        for(i in 1:ncol(data))
        {
          if(is.character(data[,i]))
          {
            data[,i]=as.factor(data[,i])
          }
        }
                
        return(data)
        
      })
      
      #Filter data according to query
      filter_data = reactive({
        data = clean_data()
        
        data = filterTable(input$querybuilder_out, data, 'table')
        
        #validate(need(data!=""&nrow(data)>0&!is.null(data), "Please build a valid query"))

        return(data)
      })
      
      #Get links to external websites
      get_links <- reactive({
        #Get table with matching organisms
        data = filter_data()
        
        #Add links
        data$`NCBI Taxonomy ID` <- sapply(X=data$`NCBI Taxonomy ID`, FUN=createLink, url="https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?mode=Info&id=")
        data$`GOLD Organism ID` <- sapply(X=data$`GOLD Organism ID`, FUN=createLink, url="https://gold.jgi.doe.gov/organism?id=")
        data$`GOLD Project ID` <- sapply(X=data$`GOLD Project ID`, FUN=createLink, url="https://gold.jgi.doe.gov/project?id=")
        data$`IMG Genome ID` <- sapply(X=data$`IMG Genome ID`, FUN=createLink, url="https://img.jgi.doe.gov/cgi-bin/m/main.cgi?section=TaxonDetail&page=taxonDetail&taxon_oid=")
        data$`IMG Genome ID max genes` <- sapply(X=data$`IMG Genome ID max genes`, FUN=createLink, url="https://img.jgi.doe.gov/cgi-bin/m/main.cgi?section=TaxonDetail&page=taxonDetail&taxon_oid=")
        data$`BacDive Organism ID` <- sapply(X=data$`BacDive Organism ID`, FUN=createLink, url="https://bacdive.dsmz.de/strain/")
        
        data$`Article link` <- sapply(X=data$`Article link`, FUN=createLinkButton)
        
        return(data)
      })
      
      #Get selected columns
      get_columns <- reactive({
        #Get table with matching organisms and links
        data = get_links()
        
        #Get names of columns selected via checkbox
        columns_info_organism = names(choices_info_organism[as.numeric(input$checkboxes_info_organism)])
        columns_info_fermentation = names(choices_info_fermentation[as.numeric(input$checkboxes_info_fermentation)])
        columns_info_JGI = names(choices_info_JGI[as.numeric(input$checkboxes_info_JGI)])
        columns_info_NCBI = names(choices_info_NCBI[as.numeric(input$checkboxes_info_NCBI)])
        columns_info_BacDive = names(choices_info_BacDive[as.numeric(input$checkboxes_info_BacDive)])

        #Keep only selected columns
        columns=c(columns_info_organism, columns_info_fermentation, columns_info_JGI,columns_info_NCBI, columns_info_BacDive)
        data = data %>% dplyr::select(all_of(columns))

        return(data)
      })
    
      #****************
      #Generate outputs
      #****************
      #Output widget for query builder
      output$querybuilder <- renderQueryBuilder({
        queryBuilder(
          data = clean_data(), 
          filters = list(
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
          list(name = 'Salt concentration unit',  type = 'string', input = 'selectize')
        ),
        autoassign = FALSE,
        default_condition = 'AND',
        allow_empty = FALSE,
        display_errors = FALSE,
        display_empty_filter = FALSE
        )
      })
      
      #Output number of matching organisms
      output$n_match_database_taxa=renderText(paste0("Query matched ", nrow(filter_data()), " organisms"))
      
      #Output table with matching organisms and columns
      output$table_search_database <- renderDataTable({
        get_columns()
        }, escape = FALSE, options = list(scrollX = TRUE))
      
      #Output downloadable csv with matching results
      output$download_data_search_database <- downloadHandler(
        filename = function() {
          paste("results", "csv", sep = ".")
        },
        content = function(file) {
          sep <- switch("csv", "csv" = ",", "tsv" = "\t")
          
          # Write to a file specified by the 'file' argument
          write.table(filter_data(), file, sep = sep,
                      row.names = FALSE)
        }
      )   
      
      hide("loading_page_database_taxa")
      show("results_page_database_taxa")
      
    }
    
    #Predict from taxonomy
    if(input$tabs == "predict_from_taxonomy"){
      #***********************
      #Get user input (events)
      #***********************
      #Read in taxonomy of query organisms
      get_query_taxonomy <- reactive({
        req(input$file1)
        query_taxonomy = read.csv(input$file1$datapath)
        return(query_taxonomy)
      })
      
      #Output downloadable csv with example taxonomy
      output$downloadTaxa <- downloadHandler(
        filename = function() {
          paste("query", "csv", sep = ".")
        },
        content = function(file) {
          table=example_query
          write.csv(table, file, row.names = FALSE)
        }
      )   
      
      #Get raw data and filter
      get_data = eventReactive({input$predict_from_taxonomy_filter},
        {
          #Get data
          data = raw_data
          
          #Filter according to preferred taxonomy
          data = filter_by_taxonomy(data=data, input_taxonomy=input$predict_from_taxonomy_filter)
          
          return(data)
        },
        ignoreNULL = FALSE,  ignoreInit=FALSE)
      
      #*************
      #Process input
      #*************
      #Get predictions
      predict_traits <- reactive({
        #Get data
        data = get_data()
        
        #Load taxonomy of query organisms
        query_taxonomy = get_query_taxonomy()
        
        #Set threshhold for making predictions (fraction of species in database that must share a given trait)  
        prediction_threshold=input$threshold
        
        #Predict traits for organisms in query
        matching_traits_all_queries=data.frame("Phylum"=character(), "Class"=character(), "Order"=character(), "Family"=character(), "Genus"=character(), "Species"=character(), 'Fermentative ability'=character(), "Substrates_for_end_products"=character(), 'End products'=character())
        for(i in 1:nrow(query_taxonomy))
        {
          #Get organisms in database with matching taxonomy (NA ignored)
          matching_organisms = data
          for(j in 1:6)
          {
            var=colnames(matching_traits_all_queries)[j]

            if(!is.na(query_taxonomy[i,][[sym(var)]]))
            {
              matching_organisms = matching_organisms %>% filter(!!sym(var)==query_taxonomy[i,][[sym(var)]])
            }
          }
          
          #Get traits of matching organisms
          matching_traits_all = matching_organisms %>% dplyr::select(Fermentative_ability, Substrates_for_end_products, Major_end_products, Minor_end_products)
          if(is.null(matching_traits_all))
          {
            matching_traits_all=NA
          }else if(nrow(matching_organisms)==0)
          {
            matching_traits_all=NA
          }else
          {
            #Separate end products into multiple columns
            #Products observed are given value of 1
            #Products not observed are given value of 0
            matching_traits_all$End_products = paste(matching_traits_all$Major_end_products,matching_traits_all$Minor_end_products, sep=";")
            matching_traits_all = matching_traits_all %>% dplyr::select(-c(Major_end_products, Minor_end_products))
            matching_traits_all$rn = 1:nrow(matching_traits_all)
            df = matching_traits_all
            df = df %>% separate_rows(End_products, sep = ";", convert = TRUE)
            df= df %>% filter(End_products!="") %>% filter(!is.na(End_products))
            df= df %>% distinct()
            df$values = 1
            df = df %>% pivot_wider(names_from = End_products, values_from = values, values_fill = 0)
            
            join_1 = df %>% dplyr::select(rn:colnames(df[ncol(df)]))
            matching_traits_all = matching_traits_all %>% full_join(y = join_1, by = "rn")
            matching_traits_all = matching_traits_all %>% dplyr::select(-End_products, -rn)
            
            #Set missing values to "NULL"
            for(j in 2:ncol(matching_traits_all))
            {
              matching_traits_all[,j] = as.character(matching_traits_all[,j])
              matching_traits_all[,j] = if_else(matching_traits_all$Fermentative_ability=="+",if_else(is.na(matching_traits_all[,j]), "NULL", matching_traits_all[,j]), matching_traits_all[,j])
            }
            
            #Get only traits that are shared by a threshold of organisms in the database
			#Exclude NULL values					
            matching_traits_above_threshold = data.frame(matrix(ncol = ncol(matching_traits_all), nrow = 0))
            colnames(matching_traits_above_threshold) = colnames(matching_traits_all)                        
            if(nrow(matching_organisms)>0)
            {
              for(j in 1:length(matching_traits_above_threshold))
              {
                table = table(matching_traits_all[,j], exclude="NULL")
                
                if((max(table)/sum(table))>=prediction_threshold)
                {
                  matching_traits_above_threshold[1,j]=attributes(table[which(table==max(table))[1]])$names[1]
                }else
                {
                  matching_traits_above_threshold[1,j]=NA
                }
              }
            }
            
            #Put end products back into a single column
            matching_traits_above_threshold[which(matching_traits_above_threshold==1)]=colnames(matching_traits_above_threshold)[which(matching_traits_above_threshold ==1)]
            matching_traits_above_threshold[which(matching_traits_above_threshold==0)]=""
            matching_traits_above_threshold = matching_traits_above_threshold %>% unite(col="End_products", c(-1,-2), sep = ";", remove = TRUE, na.rm = TRUE)
            matching_traits_above_threshold$End_products = gsub(pattern="(;)+", replacement=";", x=matching_traits_above_threshold$End_products)
            matching_traits_above_threshold$End_products = gsub(pattern="^;", replacement="", x=matching_traits_above_threshold$End_products)
            matching_traits_above_threshold$End_products = gsub(pattern=";$", replacement="", x=matching_traits_above_threshold$End_products)
            matching_traits_above_threshold$End_products[which(matching_traits_above_threshold$End_products=="")] = NA 
            
			#Add taxonomy of query organism
            for(j in 1:6)
            {
              var=colnames(matching_traits_all_queries)[j]
              matching_traits_above_threshold[[sym(var)]]=as.character(query_taxonomy[i,][j])
            }
            matching_traits_above_threshold = matching_traits_above_threshold %>% relocate("Phylum", "Class", "Order", "Family", "Genus", "Species")
            
            #Remove rows with "NA" or NA for all traits
            matching_traits_above_threshold$Fermentative_ability[which(matching_traits_above_threshold$Fermentative_ability=="NA")]=NA
            matching_traits_above_threshold$Substrates_for_end_products[which(matching_traits_above_threshold$Substrates_for_end_products=="NA")]=NA
            matching_traits_above_threshold$End_products[which(matching_traits_above_threshold$End_products=="NA")]=NA
            matching_traits_above_threshold = matching_traits_above_threshold %>% dplyr::filter(!is.na(Fermentative_ability)|!is.na(Substrates_for_end_products)|!is.na(End_products))
            
            if(nrow(matching_traits_above_threshold)>0)
            {
              #Add to dataframe with predictions for other organisms
              matching_traits_all_queries=rbind(matching_traits_all_queries, matching_traits_above_threshold)
            }
          } 
        }
        
        return(matching_traits_all_queries)
        
      })

      #****************
      #Generate outputs
      #****************
      #Output number of matching organisms and traits
      output$n_match_predict_from_taxonomy=renderText(paste0("Traits predicted for ", nrow(predict_traits()), " out of ", nrow(get_query_taxonomy()), " query taxa"))
      
      #Output downloadable csv with matching results
      output$download_data_predict_from_taxonomy <- downloadHandler(
        filename = function() {
          paste("results", "csv", sep = ".")
        },
        content = function(file) {
          sep <- switch("csv", "csv" = ",", "tsv" = "\t")
          
          # Write to a file specified by the 'file' argument
          write.table(predict_traits(), file, sep = sep,
                      row.names = FALSE)
        }
      ) 
      
      #Output table with predicted traits
      output$table_predict_from_taxonomy <- renderDataTable({
        predict_traits() %>% replace(is.na(.), "NA")
        }, 
        escape = FALSE, options = list(scrollX = TRUE)) 
      
      #Output plots
      output$plot_predict_from_taxonomy_1 = renderPlot(exp={
          plot_treemap(data=predict_traits(), var="Fermentative_ability")

      }, width=225, height=250)

      output$plot_predict_from_taxonomy_2 = renderPlot(exp={
        plot_treemap(data=predict_traits(), var="Substrates_for_end_products")
      }, width=225, height=250)
      
      output$plot_predict_from_taxonomy_3 = renderPlot(exp={
        plot_treemap(data=predict_traits(), var="End_products", sep=";")
      }, width=225, height=250)
      
      #Hide loading page and show results page
      observe({
        if(!is.null(input$file1$datapath))
        {
          is.null(input$file1$datapath)
          hide("loading_page_predict_from_taxonomy")
          show("results_page_predict_from_taxonomy")
        }
      })
      
    }  
    
    #Predict from genome
    if(input$tabs == "predict_from_genomes"){ 
      #***********************
      #Get user input (events)
      #***********************
      #Read in data files
      get_database_file <- reactive({
        req(input$file2)
        database_file = read.csv(input$file2$datapath)
        return(database_file)
      })
      
      get_reactions_file <- reactive({
        req(input$file3)
        reactions_file = read.csv(input$file3$datapath)
        return(reactions_file)
      })
      
      #Get reference reactions
      get_reference_reactions <- reactive({
        if(input$subtabs=="Database")
        {
          input_reference_reactions = reference_reactions
        }else{
          input_reference_reactions = get_reactions_file()
        }
        
        validate(need(input_reference_reactions!="", "Please upload a file or choose an organism from the database"))
        
        return(input_reference_reactions)
      })
      
      #Get database IDs
      get_database_ID <- reactive({
        if(input$subtabs=="Database")
        {
          choices_match = which(choices_organism==input$organism)
          organism_match = which(raw_data$IMG_Genome_ID_max_genes %in% gene_functions$Genome_ID)[choices_match]
          ID_match = raw_data$IMG_Genome_ID_max_genes[organism_match]
          
          database_ID = gene_functions$geneAssociation[which(gene_functions$Genome_ID==ID_match)]
        }else{
          database_ID = get_database_file()[,1]
        }
        
        validate(need(database_ID!="", "Please upload a file or choose an organism from the database"))
        
        return(database_ID)  
      })
      
      #Get substrate
      get_input_substrate <- reactive({
        if(input$subtabs=="Database")
        {
          substrate = input$substrate_1
          
        }else{
          
          substrate = input$substrate_2
        }
        
        validate(need(substrate!="", "Please choose a substrate"))
        
        return(substrate)  
      })
      
      #Get products
      get_input_products <- reactive({
        if(input$subtabs=="Database")
        {
          products = input$products_1
        }else{
          products = input$products_2
        }
        
        validate(need(products!="", "Please choose one or more products"))
        
         return(products)  
      })
        
      #Get unbalanced intermediates
      get_unbalanced_intermediate <- reactive({
        if(input$subtabs=="Database")
        {
          unbalanced_intermediate = input$unbalanced_intermediate_1
        }else{
          unbalanced_intermediate = input$unbalanced_intermediate_2
        }
        
        return(unbalanced_intermediate)  
      })
      
      #Get selected products
      observe({
        x = get_input_products()
        
        # Can use character(0) to remove all choices
        if (is.null(x))
          x <- character(0)
        
        # Can also set the label and select items
        updateSelectInput(session, 
                          inputId="product_to_display",
                          choices = x,
                          selected = head(x, 1)
        )
      
      })
      
      #*************
      #Process input
      #*************
      #Build reference model
      build_reference_model <- reactive({
          
          reference_reactions=get_reference_reactions()
        
          abbreviation=reference_reactions$abbreviation
          equation=reference_reactions$equation
          direction=reference_reactions$direction
          officialName=reference_reactions$officialName
          geneAssociation=reference_reactions$geneAssociation
          subsystem=reference_reactions$subsystem
          unbalanced_intermediate = get_unbalanced_intermediate()
          unbalanced_product = get_input_products()[get_input_products() %nin% get_unbalanced_intermediate()]
            
          #Build Metabolic Model
          reference_model = build_model(equation=equation, direction=direction, abbreviation=abbreviation, officialName=officialName, geneAssociation=geneAssociation, subsystem=subsystem, starting_metabolite=NULL, ending_metabolite=NULL, unbalanced_intermediate=unbalanced_intermediate, unbalanced_product=unbalanced_product, remove_redundant_reactions = FALSE)
          
        return(reference_model)
      })
      
      #Build reference mode for graph layout
      build_reference_model_no_unbalanced_products <- reactive({
   
          reference_reactions=get_reference_reactions()
        
          abbreviation=reference_reactions$abbreviation
          equation=reference_reactions$equation
          direction=reference_reactions$direction
          officialName=reference_reactions$officialName
          geneAssociation=reference_reactions$geneAssociation
          subsystem=reference_reactions$subsystem
          unbalanced_intermediate = get_unbalanced_intermediate()
          
          #Build Metabolic Model
          reference_model_no_unbalanced_products = build_model(equation=equation, direction=direction, abbreviation=abbreviation, officialName=officialName, geneAssociation=geneAssociation, subsystem=subsystem, starting_metabolite=NULL, ending_metabolite=NULL, unbalanced_intermediate=unbalanced_intermediate, unbalanced_product=NULL, remove_redundant_reactions = FALSE)
        
        return(reference_model_no_unbalanced_products)
      })
      
      #Get graph layout for reference model
      get_layout <- reactive({
        
        unbalanced_intermediate = get_unbalanced_intermediate()
        reference_model_no_unbalanced_products = build_reference_model_no_unbalanced_products()
        
        g = graph_enzymes(s = reference_model_no_unbalanced_products, name=unbalanced_intermediate)
        layout = layout_with_fr(graph=g)
        
        return(layout)
      })
      
      
      #Rescale layout
      get_layout_rescale <- reactive({
        
        layout_rescale = data.frame(x=rescale(get_layout()[,1]), y=rescale(get_layout()[,2]))
        layout_rescale$rn = 1:nrow(layout_rescale)
        
        return(layout_rescale)
      })
      
      #Build and solve organism-specific model
      solve_models <- reactive({
        #Get inputs
        reference_model = build_reference_model()
        database_ID = get_database_ID()
        products = get_input_products()
        substrate = get_input_substrate()

        #Set variables
        if(length(products)>0)
        {
          products = products
        }else{
          products="None"
        }
        ending_metabolite = products
        starting_metabolite = rep(substrate, times=length(ending_metabolite))
        
        #Initialize values
        s = vector("list", length(ending_metabolite))
        
        #Find Enzymes
        officialName = find_enzymes(df = reference_model, database_ID = database_ID)
        
        #Simplify Model
        df = simplify_model(df = reference_model, officialName=officialName)
        
        for(i in 1:length(ending_metabolite))
        {
          #Add Target Metabolites
          s[[i]] = add_target_metabolites(df = df, starting_metabolite=format_name(starting_metabolite[i]), ending_metabolite=format_name(ending_metabolite[i]), lowbnd=-1000, uppbnd=10^6)
          
          #Find fluxes
          s[[i]] = find_fluxes_df(s[[i]])
        }
        
        return(s)
      })
      
      #Get fluxes from models
      get_fluxes <- reactive({
        #Get inputs
        s = solve_models()
        substrate = get_input_substrate()
        products = get_input_products()

        #Set variables
        if(length(products)>0)
        {
          products = products
        }else{
          products="None"
        }
        ending_metabolite = products
        
        #Get fluxes
        flux = data.frame(matrix(NA, nrow = 1, ncol = length(ending_metabolite)))
        colnames(flux) = c(ending_metabolite)
        
        for(i in 1:length(ending_metabolite))
        {
          flux[i] = s[[i]]$flux[which(s[[i]]$abbreviation=="Ending_metabolite")]
        }
        
        return(flux)
      })
      
      #Get endproducts from fluxes
      get_endproducts <- reactive({
        flux = get_fluxes()
        
        min_flux = 1
        
        flux = flux %>% pivot_longer(cols=colnames(flux), names_to="Product", values_to="Flux")
        flux$Flux = as.numeric(flux$Flux)
        flux = flux %>% filter(Flux>min_flux)

        endproducts = unique(flux$Product)
        
        return(endproducts)
      })

      #Make plot of models
      get_plot <- reactive({
        unbalanced_intermediate = get_unbalanced_intermediate()
        
        #Get index of selected products
        i = which(get_input_products()==input$product_to_display)
        s = solve_models()[[i]]
        
        #Change fluxes to 0 if product has flux less than min_flux
        min_flux = 1
        if(s$flux[which(s$abbreviation=="Ending_metabolite")]<min_flux)
        {
          s$flux = 0
        }
        
        #Draw Enzymatic Reactions in Graph
        g = graph_enzymes(s = s, name=unbalanced_intermediate, show_flux=TRUE)
        
        #Set color of substrate and product
        V(g)$color[which(V(g)$name==format_name(get_input_products()[i]))] = "red"
        V(g)$color[which(V(g)$name==format_name(get_input_substrate()))] = "red"
                
        return(g)
      })
      
      
      #Find vertex that cursor hovers over
      hover_match = reactive({
        
        #Get x-y coordinates of vertices
        layout_rescale = get_layout_rescale()
        
        #Get x-y coordinates of cursor
        hover_x = input$plot_hover$x
        hover_y = input$plot_hover$y
        
        if(!is.null(input$plot_hover$x)|!is.null(input$plot_hover$y))
        {
          hover_x = as.numeric(hover_x)
          hover_y = as.numeric(hover_y)
          
          #Find vertex that most closely matches cursor position
          tolerance = 0.1
          
          hover_match = layout_rescale
          hover_match$x =  abs(hover_match$x-hover_x)
          hover_match$y =  abs(hover_match$y-hover_y)
          hover_match$sum = hover_match$x + hover_match$y
          hover_match = hover_match %>% filter(sum < tolerance)
          if(nrow(hover_match)>0)
          {
            hover_match = hover_match$rn[which(hover_match$sum==min(hover_match$sum))]
          }else
          {
            hover_match = NA
          }
        }else
        {
          hover_match = NA
        }
        
        return(hover_match)
      })
      
      
      #Get name of vertex that cursor hovers over
      hover_name = reactive({
        
        if(!is.na(hover_match()))
        {   
          hover_name = V(get_plot())$name[hover_match()]
        }else
        {
          hover_name = NA
        }
        
        return(hover_name)
        
      })

      #****************
      #Update selections
      #****************
      #Update choices for substrates
      observe({
        x = get_metabolite_names(equation = get_reference_reactions()$equation)  
        
        if (is.null(x))
          x <- character(0)
        
        updatePickerInput(session, 
                          inputId="substrate_2",
                          choices = x,
                          selected = "D-Glucose"
        )
        
      })
      
      #Update choices for products
      observe({
        to_remove = to_remove 
        x = get_metabolite_names(equation = get_reference_reactions()$equation, to_remove = to_remove)  

        if (is.null(x))
          x <- character(0)
        
        updatePickerInput(session, 
                          inputId="products_2",
                          choices = x,
                          selected = c("Acetate", "(S)-Lactate", "(R)-Lactate", "Ethanol", "Succinate", "Propanoate", "Butanoic acid", "Formate", "Hydrogen", "CO2")
        )
        
      })
      
      #Update choices for unbalanced intermediates
      observe({
            x = get_metabolite_names(equation = get_reference_reactions()$equation)  
        
        if (is.null(x))
          x <- character(0)
        
        updatePickerInput(session, 
                          inputId="unbalanced_intermediate_2",
                          choices = x,
                          selected = c("NAD+", "NADH", "NADP+", "NADPH", "FAD", "FADH2", "Reduced ferredoxin", "Oxidized ferredoxin", "Ubiquinone", "Ubiquinol", "Quinone", "Hydroquinone", "Menaquinone", "Menaquinol", "Oxidized hydrogenase", "Reduced hydrogenase",  "ATP", "ADP", "AMP", "GTP", "GDP", "Orthophosphate", "Diphosphate", "Polyphosphate", "H2O", "H+", "H+[side 1]", "H+[side 2]", "Na+[side 1]", "Na+[side 2]", "Sodium cation", "Formate", "Hydrogen","CO2","HCO3-")
        )
        
      })
      
      #****************
      #Generate outputs
      #****************
      #Output downloadable csv with example gene functions
      output$downloadFunctions <- downloadHandler(
        filename = function() {
          paste("e_coli", "csv", sep = ".")
        },
        content = function(file) {
          table=e_coli
          write.csv(table, file, row.names = FALSE)
        }
      )  
      
      #Output downloadable csv with example reference model
      output$downloadReference <- downloadHandler(
        filename = function() {
          paste("reference_reactions", "csv", sep = ".")
        },
        content = function(file) {
          table=reference_reactions
          write.csv(table, file, row.names = FALSE)
        }
      ) 
      
      #Output downloadable csv with example reference model
      output$downloadReferenceFructose <- downloadHandler(
        filename = function() {
          paste("reference_reactions_with_fructose", "csv", sep = ".")
        },
        content = function(file) {
          table=reference_reactions_with_fructose
          write.csv(table, file, row.names = FALSE)
        }
      ) 
      
      #Output summary text
      output$text_predict_from_genomes=renderText(paste0(length(get_endproducts()), " end products predicted"))
      
      #Output downloadable csv of results
      output$download_data_predict_from_genomes <- downloadHandler(
        filename = function() {
          paste("fluxes", "csv", sep = ".")
        },
        content = function(file) {
          sep <- switch("csv", "csv" = ",", "tsv" = "\t")
          
          # Write to a file specified by the 'file' argument
          write.table(get_fluxes(), file, sep = sep,
                      row.names = FALSE)
        }
      ) 

      #Output heat map
      observe({
        output$plot_predict_from_genomes_heatmap = renderPlot(exp={
          plot_heatmap(data=get_fluxes())
        }, width=75*length(get_input_products()))
      })

      
      #Output flux graphs
      output$plot_predict_from_genomes_flux <- renderPlot(exp={
        #Get layout
        layout = get_layout()
        g = get_plot()
        
        #Set vertex size
        i = which(get_input_products()==input$product_to_display)
        vertex.size = rep(5, times=length(V(get_plot())))
        vertex.size[which(V(g)$name==format_name(get_input_products()[i]))] = 10
        vertex.size[which(V(g)$name==format_name(get_input_substrate()))] = 10
        
        #Set margin
        par(mar=c(0,0,0,0)+.1)
        
        #Make plot
        plot(get_plot(), layout=layout, vertex.size=vertex.size, vertex.label=NA)
      }, width=400, height=400)
      
      #Output tooltip
      output$plot_predict_from_genomes_tooltip <- renderText(
        if(!is.na(hover_name()))
        {
          renderTags(
            tags$div(
              hover_name(),
              style = paste0(
                "position: absolute; ","top: ", input$plot_hover$coords_css$y + 10, "px; ","left: ", input$plot_hover$coords_css$x - 10, "px; ","background: gray; ","padding: 3px; ","color: white; "
              )
            )
          )$html
        }else
        {
          renderTags(
            tags$div()
          )$html
        }
      )
      
      output$text1 = renderText(
        "Text"
      )
      
      #Output table with predicted traits
      output$table_predict_from_genomes <- renderDataTable({
        get_fluxes()
      }, 
      escape = FALSE, options = list(scrollX = TRUE)) 
      
      
      #Hide loading page and show results page
          hide("loading_page_predict_from_genomes")
          show("results_page_predict_from_genomes")
    }  
    
    #Download database
    if(input$tabs == "download_database"){
      
      #Get data
      data = raw_data
      
      #Output downloadable csv of full database
      output$download_database_full <- downloadHandler(
        filename = function() {
          paste("database", "csv", sep = ".")
        },
        content = function(file) {
          sep <- switch("csv", "csv" = ",", "tsv" = "\t")
          
          # Write to a file specified by the 'file' argument
          write.table(data, file, sep = sep,
                      row.names = FALSE)
        }
      ) 
      
    } 
    
    #About
    if(input$tabs == "about"){
    } 
    
    #Help
    if(input$tabs == "help"){
      
    output$video_help_predict_from_taxonomy <- renderUI({
        HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/Lhlk-4vRmL4" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
    })
    
    output$video_help_predict_from_genomes <- renderUI({
      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/MOubZwqIW4I" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
    })
      
    }
    
  })
}

########
#Run app
########
shinyApp(ui = ui, server = server)