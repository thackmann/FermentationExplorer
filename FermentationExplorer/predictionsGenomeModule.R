#################
#Define functions
#################
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
  
  #Set title
  title=paste(var)
  if(remove_underscore=="TRUE")
  {
    title = gsub(pattern="_", replacement=" ", x=title)
    title = paste(title)
  }
  
  #Format data and plot tree
  if(all(is.na(data[[sym(var)]])))
  {
    data = setNames(list("No predictions", 1), c(var, "n"))
    data = data.frame(data)
    treemap(dtf=data, index=colnames(data[1]), vSize="n", type="index", title=title, palette = c("#bebebe"))
  }else{
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
    treemap(dtf=data, index=colnames(data[1]), vSize="n", type="index", title=title)
  }
  
  return(p)
}

#Plot Heatmap of Fermentation Products
plot_heatmap = function(data, max_char=13, show_legend=FALSE)
{
  min_flux = 1
  f = function(x) if_else(x>min_flux, 100, 0)
  
  #Format data
  df = data
  df =  df %>% mutate_all(.funs=f)
  df = df %>% pivot_longer(cols=colnames(df), names_to="End_products", values_to="value")
  df = df %>% group_by(End_products) %>% summarize(value = mean(value))
  
  df$sign =  df$value
  df$sign = if_else(df$value>0,"(+)", "(-)")
  
  to_truncate=which(nchar(df$End_products)>max_char)
  df$End_products[to_truncate]=substr(x=df$End_products[to_truncate], start=0, stop=(max_char-3))
  df$End_products[to_truncate]=paste0(df$End_products[to_truncate],"...")
  
  #Construct plot
  #Call ggplot2
  p = ggplot()+
    
    #Plot heat map
    geom_tile(data = df, aes(x=End_products, y=1, fill=value), size=1, color="black") +
    geom_label(data = df, aes(x=End_products, y=1, label = sign), color="white", size=6, label.size = NA, fill = NA, fontface = "bold")+
    
    #Set colors
    scale_fill_gradient(limits = c(0, 100), low="grey", high=green_color) +
    
    #Set theme
    theme(axis.title.y=element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5, colour="black", size=12),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
    )
  
  if(show_legend==FALSE)
  {
    p = p + theme(legend.position = "none")
  }else
  {
    p = p + theme(legend.position = "bottom") + 
      guides(fill=guide_legend(title="% organisms positive"))
  }
  
  return(p)
}

#Detect if any value in the column matches the pattern
detect_pattern_column <- function(data, pattern) {
  for (col_name in colnames(data)) {
    if (any(grepl(pattern, data[[col_name]], perl = TRUE))) {
      return(col_name)
    }
  }
  return(NA)
}  

#Detect delimiter and header presence
detect_delim_and_header <- function(filepath) {
  # Read first few lines
  lines <- readLines(filepath, n = 10)
  
  # Detect delimiter
  delim <- ifelse(lengths(strsplit(lines, ",")) >= lengths(strsplit(lines, "\t")), ",", "\t")
  delim = as.character(names(table(delim))[which.max(table(delim))])
  
  # Detect presence of a header
  pattern <- "^K[0-9]{5}$"
  has_header <- TRUE
  first_line <- strsplit(lines[1], delim)[[1]]
  
  for (value in first_line) {
    if (str_detect(value, pattern) || value == "") {
      has_header <- FALSE
      break
    }
  }
  
  return(list(delim = delim, has_header = has_header))
}

#Read the file with appropriate delimiter and header settings
read_custom_file <- function(filepath) {
  info <- detect_delim_and_header(filepath)
  delim <- info$delim
  has_header <- info$has_header
  
  data <- read.table(filepath, sep = delim, header = has_header, stringsAsFactors = FALSE, fill=TRUE)
  return(data)
}

##############
#Set variables
##############
#Choices for metabolites for metabolic models
choices_unbalanced_intermediates = get_metabolite_names(equation = reference_reactions$equation)  
to_remove = c("NAD+", "NADH", "NADP+", "NADPH", "FAD+", "FADH2", "Reduced ferredoxin", "Oxidized ferredoxin", "Ubiquinone", "Ubiquinol", "Quinone", "Hydroquinone", "Menaquinone", "Menaquinol", "Oxidized hydrogenase", "Reduced hydrogenase",  "ATP", "ADP", "AMP", "GTP", "GDP", "Orthophosphate", "Diphosphate", "Polyphosphate", "H2O", "H+", "H+[side 1]", "H+[side 2]", "Na+[side 1]", "Na+[side 2]", "Sodium cation", "HCO3-")
choices_metabolites = c(get_metabolite_names(equation = reference_reactions$equation, to_remove = to_remove)) 

#Choices for organisms for metabolic models 
row_match = which(raw_data$IMG_Genome_ID_max_genes %in% gene_functions$Genome_ID)
choices_organism = paste(raw_data$Genus[row_match], raw_data$Species[row_match], raw_data$Subspecies[row_match], sep=" ")
choices_organism = gsub(pattern=" NA$", replacement="", x=choices_organism)

###########################
#Define user interface (UI)
###########################
predictionsGenomeUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width=12,
             h3("Predict traits from genome")),
    ),
    sidebarPanel(width = 3,
                 tabsetPanel(id = ns("subtabs"),
                             tabPanel(title = "Database",
                                      pickerInput(inputId=ns("organism"), label = "Organism", choices=choices_organism, selected = NULL, multiple = FALSE, options = list(`actions-box` = TRUE)),
                                      pickerInput(inputId=ns("substrate_1"), label = "Substrate", choices=choices_metabolites, selected = "D-Glucose", multiple = FALSE, options = list(`actions-box` = TRUE)),
                                      pickerInput(inputId=ns("products_1"), label = "End products", choices=choices_metabolites, selected = c("Acetate", "(S)-Lactate", "(R)-Lactate", "Ethanol", "Succinate", "Propanoate", "Butanoic acid", "Formate", "Hydrogen", "CO2"), multiple = TRUE, options = list(`actions-box` = TRUE)),
                                      pickerInput(inputId=ns("unbalanced_intermediate_1"), label = "Unbalanced intermediates", choices=choices_unbalanced_intermediates, selected = c("NAD+", "NADH", "NADP+", "NADPH", "FAD", "FADH2", "Reduced ferredoxin", "Oxidized ferredoxin", "Ubiquinone", "Ubiquinol", "Quinone", "Hydroquinone", "Menaquinone", "Menaquinol", "Oxidized hydrogenase", "Reduced hydrogenase",  "ATP", "ADP", "AMP", "GTP", "GDP", "Orthophosphate", "Diphosphate", "Polyphosphate", "H2O", "H+", "H+[side 1]", "H+[side 2]", "Na+[side 1]", "Na+[side 2]", "Sodium cation", "HCO3-"), multiple = TRUE, options = list(`actions-box` = TRUE)),
                                      actionButton(ns("make_predictions_1"), "Make predictions", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                             ),
                             tabPanel(title = "File upload",
                                      fileInput_modal(ns("file_gene_functions"), "Upload predicted gene functions",multiple = TRUE, accept = c("text/csv","text/comma-separated-values,text/plain",".csv"), modalId = ns("genome_file_functions_modal"), modalLabel = "Download example"),
                                      fileInput_modal(ns("file_reference_reactions"), "Upload reference reactions",multiple = TRUE, accept = c("text/csv","text/comma-separated-values,text/plain",".csv"), modalId = ns("genome_file_reference_modal"), modalLabel = "Download example"),
                                      pickerInput(inputId=ns("substrate_2"), label = "Substrate", choices=choices_metabolites, selected = "D-Glucose", multiple = FALSE, options = list(`actions-box` = TRUE)),
                                      pickerInput(inputId=ns("products_2"), label = "End products", choices=choices_metabolites, selected = c("Acetate", "(S)-Lactate", "(R)-Lactate", "Ethanol", "Succinate", "Propanoate", "Butanoic acid", "Formate", "Hydrogen", "CO2"), multiple = TRUE, options = list(`actions-box` = TRUE)),
                                      pickerInput(inputId=ns("unbalanced_intermediate_2"), label = "Unbalanced intermediates", choices=choices_unbalanced_intermediates, selected = c("NAD+", "NADH", "NADP+", "NADPH", "FAD", "FADH2", "Reduced ferredoxin", "Oxidized ferredoxin", "Ubiquinone", "Ubiquinol", "Quinone", "Hydroquinone", "Menaquinone", "Menaquinol", "Oxidized hydrogenase", "Reduced hydrogenase",  "ATP", "ADP", "AMP", "GTP", "GDP", "Orthophosphate", "Diphosphate", "Polyphosphate", "H2O", "H+", "H+[side 1]", "H+[side 2]", "Na+[side 1]", "Na+[side 2]", "Sodium cation", "HCO3-"), multiple = TRUE, options = list(`actions-box` = TRUE)),
                                      actionButton(ns("make_predictions_2"), "Make predictions", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                             )
                 )
    ),
    mainPanel(width = 9,
              div(
                id = ns("results_page"),
                conditionalPanel(
                  condition = "input.subtabs == 'Database' & input.make_predictions_1 == 0",
                  ns = ns,
                  h4("Please make selections at left")
                ),
                conditionalPanel(
                  condition = "input.subtabs == 'File upload' & (input.make_predictions_2 == 0 | output.check_file_genome)",
                  ns = ns,
                  h4("Please upload files and make selections at left")
                ),
                conditionalPanel(
                  condition = "(input.subtabs == 'Database' & input.make_predictions_1 > 0)|(input.subtabs == 'File upload' & input.make_predictions_2 > 0 & !output.check_file_genome)",
                  ns = ns,
                  fluidRow(
                    box(
                      title = textOutput(ns("text")),
                      column(
                        column(
                          plotOutput(ns("plot_heatmap"), height="100%", width="100%") %>% withSpinner(color="#3C8DBC"),
                          width=12, align="left"
                        ),
                        width=12, align="left"),
                      downloadButton(ns('download_data'), 'Download fluxes') %>% withSpinner(color="#3C8DBC"),
                      width=12, status = "primary", solidHeader = TRUE, collapsible = TRUE, style = 'overflow-x: scroll;'
                    )
                  ),
                  fluidRow(
                    box(
                      title = "Metabolic model",
                      conditionalPanel(
                        condition = "output.flag_multiple_organisms",
                        selectInput(inputId=ns("organism_to_display"), label = "Organism", choices="", selected=NULL, multiple = FALSE, selectize=TRUE, width="200px")
                      ),
                      selectInput(inputId=ns("product_to_display"), label = "End product", choices=choices_metabolites, selected=NULL, multiple = FALSE, selectize=TRUE, width="200px"),
                      column(
                        column(width=1),
                        column(
                          plotOutput(ns("plot_flux"), hover=hoverOpts(id = ns("plot_hover"), delayType = "throttle")) %>% withSpinner(color="#3C8DBC"),
                          htmlOutput(ns("plot_tooltip")),
                          width=11, height=400, align = "left"
                        ),
                        width=12, align="left"
                      ),
                      div("Hover over vertices for names of metabolites"),
                      div("Positive fluxes are in green"),
                      downloadButton(ns('download_model'), 'Download model') %>% withSpinner(color="#3C8DBC"),
                      width=12, align = "left", status = "primary", solidHeader = TRUE, collapsible = TRUE, style = 'overflow-x: scroll;'
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
predictionsGenomeServer <- function(input, output, session, x, selected_section) {
  #Set namespace
  ns <- session$ns
  
  #***********************
  #Get user input (events)
  #***********************
  #Read in data files
  get_database_file <- reactive({
    req(input$file_gene_functions)
    database_file = read_custom_file(input$file_gene_functions$datapath)
    return(database_file)
  })
  
  get_reactions_file <- reactive({
    req(input$file_reference_reactions)
    reactions_file = read.csv(input$file_reference_reactions$datapath)
    return(reactions_file)
  })
  
  #Get reference reactions
  get_reference_reactions <- reactive({
    if(input$subtabs=="Database")
    {
      input_reference_reactions = reference_reactions
    }else if(input$subtabs=="File upload"){
      input_reference_reactions = get_reactions_file()
    }
    
    #Check for required columns
    required_columns <- c("abbreviation", "equation", "direction", "officialName", "geneAssociation", "subsystem", "reaction_ID", "Enzyme", "Genes")
    all_columns_present <- all(required_columns %in% colnames(input_reference_reactions))
    if(all_columns_present==FALSE)
    {
      input_reference_reactions=""
    }
    
    validateShowModal(need(input_reference_reactions!="", "Please check the format of the reference reactions file and try again."))
    
    return(input_reference_reactions)
  })
  
  #Get database IDs
  get_database_ID <- eventReactive({input$make_predictions_1|input$make_predictions_2}, {
    if(input$subtabs=="Database")
    {
      choices_match = which(choices_organism==input$organism)
      organism_match = which(raw_data$IMG_Genome_ID_max_genes %in% gene_functions$Genome_ID)[choices_match]
      ID_match = raw_data$IMG_Genome_ID_max_genes[organism_match]
      
      database_ID = gene_functions$geneAssociation[which(gene_functions$Genome_ID==ID_match)]
    }else{
      database_ID = get_database_file()
      database_ID = as.data.frame(database_ID)
      
      #Detect file type, then format
      ##Genome IDs in one column and KO IDs in another column
      if("Genome" %in% colnames(database_ID) & !is.na(detect_pattern_column(data=database_ID, pattern="^K[0-9]{5}$")))
      {
        column_with_pattern <- detect_pattern_column(data=database_ID, pattern="^K[0-9]{5}$")
        database_ID$Database_ID = database_ID[[column_with_pattern]]
        
        database_ID <- database_ID %>%
          group_by(Genome) %>%
          mutate(row_id = row_number()) %>%
          spread(Genome, Database_ID) %>%
          select(-row_id)
      }else if(!is.na(detect_pattern_column(data=database_ID, pattern="^K[0-9]{5}$")))
      {
        ##KO IDs in one or more columns
        column_with_pattern <- detect_pattern_column(data=database_ID, pattern="^K[0-9]{5}$")
        database_ID = database_ID[[column_with_pattern]]
      }else
      {
        database_ID = NULL
      }
    }
    
    database_ID = as.data.frame(database_ID)
    
    validateShowModal(need(database_ID!="", "Please check the format of your predicted gene functions file and try again."))
    
    return(database_ID)  
  }
  , ignoreNULL = FALSE,  ignoreInit=FALSE)
  
  #Get substrate
  get_input_substrate <- eventReactive({input$make_predictions_1|input$make_predictions_2}, {
    if(input$subtabs=="Database")
    {
      substrate = input$substrate_1
      
    }else{
      substrate = input$substrate_2
    }
    
    validateShowModal(need(substrate!="", "Please choose a substrate"))
    
    return(substrate)  
  }
  , ignoreNULL = FALSE,  ignoreInit=FALSE)
  
  #Get products
  get_input_products <- eventReactive({input$make_predictions_1|input$make_predictions_2}, {
    if(input$subtabs=="Database")
    {
      products = input$products_1
    }else{
      products = input$products_2
    }
    
    validateShowModal(need(products!="", "Please choose one or more products"))
    
    return(products)  
  }
  , ignoreNULL = FALSE,  ignoreInit=FALSE)
  
  #Get unbalanced intermediates
  get_unbalanced_intermediate <- eventReactive({input$make_predictions_1|input$make_predictions_2}, {
    if(input$subtabs=="Database")
    {
      unbalanced_intermediate = input$unbalanced_intermediate_1
    }else{
      unbalanced_intermediate = input$unbalanced_intermediate_2
    }
    return(unbalanced_intermediate)  
  }
  , ignoreNULL = FALSE,  ignoreInit=FALSE)
  
  #*************
  #Process input
  #*************
  #Build reference model
  build_reference_model <- eventReactive({input$make_predictions_1|input$make_predictions_2}, {      
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
  }
  , ignoreNULL = FALSE,  ignoreInit=FALSE)
  
  #Build reference mode for graph layout
  build_reference_model_no_unbalanced_products <- eventReactive({input$make_predictions_1|input$make_predictions_2}, {     
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
  }
  , ignoreNULL = FALSE,  ignoreInit=FALSE)
  
  #Get graph layout for reference model
  get_layout <- eventReactive({input$make_predictions_1|input$make_predictions_2}, {      
    unbalanced_intermediate = get_unbalanced_intermediate()
    reference_model_no_unbalanced_products = build_reference_model_no_unbalanced_products()
    
    g = graph_enzymes(s = reference_model_no_unbalanced_products, name=unbalanced_intermediate)
    layout = layout_with_fr(graph=g)
    
    return(layout)
  }
  , ignoreNULL = FALSE,  ignoreInit=FALSE)
  
  #Rescale layout
  get_layout_rescale <- eventReactive({input$make_predictions_1|input$make_predictions_2}, {   
    layout_rescale = data.frame(x=rescale(get_layout()[,1]), y=rescale(get_layout()[,2]))
    layout_rescale$rn = 1:nrow(layout_rescale)
    
    return(layout_rescale)
  }
  , ignoreNULL = FALSE,  ignoreInit=FALSE)
  
  #Get number of organisms
  count_organisms <- eventReactive({input$make_predictions_1|input$make_predictions_2}, {
    database_ID = get_database_ID()
    count = ncol(database_ID)
    return(count)
  }
  , ignoreNULL = FALSE,  ignoreInit=FALSE)
  
  #Build and solve organism-specific model
  solve_models <- eventReactive({input$make_predictions_1|input$make_predictions_2}, {
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
    s = lapply(1:ncol(database_ID), function(x) {
      vector("list", length(ending_metabolite))
    })
    
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
    
    for(i in 1:ncol(database_ID))
    { 
      #Find Enzymes
      filtered_database_ID = database_ID[,i][!is.na(database_ID[,i]) & database_ID[,i] != ""]
      officialName = find_enzymes(df = reference_model, database_ID = filtered_database_ID)
      
      #Simplify Model
      df = simplify_model(df = reference_model, officialName=officialName)
      
      for(j in 1:length(ending_metabolite))
      {
        #Add Target Metabolites
        s[[i]][[j]] = add_target_metabolites(df = df, starting_metabolite=format_name(starting_metabolite[j]), ending_metabolite=format_name(ending_metabolite[j]), lowbnd=-1000, uppbnd=10^6)
        
        #Find fluxes
        s[[i]][[j]] = find_fluxes_df(s[[i]][[j]])
      }
      
      # Increment the progress bar
      updateProgressBar(session = session, id = ns("pb"), value = 1/ncol(database_ID)*100*i)
    }
    
    #Hide progress bar  
    removeModal()
    
    #Print status to log
    cat(file=stderr(), paste0("Ended prediction at ", Sys.time(), "\n"))
    
    return(s)
  }
  , ignoreNULL = FALSE,  ignoreInit=TRUE)
  
  #Get fluxes from models
  get_fluxes <- eventReactive({input$make_predictions_1|input$make_predictions_2}, {
    #Get inputs
    s = solve_models()
    substrate = get_input_substrate()
    products = get_input_products()
    database_ID = get_database_ID()
    
    #Set variables
    if(length(products)>0)
    {
      products = products
    }else{
      products="None"
    }
    ending_metabolite = products
    
    #Get fluxes
    flux = data.frame(matrix(NA, nrow = ncol(database_ID), ncol = length(ending_metabolite)))
    rownames(flux) = colnames(database_ID)
    colnames(flux) = c(ending_metabolite)
    
    for(i in 1:ncol(database_ID))
    { 
      for(j in 1:length(ending_metabolite))
      {
        flux[i,j] = s[[i]][[j]]$flux[which(s[[i]][[j]]$abbreviation=="Ending_metabolite")]
      }
    }
    
    return(flux)
  }
  , ignoreNULL = FALSE,  ignoreInit=FALSE)
  
  #Get endproducts from fluxes
  get_endproducts <- eventReactive({input$make_predictions_1|input$make_predictions_2}, {
    #Get fluxes
    flux = get_fluxes()
    
    min_flux = 1
    
    flux = flux %>% pivot_longer(cols=colnames(flux), names_to="Product", values_to="Flux")
    flux$Flux = as.numeric(flux$Flux)
    flux = flux %>% filter(Flux>min_flux)
    
    endproducts = unique(flux$Product)
    
    return(endproducts)
  }
  , ignoreNULL = FALSE,  ignoreInit=FALSE)
  
  #Make plot of models
  get_plot <- reactive({
    #Get unbalanced intermediates
    unbalanced_intermediate = get_unbalanced_intermediate()
    
    #Get index of selected organism
    if(!is.null(input$organism_to_display))
    {
      i = which(colnames(get_database_ID())==input$organism_to_display)
    }else
    {
      i = 1
    }
    
    #Get index of selected products
    j = which(get_input_products()==input$product_to_display)
    
    #Get model
    s = solve_models()[[i]][[j]]
    
    #Change fluxes to 0 if product has flux less than min_flux
    min_flux = 1
    if(s$flux[which(s$abbreviation=="Ending_metabolite")]<min_flux)
    {
      s$flux = 0
    }
    
    #Draw Enzymatic Reactions in Graph
    g = graph_enzymes(s = s, name=unbalanced_intermediate, show_flux=TRUE)
    
    #Set color of substrate and product
    V(g)$color[which(V(g)$name==format_name(get_input_products()[j]))] = "red"
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
                      selected = c("NAD+", "NADH", "NADP+", "NADPH", "FAD", "FADH2", "Reduced ferredoxin", "Oxidized ferredoxin", "Ubiquinone", "Ubiquinol", "Quinone", "Hydroquinone", "Menaquinone", "Menaquinol", "Oxidized hydrogenase", "Reduced hydrogenase",  "ATP", "ADP", "AMP", "GTP", "GDP", "Orthophosphate", "Diphosphate", "Polyphosphate", "H2O", "H+", "H+[side 1]", "H+[side 2]", "Na+[side 1]", "Na+[side 2]", "Sodium cation", "HCO3-")
    )
  })
  
  #Update choices for products to display
  observe({
    x = get_input_products()
    
    if (is.null(x))
      x <- character(0)
    
    updateSelectInput(session, 
                      inputId="product_to_display",
                      choices = x,
                      selected = head(x, 1)
    )
  })
  
  #Update choices for organisms to display
  observe({
    x = colnames(get_database_ID())
    
    if (is.null(x))
      x <- character(0)
    
    updateSelectInput(session,
                      inputId="organism_to_display",
                      choices = x,
                      selected = head(x, 1)
    )
  })
  
  #****************
  #Generate outputs
  #****************
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
  
  output$downloadFunctions_2 <- downloadHandler(
    filename = function() {
      paste("gene_functions_rumen_cultured", "csv", sep = ".")
    },
    content = function(file) {
      table=gene_functions_Hungate
      write.csv(table, file, row.names = FALSE)
    }
  )  
  
  output$downloadFunctions_3 <- downloadHandler(
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
  
  #Output modal with example files
  #Create modal
  observeEvent(input$genome_file_functions_modal, ignoreInit = TRUE, {
    showModal(modalDialog(
      h3("Example files"),
      tags$ol(class = "circled-list",
              tags$li(downloadLink(outputId = ns("downloadFunctions_1"), label = "Example 1 (E. coli)")),
              tags$li(downloadLink(outputId = ns("downloadFunctions_2"), label = "Example 2 (cultured prokaryotes from rumen)")),
              tags$li(downloadLink(outputId = ns("downloadFunctions_3"), label = "Example 3 (MAGs from rumen)")),
      ),
      div("Click ",
          actionLink(ns("go_to_help"), "here"),
          " to see detailed guidelines."),
      easyClose = TRUE, footer = NULL
    ))
  })
  
  observeEvent(input$genome_file_reference_modal, ignoreInit = TRUE, {
    showModal(modalDialog(
      h3("Example files"),
      tags$ol(class = "circled-list",
              tags$li(downloadLink(outputId = ns("downloadReference_1"), label = "Example 1 (Glucose fermentation)")),
              tags$li(downloadLink(outputId = ns("downloadReference_2"), label = "Example 2 (Fructose fermentation)")),
      ),
      div("Click ",
          actionLink(ns("go_to_help"), "here"),
          " to see detailed guidelines."),
      easyClose = TRUE, footer = NULL
    ))
  })
  
  observeEvent(input$go_to_help, {
    updateNavbarPage(session=x, inputId="tabs", selected = "help")
    updateNavlistPanel(session=x, inputId="navlist_panel", selected = "Predict traits from genome")
    removeModal()
  })
  
  
  #Output flag for multiple organisms
  output$flag_multiple_organisms = reactive({
    count_organisms()>1
  })
  outputOptions(output, "flag_multiple_organisms", suspendWhenHidden = FALSE)
  
  #Output file upload status
  output$check_file_genome = reactive({
    is.null(input$file_gene_functions$datapath)|is.null(input$file_reference_reactions$datapath)
  })
  outputOptions(output, "check_file_genome", suspendWhenHidden = FALSE)
  
  #Output summary text
  output$text=renderText(
    if(nrow(get_fluxes())>1){
      paste0(length(get_endproducts()), " end products predicted for ", nrow(get_fluxes()), " organisms")
    }else{
      paste0(length(get_endproducts()), " end products predicted")
    }
  )
  
  #Output downloadable csv of fluxes
  output$download_data <- downloadHandler(
    filename = function() {
      paste("fluxes", "csv", sep = ".")
    },
    content = function(file) {
      sep <- switch("csv", "csv" = ",", "tsv" = "\t")
      
      # Write to a file specified by the 'file' argument
      if(nrow(get_fluxes())>1)
      {
        write.table(get_fluxes(), file, sep = sep, row.names = TRUE, col.names=NA)
      }else
      {
        write.table(get_fluxes(), file, sep = sep, row.names = FALSE)
      }
    }
  ) 
  
  #Output heat map
  observe({
    data = get_fluxes()
    show_legend = if_else(nrow(data)>1, "TRUE", "FALSE")
    width = 75*length(get_input_products())
    height = if_else(nrow(data)>1, 143, 100)
    
    output$plot_heatmap = renderPlot(exp={
      plot_heatmap(data = data, show_legend = show_legend)
    }, width = width, height = height)
  })
  
  #Output flux graphs
  output$plot_flux <- renderPlot(exp={
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
  output$plot_tooltip <- renderText(
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
  
  #Output downloadable csv of model
  output$download_model <- downloadHandler(
    filename = function() {
      paste("model", "csv", sep = ".")
    },
    content = function(file) {
      sep <- switch("csv", "csv" = ",", "tsv" = "\t")
      
      #Get index of selected organism
      if(!is.null(input$organism_to_display))
      {
        i = which(colnames(get_database_ID())==input$organism_to_display)
      }else
      {
        i = 1
      }
      
      #Get index of selected products
      j = which(get_input_products()==input$product_to_display)
      
      #Get model
      s = solve_models()[[i]][[j]]
      
      # Write to a file specified by the 'file' argument
      write.table(s, file, sep = sep, row.names = TRUE, col.names=NA)
    }
  ) 
}