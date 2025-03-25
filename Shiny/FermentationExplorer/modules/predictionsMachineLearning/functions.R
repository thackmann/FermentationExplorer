# Define Functions for Predictions Using Machine Learning Module
# These are functions specific to this module
# Author: Timothy Hackmann
# Date: 9 Mar 2025

#' Round Values to Binary
#'
#' This function rounds numeric values to 0 or 1 based on a threshold of 0.5.
#'
#' @param x A numeric vector of values to be rounded.
#' @return A numeric vector with values rounded to 0 or 1.
#' @export
make_binary <- function(x) {
  ifelse(x >= 0.5, 1, 0)
}

#' Extract Genomes from Dataframe
#'
#' This function extracts unique genomes from a given dataframe, removing NA and "NA" values.
#'
#' @param data A data frame containing genome information.
#' @param genome_column The column containing genome IDs.
#' @return A vector of unique genome IDs.
#' @export
extract_genomes <- function(data, genome_column = "IMG Genome ID max genes") {
  # Select the genome column, filter out NA and "NA", and get unique values
  genomes <- data %>%
    dplyr::select(!!rlang::sym(genome_column)) %>%
    dplyr::filter(!is.na(!!rlang::sym(genome_column)) & !!rlang::sym(genome_column) != "NA") %>%
    dplyr::pull(!!rlang::sym(genome_column)) %>%
    # unique() %>%
    as.character() 
  
  return(genomes)
}

#' Format Response Variable for Random Forest Model
#'
#' This function formats the response variable for a random forest model.  The input
#' is the app's database and a query string.  The query string specifies which organisms
#' in the database are positive for the trait.  The function returns a dataframe with
#' responses (0 = negative for trait, 1 = positive for trait) and genome ID for each organism.
#' By default, organisms with NA for any variables in the query are excluded.
#' It optionally subsamples a proportion of rows to reduce the number of responses.
#'
#' @param data The app's database (a data frame)
#' @param query_string A string representing the query filter (e.g., "`Gram_stain` == \"positive\"").
#' @param ignore_NA Logical. If TRUE, organisms with NA are not counted. Default is TRUE.
#' @return A response data frame with genome IDs and a binary response (1 for positive trait, 0 otherwise).
#' @export
#' @importFrom dplyr filter
#' @importFrom rlang parse_expr sym
format_response <- function(data, query_string, ignore_NA = TRUE) {
  # Get data for organisms with positive traits
  data_positive <- filter_data_by_query(data, query_string)
  
  # Get data for all organisms (excluding those with NA values if specified)
  if(ignore_NA)
  {
    data_all <- filter_data_excluding_na(data, query_string)
  }else
  {
    data_all = data
  }
  
  # Get genomes for organisms
  positive_genomes <- extract_genomes(data_positive)
  all_genomes <- extract_genomes(data_all)
  
  # Ensure both vectors are character
  positive_genomes <- as.character(positive_genomes)
  all_genomes <- as.character(all_genomes)
  
  # Create a response dataframe with binary values: 1 for positive genomes, 0 for others
  response <- data.frame(
    Genome = all_genomes,
    Response = ifelse(all_genomes %in% positive_genomes, 1, 0)
  )
  
  return(response)
}

#' Format Predictor Variables for Random Forest Model
#'
#' This function gets predictors for a random forest model from gene functions 
#' of a given set of genomes. It puts genomes in rows and gene functions in columns, 
#' converting the latter to binary values (0 = absent in genome, 1 = present in genome).  
#' It optionally subsamples a proportion of the columns to reduce the number of predictors
#' and rows to reduce the number of responses.
#'
#' @param gene_functions A data frame containing gene functions with KO IDs and Genome IDs.
#' @param seed An optional seed value for reproducibility. Default is NULL.
#' @param responses_to_keep An optional proportion of rows to keep when subsampling. Must be between 0 and 1. Default is 1
#' @param predictors_to_keep An optional proportion of columns to keep when subsampling. Must be between 0 and 1. Default is 1
#' @param only_keep_genome_ko A logical indicating whether to retain only the Genome column and KO columns. Default is TRUE
#' @return A data frame of formatted predictors.
#' @export
#' @importFrom dplyr select mutate distinct sample_n
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom rlang sym
format_predictors = function(gene_functions, seed = NULL, responses_to_keep = 1, predictors_to_keep = 1, only_keep_genome_ko = TRUE) {
  predictors = gene_functions
  
  ko_column <- detect_pattern_column(data = predictors, pattern = "^K[0-5]{5}$")
  
  predictors <- predictors %>% 
    dplyr::select(Genome, !!rlang::sym(ko_column)) %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(value = 1) %>% 
    tidyr::pivot_wider(names_from = !!rlang::sym(ko_column), values_from = value, values_fill = list(value = 0))

  # Ensure Genome is character
  predictors$Genome <- as.character(predictors$Genome) 
  predictors <- predictors %>%
    dplyr::select(Genome, everything())
  
  # Remove all non-genome and non-KO columns if the option is enabled
  if (only_keep_genome_ko) {
    ko_columns <- grep("^K[0-9]{5}$", colnames(predictors), value = TRUE)
    predictors <- predictors %>% dplyr::select(Genome, all_of(ko_columns))
  }
  
  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Subsample rows
  if (!is.null(responses_to_keep)) {
    if (responses_to_keep > 1 || responses_to_keep < 0) {
      stop("Row proportion must be between 0 and 1.")
    }
    total_rows <- nrow(predictors)
    n_rows <- round(total_rows * responses_to_keep)
    predictors <- predictors %>% dplyr::sample_n(n_rows)
  }
  
  # Subsample columns (excluding Genome column)
  if (!is.null(predictors_to_keep)) {
    if (predictors_to_keep > 1 || predictors_to_keep < 0) {
      stop("Column proportion must be between 0 and 1.")
    }
    
    non_genome_columns <- setdiff(colnames(predictors), "Genome")
    total_cols <- length(non_genome_columns)
    n_cols <- round(total_cols * predictors_to_keep)
    selected_cols <- sample(non_genome_columns, n_cols)
    
    predictors <- predictors %>%
      dplyr::select(Genome, all_of(selected_cols))
  }
  
  return(predictors)
}

#' Format Data for Random Forest Model
#'
#' This function formats the data by joining predictors and response if the "Genome" column is present,
#' or combines them using cbind if "Genome" is not present. It ensures that the response column is named "Response".
#'
#' @param predictors A data frame of formatted predictors.
#' @param response A data frame of formatted response variables.
#' @return A data frame where the first column is the response and the remaining columns are predictors.
#' @export
#' @importFrom dplyr inner_join select rename
format_rf_data <- function(predictors, response) {
  # Check for "Genome" column in both predictors and response
  if ("Genome" %in% colnames(predictors) && "Genome" %in% colnames(response)) {
    # Join predictors and response by Genome
    data <- predictors %>%
      dplyr::inner_join(response, by = "Genome") %>%
      dplyr::select(-Genome)
    
    # Ensure the response column is named "Response"
    data <- data %>% dplyr::rename(Response = last_col())
    
  } else {
    # Check if number of rows in predictors and response match
    if (nrow(predictors) != nrow(response)) {
      stop("The number of rows in 'predictors' and 'response' must be the same.")
    }
    
    # Combine predictors and response using cbind
    data <- cbind(predictors, Response = response)
  }
  
  # Do further formatting
  data$Response <- as.factor(data$Response)
  data <- data %>% dplyr::select(Response, everything())
  
  return(data)
}

#' Split Train and Test Data
#'
#' This function splits the dataset into training and test sets.
#' The response variable should be the first column.
#'
#' @param data A data frame where the first column is the response and the remaining columns are predictors.
#' @param seed An optional seed value for reproducibility. Default is 123.
#' @param training_split The proportion of data to use for training. Default is 0.7.
#' @return A list containing training and test datasets.
#' @export
split_data <- function(data, seed = 123, training_split = 0.7) {
  set.seed(seed)
  ind <- sample(2, nrow(data), replace = TRUE, prob = c(training_split, 1-training_split))
  train <- data[ind == 1, ]
  test <- data[ind == 2, ]
  
  split = list(train = train, test = test)
  
  return(split)
}

#' Train Random Forest Model
#'
#' This function trains a random forest model using the formatted data.
#' The model assumes that the first column is the response variable and the remaining columns are predictors.
#' The model can be configured with a specified number of trees and maximum nodes.
#'
#' @param data A data frame where the first column is the response and the remaining columns are predictors.
#' @param seed An optional seed value for reproducibility. Default is 123.
#' @param ntree The number of trees to grow in the random forest. Default is 500.
#' @param maxnodes The maximum number of terminal nodes trees in the forest can have. Default is NULL.
#' @param positive_class_weight The weight given to the positive class of responses. Default is 0.5 (equal weight for negative and positive classes).
#' @return A random forest model object.
#' @export
#' @importFrom randomForest randomForest
train_rf <- function(data, seed = 123, training_split = 0.7, ntree = 500, maxnodes = NULL, positive_class_weight=0.5) {
  # Get response column
  response_column <- data[[1]]
  if (!is.factor(response_column)) {
    response_column <- as.factor(response_column)
  }
  
  # The remaining columns are predictors
  predictors <- data[, -1]
  
  # Combine response and predictors into a new data frame for modeling
  modeling_data <- data.frame(Response = response_column, predictors)
  
  # Fit the random forest model
  rf <- randomForest::randomForest(Response ~ ., data = data, ntree = ntree, maxnodes = maxnodes, classwt = c("0" = 1-positive_class_weight, "1" = positive_class_weight)) 
  
  return(rf)
}

#' Evaluate Random Forest Model
#'
#' This function evaluates the random forest model using the test data.
#' It returns a confusion matrix
#'
#' @param rf A random forest model object.
#' @param data A data frame containing the test data. The first column must be the response.
#' @return A confusion matrix 
#' @export
#' @importFrom caret confusionMatrix
evaluate_rf <- function(rf, data) {
  # Get model
  model = rf
  
  # Get response column
  response_column <- data[[1]]
  if (!is.factor(response_column)) {
    response_column <- as.factor(response_column)
  }
  
  # The remaining columns are predictors
  predictors <- data[, -1]
  
  # Get predictions
  predictions = randomForest:::predict.randomForest(object = model, newdata = predictors, type = "prob")[, 2] # Assuming the second column is the probability of the positive class
  
  # Convert predicted probabilities to classes based on a threshold of 0.5
  predicted_classes <- as.factor(ifelse(predictions >= 0.5, levels(response_column)[2], levels(response_column)[1]))
  
  # Create a confusion matrix
  confusion_matrix <- caret::confusionMatrix(predicted_classes, response_column)
  
  return(confusion_matrix)
}

#' Build Random Forest Model
#'
#' This function trains and evaluates a random forest model.
#'
#' @param data A data frame where the first column is the response and the remaining columns are predictors.
#' @param seed An optional seed value for reproducibility. Default is 123.
#' @param training_split The proportion of data to use for training. Default is 0.7.
#' @param ntree The number of trees to grow in the random forest. Default is 500.
#' @param maxnodes The maximum number of terminal nodes trees in the forest can have. Default is NULL.
#' @param positive_class_weight The weight given to the positive class of responses. Default is 0.5 (equal weight for negative and positive classes).
#' @return A random forest model object with evaluation results added.
#' @export
#' @importFrom randomForest randomForest
#' @importFrom caret confusionMatrix
build_rf <- function(data, seed = 123, training_split = 0.7, ntree = 500, maxnodes = NULL, positive_class_weight = 0.5) {
  # Split data into training and test sets
  data_split <- split_data(data = data, seed = seed, training_split = training_split)
  train <- data_split$train
  test <- data_split$test
  
  # Check if training or test data is empty
  if (nrow(train) == 0|nrow(test) == 0) {
    return(NULL)
  }
  
  # Train the model
  rf <- train_rf(data = train, seed = seed, ntree = ntree, maxnodes = maxnodes, positive_class_weight = positive_class_weight)
  
  # Evaluate the model
  confusion_matrix <- evaluate_rf(rf = rf, data = test)
  
  # Add evaluation results to the random forest model
  rf$evaluation_results <- confusion_matrix
  
  return(rf)
}

#' Save Random Forest Model
#'
#' This function saves a random forest model to an RDS file with optional compression and environment cleaning to reduce file size.
#'
#' @param rf A random forest model object to save.
#' @param data_fp The file path where the model should be saved.
#' @param remove_proximity Logical. If TRUE, removes proximity data from the model to reduce file size. Default is TRUE.
#' @param clean_environment Logical. If TRUE, removes non-essential objects from the environment to reduce file size. Default is TRUE.
#' @param compress The compression method to use when saving the RDS file. Default is "xz".
#' @return Saves the random forest model to the specified file path.
#' @export
save_rf = function(rf, data_fp, remove_proximity=TRUE, clean_environment = TRUE, compress="xz")
{
  #Remove data for proximity (reduces file size)
  if(remove_proximity==TRUE)
  {
    rf$proximity <- NULL 
  }
  
  # Remove non-essential objects from the environment (reduces file size)
  if(clean_environment==TRUE)
  {
    rm(list = setdiff(ls(envir = attr(rf$terms, ".Environment")), c("train", "test", "rf", "var")), envir = attr(rf$terms, ".Environment"))
  }
  
  saveRDS(object=rf, file = data_fp, compress = compress)
}

#' Run Random Forest Model Predictions
#'
#' This function makes predictions using a list of pre-trained random forest models. 
#' It ensures that the necessary predictors are present in the dataframe and in the correct order.
#'
#' @param df A dataframe containing the predictor variables.
#' @param models A named list of random forest models to use for predictions. Non-lists will be wrapped into lists.
#' @return A dataframe in long format with columns: "Organism number", "Organism name", "Model", and "Probability".
#' @export
#' @importFrom base lapply setdiff
run_random_forest <- function(df, models) {
  predictions <- lapply(models, function(model) {
    # Check if model has 'importance' before proceeding
    if (is.null(model$importance)) {
      stop("Model importance is NULL. Check model structure.")
    }
    
    predictors <- rownames(model$importance)
    
    # Ensure all necessary predictors are present
    missing_predictors <- setdiff(predictors, colnames(df))
    if (length(missing_predictors) > 0) {
      df[missing_predictors] <- 0
    }
    
    # Ensure the predictors are in the same order as the model expects
    df_ordered <- df[, predictors, drop = FALSE]
    
    # Predict using the loaded model
    randomForest:::predict.randomForest(object = model, newdata = df_ordered, type = "prob")[, 2] # Second column is probability of positive class
  })
  
  # Convert predictions to a long dataframe
  prediction_df <- data.frame(
    "Organism number" = seq_len(nrow(df)),  
    "Organism name" = rownames(df), 
    "Model" = rep(names(models), each = nrow(df)), 
    "Probability" = unlist(predictions, use.names = FALSE), 
    check.names = FALSE
  )
  
  colnames(prediction_df) <- c("Organism number", "Organism name", "Model", "Probability")
  
  return(prediction_df)
}