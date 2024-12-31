library(dplyr)
library(pre)
library(glmnet)

# Function to load and parse varselect configuration file
varselect <- function(varselect_config_path) {
  varselect_config <- read.csv(varselect_config_path, stringsAsFactors = FALSE)
  
  # Initialize an empty list to store variables for each varset
  varset_list <- list()
  
  # Check if 'all,all' is present
  if (any(varselect_config$varset == "all" & varselect_config$variable == "all")) {
    varset_list[["all"]] <- 'all'
  }
  
  # Process each unique varset other than 'all'
  unique_varsets <- unique(varselect_config$varset[varselect_config$varset != "all"])
  for (varset in unique_varsets) {
    variables <- varselect_config$variable[varselect_config$varset == varset]
    varset_list[[varset]] <- variables
  }
  
  return(varset_list)
}


load_config <- function(config_file) {
  config <- read.delim(config_file, header=F, sep=',')
  methods <- config %>% filter(V1 == 'method') %>% pull(V2)
  # Create a named list of parameters using base R functions
  params <- split(config$V2, config$V1)
  params <- params[names(params) != 'method']  # Remove 'method' from params
  return(list(methods = methods, params = params))
}


filter_columns <- function(data) {
  data <- data[, !grepl("_imp$", colnames(data))]
#  numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]
  return(data)
}
#' Fit Group lasso Model and Generate Output Table
#'
#' This function fits a Group lasso model to an input dataset and generates a table of model coefficients.
#'
#' @param input_data_list A list of data frames to apply the Group lasso model to.
#' @param varselect A named list of variable sets to subset data by before modeling. Default is 'all' variables.
#' @param dependent_variable The dependent variable for the Group lasso model (required).
#' @param family The family of the model. Default is "gaussian".
#' @param group_mapping The mapping of variables to a group.
#' @param alpha The alpha value. Default is "1".
#' @return A list of data frames containing Group lasso model coefficients for each dataset and variable set.
#' @export

group_lasso <- function(input_data_list, varselect = list(all = 'all'), dependent_variable, 
                        family = "gaussian", group_mapping, alpha = 1) {
  
  # Initialize an empty list to store results
  results_list <- list()
  
  # Loop through each variable set in the varselect
  for (varset in names(varselect)) {
    
    print(varset)
    
    select_columns <- varselect[[varset]]
    
    # Loop through each dataset in the input data list
    for (dat in names(input_data_list)) {
      
      data <- input_data_list[[dat]]
      
      # Subset data based on variable set, or use all columns if varset is 'all'
      subsetted_data <- if (varset == 'all') {
        data
      } else {
        # Ensure the dependent variable is included in the subsetted data
        required_columns <- unique(c(select_columns, dependent_variable))
        subsetted_data <- data[, required_columns, drop = FALSE]
      }
      
      # Drop all columns ending in '_imp'
      subsetted_data <- subsetted_data[, !grepl("_imp$", colnames(subsetted_data))]
      
      # Convert character columns to factors
      dat_filt <- subsetted_data %>% mutate_if(is.character, as.factor)
      
      # Prepare data for glmnet
      X <- as.matrix(dat_filt[, colnames(dat_filt) != dependent_variable])
      y <- dat_filt[[dependent_variable]]
      
      # Map variable names to groups using group_mapping
      groups <- unname(group_mapping[colnames(X)])
      
      # Ensure group length matches the number of predictors
      if (length(groups) != ncol(X)) {
        stop("The length of 'groups' must match the number of columns in 'X'.")
      }
      
      # Fit the Group Lasso model using glmnet
      fit <- cv.glmnet(X, y, alpha = alpha, family = family, grouped = TRUE, type.measure = "mse")
      
      # Extract optimal lambda
      lambda_opt <- fit$lambda.min
      
      # Refit model using optimal lambda
      final_model <- glmnet(X, y, alpha = alpha, family = family, grouped = TRUE, lambda = lambda_opt)
      
      # Identify selected variables
      selected_vars <- colnames(X)[coef(final_model)[-1] != 0]
      
      # Store results with metadata
      results_list[[paste(dat, varset, sep = '__')]] <- list(
        model = final_model,
        selected_vars = selected_vars
      )
    }
  }
  
  return(results_list)
}




