library(dplyr)
library(pre)

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
#' Fit RuleFit Model and Generate Output Table
#'
#' This function fits a RuleFit model to an input dataset and generates a table of model coefficients.
#'
#' @param input_data_list A list of data frames to apply the RuleFit model to.
#' @param varselect A named list of variable sets to subset data by before modeling. Default is 'all' variables.
#' @param family The family of the model. Default is "gaussian".
#' @param dependent_variable The dependent variable for the RuleFit model (required).
#' @param max_depth The maximum depth of the tree used to generate in the RuleFit model. Default is 10.
#' @return A list of data frames containing RuleFit model coefficients for each dataset and variable set.
#' @export
supervised_rulefit <- function(input_data_list, varselect = list(all = 'all'), dependent_variable, 
                           family = "gaussian", max_depth = 3) {
  
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

      # Fit the RuleFit model using the provided parameters
      formula <- as.formula(paste(dependent_variable, "~ ."))
      rule_model <- pre(formula, data = dat_filt, family = family, maxdepth = max_depth)
      
      # Extract the coefficients and add metadata
      rule_coefficients <- coef(rule_model) %>%
        mutate(imputation_method = dat, variable_set = varset)
      
      # Store results in a list
      results_list[[paste(dat, varset, sep = '__')]] <- rule_model
    }
  }

  return(results_list)
}

