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

# Function to fit RuleFit model and generate output table
fit_rule_model <- function(config_file = 'config/supervised_config',input_data_list = 'user_output/tmp/imputed_data.rds') {
  
  config <- load_config(config_file)
  methods <- config$methods
  params <- config$params
  input_data_list = readRDS(input_data_list)

  varsets <- tryCatch(
  {
    suppressWarnings(varselect('config/varselect_config'))
  },
  error = function(e) {
    varsets = list()
    varsets['all'] = 'all'
    return(varsets)
  }
  )

  results_list = list()

  for(varset in names(varsets)){

    select_columns <- varsets[[varset]]

    for(dat in names(input_data_list)){

      data = input_data_list[[dat]]

      subsetted_data <- if (varset == 'all') {
        data 
      } else {
        data[, select_columns, drop = FALSE]
      }

      dat_filt <- filter_columns(data)
      dat_filt <- dat_filt %>% mutate_if(is.character, as.factor)

      # Fit the RuleFit model
      formula <- as.formula(paste(params[['dependent_variable']], "~ ."))

      rule_model <- pre(formula, data = dat_filt, family = params[['family']], max.rules = params[['max_rules']])
      
      # Extract the coefficients
      rule_coefficients <- coef(rule_model) %>% mutate(imputation_method = dat,variable_set = varset)
      
      # Combine the coefficients and descriptions into a data frame

      results_list[[paste(dat,varset,sep='__')]] <- rule_coefficients

      }
  }

  return(results_list)
}

results_out = bind_rows(fit_rule_model(config_file = 'config/supervised_config',input_data_list = 'user_output/tmp/imputed_data.rds'))
write.csv(results_out,file = 'user_output/supervised_results.csv')


