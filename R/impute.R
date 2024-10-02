# Load necessary libraries, suppressing startup messages
suppressPackageStartupMessages({
  library(dplyr)
  library(mice)
  library(VIM)
})

# Function to read the long-form configuration file
load_imputation_config <- function(config_dir = "config") {
  config_file <- file.path(config_dir, "imputation_config")
  
  # Load the config file as a dataframe without headers
  if (file.exists(config_file)) {
    config <- read.csv(config_file, header = FALSE, stringsAsFactors = FALSE, col.names = c("parameter", "value"))
  } else {
    stop("Imputation configuration file not found in config directory.")
  }
  
  return(config)
}

# Function for imputing missing values using various methods and adding an imputation tracking column
impute_data <- function(data, methods = c("mean", "median", "knn", "mice"), ks = c(5), categorical_exclude = 1) {
  
  # Helper function to calculate which values were imputed
  track_imputation <- function(original, imputed) {
    imputed_flag <- ifelse(is.na(original) & !is.na(imputed), TRUE, FALSE)
    return(imputed_flag)
  }

  imputation_results <- list()

  # Apply each method of imputation
  for (method in methods) {
    if (method == "mean") {
      imputed_data <- data %>%
        mutate(across(where(is.numeric), ~ ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)))  # Impute numeric columns
      
      # Track imputation status and append it directly
      imputed_data <- imputed_data %>%
        mutate(across(everything(), ~ track_imputation(data[[cur_column()]], .x), .names = "{.col}_imp"))
      
      imputation_results[[method]] <- imputed_data  # Return the modified dataframe with _imp columns
    }
    
    if (method == "median") {
      imputed_data <- data %>%
        mutate(across(where(is.numeric), ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)))  # Impute numeric columns
      
      # Track imputation status and append it directly
      imputed_data <- imputed_data %>%
        mutate(across(everything(), ~ track_imputation(data[[cur_column()]], .x), .names = "{.col}_imp"))
      
      imputation_results[[method]] <- imputed_data  # Return the modified dataframe with _imp columns
    }

    if (method == "knn") {
      for (k in ks) {
        knn_imputed_data <- kNN(data, k = k)
        imputation_results[[paste0("knn-k", k)]] <- knn_imputed_data  # KNN automatically adds _imp columns
      }
    }
    
    if (method == "mice") {
      mice_result <- mice(data, m = 1, method = "pmm", maxit = 5)  # Predictive mean matching
      imputed_data <- complete(mice_result)

      # Track imputation status and append it directly
      imputed_data <- imputed_data %>%
        mutate(across(everything(), ~ track_imputation(data[[cur_column()]], .x), .names = "{.col}_imp"))
      
      imputation_results[[method]] <- imputed_data  # Return the modified dataframe with _imp columns
    }
  }
  
  return(imputation_results)
}

# Function to load the data, perform imputation, and save results
impute_and_save <- function(tmp_dir, config_dir = "config") {
  
  # Load imputation configuration
  config <- load_imputation_config(config_dir)
  
  # Extract categorical exclusion threshold, methods, and k values from config
  categorical_exclude <- as.numeric(config$value[config$parameter == "categorical_exclude"])
  methods <- config$value[config$parameter == "method"]
  ks <- as.numeric(config$value[config$parameter == "k"])  # Read multiple k values

  # Define paths
  rds_file <- file.path(tmp_dir, "processed_data.rds")
  output_rds <- file.path(tmp_dir, "imputed_data.rds")
  
  # Load the processed data
  processed_data <- readRDS(rds_file)
  
  # Apply imputation methods
  imputed_data_list <- impute_data(processed_data, methods = methods, ks = ks, categorical_exclude = categorical_exclude)
  
  # Save all imputed datasets into a single RDS file
  saveRDS(imputed_data_list, output_rds)
  
  # Output success message
  cat("Imputation complete. Results saved in:", output_rds, "\n")
}

# Example usage
impute_and_save(tmp_dir = "user_output/tmp", config_dir = "config")
