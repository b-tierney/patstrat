# Load necessary libraries, suppressing startup messages
suppressPackageStartupMessages({
  library(dplyr)
  library(mice)
  library(VIM)
})

#' Impute Data Function
#'
#' Imputes missing values in the data using specified methods and adds tracking columns
#' to indicate which values were imputed.
#'
#' @param data A dataframe with missing values to be imputed.
#' @param methods A character vector of imputation methods to apply. Options include "mean", "median", "knn", and "mice".
#' @param ks An integer vector specifying values of k for KNN imputation.
#' @return A list of dataframes, each containing imputed data and corresponding `_imp` columns.
#' @export
impute_data <- function(data, methods = c("mean", "median", "knn", "mice"), ks = c(5)) {
  
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


