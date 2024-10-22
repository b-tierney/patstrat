# Load necessary libraries
suppressPackageStartupMessages({library(dplyr)})


# Helper function for setup
build_test_data <- function() {
  # Use the original six-variable synthetic dataset if in test mode
    set.seed(123)
    dummy_data <- data.frame(
      var1 = c(rnorm(15, mean = 5, sd = 1), rnorm(15, mean = 10, sd = 1)),  # Cluster 1 and Cluster 2
      var2 = c(rnorm(15, mean = 15, sd = 2), rnorm(15, mean = 25, sd = 2)),  # Continuous variable for clustering
      var3 = sample(c(10, 20, 30), 30, replace = TRUE),  # Categorical variable (unordered) with 3 levels
      var4 = factor(c(rep("white,wholemeal", 5), rep("white", 5), rep("wholemeal", 5),  # Categorical (multiple)
                      rep("mixed", 5), rep("white,wholemeal", 5), rep("mixed", 5))),
      var5 = c(rep(1, 15), rep(0, 15)),  # Binary variable
      var6 = factor(sample(c("a", "b", "c"), 30, replace = TRUE))  # New Categorical variable with levels a, b, c
    )
    dummy_data$var1[sample(1:30, 15)] <- NA
    dummy_data$var2[sample(1:30, 15)] <- NA
    dummy_data$var3[sample(1:30, 15)] <- NA

    return(dummy_data)
}

#' Preprocess Data Function
#'
#' Preprocesses the input data by handling missing values, summarizing variable types,
#' and saving processed data and summary statistics.
#'
#' @param data A dataframe to be preprocessed.
#' @param output_dir A string specifying the output directory for saving results.
#' @param continuous_cutoff An integer specifying the cutoff for continuous variables.
#' @param missing_values A vector of values to be treated as missing.
#' @return A list with processed data and summary statistics.
#' @export
preprocess_data <- function(data, continuous_cutoff = 10, missing_values = NULL) {
  
  # Define a helper function to determine variable type based on user-defined cutoff for continuous variables
  determine_variable_type <- function(x, cutoff) {
    if (is.numeric(x)) {
      unique_vals <- unique(x[!is.na(x)])
      if (length(unique_vals) == 2) {
        return("binary")
      } else if (length(unique_vals) < cutoff) {
        return("ordered_categorical")
      } else {
        return("continuous")
      }
    } else if (is.character(x)) {
      return("unordered_categorical")
    } else if (is.factor(x)) {
      if (length(unique(x[!is.na(x)])) == 2) {
        return("binary")
      } else {
        return("unordered_categorical")
      }
    } else {
      return("other")
    }
  }
  
  # Step 1: Handle user-specified missing values (if provided), without converting categorical variables to numeric
  if (!is.null(missing_values)) {
    data <- data %>%
      mutate(across(where(is.numeric), ~ ifelse(.x %in% missing_values, NA, .x)),  # Replace in numeric columns
             across(where(is.factor), ~ as.factor(ifelse(as.character(.x) %in% missing_values, NA, as.character(.x)))),  # Preserve factors
             across(where(is.character), ~ ifelse(.x %in% missing_values, NA, .x)))  # Replace in character columns
  }

  # Ensure categorical variables are converted to characters to retain their labels
  data <- data %>%
    mutate(across(where(is.factor), as.character))
  
  # Step 2: Determine the variable type for each column based on user-defined cutoff for continuous
  variable_types <- sapply(data, determine_variable_type, cutoff = continuous_cutoff)
  
  # Step 3: Summarize missingness, range (for continuous), and levels (for categorical)
  missing_summary <- sapply(data, function(x) sum(is.na(x)) / length(x))
  
  # Ensure that Range_Min and Range_Max only apply to continuous variables
  ranges_min <- sapply(data, function(x) if (is.numeric(x)) min(x, na.rm = TRUE) else NA)
  ranges_max <- sapply(data, function(x) if (is.numeric(x)) max(x, na.rm = TRUE) else NA)
  
  # Ensure that Levels only apply to categorical variables (factors or character variables)
  levels_summary <- sapply(data, function(x) if (is.character(x)) length(unique(x[!is.na(x)])) else NA)
  
  # Step 5: Create summary statistics dataframe
  summary_df <- data.frame(
    Variable = colnames(data),
    Type = as.vector(variable_types),
    Missingness = as.vector(missing_summary),
    Range_Min = as.vector(ranges_min),
    Range_Max = as.vector(ranges_max),
    Levels = as.vector(levels_summary),
    stringsAsFactors = FALSE
  )
  
  # Step 6: Save the processed data and summary statistics to the specified directories
  return(list(
    processed_data = data,
    summary_statistics = summary_df
  ))
}
