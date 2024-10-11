# Load pre
library(pre)

# Function to fit RuleFit model and generate output table
fit_rule_model <- function(formula, data, family = "gaussian", max_rules = 10) {
  
  # Fit the RuleFit model
  rule_model <- pre(formula, data = data, family = family, max.rules = max_rules)
  
  # Extract the coefficients
  rule_coefficients <- coef(rule_model)
  
  # Extract the rule descriptions
  rule_descriptions <- labels(rule_model)
  
  # Combine the coefficients and descriptions into a data frame
  results <- data.frame(
    Rule_Description = rule_descriptions,
    Coefficient = rule_coefficients
  )
  
  # Sort by absolute value of coefficient for better interpretation
  results <- results[order(abs(results$Coefficient), decreasing = TRUE), ]
  
  # Return the results table
  return(results)
}


