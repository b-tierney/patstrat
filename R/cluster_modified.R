# cluster_modified.R

# Load libraries
library(torch)
library(readr)
library(ggplot2)
library(stats)
library(cluster)
library(dplyr)
library(vegan)
library(dbscan)
library(mclust)

# Drop non-numeric columns for methods that can't handle them (all except hclust)
filter_numeric_columns <- function(data) {
  # Ensure data is a data frame
  if (is.data.frame(data)) {
    numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]  # Keep data frame structure
  } else {
    stop("Input data is not a data frame.")
  }
  return(numeric_data)
}

# Normalize data
normalize_data <- function(data) {
  return(scale(data))
}

# K-Means Clustering
kmeans_clustering <- function(data, n_clusters) {
  set.seed(42)
  kmeans_result <- kmeans(data, centers = n_clusters)
  return(kmeans_result)
}

# Hierarchical Clustering
hierarchical_clustering <- function(data, cut_quantile = NULL, cutpoint = NULL,kcut = NULL) {
  # Load the dataset
  dist_matrix <- dist(data)  # Compute distance matrix
  hclust_result <- hclust(dist_matrix, method = "complete")
  if(!is.null(kcut)){
   cut_clusters <- cutree(hclust_result, k = kcut)
   data$cluster_hierarchical <- as.factor(cut_clusters)
   data$cutpoint = NA
   data$cut_quantile = NA
   return(data)
  }
  if(!is.null(cutpoint)){
    cut_quant_var = ecdf(hclust_result$height)(cutpoint)
    cut_clusters <- cutree(hclust_result, k = cutpoint)
    data$cluster_hierarchical <- as.factor(cut_clusters)
    data$cutpoint = cutpoint
    data$cut_quantile = cut_quant_var
    return(data)
  }
  if(!is.null(cut_quantile)){
    cutpoint_var = data.frame(quantile(hclust_result$height,cut_quantile))[[1]]
    cut_clusters <- cutree(hclust_result, h = cutpoint_var)
    # Add the cluster assignments to the original data
    data$cluster_hierarchical <- as.factor(cut_clusters)
    data$cutpoint = cutpoint_var
    data$cut_quantile = cut_quantile
    return(data) 
  }
}

# DBSCAN Clustering
dbscan_clustering <- function(data, eps, minPts) {
  dbscan_result <- dbscan(data, eps = eps, minPts = minPts)
  return(dbscan_result)
}


# VAE Clustering
vae_clustering <- function(data, latent_dim = 2, hidden_dim = 128, 
                           epochs = 10, batch_size = 32, n_clusters = 5, seed = 42) {
  
  # Check if CUDA is available
  device <- if (cuda_is_available()) torch_device("cuda") else torch_device("cpu")

  # Convert data to a torch tensor and move to GPU (or CPU if no GPU available)
  x_train <- torch_tensor(as.matrix(data), device = device)
  
  # Define the VAE model (on GPU/CPU)
  vae_model <- nn_module(
    initialize = function() {
      self$fc1 <- nn_linear(ncol(x_train), hidden_dim)
      self$fc2_mean <- nn_linear(hidden_dim, latent_dim)
      self$fc2_logvar <- nn_linear(hidden_dim, latent_dim)
      self$fc3 <- nn_linear(latent_dim, hidden_dim)
      self$fc4 <- nn_linear(hidden_dim, ncol(x_train))
    },
    
    forward = function(x) {
      h <- torch_relu(self$fc1(x))
      mean <- self$fc2_mean(h)
      logvar <- self$fc2_logvar(h)
      std <- torch_exp(0.5 * logvar)
      eps <- torch_randn_like(std, device = device)  # Generate random noise for reparameterization on GPU/CPU
      z <- mean + eps * std  # Latent variable
      h_decoded <- torch_relu(self$fc3(z))
      x_recon <- torch_sigmoid(self$fc4(h_decoded))
      list(x_recon, mean, logvar)  # Return reconstructed output, mean, and logvar
    }
  )
  
  # Define the VAE loss function
  vae_loss <- function(x, x_recon, mean, logvar) {
    recon_loss <- nnf_mse_loss(x_recon, x, reduction = "mean")
    kl_loss <- -0.5 * torch_sum(1 + logvar - mean^2 - torch_exp(logvar), dim = 1)
    torch_mean(recon_loss + kl_loss)
  }
  
  # Initialize the VAE model and optimizer (on GPU/CPU)
  model <- vae_model()
  optimizer <- optim_adam(model$parameters, lr = 0.001)
  
  # Training loop with memory management and explicit GPU usage
  for (epoch in 1:epochs) {
    model$train()  # Set model to training mode
    total_loss <- 0
    
    # Iterate over batches manually
    num_batches <- ceiling(nrow(x_train) / batch_size)
    for (batch_idx in 1:num_batches) {
      start_idx <- (batch_idx - 1) * batch_size + 1
      end_idx <- min(batch_idx * batch_size, nrow(x_train))
      
      # Get a mini-batch and ensure GPU placement
      batch <- x_train[start_idx:end_idx, , drop = FALSE]
      
      optimizer$zero_grad()  # Zero out gradients
      
      # Forward pass
      out <- model(batch)
      
      # Compute loss
      loss <- vae_loss(batch, out[[1]], out[[2]], out[[3]])
      
      # Backpropagation
      loss$backward()
      
      # Update weights
      optimizer$step()
      
      total_loss <- total_loss + loss$item()
      
      # Free up memory by detaching tensors (clear computation history)
      loss$detach_()
      out[[1]]$detach_()
      out[[2]]$detach_()
      out[[3]]$detach_()
    }
    
    cat("Epoch:", epoch, "Loss:", total_loss / num_batches, "\n")
    
    # Force garbage collection after each epoch to free up memory
    gc()
  }
  
  # Use the encoder part of the model to extract the latent space (on GPU/CPU)
  model$eval()  # Set model to evaluation mode
  latent_space <- model$forward(x_train)[[2]]$detach()  # Get the mean (z_mean)
  
  # Convert torch tensor to R array for clustering
  latent_space_array <- as_array(latent_space)
  
  # Perform Gaussian Mixture Model (GMM) clustering on the latent space (z_mean)
  gmm_result <- Mclust(latent_space_array, G = n_clusters)
  
  # Cluster probabilities
  cluster_probabilities <- gmm_result$z  # Soft clustering probabilities
  
  # Add the latent space, cluster assignments, and probabilities to the original data
  data$latent_x <- latent_space_array[, 1]
  data$latent_y <- latent_space_array[, 2]
  data$cluster_vae <- as.factor(gmm_result$classification)
  
  # Add cluster probabilities for each cluster
  for (i in 1:n_clusters) {
    data[[paste0("cluster_prob_", i)]] <- cluster_probabilities[, i]
  }
  
  cat("VAE Clustering complete with GMM probabilities\n")
  # return the dataset with latent space coordinates, cluster assignments, and cluster probabilities
  return(data)
}


# Drop columns that end with '_imp'
filter_the_columns <- function(data) {
  # Drop columns that end with '_imp'
  data <- data[, !base::grepl("_imp$", base::colnames(data))]
  
  # Keep only truly numeric columns (exclude factors and characters)
  numeric_data <- data[, base::sapply(data, function(col) base::is.numeric(col) && !base::is.factor(col) && !base::is.character(col)), drop = FALSE]
  
  return(numeric_data)
}

#' Run Clustering
#'
#' Applies clustering methods to a list of dataframes using specified variable sets and parameters.
#'
#' @param input_data_list A list of dataframes to apply clustering to.
#' @param varselect A named list of variable sets to subset data by before clustering. Default is 'all' variables.
#' @param methods A character vector of clustering methods to apply. Options are "kmeans", "hierarchical", "dbscan", and "vae".
#' @param params A list of parameters specific to each clustering method. Includes settings for each method.
#' @return A list containing clustering results for each method and variable set.
#' @export
unsupervised_clustering <- function(input_data_list, varselect = list(all = 'all'),methods = c('kmeans','hierarchical','dbscan','vae'),params = list(vae = list(latent_dim = 2, hidden_dim = 128, 
                           epochs = 10, batch_size = 32, n_clusters = 5, seed = 42),dbscan = list(eps = c("0.5", "0.75"), minPts = c("2", "3")), hierarchical = list(cut_quantile = c(".5"), cutpoint = c("3"), kcut = c("5")), kmeans = list(n_clusters = c("2", "3")))) {

  results_list <- list()


  for(varset in names(varselect)){

    select_columns <- varselect[[varset]]

    # Loop through each dataframe in the list
    for (name in names(input_data_list)) {
      data <- input_data_list[[name]]

      # Subset data based on select_columns, if available, else keep all columns
      subsetted_data <- if (varset == 'all') {
        data 
      } else {
        data[, select_columns, drop = FALSE]
      }

      # Filter out columns that end in '_imp' and keep numeric ones
      filtered_data <- filter_the_columns(subsetted_data)

      print(filtered_data)

      # Run clustering methods based on config
      for (method in methods) {
        normalized_data <- normalize_data(filtered_data)
        
        if (method == 'kmeans') {
          
          kmeans_n_clusters <- params[[method]][['n_clusters']]
          for (n_clusters in kmeans_n_clusters) {
            kmeans_result <- kmeans_clustering(normalized_data, n_clusters)
            
            # Append result to list
            results_list[[paste0(name, '_kmeans_', n_clusters, '_',varset)]] <- list(method = 'kmeans', n_clusters = n_clusters, result = kmeans_result,varset = varset)
          }
        } else if (method == 'hierarchical') {
          
          # Loop through cutpoints (if provided) and store output
          if (!is.null(params[[method]][['cutpoint']])) {
            for (cutpoint in params[[method]][['cutpoint']]) {
              hclust_result <- hierarchical_clustering(filtered_data, cutpoint = as.numeric(cutpoint))
              # Append result to list
              results_list[[paste0(name, '_hclust_cutpoint_', cutpoint, '_',varset)]] <- list(method = 'hierarchical', cutpoint = cutpoint, result = hclust_result,varset = varset)
            }
          }

          if (!is.null(params[[method]][['kcut']])) {
            for (kcut in params[[method]][['kcut']]) {
              hclust_result <- hierarchical_clustering(filtered_data, kcut = as.numeric(kcut))
              # Append result to list
              results_list[[paste0(name, '_hclust_kcut_', cutpoint, '_',varset)]] <- list(method = 'hierarchical', kcut = cutpoint, result = hclust_result,varset = varset)
            }
          }


          # Loop through cut_quantiles (if provided) and store output
          if (!is.null(params[[method]][['cut_quantile']])) {
            for (cut_quantile in params[[method]][['cut_quantile']]) {
              hclust_result <- hierarchical_clustering(filtered_data, cut_quantile = as.numeric(cut_quantile))
              # Append result to list
              results_list[[paste0(name, '_hclust_cut_quantile_', cut_quantile, '_',varset)]] <- list(method = 'hierarchical', cut_quantile = cut_quantile, result = hclust_result,varset = varset)
            }
          }
          
          # Save hclust result
          results_list[[paste0(name, '_hclust', '_',varset)]] <- list(method = 'hierarchical', result = hclust_result,varset = varset)
        } else if (method == 'dbscan') {
          
          dbscan_eps <- params[[method]][['eps']]
          dbscan_minPts <- params[[method]][['minPts']]
          for (eps in dbscan_eps) {
            for (minPts in dbscan_minPts) {
              eps = as.numeric(eps)
              minPts = as.numeric(minPts)
              dbscan_result <- dbscan_clustering(normalized_data, eps, minPts)
              # Append result to list
              results_list[[paste0(name, '_dbscan_eps_', eps, '_minPts_', minPts, '_',varset)]] <- list(method = 'dbscan', eps = eps, minPts = minPts, result = dbscan_result,varset = varset)
            }
          }
        } else if (method == "vae"){
          
          vae_result <- vae_clustering(normalized_data)
          results_list[[paste0(name, '_vae', '_',varset)]] <- list(method = 'vae', result = vae_result,varset = varset)
        }
      }
    }
  }
  return(results_list)
}

# Function to extract clusters for K-Means, DBSCAN, and HClust methods
extract_clusters <- function(result_data, method_name) {
  if (method_name == 'kmeans') {
    return(result_data$result$cluster)  # K-Means Clustering Vector
  } else if (method_name == 'dbscan') {
    return(result_data$result$cluster)  # DBSCAN Cluster assignment
  } else if (method_name == 'hclust') {
    return(result_data$result$cluster_hierarchical)  # HClust Cluster assignment
  } else if (method_name == 'vae') {
    return(result_data$result$cluster_vae)  # VAE Cluster assignment
  }
  return(NULL)
}

#' Standardize Cluster Output
#'
#' Standardizes and integrates clustering results for easier analysis.
#'
#' @param results_list A list containing clustering results.
#' @return A list with two dataframes: `wide_format_df` and `long_format_df`.
#' @export
# Function to standardize the output into wide and long format
standardize_cluster_output <- function(results_list) {
  wide_format_list <- list()
  long_format_list <- data.frame(parameterset = character(), imputation = character(),
                                 method = character(), parameter = character(), value = numeric())
  
  param_counter <- 1  # Initialize parameter counter
  
  for (name in names(results_list)) {
    result_entry <- results_list[[name]]
    parts <- strsplit(name, "_")[[1]]  # Split the name to extract components
    imputation <- parts[1]  # First part is the imputation method
    method <- parts[2]  # Second part is the clustering method
    varset <- result_entry[['varset']]
    result_entry[['varset']] <- NULL

    # Create unique parameter set name (e.g., parameterset_00000001)
    param_set <- sprintf("parameterset_%08d", param_counter)
    param_counter <- param_counter + 1  # Increment the counter for the next parameter set

    # Extract clusters if it's a known method (K-Means, DBSCAN, or HClust)
    clusters <- extract_clusters(result_entry, method)
    
    if (!is.null(clusters)) {
      # For wide format: Store the cluster assignments
      wide_format_list[[param_set]] <- clusters
      
      # For long format: Store the parameters and their values

        for (param_name in names(result_entry)[-c(1, length(result_entry))]) {  # Skip 'method' and 'result'
          param_value <- result_entry[[param_name]]
          long_format_list <- rbind(long_format_list, data.frame(
            parameter_set = param_set,
            variable_set = varset,
            imputation = imputation,
            method = method,
            parameter = param_name,
            value = param_value
          ))
        }
      
    }
  }
  
  # Convert the wide format list to a DataFrame
  wide_format_df <- as.data.frame(wide_format_list)
  return(list(wide_format_df = wide_format_df, long_format_df = long_format_list))
}


