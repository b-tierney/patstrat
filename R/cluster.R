# cluster.R

# Sivateja Tangirala
# 09/24/2024



## Clustering script was re-formatted and optimized for R package integration with assistance of ChatGPT, an AI model developed by OpenAI (2024).
## For more information, visit https://openai.com.



# Install necessary packages if not already installed
required_packages <- c("torch", "readr", "ggplot2", "stats", "cluster", "vegan", "dbscan")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Load libraries
library(torch)
library(readr)
library(ggplot2)
library(stats)
library(cluster)
library(vegan)
library(dbscan)

# helper function to normalize data
normalize_data <- function(data) {
  return(scale(data))
}

# K-Means Clustering
kmeans_clustering <- function(input_csv, output_csv, n_clusters = 5) {
  # Load the dataset
  data <- read_csv(input_csv)
  
  # Remove non-numeric columns for clustering
  data_clustering <- data %>%
    select(starts_with("metabolomic"), starts_with("clinical_variable"))
  
  # Normalize the data
  data_clustering <- normalize_data(data_clustering)
  
  # Perform k-means clustering
  set.seed(42)
  kmeans_result <- kmeans(data_clustering, centers = n_clusters)
  
  # Add the cluster assignments back to the original data
  data$cluster_kmeans <- as.factor(kmeans_result$cluster)
  
  # Save the dataset with cluster assignments
  write_csv(data, output_csv)
  cat("K-Means Clustering complete. Results saved to:", output_csv, "\n")
}

# Hierarchical Clustering with Different Cutpoints
hierarchical_clustering <- function(input_csv, output_csv, k_cut = 5) {
  # Load the dataset
  data <- read_csv(input_csv)
  
  # Remove non-numeric columns for clustering
  data_clustering <- data %>%
    select(starts_with("metabolomic"), starts_with("clinical_variable"))
  
  # Normalize the data
  data_clustering <- normalize_data(data_clustering)
  
  # Perform hierarchical clustering
  dist_matrix <- dist(data_clustering)  # Compute distance matrix
  hclust_result <- hclust(dist_matrix, method = "complete")
  
  # Cut tree into k clusters
  cut_clusters <- cutree(hclust_result, k = k_cut)
  
  # Add the cluster assignments to the original data
  data$cluster_hierarchical <- as.factor(cut_clusters)
  
  # Save the dataset with cluster assignments
  write_csv(data, output_csv)
  cat("Hierarchical Clustering complete. Results saved to:", output_csv, "\n")
}

# DBSCAN Clustering
dbscan_clustering <- function(input_csv, output_csv, eps = 1.5, minPts = 10) {
  # Load the dataset
  data <- read_csv(input_csv)
  
  # Remove non-numeric columns for clustering
  data_clustering <- data %>%
    select(starts_with("metabolomic"), starts_with("clinical_variable"))
  
  # Normalize the data
  data_clustering <- normalize_data(data_clustering)
  
  # Perform DBSCAN clustering
  dbscan_result <- dbscan(data_clustering, eps = eps, minPts = minPts)
  
  # Add the DBSCAN cluster assignments to the original data
  data$cluster_dbscan <- as.factor(dbscan_result$cluster)
  
  # Save the dataset with cluster assignments
  write_csv(data, output_csv)
  cat("DBSCAN Clustering complete. Results saved to:", output_csv, "\n")
}

# PCoA and dbRDA Clustering
pcoa_dbrda_clustering <- function(input_csv, output_csv, method = "PCoA") {
  # Load the dataset
  data <- read_csv(input_csv)
  
  # Remove non-numeric columns for clustering
  data_clustering <- data %>%
    select(starts_with("metabolomic"), starts_with("clinical_variable"))
  
  # Normalize the data
  data_clustering <- normalize_data(data_clustering)
  
  # Compute distance matrix
  dist_matrix <- dist(data_clustering)
  
  if (method == "PCoA") {
    # Perform PCoA
    pcoa_result <- cmdscale(dist_matrix, k = 2)
    data$pcoa1 <- pcoa_result[, 1]
    data$pcoa2 <- pcoa_result[, 2]
    cat("PCoA complete.\n")
  } else if (method == "dbRDA") {
    # Perform dbRDA
    dbRDA_result <- capscale(dist_matrix ~ age + sex + family_income + race + t2d_status, data = data)
    dbRDA_scores <- scores(dbRDA_result, display = "sites")
    data$dbRDA1 <- dbRDA_scores[, 1]
    data$dbRDA2 <- dbRDA_scores[, 2]
    cat("dbRDA complete.\n")
  }
  
  # Save the dataset with PCoA/dbRDA scores
  write_csv(data, output_csv)
  cat("PCoA/dbRDA results saved to:", output_csv, "\n")
}

# vae_clustering <- function(input_csv, output_csv, latent_dim = 2, hidden_dim = 128, 
#                            epochs = 100, batch_size = 32, n_clusters = 5, seed = 42) {
#   # Load the dataset
#   data <- read_csv(input_csv)
#   
#   # Remove non-numeric columns for the VAE model
#   data_clustering <- data %>%
#     select(starts_with("metabolomic"), starts_with("clinical_variable"))
#   
#   # Normalize the data
#   data_clustering <- normalize_data(data_clustering)
#   
#   # Define a custom dataset class
#   vae_dataset <- dataset(
#     initialize = function(data) {
#       self$data <- torch_tensor(data, dtype = torch_float32())
#     },
#     .getitem = function(i) {
#       self$data[i, ]  # Ensure the batch is accessed without subsetting issues
#     },
#     .length = function() {
#       self$data$size(1)
#     }
#   )
#   
#   # Create dataset and data loader
#   dataset <- vae_dataset(as.matrix(data_clustering))
#   data_loader <- dataloader(dataset, batch_size = batch_size, shuffle = TRUE)
#   
#   # Define the encoder model
#   encoder <- nn_module(
#     initialize = function() {
#       self$fc1 <- nn_linear(ncol(data_clustering), hidden_dim)
#       self$fc2_mean <- nn_linear(hidden_dim, latent_dim)
#       self$fc2_logvar <- nn_linear(hidden_dim, latent_dim)
#     },
#     forward = function(x) {
#       h <- torch_relu(self$fc1(x))
#       mean <- self$fc2_mean(h)
#       logvar <- self$fc2_logvar(h)
#       list(mean, logvar)
#     }
#   )
#   
#   # Define the decoder model
#   decoder <- nn_module(
#     initialize = function() {
#       self$fc1 <- nn_linear(latent_dim, hidden_dim)
#       self$fc2 <- nn_linear(hidden_dim, ncol(data_clustering))
#     },
#     forward = function(z) {
#       h <- torch_relu(self$fc1(z))
#       torch_sigmoid(self$fc2(h))
#     }
#   )
#   
#   # Define the VAE model
#   vae <- nn_module(
#     initialize = function() {
#       self$encoder <- encoder()
#       self$decoder <- decoder()
#     },
#     reparameterize = function(mean, logvar) {
#       std <- torch_exp(0.5 * logvar)
#       eps <- torch_randn_like(std)
#       mean + eps * std
#     },
#     forward = function(x) {
#       encoder_out <- self$encoder(x)
#       mean <- encoder_out[[1]]
#       logvar <- encoder_out[[2]]
#       z <- self$reparameterize(mean, logvar)
#       x_recon <- self$decoder(z)
#       list(x_recon, mean, logvar)
#     }
#   )
#   
#   # Define the VAE loss function
#   vae_loss <- function(x, x_recon, mean, logvar) {
#     recon_loss <- nnf_mse_loss(x_recon, x, reduction = "sum")
#     kl_loss <- -0.5 * torch_sum(1 + logvar - mean^2 - torch_exp(logvar))
#     recon_loss + kl_loss
#   }
#   
#   # Initialize the model and optimizer
#   model <- vae()
#   optimizer <- optim_adam(model$parameters, lr = 0.001)
#   
#   # Training loop
#   for (epoch in 1:epochs) {
#     model$train()
#     total_loss <- 0
#     
#     # Manually iterate over the batches
#     batch_iter <- dataloader_make_iter(data_loader)
#     
#     while(TRUE) {
#       batch <- dataloader_next(batch_iter)
#       
#       if (is.null(batch)) break  # Exit when there are no more batches
#       
#       optimizer$zero_grad()
#       
#       # Forward pass: Pass the batch through the model
#       out <- model(batch)
#       
#       # Compute the loss
#       loss <- vae_loss(batch, out[[1]], out[[2]], out[[3]])
#       loss$backward()  # Backpropagation
#       optimizer$step()  # Update the model
#       
#       total_loss <- total_loss + loss$item()
#     }
#     
#     cat("Epoch:", epoch, "Loss:", total_loss / nrow(data_clustering), "\n")
#   }
#   
#   # Use the encoder to extract the latent representations
#   model$eval()
#   latent_space <- model$encoder(torch_tensor(as.matrix(data_clustering), dtype = torch_float32()))[[1]]$detach()$numpy()
#   
#   # Perform k-means clustering on the latent space
#   set.seed(seed)
#   kmeans_result <- kmeans(latent_space, centers = n_clusters)
#   
#   # Add latent space and cluster assignments back to the data
#   data$latent_x <- latent_space[, 1]
#   data$latent_y <- latent_space[, 2]
#   data$cluster_vae <- as.factor(kmeans_result$cluster)
#   
#   # Save the dataset with latent space coordinates and cluster assignments
#   write_csv(data, output_csv)
#   cat("VAE Clustering complete. Results saved to:", output_csv, "\n")
# }




# Example of running the functions:

kmeans_clustering("toy_metabolomic_clinical_clustered_dataset.csv", "kmeans_output.csv")
hierarchical_clustering("toy_metabolomic_clinical_clustered_dataset.csv", "hierarchical_output.csv")
dbscan_clustering("toy_metabolomic_clinical_clustered_dataset.csv", "dbscan_output.csv")
pcoa_dbrda_clustering("toy_metabolomic_clinical_clustered_dataset.csv", "pcoa_output.csv", method = "PCoA")
#vae_clustering("toy_metabolomic_clinical_clustered_dataset.csv", "vae_output.csv")
