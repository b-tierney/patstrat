
# cluster.R

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

# Load config file for clustering methods and parameters
load_config <- function(config_file) {
  config <- read_csv(config_file, col_names = FALSE)
  config_list <- list()

  for (i in 1:nrow(config)) {
    key <- config[i, 1]
    value <- config[i, 2]

    # Convert values enclosed in [] to lists
    if (grepl("^\[.*\]$", value)) {
      value <- as.numeric(strsplit(gsub("\[|\]", "", value), ",")[[1]])
    } else {
      value <- as.numeric(value)
    }

    config_list[[key]] <- value
  }
  
  return(config_list)
}

# Drop non-numeric columns for methods that can't handle them
filter_numeric_columns <- function(data) {
  return(data %>% select(where(is.numeric)))
}

# Normalize numeric data
normalize_data <- function(data) {
  numeric_data <- filter_numeric_columns(data)
  return(scale(numeric_data))
}

# K-Means Clustering
kmeans_clustering <- function(data, n_clusters) {
  set.seed(42)
  data_numeric <- normalize_data(data)  # Drop non-numeric columns and normalize
  kmeans_result <- kmeans(data_numeric, centers = n_clusters)
  return(kmeans_result)
}

# Hierarchical Clustering
hierarchical_clustering <- function(data) {
  dist_matrix <- dist(data)
  hclust_result <- hclust(dist_matrix)
  return(hclust_result)
}

# PCoA Clustering
pcoa_clustering <- function(data) {
  dist_matrix <- vegdist(normalize_data(data))  # Drop non-numeric columns and normalize
  pcoa_result <- cmdscale(dist_matrix)
  return(pcoa_result)
}

# DBSCAN Clustering
dbscan_clustering <- function(data, eps, minPts) {
  data_numeric <- normalize_data(data)  # Drop non-numeric columns and normalize
  dbscan_result <- dbscan(data_numeric, eps = eps, minPts = minPts)
  return(dbscan_result)
}

# Main function to run clustering based on config
run_clustering <- function(input_data_path, config_file, output_dir) {
  # Load the data output by impute in the user_output directory
  data <- read_rds(input_data_path)

  # Load the clustering config
  config <- load_config(config_file)

  # Initialize list to store results
  results_list <- list()

  # Run clustering methods based on config
  for (method in config$method) {
    if (method == 'kmeans') {
      kmeans_n_clusters <- config$kmeans_n_clusters
      for (n_clusters in kmeans_n_clusters) {
        kmeans_result <- kmeans_clustering(data, n_clusters)
        results_list[[paste0('kmeans_', n_clusters)]] <- kmeans_result
        saveRDS(kmeans_result, file = file.path(output_dir, paste0('kmeans_', n_clusters, '_result.rds')))
      }
    } else if (method == 'hierarchical') {
      hclust_result <- hierarchical_clustering(data)
      results_list[['hierarchical']] <- hclust_result
      saveRDS(hclust_result, file = file.path(output_dir, 'hclust_result.rds'))
    } else if (method == 'pcoa') {
      pcoa_result <- pcoa_clustering(data)
      results_list[['pcoa']] <- pcoa_result
      saveRDS(pcoa_result, file = file.path(output_dir, 'pcoa_result.rds'))
    } else if (method == 'dbscan') {
      dbscan_eps <- config$dbscan_eps
      dbscan_minPts <- config$dbscan_minPts
      for (eps in dbscan_eps) {
        for (minPts in dbscan_minPts) {
          dbscan_result <- dbscan_clustering(data, eps, minPts)
          results_list[[paste0('dbscan_eps_', eps, '_minPts_', minPts)]] <- dbscan_result
          saveRDS(dbscan_result, file = file.path(output_dir, paste0('dbscan_eps_', eps, '_minPts_', minPts, '_result.rds')))
        }
      }
    }
  }
  
  return(results_list)
}

# Example usage
run_clustering('path/to/imputed_data.rds', 'path/to/config/cluster_config.csv', 'user_output/')
