# patstrat

**R Package for Identifying Subpopulations of Individuals in Omic and Clinical Data**

## Overview

`patstrat` is an R package designed to identify patient subpopulations in 'omic and clinical datasets. It provides a series of simple functions to preprocess data, impute missing values, and cluster individuals using either supervised or unsupervised approaches.

The purpose is to compare numerous imputation and clustering methods simlultaneously so you can evalution the robustness of clusters to analytic choice. This is akin to our prior work with Vibration of Effects, where we compared how model specification changes observed association outcomes.

patstrat has two modes: supervised and unsupervised. Both can be run on either all variables in a dataset or specific subsets (e.g., just age, sex, bmi in one group, then just age and sex in another) so the impact of including different variables on patient stratification can be observed. 

### Supervised mode

In supervised mode, the goal is to identify subpopulations directly as a function of given depedent variable (e.g., response to treatment). patstrat uses RuleFit regression (wrapping the Pre package) to build regression-based decision trees for separating patients into groups based on their characteristics. The goal in running patstrat in supervised mode is to define patient groups that are meaningful in the context of treatment or disease (e.g., with BMI > X and the abundance of metabolite Y > X, patients are likely to respond to a given drug).

### Unsupervised mode

In unsupervised mode, patstrat can use multiple imputation (mean, median, MICE, a variational autoencoder) and clustering (kmeans, DBSCAN, hierarchical, variational autoencoder coupled with a gaussian mixture model) methods to sort patients into groups, without being guided by a potential response variable. The total set of imputation methods and clustering approaches are then returned and can be compared on, say, a PCA plot or other visualization.

In future versions of patstrat, we will implement a shinify function that generates a shiny app for a given output data structure. 


## Installation

To install this package locally from your machine, you can use the following steps:

1. Clone the repository or download the package files.
   
2. Install the package using `devtools`:

   ```r
   # If devtools is not installed, install it first
   install.packages("devtools")
   
   # Install the package
   devtools::install_local("/path/to/patstrat")
   ```

Replace `/path/to/patstrat` with the actual path to the package directory on your system.

## Usage
The workflow involves three main steps: preprocessing, imputation, and either supervised or unsupervised clustering.

### Step 1: Preprocess the Data
Use the `preprocess_data` function to normalize and prepare the dataset for analysis.

#### Arguments

| Parameter         | Description                                               | Default |
|-------------------|-----------------------------------------------------------|---------|
| data              | The input dataset to preprocess.                          | None    |
| missing_values    | Strings that indicate missing values (eg NA, none, Null)     | None    |
| continuous_cutoff | The max number of levels in a column before it is converted to a continuous variable | 10  |

#### Output

The processed dataframe as well as a dataframe ($processed_data) summarizing the included variables ($summary_statistics).

```r
# Load the package
library(patstrat)

# Preprocess the data
iris_preprocessed <- preprocess_data(iris)
```

### Step 2: Impute Missing Data
After preprocessing, handle missing values using the `impute` function. Multiple methods for imputation are supported.

#### Parameters

| Parameter         | Description                                               | Default |
|-------------------|-----------------------------------------------------------|---------|
| data              | The input dataset on which to impute missing values.  | None    |
| methods    |  A vector of methods to employ. | c('mean','median','knn','mice') |   |
| ks |An integer vector specifying values of k for KNN imputation. | c(5)  |

#### Output

Returns a list of imputed dataframes where the names correspond to the methods and the parameters used. Additionally, columns with the original labels ending in \_imp are appended to the initial dataframe indicating if a given cell in the original column was or wasn't imputed. These are not analyzed in the clustering functions downstream and are there for the users benefit.

```r
# Impute missing values
imputed_data <- impute_data(iris_preprocessed$processed_data,methods = c('mice','knn'),ks = c(6,7,8,9,10))
```

### Step 3: Clustering
Once the data has been imputed, you can proceed with either supervised or unsupervised clustering.


### Supervised Clustering
Use the `supervised` function to perform supervised clustering based on predefined labels.

#### Arguments

| Parameter         | Description                                               | Default |
|-------------------|-----------------------------------------------------------|---------|
| dependent_variable              | The dependent, or "Y" variable that you want to regress on.  | None    |
| input_data_list    |  A named list of dataframes that will be used for the regression. | None | 
| varselect | A named list of vectors of variables that you want to iterate over for clustering. Uses all variables by default (specific by a named "all" string). For example, to use age, sex, and bmi in one round and just age and bmi in another, you should specify a list as follows: list(group1 = c('age','sex','bmi'),group2 = c('age','bmi')) | list(all = 'all') |

```r
# Perform supervised clustering for ALL imputed datasets for two subsets of variables
results_supervised <- supervised_rulefit(dependent_variable = 'Sepal.Length',input_data_list = imputed_data,varselect = list(group1 = ('Petal.Length'),group2 = c('Petal.Width','Sepal.Width')))
```

#### Output

Returns a named list of rulefit regression outputs for every set of variables and for every imputation method employed.

### Unsupervised Clustering
Use the `unsupervised_clustering` function for unsupervised clustering to identify natural groupings in the data without predefined labels.

#### Arguments

| Parameter         | Description                                               | Default |
|-------------------|-----------------------------------------------------------|---------|
| input_data_list    |  A named list of dataframes that will be used for the clustering. | None |  
| varselect | A named list of vectors of variables that you want to iterate over for clustering. Uses all variables by default (specific by a named "all" string). For example, to use age, sex, and bmi in one round and just age and bmi in another, you should specify a list as follows: list(group1 = c('age','sex','bmi'),group2 = c('age','bmi')) | list(all = 'all') |
| methods | A vector of all the clustering methods you want to employ.|  c('kmeans','hierarchical','dbscan','vae') |
| params| A large named list, where each name is a different clustering method, of all the parameters you want used | list(vae = list(latent_dim = 2, hidden_dim = 128, epochs = 10, batch_size = 32, n_clusters = 5, seed = 42),dbscan = list(eps = c("0.5", "0.75"), minPts = c("2", "3")), hierarchical = list(cut_quantile = c(".5"), cutpoint = c("3"), kcut = c("5")), kmeans = list(n_clusters = c("2", "3"))).|

Clustering parameters for each method:

| Method       | Parameter    | Description                                                              |
|--------------|--------------|--------------------------------------------------------------------------|
| VAE          | latent_dim   | Dimensionality of the latent space.                                       |
| VAE          | hidden_dim   | Number of hidden units in the encoder and decoder.                        |
| VAE          | epochs       | Number of epochs to train the VAE model.                                  |
| VAE          | batch_size   | Number of samples per gradient update.                                    |
| VAE          | n_clusters   | Number of clusters to form in the latent space.                           |
| VAE          | seed         | Random seed for reproducibility.                                          |
| DBSCAN       | eps          | The maximum distance between two samples for them to be considered as in the same neighborhood. |
| DBSCAN       | minPts       | The minimum number of samples in a neighborhood for a point to be considered a core point. |
| Hierarchical | cut_quantile | The quantile used to cut the dendrogram (one of three methods of building hierarchical clusters).  |
| Hierarchical | cutpoint     | The number of clusters or groups to cut the dendrogram into (one of three methods of building hierarchical clusters).              |
| Hierarchical | kcut         | Number of clusters to use for k-means initialization in hierarchical clustering (one of three methods of building hierarchical clusters). |
| K-Means      | n_clusters   | Number of clusters to form in k-means clustering.                                               |

After the clustering step, you can coerce the output into standardized dataframes that could be compared (e.g., shown on a series of PCA plots or something similar) in order to identify co-occurence between individuals and/or stability of cluster. The function for doing so is `standardize_cluster_output`, and it take the output of the unsupervised clustering directly, returning a cleaned named list.

```r
# Perform unsupervised clustering and then cleans the output 
results_unsupervised <- unsupervised_clustering(imputed_data)
results_unsupervised_clean <- standardize_cluster_output(results_unsupervised)
```
#### Output

Returns the raw clustering (unsupervised_clustering) processed (standardize_cluster_output) output for every method in a named list.

###


## Dependencies
This package requires the following R packages to function properly:

- `dplyr`
- `mice`
- `torch`
- `pre`
- `readr`
- `stats`
- `cluster`
- `vegan`
- `dbscan`
- `mclust`

Install any missing dependencies with the following command:

```r
install.packages(c("dplyr", "mice", "torch", "pre", "readr", "stats", "cluster", "vegan", "dbscan", "mclust"))
```

## License
This package is licensed under the MIT License. See the LICENSE file for more details.
