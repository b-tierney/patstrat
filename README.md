# clinclust

**R Package for Identifying Subpopulations of Individuals in Omic and Clinical Data**

## Overview

`clinclust` is an R package designed to identify patient subpopulations in 'omic and clinical datasets. It provides a series of simple functions to preprocess data, impute missing values, and cluster individuals using either supervised or unsupervised approaches.

The purpose is to compare numerous imputation and clustering methods simlultaneously so you can evalution the robustness of clusters to analytic choice. This is akin to our prior work with Vibration of Effects, where we compared how model specification changes observed association outcomes.

Clinclust has two modes: supervised and unsupervised. Both can be run on either all variables in a dataset or specific subsets (e.g., just age, sex, bmi in one group, then just age and sex in another) so the impact of including different variables on patient stratification can be observed. 

### Supervised mode

In supervised mode, the goal is to identify subpopulations directly as a function of given depedent variable (e.g., response to treatment). Clinclust uses RuleFit regression (wrapping the Pre package) to build regression-based decision trees for separating patients into groups based on their characteristics. The goal in running Clinclust in supervised mode is to define patient groups that are meaningful in the context of treatment or disease (e.g., with BMI > X and the abundance of metabolite Y > X, patients are likely to respond to a given drug).

### Unsupervised mode

In unsupervised mode, Clinclust can use multiple imputation (mean, median, MICE, a variational autoencoder) and clustering (kmeans, DBSCAN, hierarchical, variational autoencoder coupled with a gaussian mixture model) methods to sort patients into groups, without being guided by a potential response variable. The total set of imputation methods and clustering approaches are then returned and can be compared on, say, a PCA plot or other visualization.

In future versions of Clinclust, we will implement a shinify function that generates a shiny app for a given output data structure. 


## Installation

To install this package locally from your machine, you can use the following steps:

1. Clone the repository or download the package files.
   
2. Install the package using `devtools`:

   ```r
   # If devtools is not installed, install it first
   install.packages("devtools")
   
   # Install the package
   devtools::install_local("/path/to/clinclust")
   ```

Replace `/path/to/clinclust` with the actual path to the package directory on your system.

## Usage
The workflow involves three main steps: preprocessing, imputation, and either supervised or unsupervised clustering.

### Step 1: Preprocess the Data
Use the `preprocess_data` function to normalize and prepare the dataset for analysis.

#### Parameters

| Parameter         | Description                                               | Default |
|-------------------|-----------------------------------------------------------|---------|
| data              | The input dataset to preprocess.                          | None    |
| missing_values    | Strings that indicate missing values (eg NA, none, Null)     | None    |
| continuous_cutoff | The max number of levels in a column before it is converted to a continuous variable | 10  |

#### Output

The processed dataframe as well as a dataframe ($processed_data) summarizing the included variables ($summary_statistics).

```r
# Load the package
library(clinclust)

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

Returns a list of imputed dataframes where the names correspond to the methods and the parameters used.

```r
# Impute missing values
imputed_data <- impute(iris_preprocessed$processed_data,methods = c('mice','knn'),ks = c(6,7,8,9,10))
```

### Step 3: Clustering
Once the data has been imputed, you can proceed with either supervised or unsupervised clustering.


#### Supervised Clustering
Use the `supervised` function to perform supervised clustering based on predefined labels.

```r
# Perform supervised clustering
results_supervised <- supervised(imputed_data, labels = "path/to/labels.csv")
```

#### Unsupervised Clustering
Use the `cluster_modified` function for unsupervised clustering to identify natural groupings in the data without predefined labels.

```r
# Perform unsupervised clustering
results_unsupervised <- cluster_modified(imputed_data)
```








## Example Workflow
Below is a complete example from preprocessing the data to clustering:

```r
# Load the package
library(clinclust)

# Step 1: Preprocess the data
preprocessed_data <- preprocess_data(input_file = "path/to/data.csv")

# Step 2: Impute missing values
imputed_data <- impute(preprocessed_data)

# Step 3a: Supervised clustering
results_supervised <- supervised(imputed_data, labels = "path/to/labels.csv")

# Step 3b: Unsupervised clustering (optional, choose either supervised or unsupervised)
results_unsupervised <- cluster_modified(imputed_data)
```

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
