prepare_pca_and_clusters <- function(raw_data, clusters) {
  # Scale and perform PCA on numeric columns of raw data
  numeric_data <- raw_data %>%
    select(where(is.numeric)) %>%
    scale()
  
  pca_result <- PCA(numeric_data, graph = FALSE)
  dists <- dist(numeric_data) # Compute distances once
  
  # Compute silhouette distances for each clustering method
  silhouette_results <- lapply(clusters, function(cluster_column) {
    as.numeric(as.character(cluster_column)) %>% silhouette(dists)
  })
  
  list(
    pca_result = pca_result,
    clusters = clusters,
    silhouettes = silhouette_results
  )
}

#' Run PCA and Clustering Analysis
#'
#' Perform PCA and compute related outputs, including clustering and silhouette distances.
#'
#' @param raw_data A data frame containing the raw data.
#' @param clusters A data frame or matrix with clustering results for different methods.
#'
#' @import dplyr
#' @import FactoMineR
#' @importFrom cluster silhouette
#' @export
run_pca <- function(raw_data, clusters) {
  # Perform PCA and clustering analysis
  pca_and_cluster_data <- prepare_pca_and_clusters(raw_data, clusters)
  
  return(pca_and_cluster_data)
}

#' Launch a Shiny app to explore PCA and clustering results interactively.
#'
#' @param raw_data A data frame containing the raw data.
#' @param pca_output A list containing the PCA result, clustering information, and silhouette results.
#' @param params A data frame of parameters for methods and datasets.
#'
#' @import shiny
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import FactoMineR
#' @import factoextra
#' @import cluster
#' @import RColorBrewer
#' @import DT
#' @importFrom stats dist
#' @importFrom zip zip
#' @export
shinify <- function(raw_data, pca_output, params) {
  
  # Extract PCA result, clusters, and silhouette results
  pca_result <- pca_output$pca_result
  cluster_data <- pca_output$clusters
  silhouette_results <- pca_output$silhouettes
  # Start Shiny app
  shinyApp(
    ui = fluidPage(
      tags$head(
        tags$style(HTML("
    body { background-color: #121212; color:gray}
    table.dataTable { background-color: #2b2b2b; color: white; }
    table.dataTable td, table.dataTable th { color: white !important; }
    .dataTables_wrapper .dataTables_paginate .paginate_button { color: white !important; }
    .dataTables_wrapper .dataTables_paginate .paginate_button:hover { background-color: #555555 !important; }
    .form-control, .selectize-input, .btn { color: black !important; background-color: #f5f5f5 !important; }
    .title-panel { color: white !important; }
"))
        
      ),
      titlePanel("PCA and Clustering Exploration"),
      sidebarLayout(
        sidebarPanel(
          conditionalPanel(
            condition = "input.tabs !== 'Parameter Descriptions'",
            # Dropdown for Dataset
            selectInput("datasetname", "Choose datasetname", 
                        choices = unique(params$datasetname)),
            
            # Dropdown for Method
            selectInput("method", "Choose Method", 
                        choices = colnames(cluster_data)), # Dynamically updated to clustering methods
            
            # Dynamic parameter inputs
            uiOutput("parameter_ui"),
            
            # Download button
            downloadButton("download_plots", "Download All Plots"),
          ),
          conditionalPanel(
            condition = "input.tabs === 'Parameter Descriptions'",
            h4("Lorem ipsum dolor sit amet, consectetur adipiscing elit.")
          ),
          width = 3
        ),
        mainPanel(
          tabsetPanel(
            id = "tabs", # Add ID for conditionalPanel to work
            tabPanel("Parameter Descriptions", 
                     DTOutput("selected_params")
            ),            
            tabPanel("PCA Analysis", 
                     plotOutput("pca_plot"),
                     plotOutput("variance_explained_plot"),
                     plotOutput("cluster_distribution_plot")
            ),
            tabPanel("Cluster Characteristics", 
                     uiOutput("cluster_plots"),
                     plotOutput("heatmap_plot")
            ),
            tabPanel("Silhouette Analysis", 
                     plotOutput("silhouette_plot"),
                     plotOutput("silhouette_comparison_plot")
            ),
            tabPanel("Gap Statistic", plotOutput("gap_stat_plot")),
          )
        )
      )
    ),
    
    server = function(input, output, session) {
      
      ### Step 1: User selects datasetname
      observeEvent(input$datasetname, {
        updateSelectInput(session, "method", 
                          choices = colnames(cluster_data))
        output$parameter_ui <- renderUI(NULL) # Reset parameters on method change
      })
      
      # Display parameters for selected method
      output$selected_params <- renderDT({
        req(input$method)
        params %>% select(parameter_set,method,parameter,value)
      })
      
      ### Step 2: PCA plot
      output$pca_plot <- renderPlot({
        req(input$method)
        cluster_labels <- cluster_data[[input$method]]
        pca_df <- data.frame(pca_result$ind$coord, Cluster = factor(cluster_labels))
        
        ggplot(pca_df, aes(x = Dim.1, y = Dim.2, color = Cluster)) +
          geom_point(size = 3) +
          scale_color_manual(values = scales::hue_pal()(length(unique(cluster_labels)))) +
          labs(title = "PCA of Raw Data by Clusters", x = "PC1", y = "PC2", color = "Cluster") +
          theme_minimal(base_family = "Arial", base_size = 12) +
          theme(panel.background = element_rect(fill = "black"),
                plot.background = element_rect(fill = "black"),
                legend.background = element_rect(fill = "gray20"),
                legend.text = element_text(color = "white"),
                legend.title = element_text(color = "white"),
                axis.text = element_text(color = "white"),
                axis.text.x = element_text(color = 'white',angle=60,hjust =1),
                axis.title = element_text(color = "white"),
                plot.title = element_text(color = "white", face = "bold"))
      })
      
      ### Step 3: Cluster characteristic plots
      output$cluster_plots <- renderUI({
        numeric_vars <- raw_data %>%
          select(where(is.numeric)) %>%
          gather(variable, value)
        
        plots <- lapply(unique(numeric_vars$variable), function(var) {
          plotOutput(paste("plot_", var, sep = ""))
        })
        
        do.call(tagList, plots)
      })
      
      ### Step 4: Individual cluster plots
      observe({
        numeric_vars <- raw_data %>%
          select(where(is.numeric)) %>%
          gather(variable, value)
        
        lapply(unique(numeric_vars$variable), function(var) {
          output[[paste("plot_", var, sep = "")]] <- renderPlot({
            req(input$method)
            cluster_labels <- cluster_data[[input$method]]
            ggplot(numeric_vars %>% filter(variable == var), 
                   aes(x = factor(cluster_labels), y = value, fill = factor(cluster_labels))) +
              geom_boxplot() +
              scale_fill_manual(values = scales::hue_pal()(length(unique(cluster_labels)))) +
              labs(title = paste("Boxplot for", var, "by Cluster"), x = "Cluster", y = var, fill = "Cluster") +
              theme_minimal(base_family = "Arial", base_size = 12) +
              theme(panel.background = element_rect(fill = "black"),
                    plot.background = element_rect(fill = "black"),
                    legend.background = element_rect(fill = "gray20"),
                    legend.text = element_text(color = "white"),
                    legend.title = element_text(color = "white"),
                    axis.text = element_text(color = "white"),
                    axis.title = element_text(color = "white"),
                    plot.title = element_text(color = "white", face = "bold"))
          })
        })
      })
      
      ### Step 5: Silhouette plot
      output$silhouette_plot <- renderPlot({
        req(input$method)
        silhouette_info <- silhouette_results[[input$method]]
        fviz_silhouette(silhouette_info) +
          labs(title = paste("Silhouette Plot for", input$method), x = NULL, fill = "Cluster") +
          theme_minimal(base_family = "Arial", base_size = 12) +
          theme(panel.background = element_rect(fill = "black"),
                plot.background = element_rect(fill = "black"),
                legend.background = element_rect(fill = "gray20"),
                legend.text = element_text(color = "white"),
                legend.title = element_text(color = "white"),
                axis.text = element_text(color = "white"),
                axis.text.x = element_blank(),
                axis.title = element_text(color = "white"),
                plot.title = element_text(color = "white", face = "bold"))
      })
      
      ### Step 6: Gap Statistic plot
      output$gap_stat_plot <- renderPlot({
        req(input$method)
        gap_stat <- cluster::clusGap(as.matrix(raw_data %>% select(where(is.numeric))), FUN = kmeans, K.max = 25)
        fviz_gap_stat(gap_stat) +
          theme_minimal(base_family = "Arial", base_size = 12) +
          theme(panel.background = element_rect(fill = "black"),
                plot.background = element_rect(fill = "black"),
                axis.text = element_text(color = "white"),
                axis.title = element_text(color = "white"),
                plot.title = element_text(color = "white", face = "bold")) +
          labs(title = "Gap Statistic Plot")
      })
      
      ### Step 7: Cluster Heatmap
      output$heatmap_plot <- renderPlot({
        req(input$method)
        cluster_labels <- cluster_data[[input$method]]
        heatmap_data <- raw_data %>% select(where(is.numeric))
        heatmap_data$Cluster <- factor(cluster_labels)
        
        ggplot(heatmap_data %>% gather(variable, value, -Cluster), aes(x = variable, y = Cluster, fill = value)) +
          geom_tile() +
          scale_fill_viridis_c() +
          labs(title = "Cluster Heatmap", x = "Feature", y = "Cluster") +
          theme_minimal(base_family = "Arial", base_size = 12) +
          theme(panel.background = element_rect(fill = "black"),
                plot.background = element_rect(fill = "black"),
                axis.text = element_text(color = "white"),
                axis.title = element_text(color = "white"),
                plot.title = element_text(color = "white", face = "bold"))
      })
      
      ### Step 8: Silhouette Scores Comparison
      output$silhouette_comparison_plot <- renderPlot({
        avg_silhouette <- sapply(silhouette_results, function(sil) mean(sil[, 3]))
        comparison_df <- data.frame(Method = names(avg_silhouette), Avg_Silhouette = avg_silhouette)
        
        ggplot(comparison_df, aes(x = Method, y = Avg_Silhouette, fill = Method)) +
          geom_bar(stat = "identity") +
          scale_fill_manual(values = scales::hue_pal()(length(avg_silhouette))) +
          labs(title = "Silhouette Scores Comparison", x = "Method", y = "Average Silhouette", fill = "Method") +
          theme_minimal(base_family = "Arial", base_size = 12) +
          theme(panel.background = element_rect(fill = "black"),
                plot.background = element_rect(fill = "black"),
                legend.background = element_rect(fill = "gray20"),
                legend.text = element_text(color = "white"),
                legend.title = element_text(color = "white"),
                axis.text = element_text(color = "white"),
                axis.text.x = element_text(color = 'white',angle=60,hjust =1),
                axis.title = element_text(color = "white"),
                plot.title = element_text(color = "white", face = "bold"))
      })
      
      ### Step 9: Cluster Distribution
      output$cluster_distribution_plot <- renderPlot({
        req(input$method)
        cluster_labels <- cluster_data[[input$method]]
        cluster_df <- data.frame(Cluster = factor(cluster_labels))
        
        ggplot(cluster_df, aes(x = Cluster, fill = Cluster)) +
          geom_bar() +
          scale_fill_manual(values = scales::hue_pal()(length(unique(cluster_labels)))) +
          labs(title = "Cluster Distribution", x = "Cluster", y = "Count", fill = "Cluster") +
          theme_minimal(base_family = "Arial", base_size = 12) +
          theme(panel.background = element_rect(fill = "black"),
                plot.background = element_rect(fill = "black"),
                legend.background = element_rect(fill = "gray20"),
                legend.text = element_text(color = "white"),
                legend.title = element_text(color = "white"),
                axis.text = element_text(color = "white"),
                axis.title = element_text(color = "white"),
                plot.title = element_text(color = "white", face = "bold"))
      })
      
      ### Step 10: Variance Explained
      output$variance_explained_plot <- renderPlot({
        variance_df <- data.frame(PC = seq_along(pca_result$eig[, 2]), Variance = pca_result$eig[, 2])
        
        ggplot(variance_df, aes(x = PC, y = Variance, fill = as.factor(PC))) +
          geom_bar(stat = "identity") +
          scale_fill_manual(values = scales::hue_pal()(length(variance_df$PC))) +
          labs(title = "Variance Explained by Principal Components", x = "Principal Component", y = "Variance (%)", fill = "Principal Component") +
          theme_minimal(base_family = "Arial", base_size = 12) +
          theme(panel.background = element_rect(fill = "black"),
                plot.background = element_rect(fill = "black"),
                legend.background = element_rect(fill = "gray20"),
                legend.text = element_text(color = "white"),
                legend.title = element_text(color = "white"),
                axis.text = element_text(color = "white"),
                axis.title = element_text(color = "white"),
                plot.title = element_text(color = "white", face = "bold"))
      })
      
      ### Step 11: Download Plots
      output$download_plots <- downloadHandler(
        filename = function() {
          paste0("plots_paramset_", input$method, ".zip")
        },
        content = function(file) {
          temp_dir <- tempdir()
          file_paths <- c()
          
          # Save each plot as a PNG
          ggsave(filename = file.path(temp_dir, "pca_plot.png"), plot = output$pca_plot())
          ggsave(filename = file.path(temp_dir, "variance_explained.png"), plot = output$variance_explained_plot())
          ggsave(filename = file.path(temp_dir, "cluster_distribution.png"), plot = output$cluster_distribution_plot())
          
          zip::zip(zipfile = file, files = dir(temp_dir, full.names = TRUE))
        }
      )
    }
  )
}
