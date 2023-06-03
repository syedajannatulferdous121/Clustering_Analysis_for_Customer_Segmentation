# Load required libraries
library(dplyr)
library(ggplot2)
library(cluster)
library(factoextra)
library(fpc)
library(Rtsne)
library(plotly)

# Load customer data
customer_data <- read.csv("path/to/your/customer_data.csv")

# Perform data preprocessing
preprocessed_data <- customer_data %>%
  select(-CustomerID) %>%
  scale()

# Determine the optimal number of clusters using the Elbow Method
wss <- sapply(1:10, function(k) kmeans(preprocessed_data, centers = k)$tot.withinss)
plot(1:10, wss, type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters", ylab = "Total Within-Cluster Sum of Squares")
# Add code for user input to select the optimal number of clusters based on the plot

# Perform clustering analysis using K-means algorithm with the selected number of clusters
k <- 5  # Replace with the selected number of clusters
kmeans_model <- kmeans(preprocessed_data, centers = k, nstart = 25)

# Assign cluster labels to the original data
cluster_labels <- kmeans_model$cluster
customer_data$Cluster <- cluster_labels

# Perform dimensionality reduction using t-SNE
tsne_data <- Rtsne(preprocessed_data, perplexity = 30, dims = 2, verbose = TRUE)

# Visualize the clustered data using interactive plotly scatter plot
plot_data <- cbind(tsne_data$Y, cluster_labels)
plot_df <- as.data.frame(plot_data)
colnames(plot_df) <- c("X", "Y", "Cluster")
plotly_plot <- plot_ly(plot_df, x = ~X, y = ~Y, color = ~factor(Cluster), type = "scatter", mode = "markers",
                       marker = list(size = 8, opacity = 0.8))
plotly_plot <- plotly_plot %>% layout(title = "Customer Segmentation",
                                      xaxis = list(title = "Dimension 1"),
                                      yaxis = list(title = "Dimension 2"))

# Calculate cluster statistics
cluster_stats <- customer_data %>%
  group_by(Cluster) %>%
  summarise(Avg_Spending = mean(Spending),
            Avg_Frequency = mean(Frequency))

# Print cluster statistics
print(cluster_stats)
