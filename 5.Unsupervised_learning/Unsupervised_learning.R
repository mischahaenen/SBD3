# ---------------------------------------------------------------#
# ---------------------------------------------------------------#
# ------- Comparing the different clustering methods ------------#
# ---------------------------------------------------------------#
# ---------------------------------------------------------------#

# The purpose of this script is to train and evaluate clustering algorithms on
# real datasets. Specifically, we will learn how to run:
# - k-means clustering
# - hierarchical clustering
# - DBScan
# - mixed data clustering (Gower distance)

# Install packages if needed
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("cluster")
install.packages("factoextra")
install.packages("mlbench")
# install.packages("tidyr")
# install.packages("readr")
# install.packages("FactoMineR")
install.packages("dbscan")

# Load necessary packages
library(ggplot2)
library(dplyr)
library(cluster)
library(factoextra)
library(mlbench)
library(tidyr)
library(readr)
library(FactoMineR)
library(dbscan)

# Load data
setwd("~/Library/CloudStorage/OneDrive-SharedLibraries-BernerFachhochschule/bfh-bfhinternal - General/SBD3/Week 9")
getwd()
Mall_Customers <- read_csv("../5.Unsupervised_learning/Mall_Customers.csv")
data <- Mall_Customers

# Data exploration
str(data)
summary(data)

# Step 1 - Scaling
# Scaling is often needed for clustering because most clustering algorithms are
# distance-based, which means that they calculate the distance between data points
# to determine their similarity or dissimilarity. If the data is not scaled,
# variables with larger scales or variances will dominate the distance calculations,
# and variables with smaller scales or variances will have less impact on the clustering
# results. This can lead to biased or inaccurate clustering results.

# The scale function typically performs two types of scaling:

# ** Centering: This involves subtracting the mean of each variable from the data points.
#               This transformation shifts the mean of each variable to zero.
# ** Scaling:   After centering, each variable can also be scaled (typically by dividing by
#               the standard deviation). This process changes the spread of the data so that
#               each variable has a standard deviation of one.
scaled_data <- scale(data)

# Check the scaled data
summary(scaled_data)


# ----------------- K-means Clustering ------------------- #
# Now we are ready for running the k-means algorithm
set.seed(123)

# Run k-means analysis
# To start, we set k=3. This means we want to divide the mall customers into 3
# groups.
k <- 3
kmeans_model <- kmeans(scaled_data, centers = k)

# Summary of k-means model.
# Note on fit: Ideally you want a clustering that has the properties of internal
# cohesion and external separation, i.e. the between_SS/total_SS ratio should
# approach 1.
kmeans_model
kmeans_model[["centers"]]
kmeans_model[["size"]]

# Let's see what are the mean values of the variables per the individual clusters.
aggregate(data, by = list(cluster = kmeans_model$cluster), mean)

# Let's visualize this:
data_sum <- aggregate(data, by = list(cluster = kmeans_model$cluster), mean)

# Reshape the data to long format
df_long_1 <- data_sum %>%
    pivot_longer(
        cols = c("Age", "Annual Income (k$)", "Spending Score (1-100)"),
        names_to = "variable", values_to = "mean_value"
    )

# Plot the mean values by cluster and variable
ggplot(df_long_1, aes(x = cluster, y = mean_value, fill = factor(variable))) +
    geom_col(position = "dodge") +
    scale_fill_discrete(name = "Variables") +
    labs(title = "Mean Values by Cluster and Variable", x = "Clusters", y = "Mean Value")

# Do you think this is a nice division?

# Let's evaluate this more formally. We start with the elbow method.
# Let's create and empty data frame that will contain 2 columns: one, will be
# k that will contain the values from 1 to 10 and second will be the total_withinss
# which will be empty.
elbow_data <- data.frame(
    k = 1:10,
    total_withinss = numeric(10)
)

# Next, for each value for k (that can go from 1 to 10), we run the k-means algorithm
# with the specific number of clusters and we extract the total within-cluster sum of squares
# for each into our elbow_data object.
for (i in 1:10) {
    set.seed(123)
    km <- kmeans(scaled_data, centers = i)
    elbow_data[i, "total_withinss"] <- km$tot.withinss
}

# We plot the elbow method.
ggplot(elbow_data, aes(x = k, y = total_withinss)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = seq(1, 10, 1)) +
    labs(
        title = "Elbow Method", x = "Number of Clusters",
        y = "Total Within-Cluster Sum of Squares"
    ) +
    theme_minimal()


# Which number of clusters would you choose?


# Let's plot the clusters in 2D. For this we will need to run a PCA.


# ----------------- PCA Analysis ------------------- #
# Run PCA and extract principal components
pca_fit <- PCA(scaled_data, graph = FALSE)
pc1 <- pca_fit$ind$coord[, 1]
pc2 <- pca_fit$ind$coord[, 2]

# Let's check the summary.
# Remember: the PCs are linear combinations of the original variables.
summary(pc1)
summary(pc2)

# Let's evaluate the PCA
# One of the attributes of the pca_fit object is eig.
# This is what tell us the eigenvalues of the principal components.
# Eigenvalues are a measure of the amount of variance explained by each principal
# component.
pca_fit$eig

# Cumulative variance explained by first two principal components.
# You can read it from pca_fit$eig or run the next two lines.
cumulative_var <- cumsum(pca_fit$eig[1:2, 2]) / sum(pca_fit$eig[, 2])
cat("Cumulative variance explained by PC1 and PC2:", round(cumulative_var[2], 2), "\n")

# Variable contribution in PC1 and PC2
# The pca_fit$var$contrib provides a way to understand how the original variables
# contribute to the principal components, which can help with interpretation
# and understanding of the PCA results.

# Specifically, the var$contrib: contains the contributions (in percentage) of
# the variables to the principal components.
pca_fit$var$contrib

# Which variable(s) primarily drives PC1 (Dim 1)? Which variable primarily drives PC2?
# Which variable(s) primarily drives PC1 (Dim 3)?

# Let's run the k-means with the desired number of clusters
k <- 4
set.seed(123)
kmeans_model <- kmeans(scaled_data, centers = k)

# Create data frame with cluster assignments and principal components
cluster_data <- data.frame(cluster = as.factor(kmeans_model$cluster), pc1, pc2)

# Create scatter plot with points colored by cluster assignment
ggplot(cluster_data, aes(pc1, pc2, color = cluster)) +
    geom_point() +
    labs(color = "Cluster") +
    theme_minimal()


# ----------------- Hierarchical clustering ------------------- #
# Using the same data, let's run hierarchical clustering.

# We start by computing the dissimilarity matrix
# When dist() is called on a matrix or data frame, it returns a vector containing
# the pairwise distances between the rows, where the distances are arranged in the
# order of the lower-triangular portion of a distance matrix.
dist_matrix <- dist(scaled_data)

# Run hierarchical clustering with single linkage
hc_single <- hclust(dist_matrix, method = "single")

# Run hierarchical clustering with complete linkage
hc_complete <- hclust(dist_matrix, method = "complete")

# Let's plot
# Single
plot(hc_single,
    hang = -1, main = "Single Linkage Dendrogram", cex = 0.2,
    labels = rownames(scaled_data)
)

# Add color to the branches
rect.hclust(hc_single, k = 4, border = "red")

# Complete
plot(hc_complete,
    hang = -1, main = "Complete Linkage Dendrogram", cex = 0.2,
    labels = rownames(scaled_data)
)

# Add color to the branches
rect.hclust(hc_complete, k = 4, border = "red")


#----------------- Mixed Data --------------------#
# Load a dataset with mixed data
data(BreastCancer)
?BreastCancer

# Summary statistics
summary(BreastCancer)
str(BreastCancer)

# Preprocess the data
bc_data <- BreastCancer[, -1] # Remove the first column (ID)
bc_data <- na.omit(bc_data) # Remove rows with missing values

# Calculate Gower distance matrix
# Remember: Gower distance is a measure of dissimilarity that takes into account
# the scale of the variables. It can handle variables of different types and
# scales, including continuous, categorical, and binary variables. Unlike some
# other distance metrics, such as Euclidean distance or Manhattan distance,
# Gower distance does not assume that variables are on the same scale.

# If the variables in the dataset are measured in different units or
# have vastly different ranges, it may be beneficial to scale them to ensure that
# they are all on the same scale and contribute equally to the clustering analysis.

# The daisy() function in R to calculate a dissimilarity matrix using the Gower
# distance metric.
gower_dist <- daisy(bc_data, metric = "gower")

# Perform hierarchical clustering on Gower distance matrix
# In this case, method = "ward.D2" specifies the Ward's minimum variance method
# with the squared Euclidean distance metric.
hc <- hclust(gower_dist, method = "ward.D2")

# Plot dendrogram. Try changing the k and observe the change in k.
fviz_dend(hc, cex = 0.2, k = 4, labels_track_height = 4)

# Cut dendrogram into clusters
clusters <- cutree(hc, k = 4)

# View the number of observations in each cluster
table(clusters)

# Let's calculate some summary statistics
bc_data$clusters <- clusters

# Create a copy
data_sum <- bc_data

# Identify factor variables (excluding ordered factors and the Class variable)
factor_vars <- sapply(data_sum, is.factor) & !sapply(data_sum, is.ordered) &
    !names(data_sum) %in% c("Class")

# Convert factor variables to numeric using lapply()
data_sum[factor_vars] <- lapply(data_sum[factor_vars], as.numeric)

# Recheck the summary
summary(data_sum)

# View summary of each cluster
summary_by_cluster <- data_sum %>%
    select_if(is.numeric) %>%
    group_by(clusters) %>%
    summarise_all(mean)

# Reshape the data to long format
df_long <- summary_by_cluster %>%
    pivot_longer(
        cols = c("Bare.nuclei", "Bl.cromatin", "Normal.nucleoli", "Mitoses"),
        names_to = "variable", values_to = "mean_value"
    )

# Plot the mean values by cluster and variable
ggplot(df_long, aes(x = clusters, y = mean_value, fill = factor(variable))) +
    geom_col(position = "dodge") +
    scale_fill_discrete(name = "Variables") +
    labs(title = "Mean Values by Cluster and Variable", x = "Variable", y = "Mean Value")


# ------------------ DBScan --------------------- #
# Run DBScan clustering on the previous mall customers (scaled) data  with a
# default value for the eps = 0.5
dbscan_fit <- dbscan(scaled_data, eps = 0.5, minPts = 5)

# Plot the results using 2 of the original variables
df_dbscan <- data.frame(x = scaled_data, cluster = dbscan_fit$cluster)
ggplot(df_dbscan, aes(x = df_dbscan[, 1], y = df_dbscan[, 2], color = factor(cluster))) +
    geom_point(size = 2) +
    ggtitle("DBSCAN Clustering Results") +
    xlab("Age") +
    ylab("Annual Income")

# Plot the results using the principle components
df_dbscan_pc <- data.frame(x = pc1, y = pc2, cluster = dbscan_fit$cluster)
ggplot(df_dbscan_pc, aes(x = x, y = y, color = factor(cluster))) +
    geom_point(size = 2) +
    ggtitle("DBSCAN Clustering Results") +
    xlab("PC1") +
    ylab("PC2")

# --------------------------------- END -------------------------------------- #
