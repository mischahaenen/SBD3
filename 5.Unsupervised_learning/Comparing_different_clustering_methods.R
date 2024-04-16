# ---------------------------------------------------------------#
# ---------------------------------------------------------------#
# ------- Comparing the different clustering methods ------------#
# ---------------------------------------------------------------#
# ---------------------------------------------------------------#

# The purpose of this script is to compare how different clustering methods
# would perform on different data shapes. We will generate 5 different datasets
# and check how different clustering methods (k-means clustering, hierarchical
# clustering with single and complete linkage and DBScan) would cluster the
# data points.

# We start with a clean environment
rm(list = ls())

# Calling the necessary libraries
library(ggplot2)
library(cluster) # Widely used for cluster analysis, providing a comprehensive collection of clustering algorithms and utilities to handle clustering tasks in R.
library(fpc) # Flexible Procedures for Clustering, provides various methods and algorithms for cluster analysis and unsupervised classification.
library(dbscan) # Specifically designed for density-based clustering and includes the DBSCAN (Density-Based Spatial Clustering of Applications with Noise) algorithm.


# ------------- Generate different data shapes ------------------#

# DATA 1: Generate random data
set.seed(123)
x1 <- rnorm(200, mean = 0, sd = 1)
y1 <- rnorm(200, mean = 0, sd = 1)
data1 <- data.frame(x1, y1)

# Visualize the random data (DATA 1)
ggplot(data1, aes(x = x1, y = y1)) +
    geom_point() +
    ggtitle("Random data")


# DATA 2: Generate data with three blobs
set.seed(123)
x2 <- c(rnorm(200, mean = -2, sd = 0.5), rnorm(100, mean = 0, sd = 0.5), rnorm(100, mean = 2, sd = 0.5))
y2 <- c(rnorm(200, mean = -2, sd = 0.5), rnorm(100, mean = 0, sd = 0.5), rnorm(100, mean = 2, sd = 0.5))
data2 <- data.frame(x2, y2)

# Visualize the three bloobs data (DATA 2)
ggplot(data2, aes(x = x2, y = y2)) +
    geom_point() +
    ggtitle("Data with three blobs")


# DATA 3: Generate data with two moons
data(moons)
data3 <- moons

# Visualize the moons data (DATA 3)
ggplot(data3, aes(x = X, y = Y)) +
    geom_point() +
    ggtitle("Moons Dataset")


# DATA 4: Generate data with two circles
set.seed(123)
theta <- seq(0, 2 * pi, length.out = 200)
x4 <- c(2 * cos(theta), 4 * cos(theta))
y4 <- c(2 * sin(theta), 4 * sin(theta))
data4 <- data.frame(x4, y4)

# Visualize the two circles data (DATA 4)
ggplot(data4, aes(x = x4, y = y4)) +
    geom_point() +
    ggtitle("Data with two circles")


# DATA 5: Generate data with 3 clusters of different variance
set.seed(123)
data5_1 <- data.frame(x = rnorm(100, mean = -3, sd = 0.5), y = rnorm(100, mean = 0, sd = 0.5))
data5_2 <- data.frame(x = rnorm(100, mean = 0, sd = 1), y = rnorm(100, mean = 0, sd = 1))
data5_3 <- data.frame(x = rnorm(100, mean = 5, sd = 3), y = rnorm(100, mean = 0, sd = 1))
data5 <- rbind(data5_1, data5_2, data5_3)

# Visualize the data with 3 clusters of different variance (DATA 5)
ggplot(data5, aes(x = x, y = y)) +
    geom_point() +
    ggtitle("Data with 3 clusters of different variance")


# ------------- Compare different clustering methods ------------------#
# We set the seed so to ensure reproducible results
set.seed(123)


# ------------- DATA 1 (Random data points) ------------------#
# We start with the random data
# K-means clustering
k <- 3
kmeans_data1 <- kmeans(data1, k)

# Hierarchical clustering with single linkage
hier_data1_single <- hclust(dist(data1), method = "single")
clusters_data1_single <- cutree(hier_data1_single, k = k)

# Hierarchical clustering with complete linkage
hier_data1_comp <- hclust(dist(data1), method = "complete")
clusters_data1_comp <- cutree(hier_data1_comp, k = k)

# DBScan
# Notice here we do not set the number of clusters but rather the eps and minPts
# parametars.

# Some explanations:
# - eps (short for epsilon) is a distance metric that defines the maximum distance
# between two data points for them to be considered as part of the same cluster.

# - minPts is the minimum number of data points required to form a cluster.
# Specifically, a data point is considered to be part of a cluster if it has at
# least minPts number of other data points within a distance of eps from it.
dbscan_data1 <- dbscan::dbscan(data1, eps = 0.5, minPts = 5)

# Let's visualize the different clustering
# K-means
ggplot(data1, aes(x = x1, y = y1, color = factor(kmeans_data1[["cluster"]]))) +
    geom_point() +
    labs(title = "K-means Clustering of Random Data", color = "Clusters")

# Hierarchical clustering with single linkage
ggplot(data1, aes(x = x1, y = y1, color = factor(clusters_data1_single))) +
    geom_point() +
    theme_classic() +
    labs(title = "Hierarchical Clustering with Single Linkage of Random Data", color = "Clusters")

# Hierarchical clustering with complete linkage
ggplot(data1, aes(x = x1, y = y1, color = factor(clusters_data1_comp))) +
    geom_point() +
    theme_classic() +
    labs(title = "Hierarchical Clustering with Complete Linkage of Random Data", color = "Clusters")

# DBScan
ggplot(data1, aes(x = x1, y = y1, color = factor(dbscan_data1[["cluster"]]))) +
    geom_point() +
    theme_classic() +
    labs(title = "DBScan Clustering of Random Data", color = "Clusters")

# Note: In R's implementation of DBSCAN, the dbscan() function assigns the value
# 0 to the cluster membership of points that are classified as noise. These points
# are not considered part of any cluster and are often interpreted as outliers.


# ------------- DATA 2 (Blobs data) ------------------#
# K-means clustering
k <- 3
kmeans_data2 <- kmeans(data2, k)

# Hierarchical clustering with single linkage
hier_data2_single <- hclust(dist(data2), method = "single")
clusters_data2_single <- cutree(hier_data2_single, k = k)

# Hierarchical clustering with complete linkage
hier_data2_comp <- hclust(dist(data2), method = "complete")
clusters_data2_comp <- cutree(hier_data2_comp, k = k)

# DBScan
# You can also try to change the parameters and observe how it changes the clusters
dbscan_data2 <- dbscan::dbscan(data2, eps = 0.5, minPts = 5)

# Let's visualize the different clustering
# K-means
ggplot(data2, aes(x = x2, y = y2, color = factor(kmeans_data2[["cluster"]]))) +
    geom_point() +
    labs(title = "K-means Clustering of Blobs Data", color = "Clusters")

# Hierarchical clustering with single linkage
ggplot(data2, aes(x = x2, y = y2, color = factor(clusters_data2_single))) +
    geom_point() +
    theme_classic() +
    labs(title = "Hierarchical Clustering with Single Linkage of Blobs data", color = "Clusters")

# Hierarchical clustering with complete linkage
ggplot(data2, aes(x = x2, y = y2, color = factor(clusters_data2_comp))) +
    geom_point() +
    theme_classic() +
    labs(title = "Hierarchical Clustering with Complete Linkage of Blobs data", color = "Clusters")

# DBScan
ggplot(data2, aes(x = x2, y = y2, color = factor(dbscan_data2[["cluster"]]))) +
    geom_point() +
    theme_classic() +
    labs(title = "DBScan clustering of Blobs data", color = "Clusters")


# ------------- DATA 3 (Moons data) ------------------#
# K-means clustering
# We are also changing k as the more natural structure of the created data is 4.
# clusters.
k <- 4
kmeans_data3 <- kmeans(data3, k)

# Hierarchical clustering with single linkage
hier_data3_single <- hclust(dist(data3), method = "single")
clusters_data3_single <- cutree(hier_data3_single, k = k)

# Hierarchical clustering with complete linkage
hier_data3_comp <- hclust(dist(data3), method = "complete")
clusters_data3_comp <- cutree(hier_data3_comp, k = k)

# DBScan
# We are also trying with a different value for the eps (i.e. points need to be
# a bit closer in order to be part of the cluster.
dbscan_data3 <- dbscan::dbscan(data3, eps = 0.4, minPts = 5)

# Let's visualize the different clustering
# K-means
ggplot(data3, aes(x = X, y = Y, color = factor(kmeans_data3[["cluster"]]))) +
    geom_point() +
    labs(title = "K-means Clustering of Moons Data", color = "Clusters")

# Hierarchical clustering with single linkage
ggplot(data3, aes(x = X, y = Y, color = factor(clusters_data3_single))) +
    geom_point() +
    theme_classic() +
    labs(title = "Hierarchical Clustering with Single Linkage of Moons Data", color = "Clusters")

# Hierarchical clustering with complete linkage
ggplot(data3, aes(x = X, y = Y, color = factor(clusters_data3_comp))) +
    geom_point() +
    theme_classic() +
    labs(title = "Hierarchical Clustering with Complete Linkage of Moons Data", color = "Clusters")

# DBScan
ggplot(data3, aes(x = X, y = Y, color = factor(dbscan_data3[["cluster"]]))) +
    geom_point() +
    theme_classic() +
    labs(title = "DBScan Clustering of Moons Data", color = "Clusters")


# ------------- DATA 4 (Circles data) ------------------#
# K-means clustering
# Again, we are also changing k as the more natural structure of the created
# data is 2 clusters.
k <- 2
kmeans_data4 <- kmeans(data4, k)

# Hierarchical clustering with single linkage
hier_data4_single <- hclust(dist(data4), method = "single")
clusters_data4_single <- cutree(hier_data4_single, k = k)

# Hierarchical clustering with complete linkage
hier_data4_comp <- hclust(dist(data4), method = "complete")
clusters_data4_comp <- cutree(hier_data4_comp, k = k)

# DBScan
dbscan_data4 <- dbscan::dbscan(data4, eps = 0.4, minPts = 5)

# Let's visualize the different clustering
# K-means
ggplot(data4, aes(x = x4, y = y4, color = factor(kmeans_data4[["cluster"]]))) +
    geom_point() +
    labs(title = "K-means Clustering of Circles Data", color = "Clusters")

# Hierarchical clustering with single linkage
ggplot(data4, aes(x = x4, y = y4, color = factor(clusters_data4_single))) +
    geom_point() +
    theme_classic() +
    labs(title = "Hierarchical Clustering with Single Linkage of Circles Data", color = "Clusters")

# Hierarchical clustering with complete linkage
ggplot(data4, aes(x = x4, y = y4, color = factor(clusters_data4_comp))) +
    geom_point() +
    theme_classic() +
    labs(title = "Hierarchical Clustering with Complete Linkage of Cicles Data", color = "Clusters")

# DBScan
ggplot(data4, aes(x = x4, y = y4, color = factor(dbscan_data4[["cluster"]]))) +
    geom_point() +
    theme_classic() +
    labs(title = "DBScan Clustering of Circles Data", color = "Clusters")


# ------------- DATA 5 (Clusters with different variance) ------------------#
# K-means clustering
# Again, we are also changing k as the more natural structure of the created
# data is 3 clusters.
k <- 3
kmeans_data5 <- kmeans(data5, k)

# Hierarchical clustering with single linkage
hier_data5_single <- hclust(dist(data5), method = "single")
clusters_data5_single <- cutree(hier_data5_single, k = k)

# Hierarchical clustering with complete linkage
hier_data5_comp <- hclust(dist(data5), method = "complete")
clusters_data5_comp <- cutree(hier_data5_comp, k = k)

# DBScan
dbscan_data5 <- dbscan::dbscan(data5, eps = 0.6, minPts = 5)

# Let's visualize the different clustering
# K-means
ggplot(data5, aes(x = x, y = y, color = factor(kmeans_data5[["cluster"]]))) +
    geom_point() +
    labs(title = "K-means Clustering of Data with Different Variance", color = "Clusters")

# Hierarchical clustering with single linkage
ggplot(data5, aes(x = x, y = y, color = factor(clusters_data5_single))) +
    geom_point() +
    theme_classic() +
    labs(title = "Hierarchical Clustering with Single Linkage of Data with Different Variance", color = "Clusters")

# Hierarchical clustering with complete linkage
ggplot(data5, aes(x = x, y = y, color = factor(clusters_data5_comp))) +
    geom_point() +
    theme_classic() +
    labs(title = "Hierarchical Clustering with Complete Linkage of Data with Different Variance", color = "Clusters")

# DBScan
ggplot(data5, aes(x = x, y = y, color = factor(dbscan_data5[["cluster"]]))) +
    geom_point() +
    theme_classic() +
    labs(title = "DBScan Clustering of Data with Different Variance", color = "Clusters")
