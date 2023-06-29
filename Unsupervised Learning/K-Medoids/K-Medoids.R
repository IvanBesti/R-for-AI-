# Installing Package
install.packages("cluster")
install.packages("factoextra")

# Load the necessary libraries
library(cluster)
library(factoextra)

# Load the dataset
data <- read.csv("customers.csv")
data$X <- NULL
head(data)

# Perform K-Medoids clustering
k <- 3
kmedoids <- pam(data, k)

# Print the cluster assignments
kmedoids$clustering

# Visualize the clusters
fviz_cluster(kmedoids, data = data,
             palette = c("#a6cee3", "#1f78b4", "#b2df8a"),
             repel = TRUE,
             xlab = "Age",
             ylab = "Income",
             main = "K-Medoids Plot")
