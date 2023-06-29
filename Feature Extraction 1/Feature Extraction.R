# Package installation
install.packages("FactoMineR")
install.packages("ggplot2")

# Load libraries
library(FactoMineR)
library(ggplot2)

# File path
breast_cancer <- read.csv("data.csv")
str(breast_cancer)

# Check for NULL values
colSums(is.na(breast_cancer))

# Select only numerical columns for normalization
num_cols <- sapply(breast_cancer, is.numeric)
numerical_data <- breast_cancer[, num_cols]

# Normalizing the data
normalized_data <- apply(numerical_data, 2, function(x) (x - min(x)) / (max(x) - min(x)))
scaled_data <- scale(normalized_data)

# PCA
pca_result <- PCA(scaled_data, graph = FALSE)
x_pca <- pca_result$ind$coord

# Create a data frame from the PCA result
pca_df <- data.frame(x_pca)

# Create a scatterplot using ggplot2
ggplot(pca_df, aes(x = x_pca[,1], y = x_pca[,2], color = breast_cancer$diagnosis)) +
  geom_point() +
  scale_color_manual(values = c("#00BFC4", "#F8766D")) +
  labs(x = "First principal component", y = "Second principal component", color = "Diagnosis")

# Accessing the loadings matrix
pca_loadings <- pca_result$var$coord

# Creating dataframe for loadings
df_comp <- as.data.frame(pca_loadings)
row.names(df_comp) <- colnames(scaled_data)
colnames(df_comp) <- paste0("PC", 1:ncol(pca_loadings))

# Creating heatmap
heatmap(as.matrix(df_comp), Colv = NA, Rowv = NA, scale = "none", 
        col = colorRampPalette(c("darkblue", "white", "red"))(256),
        xlab = "Principal Components", ylab = "")

legend("topleft", legend = c("Low", "High"), fill = colorRampPalette(c("darkblue", "white", "red"))(2), 
       title = "Correlation", cex = 0.8, inset = 0.05)

