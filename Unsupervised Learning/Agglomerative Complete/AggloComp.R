# Mengimpor library untuk pengelompokan hierarkis
library(cluster)

# Mengimpor dataset
data <- read.csv("identitas.csv")
data$X <- NULL
head(data)

# Scatterplot
ggplot(data, aes(Usia, Gaji, color = Pekerjaan)) +
  geom_point(size=3) +
  theme_minimal() + 
  ggtitle("Scatterplot") +
  theme(plot.title = element_text(hjust = 0.5))

# Menghapus kolom "Nama" dari dataset karena bukan data numerik
data_numeric <- data[, c("Usia", "Gaji")]

# Menjalankan algoritma agglomerative complete
result <- hclust(dist(data_numeric), method = "complete")

# Menampilkan hasil dendrogram
plot(result, main = "Dendrogram", xlab = "Objek", ylab = "Jarak Euclidean")
