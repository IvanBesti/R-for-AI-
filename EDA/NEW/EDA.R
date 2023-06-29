# instalasi paket
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("reshape2")
install.packages("corrplot")

# memuat paket
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(corrplot)

# Memuat dataset Iris
data(iris)

# Menampilkan beberapa baris pertama dari dataset
head(iris)
str(iris)
summary(iris)
tail(iris)

# menyimpan data di iris di dalam csv, 
# gunakan apabila diperlukan untuk menyimapn data dalam csv
# lalu membaca kembali data yang sudah disimpan di dalam csv
write.csv(iris, "iris.csv")
dataset <- read.csv("iris.csv")
dataset$X <- NULL # menghapus kolom X

# Menampilkan kembali data
head(dataset)
summary(dataset)
str(dataset)

# Mencari nilai duplikat pada kolom Sepal Length dan Sepal Width
duplikat <- duplicated(dataset[c("Sepal.Length", "Sepal.Width")])
iris[duplikat,]

# Mencari nilai null atau missing value pada dataset iris
isna <- apply(is.na(dataset), 2, any)
dataset[,isna]

# Mencari outlier pada kolom Sepal.Length menggunakan boxplot
boxplot(dataset$Sepal.Length, main="Boxplot Sepal Length Outlier Value")

# Menghitung IQR pada kolom Sepal.Length
qnt <- quantile(dataset$Sepal.Length, probs=c(.25, .75), na.rm = TRUE)
caps <- (qnt[2] - qnt[1]) * 1.5
outliers <- dataset$Sepal.Length[dataset$Sepal.Length < (qnt[1] - caps) | dataset$Sepal.Length > (qnt[2] + caps)]

# Menampilkan nilai outlier
if(length(outliers) > 0) {
  cat("Nilai outlier pada kolom Sepal.Length: ", paste(outliers, collapse = ", "))
} else {
  cat("Tidak ada nilai outlier pada kolom Sepal.Length.")
}

## Visualisasi data
# Menampilkan histogram dari kolom Sepal.Length, dipisahkan berdasarkan spesies iris
ggplot(dataset, aes(x = Sepal.Length, fill = Species)) +
  geom_histogram(color = "black", bins = 20, position = "dodge") +
  labs(title = "Histogram Sepal.Length by Species", x = "Sepal.Length", y = "Frequency") +
  scale_fill_discrete(name = "Species")

# menghitung korelasi antara kolom-kolom pada dataset iris
corr_matrix <- cor(dataset[,1:4])

# mengubah corr_matrix menjadi data frame
corr_df <- reshape2::melt(corr_matrix)

# menampilkan plot heatmap korelasi 
ggplot(corr_df, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), size = 3) +
  scale_fill_gradient(low = "#00FF00", high = "#006400", name = "Korelasi") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Heatmap Korelasi pada Dataset Iris")

# Menampilkan scatterplot untuk Sepal Length dan Sepal Width
ggplot(dataset, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point(size = 3, alpha = 0.5, shape = 15) +
  scale_color_manual(values = c("#FF5733", "#1E90FF", "#00FF7F")) +
  labs(title = "Scatterplot Sepal Length vs Sepal Width",
       x = "Sepal Length", y = "Sepal Width", color = "Species") +
  theme_minimal()
