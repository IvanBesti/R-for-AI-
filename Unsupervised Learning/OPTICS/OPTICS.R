# Menginstal dan memuat paket-paket yang diperlukan
install.packages("dbscan")

# Memuat paket
library(dbscan)

# Memuat dataset contoh (misalnya, data iris)
data(iris)

# Memilih kolom yang akan digunakan
# (di sini kita menggunakan kolom Sepal.Length dan Sepal.Width)
dataset <- iris[, c("Sepal.Length", "Sepal.Width")]

# Memvisualisasikan scatterplot sebelum grafik Reachability Plot
plot(dataset, col = "blue", pch = 16,
     xlab = "Sepal.Length", ylab = "Sepal.Width", main = "Scatterplot")

# Menjalankan algoritma OPTICS
optics_result <- optics(dataset, eps = 0.5, minPts = 5)

# Menampilkan hasil
print(optics_result)

# Memvisualisasikan hasil menggunakan plot OPTICS
plot(optics_result, dataset)
