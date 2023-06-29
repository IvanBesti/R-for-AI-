# Instalasi Paket
install.packages("ggplot2")
install.packages("gridExtra")

# Memuat Paket
library(ggplot2)
library(gridExtra)
library(cowplot)

# Mengimpor dataset
campak <- read.csv("data.csv", sep = ";")
head(campak)

# Mengubah nama kolom
colnames(campak) <- c("Provinsi", "2020", "2021", "2022" )
head(campak)

# Mengapus kolom dan memanipulasi data frae
campak <- campak[-1,] 
head(campak)

# Mengatur Ulang Penmoroan indeks.
row.names(campak) <- NULL
head(campak)

# Memeriksa struktur data dari dataset campak
str(campak)
summary(campak)
tail(campak)

# Mengilangkan NA Values
campak <- campak[complete.cases(campak),]
is.na(campak)

# Menghitung inersia untuk jumlah klaster 1 hingga 10
inertia <- vector()
for (k in 1:10) {
  kmeans_model <- kmeans(campak$'2020', centers = k)
  inertia[k] <- kmeans_model$tot.withinss
}

# Membuat plot elbow tahun 2020
elbow_plot <- ggplot(data.frame(k = 1:10, inertia), aes(x = k, y = inertia)) +
  geom_point() +
  geom_line(color = "blue") +
  labs(x = "Jumlah Klaster", y = "Inersia") +
  ggtitle("Grafik Elbow Dataset Campak 2020") +
  theme_minimal() 

# Menampilkan plot elbow
print(elbow_plot)

# Mengatur kategori dan warna
kategori <- c("Tinggi", "Sedang", "Rendah")
warna <- c("red", "blue", "green")

# Melakukan klasterisasi K-Means dengan k=3 hanya menggunakan kolom 2020
kmeans_model <- kmeans(campak$`2020`, centers = 3)

# Membuat dataset baru dengan kolom 2020 dan klaster
klaster_data <- data.frame(Provinsi = campak$Provinsi, `2020` = campak$`2020`, Cluster = kmeans_model$cluster)
colnames(klaster_data) <- c("Provinsi", "2020", "Cluster")
head(klaster_data)

# Menampilkan plot klasterisasi
plot2020 <- ggplot(klaster_data, aes(`2020`, Provinsi, color = factor(Cluster))) 
plot2020 + geom_point(shape = 15, size = 3) +
  scale_color_manual(values = warna, labels = kategori) +
  labs(x = "Presentase dalam %", color = "Klaster") +
  ggtitle("Klasterisasi K-Means (k = 3) dataset campak 2020") +
  theme_minimal() 

# Melakukan klasterisasi K-Means dengan k=3 hanya menggunakan kolom 2021
kmeans_model <- kmeans(campak$`2021`, centers = 3)

# Membuat dataset baru dengan kolom 2021 dan klaster
klaster_data <- data.frame(Provinsi = campak$Provinsi, `2021` = campak$`2021`, Cluster = kmeans_model$cluster)
head(klaster_data)
colnames(klaster_data) <- c("Provinsi", 2021, "Cluster")

# Menampilkan plot klasterisasi
plot2021 <- ggplot(klaster_data, aes(`2021`, Provinsi, color = factor(Cluster))) 
plot2021 + geom_point(shape = 15, size = 3) +
  scale_color_manual(values = warna, labels = kategori) +
  labs(x = "Presentase dalam %", color = "Klaster") +
  ggtitle("Klasterisasi K-Means (k = 3) dataset campak 2021") +
  theme_minimal() 

# Melakukan klasterisasi K-Means dengan k=3 hanya menggunakan kolom 2022
kmeans_model <- kmeans(campak$`2022`, centers = 3)

# Membuat dataset baru dengan kolom 2022 dan klaster
klaster_data <- data.frame(Provinsi = campak$Provinsi, `2022` = campak$`2022`, Cluster = kmeans_model$cluster)
head(klaster_data)
colnames(klaster_data) <- c("Provinsi", 2022, "Cluster")

# Menampilkan plot klasterisasi
plot2022 <- ggplot(klaster_data, aes(`2022`, Provinsi, color = factor(Cluster))) 
plot2022 + geom_point(shape = 15, size = 3) +
  scale_color_manual(values = warna, labels = kategori) +
  labs(x = "Presentase dalam %", color = "Klaster") +
  ggtitle("Klasterisasi K-Means (k = 3) dataset campak 2022") +
  theme_minimal() 

# Menggabungkan tiga plot menjadi satu plot
combined_plot <- plot_grid(plot2020, plot2021, plot2022, nrow = 1)

# Menyimpan plot gabungan dalam satu gambar
ggsave("combined_plot.png", combined_plot, width = 10, height = 5)
combined_plot
