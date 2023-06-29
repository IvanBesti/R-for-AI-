# Instalasi Paket
install.packages("randomForest")

# Memuat paket
library(randomForest)

set.seed(1000) # untuk memastikan hasil yang sama setiap kali dijalankan

# Membuat dataset Random Forest
n <- 500 # jumlah observasi
usia <- rnorm(n, mean = 30, sd = 10) # variabel usia
jenis_kelamin <- sample(c("Laki-laki", "Perempuan"), n, replace = TRUE) # variabel jenis kelamin
pendapatan <- rnorm(n, mean = 5000000, sd = 2000000) # variabel pendapatan
tingkat_pendidikan <- sample(c("SD", "SMP", "SMA", "Diploma", "Sarjana", "Magister", "Doktor"), n, replace = TRUE) # variabel tingkat pendidikan
beli_produk <- sample(c("Ya", "Tidak"), n, replace = TRUE, prob = c(0.3, 0.7)) # variabel target

# Menggabungkan kolom-kolom yang sudah dibuat
data <- data.frame(usia, jenis_kelamin, pendapatan, tingkat_pendidikan, beli_produk) # gabungkan variabel-variabel menjadi satu dataset
head(data)
str(data)

## Gunakan apabila ingin memuat data dengan fungsi read.csv
# write.csv(data, "/Users/ivanbesti/Documents/gawe/pak irwan/R KONVERSI/Supervised Learning/Random Forest/Random Forest.csv")
# data <- read.csv("Random Forest.csv")
# data$X <- NULL


# membagi dataset menjadi data training dan data testing
train_idx <- sample(1:nrow(data), nrow(data) * 0.7) # menggunakan 70% data sebagai data training
train_data <- data[train_idx, ]
test_data <- data[-train_idx, ]

# membangun model Random Forest
rf_model <- randomForest(as.factor(beli_produk) ~ usia + jenis_kelamin + pendapatan + tingkat_pendidikan, data = train_data, ntree = 500, importance = TRUE)

# melakukan prediksi pada data testing
predicted <- predict(rf_model, newdata = test_data)

# menghitung akurasi model
accuracy <- mean(predicted == test_data$beli_produk)
cat("Akurasi model:", accuracy * 100, "%\n")

# menampilkan feature importance
imp <- importance(rf_model)
varImpPlot(rf_model)
