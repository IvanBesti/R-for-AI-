# Memasang paket R
install.packages("rpart")
install.packages("rpart.plot")

# Memuat paket R
library(rpart)
library(rpart.plot)

# Membaca dataset
dataset <- read.csv("bermain.csv", sep = ";")
print(dataset)

# Membuat decision tree kategorik
decision_tree <- rpart(Bermain ~ Cuaca + Suhu + Kelembaban, data = dataset, method = "class")

# Menampilkan pohon keputusan dalam plot
rpart.plot(decision_tree, type = 4, extra = 101, box.palette = "RdBu", shadow.col = "gray", main = "Decision Tree")

# Membagi data menjadi data training dan data testing
set.seed(123) # untuk memastikan hasil random yang sama setiap kali dijalankan
n <- nrow(dataset)
ntrain <- round(0.7 * n) # menggunakan 70% data sebagai data training
indices <- sample(n)
train <- dataset[indices[1:ntrain], ]
test <- dataset[indices[(ntrain + 1):n], ]

# Membuat decision tree kategorik menggunakan data training
decision_tree <- rpart(Bermain ~ Cuaca + Suhu + Kelembaban, data = train, method = "class")

# Memprediksi data testing menggunakan decision tree yang telah dibuat
predictions <- predict(decision_tree, test, type = "class")

# Menghitung akurasi model
accuracy <- sum(predictions == test$Bermain) / nrow(test)
cat("Akurasi model:", round(accuracy * 100, 2), "%")

# Membuat prediksi menggunakan decision tree
prediksi <- predict(decision_tree, newdata = dataset, type = "class")

# Menampilkan confusion matrix
table(dataset$Bermain, prediksi)

# Menghitung akurasi model
akurasi <- sum(diag(table(dataset$Bermain, prediksi))) / sum(table(dataset$Bermain, prediksi)) * 100
cat("Akurasi model:", akurasi, "%")
