# Mengimpor library yang diperlukan
library(caret)
library(e1071)

# Mengimpor dataset
data <- read.csv("data.csv")
data$X <- NULL
head(data)

# Menentukan grid parameter yang akan diuji
grid <- expand.grid(C = c(0.1, 1, 10),
                    sigma = c(0.5, 1, 2))

# Memisahkan dataset menjadi data training dan data testing
set.seed(123) # Untuk reproducibility
trainIndex <- createDataPartition(data$mood, p = 0.7, list = FALSE)
training <- data[trainIndex, ]
testing <- data[-trainIndex, ]

# Menentukan metode evaluasi
ctrl <- trainControl(method = "cv", number = 5) # 5-fold cross-validation

# Melakukan grid search dengan SVM
model <- train(mood ~ ., data = training,
               method = "svmRadial",
               trControl = ctrl,
               tuneGrid = grid)

# Menampilkan hasil grid search
print(model)

# Memprediksi menggunakan model terbaik
predictions <- predict(model, newdata = testing)

# Menampilkan hasil prediksi
result <- data.frame(actual = testing$mood, predicted = predictions)
print(result)
