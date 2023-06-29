# load library
library(rpart)
library(caret)
library(readxl)

# Membaca dataset dari file CSV
data <- read.csv("dataset_mall.csv")
data

# Mengubah kata negatif menjadi 0 dan positif menjadi 1 pada kolom-kolom yang diinginkan
kolom <- c("c")
for (i in kolom) {
  data[, i] <- ifelse(data[, i] == "Ya", 1, 0)
}

# Mengubah kata tertentu di dalam dataset menjadi boolean
data$Cuaca <- gsub("Banyak", "1", data$Uang)
data$Cuaca <- gsub("Sedikit", "0", data$Uang)


# Menampilkan hasil
data


# Menampilkan dataset yang sudah diuba
data

# split data into training and testing set
set.seed(10000000)
train_index <- sample(1:nrow(data), size = 0.7 * nrow(data))
train <- data[train_index, ]
test <- data[-train_index, ]
test

# make sure factor levels are the same
train$Decision <- factor(train$Decision, levels = c(0, 1))
test$Decision <- factor(test$Decision, levels = c(0, 1))

# build decision tree model
tree <- rpart(Decision ~ ., data = train, method = "class")
tree

# make predictions on test set
predictions <- predict(tree, test, type = "class")

# calculate accuracy of model
cm <- confusionMatrix(predictions, test$Decision)
accuracy <- cm$overall["Accuracy"]
print(paste("Accuracy:", accuracy))
