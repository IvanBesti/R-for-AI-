# Membuat dataset
mydf <- read.csv("biodata.csv")

# Menampilkan head dataset
mydf$X <- NULL
mydf$umur <- factor(mydf$umur)
mydf$status <- factor(mydf$status)
str(mydf)
head(mydf)

# Mengimpor library dan membagi dataset menjadi data training dan testing
library(caret)
set.seed(123)
trainIndex <- createDataPartition(mydf$status, p = 0.7, list = FALSE)
train <- mydf[trainIndex, ]
test <- mydf[-trainIndex, ]

# Melakukan KNN dengan k=3
library(class)
predicted <- knn(train[,1:4], test[,1:4], train$status, k = 3)

# Melihat akurasi KNN
confusionMatrix(predicted, test$status)
