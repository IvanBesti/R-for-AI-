# Instalasi paket
install.packages("rpart")
install.packages("caret")

# Memuat library untuk decision tree
library(rpart)
library(caret)

# Membuat dataset
set.seed(100) # Set seed untuk reproducibility

# Membuat kolom-kolom
age <- sample(18:70, 100, replace = TRUE)
income <- rnorm(100, mean = 50000, sd = 10000)
gender <- sample(c("Male", "Female"), 100, replace = TRUE)
marital_status <- sample(c("Married", "Single", "Divorced"), 100, replace = TRUE)
smoker <- sample(c("Yes", "No"), 100, replace = TRUE)
heart_disease <- sample(c("Yes", "No"), 100, replace = TRUE)

# Menggabungkan kolom-kolom menjadi dataframe
data <- data.frame(age, income, gender, marital_status, smoker, heart_disease)
#data <- read.csv("smokere.csv", sep = ";")

# Melihat 5 data teratas
head(data)
#qplot(data = datas, x=income, y = smoker, color= marital_status, size = I(5))

# Membuat decision tree model
model <- rpart(heart_disease ~ ., data = data, method = "class")

# Melihat summary model
summary(model)

# Melakukan validasi silang dengan 10 fold cross validation
set.seed(10) # Set seed untuk reproducibility
kfold <- trainControl(method = "cv", number = 10)
accuracy <- train(heart_disease ~ ., data = data, method = "rpart", trControl = kfold)
print(accuracy)
