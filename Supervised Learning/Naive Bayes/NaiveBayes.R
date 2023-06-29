# Load library e1071
library(e1071)

# Mengimpor data
df <- read.csv("pinjaman.csv")
df$X <- NULL
head(df)

# Tampilkan dataset
head(df)

# Split data into train and test sets
set.seed(123)
train_idx <- sample(nrow(df), 700)
train <- df[train_idx, ]
test <- df[-train_idx, ]

# Train Naive Bayes model
nb_model <- naiveBayes(pinjaman ~ ., data = train)

# Predict test data using trained model
test$pred_pinjaman <- predict(nb_model, test)

# Evaluate model accuracy
accuracy <- sum(test$pred_pinjaman == test$pinjaman) / nrow(test)
cat("Model accuracy:", round(accuracy, 2))


