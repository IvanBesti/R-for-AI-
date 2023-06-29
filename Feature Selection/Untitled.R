# Load package
library(ggcorrplot)
library(caret)
library(randomForest)
library(gam)

# load the data
winequality <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", sep = ";")
head(winequality)

## Correlation Matrix with Heatmap
# Calculate correlation matrix
cor_matrix <- cor(winequality)

# Create heatmap
ggplot(data = reshape2::melt(cor_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", size = 0.25) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap", x = "", y = "") +
  coord_fixed() +
  geom_text(aes(label = round(value, 2)), 
            position = position_identity(), 
            vjust = 0.5, 
            color = "black", 
            size = 2)

## Mencari fitur penting
# Fungsi untuk menemukan variabel berulang
findCorrelation(cor(winequality), cutoff=0.75)

# Kalkulasi fitur penting tanpa model
# gunakan area roc_curve sebagai skor
roc_imp <- filterVarImp(x = winequality[,1:11], y = winequality$quality)

# urutkan skor dalam urutan menurun
roc_imp <- data.frame(cbind(variable = rownames(roc_imp), score = roc_imp[,1]))
roc_imp$score <- as.double(roc_imp$score)
roc_imp[order(roc_imp$score,decreasing = TRUE),]

# latih model hutan acak dan hitung kepentingan fitur
rf = randomForest(x= winequality[,1:11],y= winequality[,12])
var_imp <- varImp(rf, scale = FALSE)

# mengurutkan skor dalam urutan menurun
var_imp_df <- data.frame(cbind(variable = rownames(var_imp), score = var_imp[,1]))
var_imp_df$score <- as.double(var_imp_df$score)
var_imp_df[order(var_imp_df$score,decreasing = TRUE),]

# Menampilkan plot
ggplot(var_imp_df, aes(x=reorder(variable, score), y=score)) + 
  geom_point() +
  geom_segment(aes(x=variable,xend=variable,y=0,yend=score)) +
  ylab("IncNodePurity") +
  xlab("Variable Name") +
  coord_flip()

## Univariate Feature Selection
filterCtrl <- sbfControl(functions = rfSBF, method = "repeatedcv", repeats = 3)
rfWithFilter <- sbf(x= winequality[,1:11],y= winequality[,12], sbfControl = filterCtrl)
rfWithFilter

# Rekursi Elminasi Fitur
filterCtrl <- rfeControl(functions=rfFuncs, method="cv", number=3)
results <- rfe(x= winequality[,1:11],y= winequality[,12], sizes=c(1:11), rfeControl=filterCtrl)
results

# Tampilkan plot hasil
plot(results, type=c("g", "o"))

##########################
# Feature Importance

# Instalasi package
install.packages("readr")
install.packages("tree")
install.packages("ggplot2")

library(rpart)
library(ggplot2)

# Membaca data
data <- read.csv("golf.txt")

# Membuat formula untuk decision tree
formula <- as.formula("Decision ~ Outlook + Temp. + Humidity + Wind")

# Melatih model decision tree
model <- rpart(formula, data = data, method = "class", control = rpart.control(cp = 0.01, minsplit = 1))

# Menghitung feature importance

# Menghitung feature importance
importance <- varImp(model, type = 1)

# Menampilkan hasil
importance

# Membuat plot feature importance
plot <- ggplot(importance, aes(x = rownames(importance), y = Overall)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Predictor", y = "Importance") +
  ggtitle("Feature Importance")

# Menampilkan plot
plot

##########################
# Forward selection & backward selection
# Instalasi library
# Memuat library
# Memuat library MASS
library(MASS)

# Memuat data
data <- read.csv("golf_label_num.txt")

# Membagi data menjadi data training dan data testing
set.seed(123)
train <- sample(1:nrow(data), size = round(0.7 * nrow(data)), replace = FALSE)
test <- setdiff(1:nrow(data), train)

summary(data$Decision)
data$Decision <- ifelse(data$Decision == 1, 0, 1)
summary(data$Decision)



# Forward selection
model <- glm(Decision ~ 1, data = data[train, ], family = binomial)

while (length(model$coefficients) < ncol(data) - 1) {
  candidate <- setdiff(names(data), names(model$coefficients))
  fit <- sapply(candidate, function(x) {
    glm(formula = Decision ~ ., data = data[train, ], family = binomial, response = "Decision")
    
  })
  next.var <- names(fit)[which.min(fit)]
  model <- update(model, as.formula(paste0(". ~ . + ", next.var)))
}


# Melihat hasil dari forward selection
summary(model)

# Memprediksi data testing
prob <- predict(model, newdata = data[test, ], type = "response")
pred <- ifelse(prob > 0.5, 1, 2)

# Membuat dataframe hasil prediksi
result <- data[test, ]
result$Prediction <- pred
result$Decision <- ifelse(result$Decision == 1, 0, 1)
result$Correct <- ifelse(result$Prediction == result$Decision, "Yes", "No")

# Menampilkan hasil prediksi
result


# Melihat hasil prediksi
table(pred, data[test, "Decision"])

# Backward selection
# Initialize model with all predictors
model <- glm(Decision ~ ., data = data[train, ], family = binomial)

while (length(model$coefficients) > 1) {
  candidate <- names(model$coefficients)[-1]
  fit <- sapply(candidate, function(x) {
    glm(as.formula(paste0("Decision ~ . - ", x)), data = data[train, ], family = binomial)$null.deviance
  })
  drop.var <- names(fit)[which.min(fit)]
  model <- update(model, as.formula(paste0(". ~ . - ", drop.var)))
}

# Melihat hasil dari forward selection
summary(model)

# Memprediksi data testing
prob <- predict(model, newdata = data[test, ], type = "response")
pred <- ifelse(prob > 0.5, 1, 2)

# Membuat dataframe hasil prediksi
result <- data[test, ]
result$Prediction <- pred
result$Decision <- ifelse(result$Decision == 1, 0, 1)
result$Correct <- ifelse(result$Prediction == result$Decision, "Yes", "No")

# Menampilkan hasil prediksi
result



