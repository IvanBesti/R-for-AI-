# install.packages("ggplot2")
# memuat paket ggplot2
library(ggplot2)
library(caret)

# membaca file csv
nilai <- read.csv("nilai.csv")

head(nilai)

dim(nilai)

summary(nilai)

# membuat scatterplot
plot(nilai$Hours, nilai$Scores, main = "Scatterplot of Hours and Scores", xlab = "Hours", ylab = "Scores")

# membuat jointplot
ggplot(nilai, aes(x = Hours, y = Scores)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Jointplot of Hours and Scores", x = "Hours", y = "Scores")

# melakukan linear regression
model <- lm(Scores ~ Hours, data = nilai)

# menampilkan hasil linear regression
summary(model)
model

# membuat data frame baru yang berisi nilai actual dan predicted
df <- data.frame(nilai$Scores, predict(model))

# memberi nama kolom pada data frame
colnames(df) <- c("Actual", "Predicted")

# menampilkan data frame
df

# membuat prediksi dengan model linear regression
predicted <- predict(model, newdata = nilai)

# menghitung nilai MAE, MSE, RMSE, dan R2
MAE <- MAE(nilai$Scores, predicted)
MSE <- function(actual, predicted) {
  mean((actual - predicted) ^ 2)
}
MSE <- MSE(nilai$Scores, predicted)
RMSE <- RMSE(nilai$Scores, predicted)
R2 <- R2(predicted, nilai$Scores)

# menampilkan nilai MAE, MSE, RMSE, dan R2
cat("Mean Absolute Error: ", MAE, "\n")
cat("Mean Squared Error: ", MSE, "\n")
cat("Root Mean Squared Error: ", RMSE, "\n")
cat("Model R^2 Square value: ", R2, "\n")

# membuat data uji
test <- data.frame(Hours = c(2.5, 7.7, 9.25))

# membuat plot garis regresi pada data uji
ggplot(data = test, aes(x = Hours, y = predict(model, newdata = test))) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Hours", y = "Scores", title = "Regression line on test set")

# membuat residual plot
ggplot(data = data.frame(predicted = predict(model, nilai), residuals = model$residuals),
       aes(x = predicted, y = residuals)) +
  geom_point() +
  labs(x = "Fitted Values", y = "Residuals", title = "Residual Plot")
