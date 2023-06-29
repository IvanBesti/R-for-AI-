# Load the library for OLS Regression
library(lmtest)
library(readr)

# Read the CSV file and store it in a variable called 'data'
data <- read.csv("50_Startups.csv")
data

# Create a linear regression model
model <- lm(Profit ~ R.D.Spend + Administration + Marketing.Spend + State, data = data)

# Predict the Profit based on the model
predicted <- predict(model, data)

# Create a data frame with Actual and Predicted values
df <- data.frame(Actual = data$Profit, Predicted = predicted)

# Print the data frame
print(df)

# Create a model using lm() function
model <- lm(Profit ~ R.D.Spend + Administration + Marketing.Spend + State, data = data)

# Perform OLS regression using summary() function
ols_reg <- summary(model)

# Print the summary of the OLS regression results
print(ols_reg)

# Create a model using lm() function
model <- lm(Profit ~ R.D.Spend + Administration + Marketing.Spend, data = data)

# Perform OLS regression using summary() function
ols_reg <- summary(model)

# Print the summary of the OLS regression results
print(ols_reg)

# Create a model using lm() function
model <- lm(Profit ~ R.D.Spend + Administration, data = data)

# Perform OLS regression using summary() function
ols_reg <- summary(model)

# Print the summary of the OLS regression results
print(ols_reg)

# Create a model using lm() function
model <- lm(Profit ~ R.D.Spend, data = data)

# Perform OLS regression using summary() function
ols_reg <- summary(model)

# Print the summary of the OLS regression results
print(ols_reg)



