# Install the packages
install.packages("klaR")
install.packages("graphics")

# Load the library
library(klaR)
library(graphics)

# Import the dataset
df <- read.csv("income.csv")
df$X <- NULL
head(df)

# Create a k-modes model
model <- kmodes(df, 2)

# Print the result
model

# Create a scatter plot of the data
plot(df$age, df$income, col=model$cluster, pch=19,
     xlab = "Age",
     ylab = "Income",
     main = "K-Modes")


