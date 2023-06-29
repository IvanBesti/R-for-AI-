## Binning dengan ketentuan sendiri
# Package installation
install.packages("caret")
install.packages("magrittr")

# Load required packages
library(caret)
library(magrittr)

# Load data
data <- read.csv("/Users/ivanbesti/Documents/gawe/pak irwan/Data Normalization & Discretization/datasetgaji.csv", sep=",")

# Show the first 6 rows of the data
head(data, n = 6)

# Define the bin boundaries
batas_bin <- c(0, 1400000, 4000000)

# Define the categories
kategori <- c("Kecil", "Besar")

# Create a new column for binned data
data$gaji_binned_1 <- cut(data$`Gaji.Orang.Tua`, breaks=batas_bin, labels=kategori)
data

## Binning dengan linespace
# Define number of bins
n_bins <- 3

# Define bin edges using linspace function
bins <- seq(min(data$`Gaji.Orang.Tua`), max(data$`Gaji.Orang.Tua`), length.out = n_bins)
bins

## Binning dengan Quantile
# Define the categories
kategori <- c("Kecil", "Besar" )

# Create new column 'gaji_binned2' using cut function
data$gaji_binned2 <- cut(data$`Gaji.Orang.Tua`, breaks = bins, labels = kategori, include.lowest = TRUE)
data

# Define the Categories
kategori <- c("Kecil", "Besar")

# Create new column 'gaji_binned3' using cut function
data$Gaji_binned3 <- cut(data$`Gaji.Orang.Tua`, breaks = quantile(data$`Gaji.Orang.Tua`, probs = 0:2/2), labels = kategori, include.lowest = TRUE)
data
