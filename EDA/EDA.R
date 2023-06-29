## Instalasi Packages
install.packages("tidyverse")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("lattice")
install.packages("gridExtra")
install.packages("plyr")
install.packages("stringr")
install.packages("caret")
install.packages("randomForest")
install.packages("e1071")
install.packages("DataExplorer")
install.packages("reshape2")

## Mengimpor library/packages yang dibutuhkan untuk EDA
library(tidyverse)

# Import data manipulation libraries
library(dplyr)
library(tidyr)

# Import data visualization libraries
library(ggplot2)
library(lattice)
library(gridExtra)
library(reshape2)

# Import data analysis libraries
library(plyr)
library(stringr)

# Import data modeling libraries
library(caret)
library(randomForest)
library(e1071)

# Import data exploration libraries
library(DataExplorer)

# Set theme for data visualization
theme_set(theme_classic())

## Menampung data ke dalam data frame
# Membaca file "data_car.csv"
df <- read.csv("/Users/ivanbesti/Documents/gawe/pak irwan/R KONVERSI/EDA/data_car.csv")

# Menampilkan 5 baris atas pertama
head(df, 5)

# Menampilkan 5 baris bawah pertama
tail(df, 5)

## Mengecek tipe data
str(df)
View(df)

## Membuang kolom yang tidak relevan
# Menghapus kolom yang tidak diperlukan
df <- df[, -c(4,9,10,11,12,15)]
#"Engine Fuel Type', 'Market Category', 'Vehicle Style', 'Popularity', 'Number of Doors', 'Vehicle Size'"

# Menampilkan 5 baris atas data frame
head(df, 5)
View(df)

## Menamai kembali kolom
# Menamai ulang kolom pada data frame
colnames(df) <- c("Make", "Model", "Year","HP", "Cylinders", "Transmission", "DriveMode", "MPG-H", "MPG-C", "Price")

# Menampilkan 5 baris atas data frame
head(df, 5)
View(df)

# Menghitung jumlah baris
df_count <- data.frame(`Variable Count` = paste(names(df), colSums(!is.na(df))), check.names = FALSE)

print(df_count)


## Membuang baris ganda
# Mencari baris duplikat pada data frame
duplicate_rows_df <- df[duplicated(df), ]

# Menampilkan jumlah baris duplikat
cat("number of duplicate rows: ", nrow(duplicate_rows_df), "\n")

# Menampilkan jumlah baris pada data frame
cat("number of rows: ", nrow(df), "\n")

# Menampilkan jumlah kolom pada data frame
cat("number of columns: ", ncol(df), "\n")

# Menghapus baris yang terduplikat
df <- unique(df)

# Menampilkan 5 baris pertama pada data frame
head(df, 5)

# Menghitung jumlah baris
df_count <- data.frame(`Variable Count` = paste(names(df), colSums(!is.na(df))), check.names = FALSE)

print(df_count)

## Membuang nilai yang hilang atau null
# Membuat data frame dengan jumlah nilai hilang
missing_df <- data.frame(`Variable Missing` = paste(names(df), colSums(is.na(df))), check.names = FALSE)

# Menampilkan data frame
print(missing_df)


# Menghapus baris yang memiliki nilai yang hilang
df <- na.omit(df)

# Menghitung jumlah baris dalam setiap kolom
col_counts <- colSums(!is.na(df))

# Membuat data frame untuk menampilkan hasil perhitungan
count_df <- data.frame(col_counts)

# Menampilkan data frame hasil perhitungan
print(count_df, row.names = TRUE)

# menghitung jumlah nilai null pada setiap kolom
col_counts <- colSums(is.na(df))

# Membuat data frame untuk menampilkan hasil perhitungan
count_df <- data.frame(col_counts)

# Menampilkan data frame hasil perhitungan
print(count_df, row.names = TRUE)

## Mendetetksi nilai outlier
# create a figure with two subplots
p1 <- ggplot(df, aes(x = HP)) +
  geom_density() +
  ggtitle("Density Plot of HP") +
  labs(x = "HP")

p2 <- ggplot(df, aes(x = "", y = HP)) +
  geom_boxplot(width = 0.2) +
  ggtitle("Box Plot of HP") +
  labs(x = "")

grid.arrange(p1, p2, ncol = 2, widths = c(3, 2))

# Membuat plot box plot dengan ggplot
ggplot(df, aes(y=Price/1e6, x="")) +
  geom_boxplot() +
  scale_y_continuous(name = "Price (1e6)", labels = function(x) format(x, scientific = FALSE)) +
  coord_flip()

# Menggambar box plot
ggplot(df, aes(x = HP)) + 
  geom_boxplot() +
  xlab("HP") +
  ylab("Price (1e6)") +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6))

# Membuat plot dengan ggplot
ggplot(df, aes(y=Cylinders, x=1, fill=Cylinders)) + 
  geom_boxplot() + 
  scale_y_continuous(limits=c(0, max(df$Cylinders)),
                     breaks=seq(0, max(df$Cylinders), 2)) +
  coord_flip() +
  labs(x="", y="Cylinders", fill="") +
  theme_minimal()

## Plot fitur yang berbeda terhadap satu sama lain (menyebar), terhadap frekuensi (histogram)
summary(df$Price)

# Menghitung jumlah mobil berdasarkan merek
df_make <- data.frame(table(df$Make))
names(df_make) <- c("Make", "count")

# Mengambil 40 data teratas berdasarkan jumlah mobil
df_top_make <- df_make %>% 
  top_n(40, count) %>% 
  arrange(desc(count))

# Membuat grafik batang
ggplot(df_make, aes(x = reorder(Make, -count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Number of cars by make", x = "Make", y = "Number of cars") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_continuous(breaks = seq(0, 1000, 200))

# Heat Maps
# Select only the numeric columns from the data frame
numeric_df <- df[, sapply(df, is.numeric)]

# Compute the correlation matrix
c <- cor(numeric_df)

# Plot the heatmap
# Membuat dataframe yang berisi data numerik
df_num <- df[,sapply(df, is.numeric)]

# Menghitung korelasi dan membuat heatmap
c <- cor(df_num)
ggplot2::ggplot(data = reshape2::melt(c), aes(Var1, Var2, fill = value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "red", high = "green", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 8, hjust = 1)) + 
  coord_fixed()

colnames(df)

# membuat pivot table
result <- dcast(df, Transmission ~ DriveMode, value.var = "Price", mean)

# membuat plot heatmap menggunakan ggplot2
ggplot(melt(result), aes(x = variable, y = Transmission, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "green", mid = "yellow", high = "red", midpoint = 0.117) +
  labs(x = "Drive Mode", y = "Transmission", title = "Heatmap of Price by Transmission and Drive Mode") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = sprintf("%.3f", round(value, 3))), color = "black", size = 3)



# Scatterplot
# Mengatur tema
theme_set(theme_bw())

# Membuat plot
ggplot(df, aes(x = HP, y = Price, color = `DriveMode`)) +
  geom_point() + # Menambahkan titik pada plot
  geom_smooth(method = "lm", se = FALSE) + # Menambahkan garis regresi
  scale_color_brewer(palette = "Set2") + # Mengatur palet warna
  labs(x = "HP", y = "Price") + # Memberikan label pada sumbu-sumbu
  theme(legend.position = "bottom") # Mengatur posisi legenda

# membuat pairplot dengan ggplot2
ggplot(df, aes(x=HP, y=Price, color=`DriveMode`)) +
  geom_point() + # menambahkan titik pada plot
  geom_smooth(method=lm, se=FALSE) + # menambahkan garis regresi pada plot
  labs(x="HP", y="Price", title="Pairplot of HP, DriveMode, and Price") + # memberikan label sumbu dan judul plot
  facet_wrap(~`DriveMode`) # memisahkan plot berdasarkan jenis Drive Mode

# Correlation Matrix 
# menghitung korelasi antara kolom HP dan Price
result <- cor(df[c("HP", "Price")])

# menampilkan hasil korelasi
print(result)

