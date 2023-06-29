# Instalasi Paket
install.packages("ggplot2")

# Memuat paket
library(ggvis)
library(tidyverse)
library(ggplot2)

getwd()

# Mengimpor data
bike_buyers = read.csv('bike_buyers.csv', header=T, na.strings='')
head(bike_buyers)

# mengubah tipe data karakter menjadi faktor
bike_buyers[] <- lapply(bike_buyers, function(x) if(is.character(x)) factor(x) else x)

# memeriksa hasil
str(bike_buyers)

# Menampilkan data
levels(bike_buyers$Education)
head(bike_buyers)
summary(bike_buyers)
str(bike_buyers)

# Visualisasi histogram umur berdasarkan pendidikan
ggplot(data = bike_buyers, aes(x=Age, color=Education)) + 
  geom_histogram(aes(fill = Education), color = "black") +
  ggtitle("Bike Buyer Age & Education")

# Visualisasi histogram jumlah anak berdasarkan pendidikan
ggplot(data = bike_buyers, aes(x=Children, color=Education)) + 
  geom_histogram(aes(fill = Education), color = "black") +
ggtitle("Bike Buyer Jumlah anak & Pendidikan")

# Visualisasi Income & Education
ggplot(data = bike_buyers, aes(x=Income, color=Education)) + 
  geom_histogram(aes(fill = Education), color = "black") +
  ggtitle("Bike Buyer Income & Education")

# Visualisasi Occupation vs. Income
ggplot(data = bike_buyers, aes(x=Income, color=Gender)) + 
  geom_histogram(aes(fill = Gender), color = "black") +
  ggtitle("Bike Buyer Income & Education")


# Membersihkan data
# Menghapus NA value atau nilai kosong
median(na.omit((bike_buyers$Income)))
median(na.omit((bike_buyers$Age)))

bike_buyers_clean <- bike_buyers
colSums(is.na(bike_buyers_clean))

# Income replaced with Median
bike_buyers_clean$Income[is.na(bike_buyers_clean$Income)] <- 
  median(na.omit((bike_buyers$Income)))

# Age replaced with Median
bike_buyers_clean$Age[is.na(bike_buyers_clean$Age)] <- 
  median(na.omit((bike_buyers$Age)))

colSums(is.na(bike_buyers_clean))

# Fungsi mode
get_mode <- function(x) {                 
  unique_x <- unique(x)
  tabulate_x <- tabulate(match(x, unique_x))
  unique_x[tabulate_x == max(tabulate_x)]
}


# Marital Status replaced with Mode
bike_buyers_clean$Marital.Status[is.na(bike_buyers_clean$Marital.Status)] <- 
  get_mode(bike_buyers$Marital.Status)

# Gender replaced with Mode
bike_buyers_clean$Gender[is.na(bike_buyers_clean$Gender)] <- 
  get_mode(bike_buyers$Gender)

# Children replaced with Mode
bike_buyers_clean$Children[is.na(bike_buyers_clean$Children)] <- 
  get_mode(bike_buyers$Children)

# Home Owner replaced with Mode
bike_buyers_clean$Home.Owner[is.na(bike_buyers_clean$Home.Owner)] <- 
  get_mode(bike_buyers$Home.Owner)

colSums(is.na(bike_buyers_clean))

# Cars replaced with Mean
bike_buyers_clean$Cars[is.na(bike_buyers_clean$Cars)] <- 
  mean(bike_buyers$Cars, na.rm = TRUE)

colSums(is.na(bike_buyers_clean))
View(bike_buyers_clean)

head(bike_buyers_clean)

# Menyimpan dataset yang sudah bersih
write.csv(bike_buyers_clean,"bike_buyers_clean.csv", quote = FALSE, row.names = TRUE)
bike_buyers <- bike_buyers_clean

# Menampilkan barplot dataset yang telah dibersihkan
ggplot(data = bike_buyers, aes(x=Cars, color=Gender)) +
  geom_bar(aes(fill=Gender), color="black")

# Menampilkan scatterplot 
u <- ggplot(data = bike_buyers, aes(x=Age, y=Education, 
                                    color=Marital.Status, size = Income)) 
u + geom_point(alpha=0.5) 
u + geom_point(alpha=0.5) + facet_grid(Region~.)
u + geom_point(alpha=0.5) + facet_grid(Region~Purchased.Bike)
u + geom_point(alpha=0.5) + facet_grid(Region~Purchased.Bike) +
  ggtitle("Bikers Classification") +
theme(plot.title = element_text(size = 20))

# Menampilkan density plot
v <- ggplot(data = bike_buyers, aes(x=Income))
v + geom_density(aes( position = "stack"))  
v + geom_density(aes(fill = Region, position = "stack"))  + 
  ggtitle("Income Density")

# Menampilkan boxplot
boxplot(bike_buyers$Income)

# Menangani nilai outlier
OutVals = boxplot(bike_buyers$Income)$out
print(OutVals)

which(bike_buyers$Income %in% OutVals)

x = bike_buyers$Income [!(bike_buyers$Income %in% OutVals) ]
boxplot(x)
