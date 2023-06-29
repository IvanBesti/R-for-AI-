# Instalasi package
install.packages("dplyr")
install.packages("ggpplot2")

# Memuat package
library(dplyr)  # Memuat library dplyr
library(ggplot2)  # Memuat library ggplot2

# Membaca file
df <- read.csv("/Users/ivanbesti/Documents/gawe/pak irwan/R KONVERSI/CT/internet2.csv", header=TRUE)

# Menampilkan rata-rata waktu
mean(df$Time)

# Menampilkan tipe data waktu
class(df$Time)

# Menampilkan median waktu
median(df$Time)

# Menampilkan rata-rata usia
mean(df$Age)

# Menampilkan tipe data usia
class(df$Age)

# Menampilkan median usia
median(df$Age)

# Menampilkan boxplot
ggplot(df, aes(y="", x=Age, fill)) + 
  geom_boxplot() +
  coord_flip() +
  labs(x="Age", y="") 

# Menampilkan Barplot
ggplot(df, aes(x=App, y=Age)) +
  geom_bar(stat="identity", position="dodge") +  # tambahkan argument position="dodge"
  scale_y_continuous(limits=c(0, 60)) +
  labs(x="App", y="Age")

# Menghitung jumlah kemunculan setiap nilai
table(df$App)
