# Instalasi paket
install.packages("ggplot2")
install.packages("dbscan")

# Mengimpor library yang diperlukan
library(ggplot2)
library(dbscan)

# Membuat dataset bulan sabit
n <- 500 # Jumlah sampel
noise <- 0.1 # Tingkat kebisingan

# Membuat dataset bulan sabit
theta <- seq(0, pi, length.out = n)
radius_inner <- 1
radius_outer <- 2
x_inner <- radius_inner * cos(theta) + rnorm(n, sd = noise)
y_inner <- radius_inner * sin(theta) + rnorm(n, sd = noise)
x_outer <- radius_outer * cos(theta) + rnorm(n, sd = noise)
y_outer <- radius_outer * sin(theta) + rnorm(n, sd = noise)

moon_data <- data.frame(x = c(x_inner, x_outer), y = c(y_inner, y_outer))

# ---------------------------------------------- 1st parameter
# Melakukan DBSCAN
dbscan_result <- dbscan(moon_data, eps = 0.3, MinPts = 5)

# Menambahkan label cluster ke dalam dataset
moon_data$cluster <- dbscan_result$cluster

# Menampilkan scatter plot
ggplot(data = moon_data, aes(x = x, y = y, color = factor(cluster))) +
  geom_point() +
  theme_minimal() +
  labs(x = expression(x[1]), y = expression(x[2]), color = "Cluster") +
  ggtitle("DBSCAN 1st/Default Parameter") +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title  = element_text(size = 20, hjust = 0.5))

# ---------------------------------------------- 2nd parameter
# Melakukan DBSCAN
dbscan_result <- dbscan(moon_data, eps = 0.1, MinPts = 5)

# Menambahkan label cluster ke dalam dataset
moon_data$cluster <- dbscan_result$cluster

# Menampilkan scatter plot
ggplot(data = moon_data, aes(x = x, y = y, color = factor(cluster))) +
  geom_point() +
  theme_minimal() +
  labs(x = expression(x[1]), y = expression(x[2]), color = "Cluster") +
  ggtitle("DBSCAN 2nd Parameter") +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title  = element_text(size = 20, hjust = 0.5))

# ---------------------------------------------- 3rd parameter
# Melakukan DBSCAN
dbscan_result <- dbscan(moon_data, eps = 0.4, MinPts = 7)

# Menambahkan label cluster ke dalam dataset
moon_data$cluster <- dbscan_result$cluster

# Menampilkan scatter plot
ggplot(data = moon_data, aes(x = x, y = y, color = factor(cluster))) +
  geom_point() +
  theme_minimal() +
  labs(x = expression(x[1]), y = expression(x[2]), color = "Cluster") +
  ggtitle("DBSCAN 3rd Parameter") +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title  = element_text(size = 20, hjust = 0.5))

