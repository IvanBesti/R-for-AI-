# Install dan memuat paket arules
install.packages("arules")
install.packages("arulesViz")

# Memuat paket
library(arules)
library(arulesViz)

# Memuat data transaksi contoh
data("Groceries")

# Membuat objek transaksi
transaksi <- as(Groceries, "transactions")

# Menjalankan algoritma Apriori
rules <- apriori(transaksi,
                 parameter = list(support = 0.001, confidence = 0.5, minlen = 2))

# Menampilkan aturan yang ditemukan
inspect(rules)

# Menampilkan metrik pendukung aturan
summary(rules)

# Menyaring aturan berdasarkan kriteria tertentu
filtered_rules <- subset(rules, subset = lift > 1.5 & confidence > 0.6)
inspect(filtered_rules)

# Menampilkan visualisasi aturan menggunakan plot
plot(rules, method = "graph", control = list(type = "items"))

# Menampilkan visualisasi aturan menggunakan matrix plot
plot(rules, method = "matrix", control = list(reorder = "support"))

# Menjalankan perhitungan aturan berdasarkan ukuran lift
lift <- sort(rules, by = "lift")
inspect(lift)
