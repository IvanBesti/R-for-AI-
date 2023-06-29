# Instalasi paket
install.packages("imager")
install.packages("magick")
install.packages("ggplot2")

# Mengimpor paket image
library(imager)
library(magick)
library(ggplot2)

# Membaca gambar
img <- image_read("image.jpeg")

# Mengecek informasi gambar
info <- image_info(img)
width <- info$width
height <- info$height
cat(paste0("Lebar: ", width, "\nTinggi: ", height))

# Menampilkan gambar dan dimensi
plot(0, 0, type="n", xlim=c(0, width), ylim=c(0, height), xlab="Gambar Original", ylab="tinggi")
rasterImage(as.raster(img), 0, 0, width, height)

# Mengecek dimensi gambar
dim(img_jpeg)

# Konversi objek menjadi "magick image object"
img_magick <- image_convert(img, "gray")
plot(img_magick)

# Menampilkan gambar asli dan hasil konversi menjadi gray di plot yang sama
info1 <- image_info(img_magick)
par(mfrow=c(1, 2))
plot(0, 0, type="n", xlim=c(0, info$width), ylim=c(0, info$height), xlab="Gambar Original", ylab="")
rasterImage(as.raster(img_jpeg), 0, 0, info$width, info$height)
plot(0, 0, type="n", xlim=c(0, info1$width), ylim=c(0, info1$height), xlab="Gambar Objek", ylab="")
rasterImage(as.raster(img_magick), 0, 0, info1$width, info1$height)

# Melakukan rotasi vertikal
img_flipv <- image_flip(img_magick)
info_flip_v <- image_info(img_flipv)

# Melakukan rotasi horizontal
img_fliph <- image_flop(img_magick) 
info_flip_h <- image_info(img_fliph)

# Menampilkan gambar asli, rotasi horizontal, dan rotasi vertikal
par(mfrow=c(1, 3))
plot(0, 0, type="n", xlim=c(0, info$width), ylim=c(0, info$height), xlab="Gambar Original", ylab="")
rasterImage(as.raster(img_jpeg), 0, 0, info$width, info$height)
plot(0, 0, type="n", xlim=c(0, info_flip_h$width), ylim=c(0, info_flip_h$height), xlab="Rotasi Horizontal", ylab="")
rasterImage(as.raster(img_fliph), 0, 0, info_flip_h$width, info_flip_h$height)
plot(0, 0, type="n", xlim=c(0, info_flip_v$width), ylim=c(0, info_flip_v$height), xlab="Rotasi Vertikal", ylab="")
rasterImage(as.raster(img_flipv), 0, 0, info_flip_v$width, info_flip_v$height)

# Melakukan crop 
img_crop <- image_crop(img_magick, "400x400+200+200")
img_crop
info_crop <- image_info(img_crop)

# Menampilkan gambar asli dan gambar crop
par(mfrow=c(1, 2))
plot(0, 0, type="n", xlim=c(0, info$width), ylim=c(0, info$height), xlab="Gambar Original", ylab="")
rasterImage(as.raster(img_jpeg), 0, 0, info$width, info$height)
plot(0, 0, type="n", xlim=c(0, info_crop$width), ylim=c(0, info_crop$height), xlab="Gambar Crop", ylab="")
rasterImage(as.raster(img_crop), 0, 0, info_crop$width, info_crop$height)

# Melakukan rotasi 45 derajat
img_rotate1 <- image_rotate(img_magick, degrees = 45)
img_rotate1
info_rotate1 <- image_info(img_rotate)
plot(img_rotate1)

# Melakukan rotasi -45 derajat
img_rotate2 <- image_rotate(img_magick, degrees = -45)
img_rotate2
info_rotate2 <- image_info(img_rotate)

# Menampilkan gambar asli, gambar rotasi 45 derajat dan -45 derajat
par(mfrow=c(1, 3))
plot(0, 0, type="n", xlim=c(0, info$width), ylim=c(0, info$height), xlab="Gambar Original", ylab="")
rasterImage(as.raster(img_jpeg), 0, 0, info$width, info$height)
plot(0, 0, type="n", xlim=c(0, info_rotate1$width), ylim=c(0, info_rotate1$height), xlab="Gambar Rotasi 45 derajat", ylab="")
rasterImage(as.raster(img_rotate1), 0, 0, info_rotate1$width, info_rotate1$height)
plot(0, 0, type="n", xlim=c(0, info_rotate2$width), ylim=c(0, info_rotate2$height), xlab="Gambar Rotasi -45 derajat", ylab="")
rasterImage(as.raster(img_rotate2), 0, 0, info_rotate2$width, info_rotate2$height)

# Menambahkan efek blur pada gambar
img_blur <- image_blur(img_magick, radius = 10, sigma = 10)
img_blur
info_blur <- image_info(img_blur)

# Menampilkan gambar asli dan gambar blur
par(mfrow=c(1, 2))
plot(0, 0, type="n", xlim=c(0, info$width), ylim=c(0, info$height), xlab="Gambar Original", ylab="")
rasterImage(as.raster(img_jpeg), 0, 0, info$width, info$height)
plot(0, 0, type="n", xlim=c(0, info_blur$width), ylim=c(0, info_blur$height), xlab="Gambar Blur", ylab="")
rasterImage(as.raster(img_blur), 0, 0, info_blur$width, info_blur$height)

# Menambahkan efek noise pada gambar
img_noise <- image_noise(img_magick, noisetype = "gaussian")
img_noise
info_noise <- image_info(img_noise)

# Menampilkan gambar asli dan gambar noisy
par(mfrow=c(1, 2))
plot(0, 0, type="n", xlim=c(0, info$width), ylim=c(0, info$height), xlab="Gambar Original", ylab="")
rasterImage(as.raster(img_jpeg), 0, 0, info$width, info$height)
plot(0, 0, type="n", xlim=c(0, info_noise$width), ylim=c(0, info_noise$height), xlab="Gambar Diberi Noise", ylab="")
rasterImage(as.raster(img_noise), 0, 0, info_noise$width, info_noise$height)

# Mengkonversi foto ke dalam format JPEG
img_rotate_jpeg <- image_convert(img_rotate, format = "jpeg")
