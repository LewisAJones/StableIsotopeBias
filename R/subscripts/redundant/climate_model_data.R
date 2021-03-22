library(raster)

r <- raster(res = 1)
modern <- raster("./data/layers/Pre_industrial.nc")
albian <- raster("./data/layers/Albian.nc")

modern <- resample(modern, r)
albian <- resample(albian, r)

lats <- data.frame(rasterToPoints(albian))
lats <- unique(lats$y)

modern <- rowMeans(as.matrix(modern), na.rm = TRUE)
albian <- rowMeans(as.matrix(albian), na.rm = TRUE)
lats <- seq(-90, 90, length.out = length(albian))

data <- cbind.data.frame(modern, albian, lats)

plot(x = lats, y = albian, data = data, xlab = "Latitude", ylab = "Temperature", pch = 20, col = "red")
points(x = lats, y = modern, data = data, pch = 20, col = "blue")
legend("topleft", legend=c("Albian", "Modern"),
       col=c("red", "blue"), pch = 20, cex=0.8)
