library(chronosphere)
library(raster)
library(dplyr)
library(ncdf4)
library(rgeos)
library(rgdal)
###Read the isotope data set
idb <- read.csv("./data/rotated_Veizer_data.csv")
periods <- read.csv("./data/period_bins.csv")

idb <- subset(idb, interval_name == "Aptian")

dems <- fetch(dat="paleomap", var="dem", res=1, ver="20190719")

selection <- c("120")
vec <- which(names(dems) %in% selection)

r <- brick(dems[[vec[1]]])

r <- stack(r)

crs(r) <- CRS('+proj=longlat')

r <- disaggregate(r, 10, method='bilinear')
r[r >= 0 & r < 1500] <- 2
r[r >= 1500] <- 1
r[r >= -300 & r <= 0] <- 3
r[r < 0] <- 4

rasMoll <- projectRaster(r, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84")
xy <- unique(cbind(idb$palaeolng, idb$palaeolat))
colnames(xy) <- c("x", "y")
xy <- project(xy = xy, proj = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84")

jpeg("./figures/Aptian.jpg", width = 300, height = 200, res = 600, units = "mm")
par(mar=c(1,4,1,1), oma=c(1,1,1,1))
plot(rasMoll, col = c(rgb(0.825,0.725,0.625,1), rgb(0.9,0.8,0.7,1), rgb(0.85,0.99,0.99,1), rgb(0.6,0.85,0.9,1)), legend = F, add = F, 
     box = F, axes = F, bg = "white", ylim = c(-9000000,9000000),
     xpd = F, interpolate = TRUE)

points(xy, pch = 1, col = "black", lwd = 3, cex = 0.5)

dev.off()


