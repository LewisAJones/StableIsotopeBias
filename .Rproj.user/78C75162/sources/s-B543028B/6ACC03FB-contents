library(chronosphere)
library(raster)
library(dplyr)
library(ncdf4)
library(sf)
library(rgeos)
library(rgdal)
###Read the isotope data set
idb <- read.csv("./data/rotated_Veizer_data.csv")
periods <- read.csv("./data/period_bins.csv")

idb$period <- NA

for(i in 1:nrow(periods)){
  vec <- which(idb$gts2012 <= periods$max_ma[i] & idb$gts2012 >= periods$min_ma[i])
  idb$period[vec] <- periods$interval_name[i]
}

dems <- fetch(dat="paleomap", var="dem", res=1, ver="20190719")

selection <- c("0", "10", "40", "115", "175", "225", "270", "330", "385", "430", "460", "520")
vec <- which(names(dems) %in% selection)

Quaternary <- brick(dems[[vec[1]]])
Neogene <- brick(dems[[vec[2]]])
Paleogene <- brick(dems[[vec[3]]])
Cretaceous <- brick(dems[[vec[4]]])
Jurassic <- brick(dems[[vec[5]]])
Triassic <- brick(dems[[vec[6]]])
Permian <- brick(dems[[vec[7]]])
Carboniferous <- brick(dems[[vec[8]]])
Devonian <- brick(dems[[vec[9]]])
Silurian <- brick(dems[[vec[10]]])
Ordovician <- brick(dems[[vec[11]]])
Cambrian <- brick(dems[[vec[12]]])

stk <- stack(Quaternary, Neogene, Paleogene, Cretaceous, Jurassic, Triassic, 
             Permian, Carboniferous, Devonian, Silurian, Ordovician, Cambrian)

names(stk) <- c("Quaternary", "Neogene", "Paleogene", "Cretaceous", "Jurassic", "Triassic", 
                "Permian", "Carboniferous", "Devonian", "Silurian", "Ordovician", "Cambrian")

crs(stk) <- CRS('+proj=longlat')

stk <- resample(x = stk, y = raster(res = 0.1))
stk[stk >= 0 & stk < 1500] <- 2
stk[stk >= 1500] <- 1
stk[stk >= -300 & stk <= 0] <- 3
stk[stk < 0] <- 4

#plot(stk$Quaternary)

stk <- projectRaster(stk, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84")

period_plot <- function(x, xy){
  par(mfrow = c(4,3), mar = c(1,0.1,1,0.1))
  for(i in nlayers(x):1){
    
    name <- as.character(names(x)[i])
    xy <- subset(idb, period == name)
    xy <- unique(cbind(xy$palaeolng, xy$palaeolat))
    colnames(xy) <- c("x", "y")
    xy <- project(xy = xy, proj = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84")
    
    plot(x[[i]], col = c(rgb(0.825,0.725,0.625,1), rgb(0.9,0.8,0.7,1), rgb(0.85,0.99,0.99,1), rgb(0.6,0.85,0.9,1)), legend = F, add = F, 
         box = F, axes = F, bg = "white", ylim = c(-9000000,9000000),
         xpd = F, interpolate = TRUE, main = name)
    
    points(xy, pch = 1, col = rgb(0,0,0,0.4), lwd = 3, cex = 0.25)
    
  }

}
png("./figures/maps.png", width = 200, height = 150, res = 300, units = "mm")
period_plot(x = stk, xy = idb)
dev.off()

