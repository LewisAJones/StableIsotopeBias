#map plots
# 
library(rgdal)
library(ggpubr)
library(dplyr)
#
### Read in transformed map files
periodmaplisttrans <- readRDS("./R/figures/period_maps_transformed.rds")
# Define periods
periodnames <- c("Cambrian", "Ordovician", "Silurian", "Devonian", "Carboniferous", "Permian", "Triassic", "Jurassic", "Cretaceous", "Paleogene", "Neogene", "Quaternary")
periodnames_idb <- list("Cambrian", "Ordovician", "Silurian", "Devonian", "Carboniferous", "Permian", "Triassic", "Jurassic", "Cretaceous", "Paleogene", "Neogene", "Quaternary")
#
###Read the isotope data set
idb <- read.csv("./data/rotated_Veizer_data.csv")
periods <- read.csv("./data/period_bins.csv")

idb$periods <- NA

for(i in 1:nrow(periods)){
  vec <- which(idb$gts2012 <= periods$max_ma[i] & idb$gts2012 >= periods$min_ma[i])
  idb$periods[vec] <- periods$interval_name[i]
}

#idb <- subset(idb, interval_name == "Aptian")

### Function to plot

periodmap <- function(data, width1 = 20, height1 = 15) {
  periodcoordinates <- list(NULL)
  # transform coordinates from data (chose the correct columns)
  for(i in 1:12) {
    subs <- subset(data, periods %in% periodnames_idb[[i]])
    subs_coord <- unique(cbind(subs$palaeolng,subs$palaeolat))
    periodcoordinates[[i]] <-  project(subs_coord,proj = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84")
  }
  # plot the results on paleomaps
  #windows(width1,height1)
  par(mfrow = c(4,3), mar = c(1,1,1,1))
  for(i in 1:12) {
    p1 <- plot(periodmaplisttrans[[13]] , lwd=2, col=rgb(0.6,0.85,0.9,1), add = F, bg = "white",border = F, ylim = c(-9000000,9000000),
               xpd = T, main = periodnames[i])
    
    plot(periodmaplisttrans[[i]][[1]], col=rgb(0.85,0.99,0.99,1), add = T, bg = NA, border = F)
    
    plot(periodmaplisttrans[[i]][[2]], lwd=2, col= rgb(0.9,0.8,0.7,1), add = T, bg = NA,border = F)
    
    plot(periodmaplisttrans[[i]][[3]], lwd=2, col= rgb(0.825,0.725,0.625,1), add = T, bg = NA,border = F)
    
    points(periodcoordinates[[i]], pch = 1, col = rgb(0,0,0,0.4), lwd = 3, cex = 0.25)
    
  }
  
}

png("./figures/maps.png", width = 200, height = 150, res = 300, units = "mm")
periodmap(data = idb)
dev.off()

