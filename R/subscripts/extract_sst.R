# Extract sea surface temperature data
# Lewis A. Jones
# March 2020
#---------------------------------
# Load package
library(raster)
source("./R/options.R")
#---------------------------------
data <- read.csv("./data/cleaned_StabIsoDB.csv")
sst_artificial <- raster("./layers/Pre_industrial.nc")
data$modern_temp_mean <- cellStats(sst_artificial, mean)
data$modern_temp_ext <- raster::extract(x = sst_artificial, y = data[,c("palaeolng", "palaeolat")])

write.csv(data, "./results/SST/extracted_temp.csv", row.names = FALSE)

sst_lat <- sapply(1:nrow(sst_artificial), function(i){mean(sst_artificial[i,], na.rm = TRUE)}) 
sst <- sst_lat
lats <- seq(from = -90, to = 90, length.out = nrow(sst_artificial))

dat <- cbind.data.frame(lats, sst)

plot(dat)

write.csv(dat, "./results/SST/lat_temp_curve.csv", row.names = FALSE)
