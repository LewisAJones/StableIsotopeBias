# Extract sea surface temperature data
# Lewis A. Jones
# March 2020
#---------------------------------
# Load package
library(raster)
source("./R/options.R")
#---------------------------------
data <- read.csv("./data/cleaned_StabIsoDB.csv")
sst_artificial <- raster("./results/SST/SST_artificial.tif")
data$modern_temp_mean <- cellStats(sst_artificial, mean)
data$modern_temp_ext <- raster::extract(x = sst_artificial, y = data[,c("palaeolng", "palaeolat")])

write.csv(data, "./results/SST/extracted_temp.csv", row.names = FALSE)

#sst_lat <- sapply(1:nrow(sst_artificial), function(i){mean(sst_artificial[i,], na.rm = TRUE)}) 
