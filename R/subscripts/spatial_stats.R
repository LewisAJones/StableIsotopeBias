#calculate spatial statistics
######################
library(dggridR)
library(vegan)
library(geosphere)
library(plyr)
source("./R/options.R")
source("./R/functions/get_MST.R")
source("./R/functions/get_occupancy.R")
source("./R/functions/get_centroid.R")
######################
#read data
data <- read.csv("./data/cleaned_StabIsoDB.csv")

#get unique stages
stages <- unique(data$interval_name)

#run for loop for each stage
master <- data.frame()
for(s in stages){
  interval_name <- s #save stage name for later
  out <- subset(data, stage == s) #subset by stage
  MST <- get_MST(xy = out[,c("palaeolng", "palaeolat")]) #get MST
  occupancy <- get_occupancy(xy = out[,c("palaeolng", "palaeolat")], spacing = spacing) #get grid cell occupancy
  centroid <- get_lat_centroid(lat = out[,c("palaeolat")])
  out <- cbind.data.frame(interval_name, MST, occupancy, centroid) #bind data
  master <- rbind.data.frame(master, out) #bind to master dataframe
}
#download stages
stages <- read.csv("./data/stage_bins.csv")#get stage mid age for plotting later
#join to data
master <- plyr::join(x = master, y = stages, by = "interval_name", type = "left", match = "all")
#write data
write.csv(master, "./results/spatial_stats/stage_level_spatial_stats.csv", row.names = FALSE)
