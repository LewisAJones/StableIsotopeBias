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
data <- read.csv("./data/rotated_Veizer_data.csv")
data <- subset(data, !is.na(palaeolng) | !is.na(palaeolat))

#get unique stages
stages <- unique(data$interval_name)

#run for loop for each stage
master <- data.frame()
for(s in stages){
  interval_name <- s #save stage name for later
  out <- subset(data, interval_name == s) #subset by stage
  MST <- get_MST(xy = out[,c("palaeolng", "palaeolat")]) #get MST
  occupancy <- get_occupancy(xy = out[,c("palaeolng", "palaeolat")], spacing = spacing) #get grid cell occupancy
  terrestrial <- get_occupancy(xy = subset(out, ODP == 0)[,c("palaeolng", "palaeolat")], spacing = spacing) #get grid cell occupancy
  ODP <- get_occupancy(xy = subset(out, ODP == 1)[,c("palaeolng", "palaeolat")], spacing = spacing) #get grid cell occupancy
  centroid <- get_lat_centroid(lat = out[,c("palaeolat")])
  out <- cbind.data.frame(interval_name, MST, occupancy, terrestrial, ODP, centroid) #bind data
  master <- rbind.data.frame(master, out) #bind to master dataframe
}
#download stages
stages <- read.csv("./data/stage_bins.csv")#get stage mid age for plotting later
#join to data
master <- plyr::join(x = stages, y = master, by = "interval_name", type = "left", match = "all")
#any stages with only one point replace with NA
#vec <- which(is.na(master$upper))
#master[vec, c("upper", "mean", "lower")] <- NA
#write data
write.csv(master, "./results/spatial_stats/stage_level_spatial_stats.csv", row.names = FALSE)
