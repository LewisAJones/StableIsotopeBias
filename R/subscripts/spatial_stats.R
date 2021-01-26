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
stages <- unique(data$stage)

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
stages <- read.csv("https://paleobiodb.org/data1.2/intervals/list.txt?scale_id=1&scale_level=5")#get stage mid age for plotting later
stages$mid_ma <- (stages$max_ma+stages$min_ma)/2
#add stage abbreviation
stages$abbrev <- substr(stages$interval_name, 1, 3) 
#join to data
master <- plyr::join(x = master, y = stages, by = "interval_name", type = "left", match = "all")
#write data
write.csv(master, "./results/spatial_stats/stage_level_spatial_stats.csv", row.names = FALSE)
