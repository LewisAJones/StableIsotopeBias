#data prep and clean
######################
#load libraries and functions
library(dplyr)
library(pbapply)
library(chronosphere)
source("./R/functions/calculate_T.R")
######################
#load data
data <- read.csv("./data/Cleaned_Veizer_Centroid.csv")

#remove data without coordinates
data <- subset(data, !is.na(lat) & !is.na(lon))

#calculate temperate
data <- subset(data, !is.na(d18O))

#correct for aragonite-calcite d18O differences as in Grossman and Joachimski (2020)

data$temperature <- sapply(1:nrow(data), function(x) 
  calculate_T(d = data$d18O[x], t = data$gts2012[x], mineral = "calcite", 
              carbonate_equation = "Veizer trend", apatite_equation = "trend"))

#round age of data
data$rounded_age <- round(data$gts2012)

#palaeorotate coordinates
pm <- fetch("paleomap", "model", datadir="./data/model/") #download plate model

uniq <- unique(data[,c("lon", "lat", "rounded_age")])
ages <- unique(uniq$rounded_age)

rotations <- pblapply(1:length(ages), function(i) {
  rot_age <- ages[i]
  rot_coords <- subset(uniq, rounded_age == rot_age)[, c("lon", "lat")]
  coords <-   reconstruct(x = rot_coords, #coordinates of data
                          age = rot_age, #age of data 
                          model=pm, #plate model
                          dir = "./data/model/", #directory of plate model
                          #path.gplates="/Volumes/GPlates-2.2.0-Darwin-x86_64/",
                          cleanup = TRUE,
                          verbose = FALSE) 
  
  files <- list.files("./data/model/", full.names = TRUE)
  do.call(file.remove, list(files))
  coords <- round(coords, digits = 2)
  colnames(coords) <- c("palaeolng", "palaeolat")
  coords <- data.frame(coords)
  coords$rounded_age <- rot_age
  coords <- cbind.data.frame(rot_coords, coords)
  coords
  
})

rotations <- dplyr::bind_rows(rotations)

data <- plyr::join(x = data, y = rotations, type = "full", by = c("lon" = "lon", "lat" = "lat", "rounded_age" = "rounded_age"), match = "all")

stages <- read.csv("./data/stage_bins.csv")

data$mid_ma <- NA

for(i in 1:nrow(stages)){
  vec <- which(data$gts2012 <= stages$max_ma[i] & data$gts2012 >= stages$min_ma[i])
  data$mid_ma[vec] <- stages$mid_ma[i]
}

data <- plyr::join(x = data, y = stages, type = "full", by = c("mid_ma"), match = "all")

myr10 <- seq(from = 0, to = 510, by = 10)

data$mid_ma_10myr <- NA

for(i in 1:length(myr10)){
  vec <- which(data$gts2012 >= myr10[i] & data$gts2012 <= myr10[i+1])
  data$mid_ma_10myr[vec] <- (myr10[i] + myr10[i+1])/2
}

write.csv(data, "./data/rotated_Veizer_data.csv", row.names = FALSE)
