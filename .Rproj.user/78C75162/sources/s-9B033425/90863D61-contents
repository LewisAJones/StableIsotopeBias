#data prep and clean
######################
#load libraries and functions
library(dplyr)
library(pbapply)
library(chronosphere)
source("./R/functions/palaeorotate.R")
source("./R/functions/calculate_T.R")
######################
#load data
data <- read.csv("./data/Cleaned_Veizer_15_02_2021.csv")

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
pm <- fetch("paleomap", "model", datadir="./data/") #download plate model

uniq <- unique(data[,c("lon", "lat", "rounded_age")])

rotations <- data.frame(t(pbsapply(1:nrow(uniq), function(i) {
  coords <- palaeorotate(lng = uniq$lon[i], lat = uniq$lat[i], age = uniq$rounded_age[i])
  #coords <-   reconstruct(x = uniq[i, c("lon", "lat")], #coordinates of data
  #                        age = uniq$rounded_age[i], #age of data 
  #                        model=pm, #plate model
  #                        dir = "./data/", #directory of plate model
  #                        path.gplates="C:/Program Files/GPlates/GPlates 2.2.0/gplates-2.2.0.exe",
  #                        cleanup = TRUE,
  #                        verbose = TRUE) 
  
  #files <- list.files("./data/", full.names = TRUE)
  #files <- files[files != c("./data/Cleaned_Veizer_15_02_2021.csv")]
  #files <- files[files != c("./data/stage_bins.csv")]
  #do.call(file.remove, list(files))
  coords <- round(coords, digits = 2)
  coords
  
}, simplify = TRUE)))

colnames(rotations) <- c("palaeolng", "palaeolat")

rotations <- cbind.data.frame(uniq, rotations)

data <- plyr::join(x = data, y = rotations, type = "full", by = c("lon" = "long", "lat" = "lat", "rounded_age" = "rounded_age"), match = "all")

stages <- read.csv("./data/stage_bins.csv")

data$mid_ma <- NA

for(i in 1:nrow(stages)){
  vec <- which(data$age <= stages$max_ma[i] & data$age >= stages$min_ma[i])
  data$mid_ma[vec] <- stages$mid_ma[i]
}

data <- plyr::join(x = data, y = stages, type = "full", by = c("mid_ma"), match = "all")

write.csv(data, "./data/rotated_Veizer_data.csv", row.names = FALSE)
