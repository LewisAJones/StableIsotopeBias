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
data <- read.csv("./data/StabIsoDB.csv")

#remove data without coordinates
data <- subset(data, !is.na(lat) & !is.na(long))
                        
#retain only aragonite, calcite and apatite data
data <- data %>% filter(mineralogy %in% c("aragonite","calcite", "Calcite", "apatite"))

#update so database is uniform
data[which(data$mineralogy == "Calcite"),]$mineralogy <- c("calcite")

#remove unwanted taxonomic groups from dataset
data <- data %>% filter(!fossil.group %in% c("Fish", 
                                             "Conodont", 
                                             "Reptile", 
                                             "Icthyosaur", 
                                             "Worm", 
                                             "Brachiopod phosphate", 
                                             "na"))

#remove data not resolved to stage level
data <- data %>% filter(!stage %in% c("Neogene", 
                                      "Cretaceous"))

#remove data younger than Piacenzian
data <- data %>% filter(!stage %in% c("Modern", 
                                      "Holocene", 
                                      "Tarantian", 
                                      "Ionian", 
                                      "Calabrian", 
                                      "Gelasian"))

#clean up errors
data$stage[which(data$stage == "Kimmerigian")] <- c("Kimmeridgian")

#calculate temperate
data <- subset(data, !is.na(d18o.per.mille))

#correct for aragonite-calcite d18O differences as in Grossman and Joachimski (2020)

data[which(data$mineralogy == "aragonite"),]$d18o.per.mille <- data[which(data$mineralogy == "aragonite"),]$d18o.per.mille - 0.6

data$temperature <- sapply(1:nrow(data), function(x) 
  calculate_T(d = data$d18o.per.mille[x], t = data$age[x], mineral = data$mineralogy[x], 
              carbonate_equation = "Veizer trend", apatite_equation = "trend"))

#round age of data
data$rounded_age <- round(data$age)

#palaeorotate coordinates
pm <- fetch("paleomap", "model", datadir="./data/") #download plate model

uniq <- unique(data[,c("long", "lat", "rounded_age")])

rotations <- data.frame(t(pbsapply(1:nrow(uniq), function(i) {
  #coords <- palaeorotate(lng = uniq$long[i], lat = uniq$lat[i], age = uniq$rounded_age[i])
  coords <-   reconstruct(x = (uniq[i, c("long", "lat")]), #coordinate sof data
              age = uniq$rounded_age[i], #age of data 
              model=pm, #plate model
              dir = "./data/", #directory of plate model
              path.gplates="C:/Program Files (x86)/GPlates/GPlates 2.2.0/gplates-2.2.0.exe",
              cleanup = TRUE,
              verbose = TRUE) # directory of gplates
  
  files <- list.files("./data/", full.names = TRUE)
  files <- files[files != c("./data/StabIsoDB.csv")]
  #files <- files[files != c("./data/layers")]
  do.call(file.remove, list(files))
  coords <- round(coords, digits = 2)
  coords
  
  }, simplify = TRUE)))

colnames(rotations) <- c("palaeolng", "palaeolat")

rotations <- cbind.data.frame(uniq, rotations)

data <- plyr::join(x = data, y = rotations, type = "full", by = c("long" = "long", "lat" = "lat", "rounded_age" = "rounded_age"), match = "all")

write.csv(data, "./data/cleaned_StabIsoDB.csv", row.names = FALSE)
