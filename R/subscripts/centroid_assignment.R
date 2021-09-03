#centroid assignment
library(sf)
library(spData)
library(dplyr)
library(sp)

data <- read.csv("./data/Cleaned_Veizer_19_02_2021.csv")
df <- subset(data, comments == "Random assignment")
df <- df[,c("literature", "Location")]
df <- unique(df)
df$lon <- NA
df$lat <- NA

for(i in 1:nrow(df)){
  country <- world[world$name_long == df$Location[i], 0]
  centroid <- st_coordinates(st_centroid(x = country))
  df$lon[i] <- as.numeric(centroid[1,"X"])
  df$lat[i] <- as.numeric(centroid[1,"Y"])
}

for(i in 1:nrow(df)){
  vec <- which(data$literature == df$literature[i] & data$Location == df$Location[i])
  data[vec,c("lon", "lat")] <- df[i, c("lon","lat")]
}


write.csv(data, "./data/Cleaned_Veizer_Centroid.csv")

beepr::beep(2)