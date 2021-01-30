#binned values
stages <- read.csv("./data/stage_bins.csv")
#read data
data <- read.csv("./results/SST/extracted_temp.csv")
#calculate stage values
stages$temperature <- NA
stages$ext_temperature <- NA
#calcualte mean temperature 
for(i in 1:nrow(stages)){
  out <- subset(data, age <= stages$max_ma[i] & age >= stages$min_ma[i])
  stages$temperature[i] <- mean(out$temperature, na.rm = TRUE)
  stages$ext_temperature[i] <- mean(out$modern_temp_ext, na.rm = TRUE)
}

write.csv(stages, "./results/SST/binned_vals.csv", row.names = FALSE)