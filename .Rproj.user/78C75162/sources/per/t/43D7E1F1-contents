#binned values
stages <- read.csv("./data/stage_bins.csv")
#read data
data <- read.csv("./results/SST/extracted_temp.csv")
#calculate stage values
stages$temperature_stage <- NA
stages$ext_temperature_stage_modern <- NA
stages$ext_temperature_stage_eocene <- NA
#calcualte mean temperature 
for(i in 1:nrow(stages)){
  out <- subset(data, Age..Ma. <= stages$max_ma[i] & Age..Ma. >= stages$min_ma[i])
  stages$temperature_stage[i] <- mean(out$T.2010..., na.rm = TRUE)
  stages$ext_temperature_stage_modern[i] <- mean(out$modern_ext_t, na.rm = TRUE)
  stages$ext_temperature_stage_eocene[i] <- mean(out$eocene_ext_t, na.rm = TRUE)
}

write.csv(stages, "./results/SST/binned_vals.csv", row.names = FALSE)
