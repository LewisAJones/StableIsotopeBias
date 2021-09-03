#binned values
stages <- read.csv("./data/stage_bins.csv")
#read data
data <- read.csv("./results/SST/extracted_temp.csv")
#calculate stage values
stages$temperature_stage <- NA
stages$ext_temperature_stage_modern <- NA
stages$ext_temperature_stage_eocene <- NA
#calculate mean temperature 
for(i in 1:nrow(stages)){
  out <- subset(data, gts2012 <= stages$max_ma[i] & gts2012 >= stages$min_ma[i])
  stages$temperature_stage[i] <- mean(out$temperature, na.rm = TRUE)
  stages$ext_temperature_stage_modern[i] <- mean(out$modern_ext_t, na.rm = TRUE)
  stages$ext_temperature_stage_eocene[i] <- mean(out$eocene_ext_t, na.rm = TRUE)
}

write.csv(stages, "./results/SST/stage_binned_vals.csv", row.names = FALSE)

#binned values
myr10 <- data.frame(seq(from = 0, to = 510, by = 10))
colnames(myr10) <- c("myr10")
#read data
data <- read.csv("./results/SST/extracted_temp.csv")
#calculate stage values
myr10$temperature_stage <- NA
myr10$ext_temperature_stage_modern <- NA
myr10$ext_temperature_stage_eocene <- NA
#calculate mean temperature 
for(i in 1:nrow(myr10)){
  out <- subset(data, gts2012 >= myr10[i, "myr10"] & gts2012 <= myr10[i+1, "myr10"])
  myr10$temperature_stage[i] <- mean(out$temperature, na.rm = TRUE)
  myr10$ext_temperature_stage_modern[i] <- mean(out$modern_ext_t, na.rm = TRUE)
  myr10$ext_temperature_stage_eocene[i] <- mean(out$eocene_ext_t, na.rm = TRUE)
  myr10$myr10[i] <- (myr10[i, "myr10"] + myr10[i+1, "myr10"])/2
}

write.csv(myr10, "./results/SST/10myr_binned_vals.csv", row.names = FALSE)
