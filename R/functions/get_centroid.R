get_lat_centroid <- function(lat){
  lat <- abs(lat)
  quants <- quantile(lat, probs = c(0.25, 0.5, 0.75), names = FALSE)
  max <- max(lat)
  min <- min(lat)
  centroid <- t(data.frame(c(quants, min, max)))
  row.names(centroid) <- c()
  colnames(centroid) <- c("Lower", "Median", "Upper", "Min", "Max")
  return(centroid)
}

#get_lat_centroid <- function(lat){
#  lat <- abs(lat)
#  mean <- mean(lat)
#  max <- max(lat)
#  min <- min(lat)
#  centroid <- cbind.data.frame(mean, min, max)
#  row.names(centroid) <- c()
#  colnames(centroid) <- c("Mean", "Min", "Max")
#  return(centroid)
#}