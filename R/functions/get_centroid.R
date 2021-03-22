get_lat_centroid <- function(lat){
  lat <- abs(lat)
  mean <- mean(lat)
  max <- max(lat)
  min <- min(lat)
  centroid <- cbind.data.frame(mean, max, min)
  return(centroid)
}

