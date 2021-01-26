get_lat_centroid <- function(lat){
  lat <- abs(lat)
  centroid <- mean(lat)
  LQ_centroid <- quantile(lat, probs = c(0.025), names = FALSE)
  UQ_centroid <- quantile(lat, probs = c(0.975), names = FALSE)
  centroid <- cbind.data.frame(centroid, LQ_centroid, UQ_centroid)
  return(centroid)
}

