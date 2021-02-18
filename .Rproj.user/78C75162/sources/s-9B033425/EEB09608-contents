#get palaeocoordinates

#Example
#palaeorotate(lng = 5, lat = 45, age = 300)

palaeorotate <- function(lng, lat, age){
  #create API for rotation
  API <- paste("http://gws.gplates.org//reconstruct/reconstruct_points/?points=", lng, ",", lat, "&time=", age, "&model=PALEOMAP", sep ="")
  #pull data
  dat <- readLines(API, warn = F)
  #format data
  dat <- matrix(as.numeric(unlist(regmatches(dat, gregexpr("-?[[:digit:]]+\\.*[[:digit:]]+", dat)))), ncol=2, byrow = TRUE)
  dat <- data.frame(dat)
  colnames(dat) <- c("palaeolng", "palaeolat")
  
  if(nrow(dat) == 0){dat[1,] <- NA}
  
  return(dat)
}
