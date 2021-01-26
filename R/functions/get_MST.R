get_MST <- function(xy){
  
    MST <- 0
    
    if(nrow(xy) > 1){
      gcdists <- geosphere::distm(xy, fun = geosphere::distGeo)
      mst_sp <- vegan::spantree(gcdists)
      MST <- sum(mst_sp$dist)/1000
      MST <- round(MST, digits = 2)
    }
  
  return(MST)
}
