LBG_type <- function(type, sd = 20, res = 1){
  
  if(type != "unimodal" & type != "bimodal" & type != "flat"){
    warning("type must be unimodal, bimodal or flat")
  }
  
  r <- raster::raster(res = res, val = 1)
  
  if(type == "unimodal"){
    x <- seq(from = -89, to = 90, by = 1)
    y <- dnorm(x, mean = 0, sd = sd)
    r[1:nrow(r),] <- rep(y, each = ncol(r))
    r <- (r-raster::cellStats(r,"min"))/(raster::cellStats(r,"max")-raster::cellStats(r,"min"))
    plot(r, main = "Unimodal-type", legend = FALSE)
  }
  
  
  if(type == "bimodal"){
    x <- seq(from = 0, to = 89, by = 1)
    y <- dnorm(x, mean = 45, sd = sd)
    y <- append(y, y)
    r[1:nrow(r),] <- rep(y, each = ncol(r))
    r <- (r-raster::cellStats(r,"min"))/(raster::cellStats(r,"max")-raster::cellStats(r,"min"))
    plot(r, main = "Bimodal-type", legend = FALSE)
  }
  
  if(type == "flat"){
    plot(r, main = "Flat-type", col = "green3", legend = FALSE)
  }
  return(r)
}

