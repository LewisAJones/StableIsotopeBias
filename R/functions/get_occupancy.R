get_occupancy <- function(xy, spacing){
  
  dggs <- dggridR::dgconstruct(spacing = spacing, resround='nearest', metric = TRUE)
  cell <- dggridR::dgGEO_to_SEQNUM(dggs = dggs, in_lon_deg = xy[,1], in_lat_deg = xy[,2])
  cell <- length(unique(cell$seqnum))
  
  return(cell)
}