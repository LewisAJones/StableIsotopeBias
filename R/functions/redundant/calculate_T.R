#calculate temperature
#d = data (d18O per mil)
#t = time (million of years)

calculate_T <- function(d,t) { 
  if(!(identical(d,numeric(0))) & !(identical(t, numeric(0)))) 16.9 - 4*(d- (-0.00003*t^2 + 0.0046*t) -0.27)
  else NA
}