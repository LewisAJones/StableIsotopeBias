calculate_T <- function(d,t,mineral,carbonate_equation = "Veizer trend",apatite_equation = "trend", dO18_water = -1.1) {
  if(mineral == "calcite") {
    if(!(identical(d,numeric(0))) & !(identical(t, numeric(0))) & carbonate_equation == "Veizer trend") 16.9 - 4*(d- (-0.00003*t^2 + 0.0046*t) -0.27) else
      if(!(identical(d,numeric(0))) & !(identical(t, numeric(0))) & carbonate_equation == "Veizer no trend") 16.9 - 4*(d- (dO18_water) -0.27) else 
        if(!(identical(d,numeric(0))) & !(identical(t, numeric(0))) & carbonate_equation == "Hays no trend") 15.7 - 4.36*(d - dO18_water) + 0.12*(d-0.6-dO18_water)^2 else
          
          NA
  } else 
    
    if(mineral == "aragonite") {
      if(!(identical(d,numeric(0))) & !(identical(t, numeric(0))) & carbonate_equation == "Veizer trend") 16.9 - 4*(d-0.6- (-0.00003*t^2 + 0.0046*t) -0.27) else
        if(!(identical(d,numeric(0))) & !(identical(t, numeric(0))) & carbonate_equation == "Veizer no trend") 16.9 - 4*(d-0.6 - dO18_water -0.27) else
          if(!(identical(d,numeric(0))) & !(identical(t, numeric(0))) & carbonate_equation == "Hays no trend") 15.7 - 4.36*(d-0.6 - dO18_water) + 0.12*(d-0.6-dO18_water)^2 else
            
            NA
    } else 
      
      if(mineral == "apatite") {
        if(!(identical(d,numeric(0))) & !(identical(t, numeric(0))) & apatite_equation == "trend") 117.4 - 4.5*(d - (-0.00003*t^2 + 0.0046*t)) else
          if(!(identical(d,numeric(0))) & !(identical(t, numeric(0))) & apatite_equation == "no trend") 117.4 - 4.5*(d - (dO18_water)) else
            NA
      } else NA
  
}