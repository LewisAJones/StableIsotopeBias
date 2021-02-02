#run analyses
source("./R/subscripts/data_clean.R")
rm(list = ls())
source("./R/subscripts/spatial_stats.R")
rm(list = ls())
source("./R/subscripts/get_sst_data.R")
rm(list = ls())
source("./R/subscripts/get_binned_vals.R")
rm(list = ls())

#figures
source("./R/figures/spatial_stats.R")
rm(list = ls())
source("./R/figures/sst_plot.R")
rm(list = ls())
source("./R/figures/mapplot_period.R")
rm(list = ls())