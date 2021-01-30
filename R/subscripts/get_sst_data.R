# Get sea surface temperature data
# Lewis A. Jones
# March 2020
#---------------------------------
# Load package
library(sdmpredictors)
library(raster)
library(matrixStats)
library(ggplot2)
source("./R/options.R")
#---------------------------------
# Explore datasets in the package
#list_datasets()

# Explore layers in a dataset
#list_layers()

# Download specific layers to the current directory
sst <- load_layers(c("BO_sstmean"))
sst <- sst$BO_sstmean
mean <- rowMeans(as.matrix(sst), na.rm = TRUE)
sd <- rowSds(as.matrix(sst), na.rm = TRUE)
lats <- seq(-90+(180/nrow(sst)/2), 90-(180/nrow(sst)/2), by = 180/nrow(sst))

df <- cbind.data.frame(mean, sd, lats)
df <- na.omit(df)

p <- ggplot(df) +
  geom_errorbar(aes(x=lats, ymin=mean-sd, ymax=mean+sd), width=0.4, colour="#2ca25f", alpha=0.9, size=1.3) +
  geom_line(aes(x=lats, y=mean), colour="black", size = 1.25, alpha=0.7) +
  labs(x = expression(bold(paste("Latitude (",degree,")"))), y = expression(bold(paste("Sea surface temperature (",degree, C,")")))) +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.grid.minor.y = element_line(colour = "grey90"),
        panel.grid.minor.x = element_line(colour = "grey90"),
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        legend.position = c(0.2, 0.9),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = NA, fill = NA),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.margin=unit(0, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 0),
        axis.title.y = element_text(size = 14, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 14, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 14, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        aspect.ratio = 0.4)
  
p

ggsave(file = "./results/SST/sst_gradient.png", plot = p, width = 150, height = 75, units = "mm", dpi = 300, scale = 1.7)



#r <- raster(res = spatial_res)
#sst <- resample(sst, r) #resample to elected size for building spatial model


sst_artificial <- raster(res = res(sst), val = rep(mean, each = ncol(sst)))
plot(sst_artificial)

writeRaster(sst, "./results/SST/SST_raster.tif", overwrite = TRUE) #save data
writeRaster(sst_artificial, "./results/SST/SST_artificial.tif", overwrite = TRUE) #save data

