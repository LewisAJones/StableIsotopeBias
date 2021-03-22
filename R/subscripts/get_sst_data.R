# Get sea surface temperature data
# Lewis A. Jones
# March 2020
#---------------------------------
# Load package
library(sdmpredictors)
library(raster)
library(matrixStats)
library(ggplot2)
library(ggpubr)
library(ggpmisc)
source("./R/options.R")
#---------------------------------
# Explore datasets in the package
#list_datasets()

# Explore layers in a dataset
#list_layers()

# Download specific layers to the current directory
sst <- load_layers(c("BO_sstmean"))
sst <- sst$BO_sstmean
sst <- resample(sst, y = raster(res = 1))

mean <- rowMeans(as.matrix(sst), na.rm = TRUE)
sd <- rowSds(as.matrix(sst), na.rm = TRUE)
lats <- seq(-90+(180/nrow(sst)/2), 90-(180/nrow(sst)/2), by = 180/nrow(sst))

df <- cbind.data.frame(mean, sd, lats)
df <- na.omit(df)

sst_artificial <- raster(res = res(sst), val = rep(mean, each = ncol(sst)))
plot(sst_artificial)

writeRaster(sst, "./results/SST/SST_raster.tif", overwrite = TRUE) #save data
writeRaster(sst_artificial, "./results/SST/SST_artificial.tif", overwrite = TRUE) #save data

p1 <- ggplot(df, aes(x = abs(lats), y = mean)) +
  geom_point(colour="black", size = 1.25, alpha=0.7) +
  stat_smooth(method = "gam", fullrange = TRUE, xseq = seq(0,90, by=1)) +
  scale_x_continuous(expand = c(0,0), limits = c(-2, 92), breaks = seq(0, 90, 30), labels = seq(0, 90, 30)) +
  scale_y_continuous(expand = c(0,0), limits = c(-5, 32), breaks = seq(0, 30, 10), labels = seq(0, 30, 10)) +
  labs(subtitle = "Modern", x = expression(bold(paste("Latitude (",degree,")"))), y = expression(bold(paste("Sea surface temperature (",degree, C,")")))) +
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
  
p1

data <- read.csv("./data/zhang_et_al_2019_eocene.csv")

p2 <- ggplot(data, aes(x = abs(paleolat), y = MAT)) +
  geom_point(colour="black", size = 1.25, alpha=0.7) +
  stat_smooth(method = "gam", fullrange = TRUE, xseq = seq(0,90, by=1)) +
  scale_x_continuous(expand = c(0,0), limits = c(-2, 92), breaks = seq(0, 90, 30), labels = seq(0, 90, 30)) +
  scale_y_continuous(expand = c(0,0), limits = c(-5, 42), breaks = seq(0, 40, 10), labels = seq(0, 40, 10)) +
  labs(subtitle = "Late Paleocene-Early Eocene", x = expression(bold(paste("Latitude (",degree,")"))), y = expression(bold(paste("Sea surface temperature (",degree, C,")")))) +
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

p2

p <- ggarrange(p1, p2, ncol=1, nrow=2, widths = c(1,1), labels = "AUTO", align = "v")

ggsave(file = "./figures/sst_gradient.png", plot = p, width = 150, height = 150, units = "mm", dpi = 300, scale = 1.25)

modern <- ggplot_build(p1)$data[[2]]
eocene <- ggplot_build(p2)$data[[2]]

modern <- modern[,1:5]
eocene <- eocene[,1:5]

write.csv(modern, "./results/SST/modern_temp_grad.csv", row.names = FALSE)
write.csv(eocene, "./results/SST/eocene_temp_grad.csv", row.names = FALSE)

data <- read.csv("./data/rotated_Veizer_data.csv")

data$modern_ext_t <- NA

for(i in 1:nrow(modern)){
  vec <- which(round(abs(data$palaeolat )) == modern$x[i])
  data$modern_ext_t[vec] <- modern$y[i]
}

data$eocene_ext_t <- NA

for(i in 1:nrow(eocene)){
  vec <- which(round(abs(data$palaeolat )) == eocene$x[i])
  data$eocene_ext_t[vec] <- eocene$y[i]
}

write.csv(data, "./results/SST/extracted_temp.csv", row.names = FALSE)

#global mean temp

ras_area <- area(x = raster(res = 1))
#ras_area <- mask(x = ras_area, mask = sst)
weights <- rowSums(as.matrix(ras_area), na.rm = TRUE)
weights <- weights/sum(weights)

global_modern <- rev(modern$y)
global_modern <- weighted.mean(x = global_modern, w = weights[1:91], na.rm = TRUE)

global_eocene <- rev(eocene$y)
global_eocene <- weighted.mean(x = global_eocene, w = weights[1:91], na.rm = TRUE)
