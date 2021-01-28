#SST plot
#---------------------------------
library(ggplot2)
library(ggpubr)
library(raster)
library(matrixStats)
#---------------------------------
res <- raster(res = 0.1)
modern <- raster("./data/layers/Pre_industrial.nc")
albian <- raster("./data/layers/Albian.nc")

modern <- resample(modern, res)
albian <- resample(albian, res)

vals_mean_modern <- sapply(1:nrow(modern), function(x){mean(modern[x,], na.rm = TRUE)})
vals_sd_modern <- sapply(1:nrow(modern), function(x){sd(modern[x,], na.rm = TRUE)})

vals_mean_albian <- sapply(1:nrow(albian), function(x){mean(albian[x,], na.rm = TRUE)})
vals_sd_albian <- sapply(1:nrow(albian), function(x){sd(albian[x,], na.rm = TRUE)})

lats <- seq(-90, 90, by = res(albian)[1])
lats <- lats[1:length(lats)-1]

data <- cbind.data.frame(lats, vals_mean_modern, vals_sd_modern, vals_mean_albian, vals_sd_albian)

p1 <- ggplot() +
  geom_ribbon(data = data, mapping=aes(x = lats, ymin = vals_mean_modern - vals_sd_modern, ymax = vals_mean_modern + vals_sd_modern), colour = "#b10026", fill = "#b10026", size = 1.1, alpha  = 0.7) +
  geom_ribbon(data = data, mapping=aes(x = lats, ymin = vals_mean_albian - vals_sd_albian, ymax = vals_mean_albian + vals_sd_albian), colour = "#084594", fill = "#084594", size = 1.1, alpha  = 0.7) +
  geom_point(data = data, mapping=aes(x = lats, y = vals_mean_modern, colour = "#b10026"), size = 1.1, alpha  = 1) +
  geom_line(data = data, mapping=aes(x = lats, y = vals_mean_modern, colour = "#b10026"), size = 1.1, alpha  = 1) +
  geom_point(data = data, mapping=aes(x = lats, y = vals_mean_albian,  colour = "#084594"), size = 1.1, alpha  = 1) +
  geom_line(data = data, mapping=aes(x = lats, y = vals_mean_albian, colour = "#084594"), size = 1.1, alpha  = 1) +
  scale_color_manual(values = c("#084594", "#b10026"), labels = c("Albian", "Modern")) +
  scale_x_reverse(expand=c(0,0)) +
  labs(x = expression(bold(paste("Latitude (",degree,")"))), y = expression(bold(paste("Sea surface temperature (",degree, C,")")))) +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.grid.minor.y = element_line(colour = "grey90"),
        panel.grid.minor.x = element_line(colour = "grey90"),
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        legend.position = "top",
        #legend.background = element_blank(),
        #legend.box.background = element_rect(colour = NA, fill = NA),
        #legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        #legend.margin=unit(0, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 0),
        axis.title.y = element_text(size = 14, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 14, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 14, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        aspect.ratio = 0.6)
p1

ggsave("./figures/lat_temp_plot.png", plot = p1, width = 300, height = 200, dpi = 300, units = c("mm"))
