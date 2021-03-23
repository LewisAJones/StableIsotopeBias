#SST plot
#---------------------------------
library(ggplot2)
library(ggpubr)
library(raster)
library(matrixStats)
#---------------------------------
modern <- read.csv("./results/SST/modern_temp_grad.csv")
eocene <- read.csv("./results/SST/eocene_temp_grad.csv")

p <- ggplot() +
  geom_ribbon(data = eocene, mapping=aes(x = x, ymin = ymin, ymax = ymax), colour = "#b10026", fill = "#b10026", size = 1.1, alpha  = 0.7) +
  geom_ribbon(data = modern, mapping=aes(x = x, ymin = ymin, ymax = ymax), colour = "#084594", fill = "#084594", size = 1.1, alpha  = 0.7) +
  #geom_point(data = modern, mapping=aes(x = x, y = y, colour = "#b10026"), size = 1.1, alpha  = 1) +
  geom_line(data = modern, mapping=aes(x = x, y = y, colour = "#b10026"), size = 1.1, alpha  = 1) +
  #geom_point(data = eocene, mapping=aes(x = x, y = y,  colour = "#084594"), size = 1.1, alpha  = 1) +
  geom_line(data = eocene, mapping=aes(x = x, y = y, colour = "#084594"), size = 1.1, alpha  = 1) +
  scale_color_manual(values = c("#b10026","#084594"), labels = c("Eocene", "Modern")) +
  #scale_x_reverse(expand=c(0,0), limits = c(0, 95)) +
  labs(x = expression(bold(paste("Latitude (",degree,")"))), y = expression(bold(paste("Temperature (",degree, C,")")))) +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.grid.minor.y = element_line(colour = "grey90"),
        panel.grid.minor.x = element_line(colour = "grey90"),
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        legend.position = c(0.1, 0.1),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = NA, fill = NA),
        legend.key = element_blank(),
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
p

ggsave("./figures/lat_temp_single_plot.png", plot = p, width = 200, height = 130, dpi = 300, units = c("mm"))

modern <- rbind.data.frame(modern, modern)
modern$x[92:182] <- modern$x[92:182] * -1
eocene <- rbind.data.frame(eocene, eocene)
eocene$x[92:182] <- eocene$x[92:182] * -1

p <- ggplot() +
  geom_ribbon(data = eocene, mapping=aes(x = x, ymin = ymin, ymax = ymax), colour = "#b10026", fill = "#b10026", size = 1.1, alpha  = 0.7) +
  geom_ribbon(data = modern, mapping=aes(x = x, ymin = ymin, ymax = ymax), colour = "#084594", fill = "#084594", size = 1.1, alpha  = 0.7) +
  #geom_point(data = modern, mapping=aes(x = x, y = y, colour = "#b10026"), size = 1.1, alpha  = 1) +
  geom_line(data = modern, mapping=aes(x = x, y = y, colour = "#b10026"), size = 1.1, alpha  = 1) +
  #geom_point(data = eocene, mapping=aes(x = x, y = y,  colour = "#084594"), size = 1.1, alpha  = 1) +
  geom_line(data = eocene, mapping=aes(x = x, y = y, colour = "#084594"), size = 1.1, alpha  = 1) +
  scale_color_manual(values = c("#b10026","#084594"), labels = c("Eocene", "Modern")) +
  scale_x_reverse(expand=c(0,0), limits = c(95, -95)) +
  labs(x = expression(bold(paste("Latitude (",degree,")"))), y = expression(bold(paste("Temperature (",degree, C,")")))) +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.grid.minor.y = element_line(colour = "grey90"),
        panel.grid.minor.x = element_line(colour = "grey90"),
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        legend.position = c(0.1, 0.9),
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
p

ggsave("./figures/lat_temp_plot.png", plot = p, width = 200, height = 130, dpi = 300, units = c("mm"))
