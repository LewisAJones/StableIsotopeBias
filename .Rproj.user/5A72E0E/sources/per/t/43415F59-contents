#spatial statistics plot
#---------------------------------
library(ggplot2)
library(ggpubr)
#---------------------------------
#plot style
#get stages
stages <- read.csv("https://paleobiodb.org/data1.2/intervals/list.txt?scale_id=1&scale_level=5")
#get periods
periods <- read.csv("https://paleobiodb.org/data1.2/intervals/list.txt?scale_id=1&scale_level=3")
periods$mid_ma <- (periods$max_ma+periods$min_ma)/2
#shading data
s <- seq(2, nrow(periods), 2)
throwing_shade <- periods[s,]
#define colours for plots
col <- c("black")
#---------------------------------
#read data
data <- read.csv("./results/spatial_stats/stage_level_spatial_stats.csv")

plot_a <- ggplot() +
  #geom_segment(data = periods, mapping=aes(x = min_ma, xend = min_ma, y = -4.5, yend = Inf), linetype = 2, size = 1, color = "grey90") +
  #geom_segment(data = periods, mapping=aes(x = max_ma, xend = max_ma, y = -4.5, yend = Inf), linetype = 2, size = 1, color = "grey90") +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_ma, xmax=max_ma, ymin = -6.75, ymax= Inf), linetype = 0, color="grey90", alpha=0.1)  +
  #geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -6, ymax= 0), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin = min_ma, xmax = max_ma, ymin = -6.75, ymax = 0), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x = mid_ma, y = -3.375, label = abbrev), size = 4, colour = "black", alpha=1)  +
  geom_ribbon(data = data, mapping = aes(x = mid_ma, y = centroid, ymin = LQ_centroid, ymax = UQ_centroid), colour = NA, fill = col[1], size = 0.75, alpha = 0.5) +
  geom_line(data = data, mapping=aes(x = mid_ma, y = centroid), colour = col[1], size = 1.1, alpha  = 0.75) +
  scale_x_reverse(expand=c(0,0), limits = c(541, 0)) +
  scale_y_continuous(expand=c(0,0), limits = c(-6.75, 90)) +
  labs(x = "", y = expression(bold(paste("Absolute palaeolatitude (",degree,")")))) +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        #panel.grid.minor.y = element_line(colour = "grey90"),
        #panel.grid.minor.x = element_line(colour = "grey90"),
        #panel.grid.major.y = element_line(colour = "grey90"),
        #panel.grid.major.x = element_line(colour = "grey90"),
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
plot_a

#---------------------------------

plot_b <- ggplot() +
  #geom_segment(data = periods, mapping=aes(x = min_ma, xend = min_ma, y = -2.5, yend = Inf), linetype = 2, size = 1, color = "grey90") +
  #geom_segment(data = periods, mapping=aes(x = max_ma, xend = max_ma, y = -2.5, yend = Inf), linetype = 2, size = 1, color = "grey90") +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_ma, xmax=max_ma, ymin = -5.25, ymax= 70), linetype = 0, color="grey90", alpha=0.1)  +
  #geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -3, ymax= 0), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin = min_ma, xmax = max_ma, ymin = -5.25, ymax = 0), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x = mid_ma, y = -2.625, label = abbrev), size = 4, colour = "black", alpha=1)  +
  geom_line(data = data, mapping=aes(x = mid_ma, y = occupancy), colour = col[1], size = 1.1, alpha  = 0.75) +
  scale_x_reverse(expand=c(0,0), limits = c(541, 0)) +
  scale_y_continuous(expand=c(0,0), limits = c(-5.25, 70)) +
  labs(x = "", y = expression(bold(paste("Occuppied grid cells")))) +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        #panel.grid.minor.y = element_line(colour = "grey90"),
        #panel.grid.minor.x = element_line(colour = "grey90"),
        #panel.grid.major.y = element_line(colour = "grey90"),
        #panel.grid.major.x = element_line(colour = "grey90"),
        legend.position = c(0.2, 0.9),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = NA, fill = NA),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.margin=unit(0, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y = element_text(size = 14, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 14, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 14, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        aspect.ratio = 0.4)
plot_b

#---------------------------------
plot_c <- ggplot() +
  #geom_segment(data = periods, mapping=aes(x = min_ma, xend = min_ma, y = -3000, yend = Inf), linetype = 2, size = 1, color = "grey90") +
  #geom_segment(data = periods, mapping=aes(x = max_ma, xend = max_ma, y = -3000, yend = Inf), linetype = 2, size = 1, color = "grey90") +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_ma, xmax=max_ma, ymin = -3000, ymax= Inf), linetype = 0, color="grey90", alpha=0.1)  +
  #geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -2600, ymax= 0), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin = min_ma, xmax = max_ma, ymin = -3000, ymax = 0), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x = mid_ma, y = -1500, label = abbrev), size = 4, colour = "black", alpha=1)  +
  geom_line(data = data, mapping=aes(x = mid_ma, y = MST), colour = col[1], size = 1.1, alpha  = 0.75) +
  scale_x_reverse(expand=c(0,0), limits = c(541, 0)) +
  scale_y_continuous(expand=c(0,0), limits = c(-3000, 40000)) +
  labs(x = "Time (Ma)", y = expression(bold(paste("Summed MST length (km)")))) +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        #panel.grid.minor.y = element_line(colour = "grey90"),
        #panel.grid.minor.x = element_line(colour = "grey90"),
        #panel.grid.major.y = element_line(colour = "grey90"),
        #panel.grid.major.x = element_line(colour = "grey90"),
        legend.position = c(0.2, 0.9),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = NA, fill = NA),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.margin=unit(0, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y = element_text(size = 14, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 14, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 14, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        aspect.ratio = 0.4)
plot_c

#---------------------------------
plots <- ggarrange(plot_a, plot_b, plot_c, ncol=1, nrow=3, widths = c(1,1,1), labels = "AUTO", align = "v")
#plots
ggsave(file = "./figures/spatial_stats.png", plot = plots, width = 120, height = 160, units = "mm", dpi = 600, scale = 1.7)
#unlink("./figures/spatial_stats.png")
