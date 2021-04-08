#SST plot
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
periods$color <- "white"
#define colours for plots
col <- c("black")
#---------------------------------
#read data
data <- read.csv("./results/SST/extracted_temp.csv")
stage_vals <- read.csv("./results/SST/binned_vals.csv")
stage_vals$mid_ma <- (stage_vals$max_ma + stage_vals$min_ma)/2

p1 <- ggplot() +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_ma, xmax=max_ma, ymin = -8, ymax= Inf), linetype = 0, color="grey90", alpha=0.1)  +
  geom_rect(data = periods, mapping=aes(xmin = min_ma, xmax = max_ma, ymin = -8, ymax = -2), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x = mid_ma, y = -5, label = abbrev), colour = "black", alpha=1)  +
  geom_point(data = data, mapping=aes(x = gts2012, y = temperature), colour = "darkgrey", size = 1.1, alpha  = 1) +
  geom_point(data = stage_vals, mapping=aes(x = mid_ma, y = temperature_stage), colour = "black", size = 1.1, alpha  = 1) +
  geom_line(data = stage_vals, mapping=aes(x = mid_ma, y = temperature_stage), colour = "black", size = 1.1, alpha  = 1) +
  #geom_hline(data = NULL, mapping=aes(yintercept = 17.84), colour = "#0570b0", linetype = "dashed", size = 1, alpha = 1) +
  scale_x_reverse(expand=c(0,0), limits = c(541, 0)) +
  scale_y_continuous(expand=c(0,0), limits = c(-8, 65)) +
  labs(x = "", y = expression(bold(paste("Temperature (",degree, C,")"))), subtitle = "Veizer data") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.25,0.5,0.25,0.5, "cm"),
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
#p1

p2 <- ggplot() +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_ma, xmax=max_ma, ymin = -5, ymax= Inf), linetype = 0, color="grey90", alpha=0.1)  +
  geom_rect(data = periods, mapping=aes(xmin = min_ma, xmax = max_ma, ymin = -5, ymax = -2), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x = mid_ma, y = -3.5, label = abbrev), colour = "black", alpha=1)  +
  geom_hline(data = NULL, mapping=aes(yintercept = 17.84), colour = "black", linetype = "dashed", size = 1, alpha = 1) +
  geom_point(data = data, mapping=aes(x = gts2012, y = modern_ext_t), colour = "darkgrey", size = 1.1, alpha  = 1) +
  geom_point(data = stage_vals, mapping=aes(x = mid_ma, y = ext_temperature_stage_modern), colour = "#0570b0", size = 1.1, alpha  = 1) +
  geom_line(data = stage_vals, mapping=aes(x = mid_ma, y = ext_temperature_stage_modern), colour = "#0570b0", size = 1.1, alpha  = 1) +
  scale_x_reverse(expand=c(0,0), limits = c(541, 0)) +
  scale_y_continuous(expand=c(0,0), limits = c(-5, 30)) +
  labs(x = "", y = "", subtitle = "Extracted Modern-type") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.25,0.5,0.25,0.5, "cm"),
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
#p2

p3 <- ggplot() +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_ma, xmax=max_ma, ymin = 18.5, ymax= Inf), linetype = 0, color="grey90", alpha=0.1)  +
  geom_rect(data = periods, mapping=aes(xmin = min_ma, xmax = max_ma, ymin = 18.5, ymax = 20), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x = mid_ma, y = 19.25, label = abbrev), colour = "black", alpha=1)  +
  geom_hline(data = NULL, mapping=aes(yintercept = 27.58), colour = "black", linetype = "dashed", size = 1, alpha = 1) +
  geom_point(data = data, mapping=aes(x = gts2012, y = eocene_ext_t), colour = "darkgrey", size = 1.1, alpha  = 1) +
  geom_point(data = stage_vals, mapping=aes(x = mid_ma, y = ext_temperature_stage_eocene), colour = "#b10026", size = 1.1, alpha  = 1) +
  geom_line(data = stage_vals, mapping=aes(x = mid_ma, y = ext_temperature_stage_eocene), colour = "#b10026", size = 1.1, alpha  = 1) +
  scale_x_reverse(expand=c(0,0), limits = c(541, 0)) +
  scale_y_continuous(expand=c(0,0), limits = c(18.5, 35)) +
  labs(x = "Time (Ma)", y = expression(bold(paste("Temperature (",degree, C,")"))), subtitle = "Extracted Eocene-type") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.25,0.5,0.25,0.5, "cm"),
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
#p3

p4 <- ggplot() +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_ma, xmax=max_ma, ymin = -4.5, ymax= Inf), linetype = 0, color="grey90", alpha=0.1)  +
  geom_rect(data = periods, mapping=aes(xmin = min_ma, xmax = max_ma, ymin = -4.5, ymax = 0), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x = mid_ma, y = -2.25, label = abbrev), colour = "black", alpha=1)  +
  geom_point(data = stage_vals, mapping=aes(x = mid_ma, y = temperature_stage), colour = "black", size = 1.1, alpha  = 1) +
  geom_line(data = stage_vals, mapping=aes(x = mid_ma, y = temperature_stage), colour = "black", size = 1.1, alpha  = 1) +
  geom_point(data = stage_vals, mapping=aes(x = mid_ma, y = ext_temperature_stage_modern), colour = "#0570b0", size = 1.1, alpha  = 1) +
  geom_line(data = stage_vals, mapping=aes(x = mid_ma, y = ext_temperature_stage_modern), colour = "#0570b0", size = 1.1, alpha  = 1) +
  geom_point(data = stage_vals, mapping=aes(x = mid_ma, y = ext_temperature_stage_eocene), colour = "#b10026", size = 1.1, alpha  = 1) +
  geom_line(data = stage_vals, mapping=aes(x = mid_ma, y = ext_temperature_stage_eocene), colour = "#b10026", size = 1.1, alpha  = 1) +
  scale_x_reverse(expand=c(0,0), limits = c(541, 0)) +
  scale_y_continuous(expand=c(0,0), limits = c(-4.5, 45)) +
  labs(x = "Time (Ma)", y = "", subtitle = "Comparison") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.25,0.5,0.25,0.5, "cm"),
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
#p4


plots <- ggarrange(p1, p2, p3, p4, ncol=2, nrow=2, labels = "AUTO", align = "h", font.label = list(size = 20))

ggsave(file = "./figures/extracted_temperature.png", plot = plots, width = 200, height = 95, units = "mm", dpi = 300, scale = 1.75)
