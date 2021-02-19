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
#define colours for plots
col <- c("black")
#---------------------------------
#read data
data <- read.csv("./results/SST/extracted_temp.csv")
stage_vals <- read.csv("./results/SST/binned_vals.csv")
stage_vals$mid_ma <- (stage_vals$max_ma + stage_vals$min_ma)/2

p1 <- ggplot() +
  #geom_segment(data = periods, mapping=aes(x = min_ma, xend = min_ma, y = -6, yend = Inf), linetype = 2, size = 1, color = "grey90") +
  #geom_segment(data = periods, mapping=aes(x = max_ma, xend = max_ma, y = -6, yend = Inf), linetype = 2, size = 1, color = "grey90") +
  #geom_segment(data = periods, mapping=aes(x = 0, xend = 541, y = data$modern_ext_t[1], yend = data$modern_temp_mean[1]), linetype = 1, size = 1, color = "royalblue1") +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_ma, xmax=max_ma, ymin = -6, ymax= Inf), linetype = 0, color="grey90", alpha=0.1)  +
  #geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -6, ymax= 0), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin = min_ma, xmax = max_ma, ymin = -6, ymax = -2), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x = mid_ma, y = -4, label = abbrev), colour = "black", alpha=1)  +
  geom_point(data = data, mapping=aes(x = Age..Ma., y = modern_ext_t), colour = "darkgrey", size = 1.1, alpha  = 1) +
  geom_point(data = stage_vals, mapping=aes(x = mid_ma, y = ext_temperature_stage_modern), colour = "black", size = 1.1, alpha  = 1) +
  geom_line(data = stage_vals, mapping=aes(x = mid_ma, y = ext_temperature_stage_modern), colour = "black", size = 1.1, alpha  = 1) +
  #geom_smooth(data = data, mapping=aes(x = age, y = modern_temp_ext), colour = "red", size = 1.1, alpha  = 0.75) +
  scale_x_reverse(expand=c(0,0), limits = c(541, 0)) +
  scale_y_continuous(expand=c(0,0), limits = c(-6, 65)) +
  labs(x = "", y = expression(bold(paste("Temperature (",degree, C,")")))) +
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
p1

p2 <- ggplot() +
  #geom_segment(data = periods, mapping=aes(x = min_ma, xend = min_ma, y = -6, yend = Inf), linetype = 2, size = 1, color = "grey90") +
  #geom_segment(data = periods, mapping=aes(x = max_ma, xend = max_ma, y = -6, yend = Inf), linetype = 2, size = 1, color = "grey90") +
  #geom_segment(data = periods, mapping=aes(x = 0, xend = 541, y = data$modern_temp_mean[1], yend = data$modern_temp_mean[1]), linetype = 1, size = 1, color = "royalblue1") +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_ma, xmax=max_ma, ymin = -6, ymax= Inf), linetype = 0, color="grey90", alpha=0.1)  +
  #geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -6, ymax= 0), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin = min_ma, xmax = max_ma, ymin = -6, ymax = -2), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x = mid_ma, y = -4, label = abbrev), colour = "black", alpha=1)  +
  geom_point(data = data, mapping=aes(x = Age..Ma., y = T.2010...), colour = "darkgrey", size = 1.1, alpha  = 1) +
  geom_point(data = stage_vals, mapping=aes(x = mid_ma, y = temperature_stage), colour = "black", size = 1.1, alpha  = 1) +
  geom_line(data = stage_vals, mapping=aes(x = mid_ma, y = temperature_stage), colour = "black", size = 1.1, alpha  = 1) +
  #geom_smooth(data = data, mapping=aes(x = age, y = temperature), method = "loess", colour = "red", size = 1.1, alpha  = 0.75) +
  scale_x_reverse(expand=c(0,0), limits = c(541, 0)) +
  scale_y_continuous(expand=c(0,0), limits = c(-6, 65)) +
  labs(x = "", y = expression(bold(paste("Temperature (",degree, C,")")))) +
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
p2

plots <- ggarrange(p2, p1, ncol=1, nrow=2, widths = c(1,1), labels = "AUTO", align = "v")
plots
ggsave(file = "./figures/extracted_temperature.png", plot = plots, width = 150, height = 150, units = "mm", dpi = 600, scale = 1.7)


df1 <- ggplot_build(p2)$data[[8]]
df2 <- ggplot_build(p1)$data[[8]]
df <- df1
df$y <- df1$y - df2$y

p <- ggplot() + 
  geom_segment(data = periods, mapping=aes(x = min_ma, xend = min_ma, y = -8, yend = Inf), linetype = 2, size = 1, color = "grey90") +
  geom_segment(data = periods, mapping=aes(x = max_ma, xend = max_ma, y = -8, yend = Inf), linetype = 2, size = 1, color = "grey90") +
  #geom_segment(data = periods, mapping=aes(x = 0, xend = 541, y = data$modern_temp_mean[1], yend = data$modern_temp_mean[1]), linetype = 1, size = 1, color = "royalblue1") +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_ma, xmax=max_ma, ymin = -8, ymax= Inf), linetype = 0, color="grey90", alpha=0.1)  +
  geom_rect(data = periods, mapping=aes(xmin = min_ma, xmax = max_ma, ymin = -8, ymax = -6), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x = mid_ma, y = -7, label = abbrev), colour = "black", alpha=1)  +
  #geom_ribbon(data = df, mapping=aes(x = abs(x), ymin = ymin, ymax = ymax), fill = "red", colour = NA, size =1.1, alpha = 0.75) +
  geom_line(data = df, mapping=aes(x = abs(x), y = y), colour = "red", size = 1.1, alpha  = 0.75) +
  scale_x_reverse(expand=c(0,0), limits = c(541, 0)) +
  scale_y_continuous(expand=c(0,0), limits = c(-8, 20), breaks = seq(-5, 20, 5), labels = seq(-5, 20, 5)) +
  labs(x = "", y = expression(bold(paste(Delta, "Temperature (",degree, C,")")))) +
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
  
ggsave(file = "./figures/corrected_temperature.png", plot = p, width = 150, height = 75, units = "mm", dpi = 600, scale = 1.7)

