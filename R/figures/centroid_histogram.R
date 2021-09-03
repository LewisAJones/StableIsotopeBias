#histogram
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
periods$color <- "white"
#shading data
s <- seq(2, nrow(periods), 2)
throwing_shade <- periods[s,]
#define colours for plots
col <- c("black")
periods$duration <- (periods$max_ma - periods$min_ma)
#---------------------------------
#read data
data <- read.csv("./data/rotated_Veizer_data.csv")
data <- subset(data, comments == "Random assignment")

#data$mid_ma <- NA
#
#for(i in 1:nrow(periods)){
#  vec <- which(data$gts2012 <= periods$max_ma[i] & data$gts2012 >= periods$min_ma[i])
#  data$mid_ma[vec] <- periods$mid_ma[i]
#  data$max_ma[vec] <- periods$max_ma[i]
#}

p <- ggplot() +
  #geom_segment(data = periods, mapping=aes(x = min_ma, xend = min_ma, y = -4.5, yend = Inf), linetype = 2, size = 1, color = "grey90") +
  #geom_segment(data = periods, mapping=aes(x = max_ma, xend = max_ma, y = -4.5, yend = Inf), linetype = 2, size = 1, color = "grey90") +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_ma, xmax=max_ma, ymin = -21, ymax= Inf), linetype = 0, color="grey90", alpha=0.1)  +
  #geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -6, ymax= 0), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin = min_ma, xmax = max_ma, ymin = -21, ymax = 0), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x = mid_ma, y = -10.5, label = abbrev), size = 3, colour = "black", alpha=1)  +
  geom_histogram(data = data, mapping=aes(x = gts2012), breaks = periods$max_ma, colour = "black", fill = "grey50") +
  scale_x_reverse(expand=c(0,0), limits = c(541, 0)) +
  scale_y_continuous(expand=c(0,0), limits = c(-21, 400)) +
  labs(x = "Time (Ma)", y = "Frequency") +
  theme(panel.background = element_rect(colour = "white", fill = "white"),
        plot.margin = margin(0.5,1,0.5,1, "cm"),
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
        axis.title.y = element_text(size = 16, face = "bold", vjust = 4),
        #axis.title.y.right = element_text(size = 14, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        aspect.ratio = 0.5)
p

ggsave(file = "./figures/centroid_assignment.jpg", plot = p, width = 100, height = 50, units = "mm", dpi = 600, scale = 2)


