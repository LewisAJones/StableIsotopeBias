#SST plot
#---------------------------------
library(ggplot2)
library(ggpubr)
library(rworldmap)
library(sp)
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
#---------------------------------
#read data
data <- read.csv("./results/SST/extracted_temp.csv")
data <- subset(data, ODP == 0)

coords2continent = function(points)
{  
  countriesSP <- getMap(resolution='low')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  
  # converting points to a SpatialPoints object
  # setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  #indices$continent   # returns the continent (6 continent model)
  indices$REGION   # returns the continent (7 continent model)
  #indices$ADMIN  #returns country name
  #indices$ISO3 # returns the ISO3 code 
}

data$continent <- coords2continent(data[,c("lon", "lat")])

data <- subset(data, continent != "<NA>")

col <- rev(c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33", "#a65628"))

p1 <- ggplot() +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_ma, xmax=max_ma, ymin = -105, ymax= Inf), linetype = 0, color="grey90", alpha=0.1)  +
  geom_rect(data = periods, mapping=aes(xmin = min_ma, xmax = max_ma, ymin = -105, ymax = -90), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x = mid_ma, y = -97.5, label = abbrev), colour = "black", alpha=1)  +
  geom_point(data = data, mapping=aes(x = gts2012, y = palaeolat, fill = continent), shape = 21, colour = "black", size = 1.5, alpha  = 0.75) +
  #geom_point(data = stage_vals, mapping=aes(x = mid_ma, y = temperature_stage), colour = "black", size = 1.1, alpha  = 1) +
  #geom_line(data = stage_vals, mapping=aes(x = mid_ma, y = temperature_stage), colour = "black", size = 1.1, alpha  = 1) +
  #geom_hline(data = NULL, mapping=aes(yintercept = 17.84), colour = "#0570b0", linetype = "dashed", size = 1, alpha = 1) +
  scale_x_reverse(expand=c(0,0), limits = c(541, 0)) +
  scale_y_continuous(expand=c(0,0), limits = c(-105, 90), breaks = seq(-90, 90, 30), labels = seq(-90, 90, 30)) +
  scale_fill_manual(values = col) +
  labs(x = "Time (Ma)", y = expression(bold(paste("Palaeolatitude (",degree,")"))), subtitle = "") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.25,0.25,0.25,0.25, "cm"),
        legend.position = "top",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = NA, fill = NA),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.margin=unit(0, "cm"),
        legend.key.size = unit(1,"line"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 0),
        axis.title.y = element_text(size = 14, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 14, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 14, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        aspect.ratio = 0.5)

p1

ggsave(file = "./figures/terrestrial_data.png", plot = p1, width = 150, height = 95, units = "mm", dpi = 300, scale = 1.2)

