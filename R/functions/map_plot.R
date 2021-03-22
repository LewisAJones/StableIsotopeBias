library(ggplot2)
library(ggthemes)
library(rnaturalearth)
library(rnaturalearthdata)
library(maps)

map_plot <- function(x, xy, lab){
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  df <- as.data.frame(x, xy = TRUE)
  colnames(df) <- c("x", "y", "value")
  
  p <- ggplot() +
    #geom_sf(data = world, fill = "white", colour = "white") +
    geom_raster(data = df, aes(x=x, y=y, fill = as.factor(value)), interpolate = TRUE)  +
    #geom_tile(data = df, aes(x=x, y=y, fill = as.factor(value)))  +
    geom_point(data = xy, aes(x=x, y=y), shape = 21, size = 1.5, fill = "black", colour = "black", alpha = 0.5)  +
    labs(subtitle = lab) +
    scale_fill_manual(values = c(rgb(0.9,0.8,0.7,1), rgb(0.85,0.99,0.99,1), rgb(0.6,0.85,0.9,1))) +
    xlim(-180, 180) +
    #coord_map(projection = "mollweide") +
    theme(plot.margin = unit(c(0.25,0.25,0.25,0.25), "cm"),
          axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          plot.subtitle = element_text(size = 12, colour = "black", face = "bold", hjust = 0.5),
          legend.position="",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
  
  p
  
  return(p)
}