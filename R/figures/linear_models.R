#linear model plot
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
spat <- read.csv("./results/spatial_stats/stage_level_spatial_stats.csv")
# temperature stage-level
temp <- read.csv("./results/SST/stage_binned_vals.csv")

spat$temp <- temp$temperature_stage
spat <- subset(spat, !is.na(temp))

x <- diff(rev(spat$Median))
y <- diff(rev(spat$temp))
names <- rev(spat$interval_name[1:87])
stagecol <- rev(spat$color[1:87])

jpeg("./figures/linear_models.jpg", width = 230, height = 110, res = 300, units = "mm")
par(mfrow = c(1,2), mar = c(4.5,4.5,1,1), mgp = c(2.25,0.75,0), las = 1)


df <- cbind.data.frame(x, y, names, stagecol)

olm <- lm(y ~ x, data = df)

lm_coef <- list(pval = round(summary(olm)$coefficients[8], digits = 3),
                r2 = round(summary(olm)$r.squared, digits = 3));

if(lm_coef$pval <= 0.000){
  lm_coef$pval <- c("< 0.001")
}


newx = seq(min(df$x),max(df$x),by = 0.05)

conf_interval <- predict(olm, newdata=data.frame(x=newx), interval="confidence",
                         level = 0.95)

plot(df$x,df$y, xlab = expression(Delta~"Absolute palaeolatitudinal median ("*degree*")"), 
     ylab = expression(Delta~"Global mean temperature ( "*degree~"C) "), main = "", pch = 21, bg = df$stagecol, cex = 1.5, 
     type = "n", cex.lab = 1.2, cex.axis = 0.85) #xlim = c(min(x)-1,max(x)+1))


# plot points
points(df$x,df$y, pch = 21, bg = df$stagecol, cex = 1.2, xpd = T)
abline(lm(df$y~df$x), lwd = 2)
lines(newx, conf_interval[,2], col="red", lty=2, lwd = 2)
lines(newx, conf_interval[,3], col="red", lty=2, lwd = 2)
#
# Add text with R2 and p
text(-30,14, substitute(~~italic(R)^2~"="~r2~","~~italic(P)~"="~pval, lm_coef), cex = 1)
#
# add line segments
# 
# Add "A"
text(-90,16,expression(bold("A")),xpd = T, cex = 1.5)

df1 <- subset(df, names != "Olenekian")
df1 <- subset(df1, names != "Anisian")

olm <- lm(y ~ x, data = df1)

lm_coef <- list(pval = round(summary(olm)$coefficients[8], digits = 3),
                r2 = round(summary(olm)$r.squared, digits = 3));

if(lm_coef$pval <= 0.000){
  lm_coef$pval <- c("< 0.001")
}


newx = seq(min(df1$x),max(df1$x),by = 0.05)

conf_interval <- predict(olm, newdata=data.frame(x=newx), interval="confidence",
                         level = 0.95)

plot(df$x,df$y, xlab = expression(Delta~"Absolute palaeolatitudinal median ("*degree*")"), 
     ylab = expression(Delta~"Global mean temperature ( "*degree~"C) "), main = "", pch = 21, bg = df$stagecol, cex = 1.5, 
     type = "n", cex.lab = 1.2, cex.axis = 0.85) #xlim = c(min(x)-1,max(x)+1))


# plot points
points(df1$x,df1$y, pch = 21, bg = df1$stagecol, cex = 1.2, xpd = T)
points(df$x[40:41],df$y[40:41], pch = 4, cex = 1.2, xpd = T)

abline(lm(df1$y~df1$x), lwd = 2)
lines(newx, conf_interval[,2], col="red", lty=2, lwd = 2)
lines(newx, conf_interval[,3], col="red", lty=2, lwd = 2)
#
# Add text with R2 and p
text(-27,14, substitute(~~italic(R)^2~"="~r2~","~~italic(P)~"="~pval, lm_coef), cex = 1)
#
# add line segments
# 
# Add "A"
text(-90,16,expression(bold("B")),xpd = T, cex = 1.5)

dev.off()
