### load libraries
library(nlme)
library(AICcmodavg)
library(geoscale)
#
### function for plotting the confidence interval band:
error_polygon <- function(ep,en,tstart,tend,tmid,color) {
  polygon( c(tstart, tmid, tend, tend, rev(tmid), tstart),
           c((ep)[1],ep, (ep)[length(ep)], (en)[length(en)], rev(en), (en)[1]),
           border = NA, col = color)
}
#
### Read files
# time scale
myscale <- read.csv("./data/timescale.csv")
# spatial statistics stage-level
ispat <- read.csv("./results/spatial_stats/stage_level_spatial_stats.csv")
# temperature stage-level
itemp <- read.csv("./results/SST/binned_vals.csv")
#
### prepare stagecolours (geoscale library):
timescale <- timescales[[1]]
stages <- timescale[which(timescale$Type == "Age"),]

pointtype <- c(rep(21,100))
png("./figures/linear_models.png", width = 220, height = 100, res = 300, units = "mm")
par(mfrow = c(1,2), mar = c(4.5,4.5,1,1), mgp = c(2.25,0.75,0), las = 1)
#



### Preparation for A

# cut out 3 Pleistocene stages
stagecol <- rgb(stages[c("Col_R","Col_G", "Col_B")], maxColorValue=255)
stagecol <- stagecol[c(1,5:length(stagecol))]
#
#
#
#
# Indexes of data to use 
index <- c(1:48,50:87,91:92)
stagecol <- stagecol
# select data
x <- diff(rev(ispat$mean[index]))
y <- diff(rev(itemp$temperature_stage[index]))

# define new data for confidence interval
regrange <- seq(min(x,na.rm =T),max(x, na.rm =T),0.02)
#

### Create confidence intervals: 
new <- data.frame("x"=regrange)
#
### OLS: (use all data, no exclusion of Cambrian data or NA necessary)
#x <- ispat$mean[]
#y <- itemp$temperature_stage[]
olm <- lm(y~x)
prediction95 <- predict(olm,newdata = new,interval = "confidence", level = 0.95)

# larger range of the regression
#
#
### Plot
#

#
# A - with all data:
#
plot(x,y, xlab = expression(Delta~"Absolute palaeolatitudinal centroid ("*degree*")"), 
     ylab = expression(Delta~"Global mean temperature ( "*degree~"C) "), main = "", pch = 21, bg = stagecol, cex = 1.2, 
     type = "n", cex.lab = 1, cex.axis = 0.85) #xlim = c(min(x)-1,max(x)+1))
#
# plot confidence interval of GLS
error_polygon(prediction95[,2], prediction95[,3],regrange[1],regrange[length(regrange)],
              regrange, col = rgb(0,0,0,0.2))

# plot OLS
points(regrange,olm$coefficients[1]+olm$coefficients[2]*regrange, type = "l",col = rgb(0,0,0,1), lwd = 2)
# 
# plot points
points(x,y, pch = 21, bg = rev(stagecol[index])[-1], cex = 1.2, xpd = T)
#
# Add text with R2 and p
text(-15,13.7,expression(italic("R"^2)~"= 0.00,"~italic("P")*" = 0.64"), cex = 0.75)
#
# add line segments
lines(c(5.9,7.7),c(38.6,38.6),lty=2)
lines(c(5.9,7.7),c(34.7,34.7),lty=1, lwd = 2)
# 
# Add "A"
text(-58,15.9,expression(bold("A")),xpd = T)
#
summary(olm)

#
#
# ### Preparation for B

# cut out 3 Pleistocene stages
stagecol <- rgb(stages[c("Col_R","Col_G", "Col_B")], maxColorValue=255)
stagecol <- stagecol[c(1,5:length(stagecol))]
#
#
#
#
# Indexes of data to use 
index <- c(1:48,50:87,91:92)
stagecol <- stagecol
# select data
x2 <- diff(rev(ispat$mean[index]))
y2 <- diff(rev(itemp$temperature_stage[index]))


# define new data for confidence interval
regrange <- seq(min(x2,na.rm =T),max(x2, na.rm =T),0.02)
#

### Create confidence intervals: 
new <- data.frame("x2"=regrange)
#
### OLS: (use all data, no exclusion of Cambrian data or NA necessary)
#x <- ispat$mean[]
#y <- itemp$temperature_stage[]
olm <- lm(y2~x2)
prediction95 <- predict(olm,newdata = new,interval = "confidence", level = 0.95)


#
### Plot
#

#
# B - remove Olenekian
#
plot(x,y, xlab = expression(Delta~"Absolute palaeolatitudinal centroid ("*degree*")"), 
     ylab = expression(Delta~"Global mean temperature ( "*degree~"C) "), main = "", pch = 21, bg = stagecol, cex = 1.2, 
     type = "n", cex.lab = 1, cex.axis = 0.85) #xlim = c(min(x)-1,max(x)+1))
#
# plot confidence interval of GLS
error_polygon(prediction95[,2], prediction95[,3],regrange[1],regrange[length(regrange)],
              regrange, col = rgb(0,0,0,0.2))

# plot OLS
points(regrange,olm$coefficients[1]+olm$coefficients[2]*regrange, type = "l",col = rgb(0,0,0,1), lwd = 2)
# 
# plot points
points(x2,y2, pch = 21, bg = rev(stagecol[index])[-1], cex = 1.2, xpd = T)
points(x[40:41],y[40:41], pch = 4, cex = 1.2, xpd = T)

#
# Add text with R2 and p
text(-15,13.7,expression(italic("R"^2)~"= 0.11,"~italic("P")*" = 0.002"), cex = 0.75)
#
# add line segments
lines(c(5.9,7.7),c(38.6,38.6),lty=2)
lines(c(5.9,7.7),c(34.7,34.7),lty=1, lwd = 2)
# 
# Add "A"
text(-58,15.9,expression(bold("B")),xpd = T)
#
summary(olm)
# 

cor.test(x2,y2, method = "spearman")
cor.test(x,y, method = "spearman")
cor.test(x,y, method = "pearson")
dev.off()