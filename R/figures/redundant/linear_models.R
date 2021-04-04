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
stagecol <- rgb(stages[c("Col_R","Col_G", "Col_B")], maxColorValue=255)
# cut out 3 Pleistocene stages
stagecol <- stagecol[c(1,5:length(stagecol))]
#
# Give Cambrian data rectangular point type
pointtype <- c(rep(21,90),rep(22,10))
#
#
### Preparation for A
#
# Indexes of data to use for GLS model:
index <- c(1:48,50:87)
# select data
x <- (ispat$mean[index])
y <- (itemp$temperature_stage[index])
# define new data for confidence interval
regrange <- seq(min(x,na.rm =T)-1,max(x, na.rm =T)+1,0.02)
#
### Do the GLS regression 
tsindex <- length(index):1 # just the order of the time series points (not necessary here, will default to 1...X if ~1 is specified)
glsm <- gls(y~x,  method="ML", correlation=corAR1(form = ~ tsindex))
#
# Create an intercept-only model for the likely hood ratio R2
gls_null <- gls(y ~ 1,correlation=corAR1(form = ~ tsindex), method="ML") 
m <- glsm
ll_gls <- logLik(m)
ll_gls_null <- logLik(gls_null)
ll_R2 <- 1-exp(-(2/length(index))*(ll_gls[1]-ll_gls_null[1]))
ll_R2 # this is the likelyhood ratio R2
#
### Create confidence intervals: 
new <- data.frame("x"=regrange)
prediction95 <- predictSE.gls(glsm,newdata = new,interval = "confidence", level = 0.95)
#
### OLS: (use all data, no exclusion of Cambrian data or NA necessary)
x <- ispat$mean[]
y <- itemp$temperature_stage[]
olm <- lm(y~x)
# larger range of the regression
regrange_ols <- seq(min(x,na.rm =T)-1,max(x, na.rm =T)+1,0.02)
#
#
### Plot
#
#windows(6,6)
png("./figures/linear_models.png", width = 300, height = 120, res = 300, units = "mm")
par(mfrow = c(1,2), mar = c(4.5,4.5,1,1), mgp = c(2.25,0.75,0), las = 1)
#
#
# A - with all data:
#
plot(ispat$mean,itemp$temperature_stage, xlab = expression("Absolute palaeolatitudinal centroid ("*degree*")"), 
     ylab = expression("Global mean temperature ( "*degree~"C)"), main = "", pch = 21, bg = stagecol, cex = 1.2, 
     type = "n", xlim = c(-2,57), xaxs = "i", cex.lab = 1.2, cex.axis = 1)
#
# plot confidence interval of GLS
error_polygon(prediction95$fit+2*prediction95$se.fit,prediction95$fit-2*prediction95$se.fit,regrange[1],regrange[length(regrange)],
              regrange, col = rgb(0,0,0,0.2))
# plot GLS
points(regrange,glsm$coefficients[1]+glsm$coefficients[2]*regrange, type = "l",col = rgb(0,0,0,1), lwd = 2.5, lty = 1)
#
# plot OLS
points(regrange_ols,olm$coefficients[1]+olm$coefficients[2]*regrange_ols, type = "l",col = rgb(0,0,0,1), lwd = 1.25, lty = 2)
# 
# plot points
points(x,y, pch = pointtype, bg = stagecol, cex = 1.2, xpd = T)
#
# Add text with R2 and p
text(42,39,expression("OLS:"~italic("R"^2)~"= 0.13,"~italic("P < 0.001")), cex = 0.8)
text(41.5,36,expression("GLS:"~italic("R"^2)~"= 0.00"~italic("P = 0.99")), cex = 0.8)
#
# add line segments
lines(c(28.5,31.5),c(38.6,38.6),lty=2)
lines(c(28.5,31.5),c(35.8,35.8),lty=1, lwd = 2)
# 
# Add "A"
text(-10,43,expression(bold("A")),xpd = T, cex = 2)
#
# 
### Preparation for B
#
# Indexes of data to use for GLS model (no Olenekian, no Cambrian, no NA:
index <- c(1:47,50:87)
# select data
x <- (ispat$mean[index])
y <- (itemp$temperature_stage[index])
# define new data for confidence interval
regrange <- seq(min(x,na.rm =T)-1,max(x, na.rm =T)+1,0.02)
#
### Do the GLS regression 
tsindex <- length(index):1 # just the order of the time series points (not necessary here, will default to 1...X if ~1 is specified)
glsm <- gls(y~x,  method="ML", correlation=corAR1(form = ~ tsindex))
#
# Create an intercept-only model for the likely hood ratio R2
gls_null <- gls(y ~ 1,correlation=corAR1(form = ~ tsindex), method="ML") 
m <- glsm
ll_gls <- logLik(m)
ll_gls_null <- logLik(gls_null)
ll_R2 <- 1-exp(-(2/length(index))*(ll_gls[1]-ll_gls_null[1]))
ll_R2 # this is the likelyhood ratio R2
#
### Create confidence intervals: 
new <- data.frame("x"=regrange)
prediction95 <- predictSE.gls(glsm,newdata = new,interval = "confidence", level = 0.95)
#
### OLS: (don't use the Olenekian, no exclusion of Cambrian data or NA necessary)
x <- (ispat$mean[c(1:47,49:nrow(ispat))])
y <- (itemp$temperature_stage[c(1:47,49:nrow(ispat))])
olm <- lm(y~x)
# larger range of the regression
regrange_ols <- seq(min(x,na.rm =T)-1,max(x, na.rm =T)+1,0.02)
#
#
# B - excluding the Olenekian:
#
plot(ispat$mean,itemp$temperature_stage, xlab = expression("Absolute palaeolatitudinal centroid ("*degree*")"), 
     ylab = expression("Global mean temperature ( "*degree~"C)"), main = "", pch = 21, bg = stagecol, cex = 1.2, 
     type = "n", xlim = c(-2,57), xaxs = "i", cex.lab = 1.2, cex.axis = 1)
#
# plot confidence interval of GLS
error_polygon(prediction95$fit+2*prediction95$se.fit,prediction95$fit-2*prediction95$se.fit,regrange[1],regrange[length(regrange)],
              regrange, col = rgb(0,0,0,0.2))
# plot GLS
points(regrange,glsm$coefficients[1]+glsm$coefficients[2]*regrange, type = "l",col = rgb(0,0,0,1), lwd = 2.5, lty = 1)
#
# plot OLS
points(regrange_ols,olm$coefficients[1]+olm$coefficients[2]*regrange_ols, type = "l",col = rgb(0,0,0,1), lwd = 1.25, lty = 2)
# 
# plot points
points(x,y, pch = pointtype, bg = stagecol, cex = 1.2, xpd = T)
# Plot Olenekian as "x"
points(ispat$mean[48],itemp$temperature_stage[48],pch = 4)

# Add text with R2 and p
text(42,39,expression("OLS:"~italic("R"^2)~"= 0.25,"~italic("P < 0.001")), cex = 0.8)
text(42,36,expression("GLS:"~italic("R"^2)~"= 0.08"~italic("P = 0.008")), cex = 0.8)
#
# add line segments
lines(c(28.5,31.5),c(38.6,38.6),lty=2)
lines(c(28.5,31.5),c(35.8,35.8),lty=1, lwd = 2)

# 
# Add "B"
text(-10,43,expression(bold("B")),xpd = T, cex = 2)
dev.off()
