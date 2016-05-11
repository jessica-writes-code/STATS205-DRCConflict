### Set up coding environment
# Clear Data Sets & Plots
rm(list=ls())

#
library(distr)

# Map paths
setwd('/Users/jmoore523/Dropbox/Graduate School/Q3 - Spring 2016/STATS205/Project/STATS205-DRCConflict')
input <- file.path(".", "Input")
output <- file.path(".", "Output")
temp <- file.path(".", "Temp")

### Plot Dist. vs. KDE
myNormUnif <- UnivarMixingDistribution(Norm(2,.5),Beta(shape=2, shape2=2))
dmyNormUnif <- d(myNormUnif)
rmyNormUnif <- r(myNormUnif)

# Sampled data
set.seed(3)
sampled.values <- rmyNormUnif(10)

# True density
true.sequence <- seq(0,3,length.out = 100)
true.density <- dmyNormUnif(true.sequence)

# Density Estimate
h <- .3
n <- length(sampled.values)
est.density <- rep(0,100)
for (i in 1:length(sampled.values)) {
  est.density <- dnorm(true.sequence, mean=sampled.values[i], sd=h) + est.density
}
est.density <- 1/n * est.density

# Plot
png(paste0(output,'/UnivarExample.png'))
par(mar=c(3.5,4.0,3,2.1))
plot(true.sequence, true.density, type='l', yaxs="i", col='red', xlab="", xaxt="n", ylab="", yaxt="n", ylim=c(0,.8))
axis(1, at=c(0,.5,1,1.5,2,2.5,3), cex.axis=1.4)
axis(2, at=c(0,.1,.2,.3,.4,.5,.6,.7,.8), cex.axis=1.4)
points(true.sequence, est.density, type='l')
for (i in 1:length(sampled.values)) {
  temp.sequence <- seq(sampled.values[i]-.7, sampled.values[i]+.7, length.out = 50)
  temp.dens <- 1/n*dnorm(temp.sequence, mean=sampled.values[i], sd=h)
  points(temp.sequence, temp.dens, type='l', lty=2) 
}
title(ylab = "Density", line = 2.1, cex.lab=1.75)
dev.off()