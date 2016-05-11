### Set up coding environment
# Clear Data Sets & Plots
rm(list=ls())

# Load libraries
library(ggplot2)
library(maptools)
library(ks)
library(xlsx)

# Map paths
setwd('/Users/jmoore523/Dropbox/Graduate School/Q3 - Spring 2016/STATS205/Project/STATS205-DRCConflict')
input <- file.path(".", "Input")
output <- file.path(".", "Output")
temp <- file.path(".", "Temp")

# Load data
load(file=paste0(temp,"/drc.R"))
load(file=paste0(temp,"/drcgrid.R"))
load(file=paste0(temp,"/acleddrc.R"))
load(file=paste0(temp,"/drcgridcentroids.R"))

### Count 2015 Events by Grid Square
# Subset data
acled.drc2015 <- subset(acled.drc, YEAR==2015)
acled.drc2015.b <- subset(acled.drc2015, event_type_std=="Battle")
acled.drc2015.r <- subset(acled.drc2015, event_type_std=="Riots/Protests")
acled.drc2015.v <- subset(acled.drc2015, event_type_std=="Violence against civilians")

# Count All Events
points <- over(acled.drc2015, drcgrid)
points.all <- as.data.frame(table(points$ID))
names(points.all)[names(points.all)=="Var1"] <- "ID"

# Count Battles
points <- over(acled.drc2015.b, drcgrid)
points.b <- as.data.frame(table(points$ID))
names(points.b)[names(points.b)=="Var1"] <- "ID"

# Count Riots
points <- over(acled.drc2015.r, drcgrid)
points.r <- as.data.frame(table(points$ID))
names(points.r)[names(points.r)=="Var1"] <- "ID"

# Count Violence Against Civilians
points <- over(acled.drc2015.v, drcgrid)
points.v <- as.data.frame(table(points$ID))
names(points.v)[names(points.v)=="Var1"] <- "ID"

### Create Surveillance Plots
## Function to put data in format for surveillance plots
data.surveillance <- function(pointsbygridID, densitybygridID) {
  densitybygridID <- data.frame(densitybygridID)
  
  # Merge Data Sets
  pointsbygridID$ID <- as.character(pointsbygridID$ID)
  densitybygridID$ID <- as.character(densitybygridID$ID)
  dataforplot <- merge(pointsbygridID,densitybygridID, by="ID", all.y=TRUE)
  dataforplot$Freq[is.na(dataforplot$Freq)] <- 0
  names(dataforplot) <- c("ID","Events","EstimatedDensity")
  
  # Create CDF based on Density Estimates
  dataforplot <- dataforplot[order(-dataforplot$EstimatedDensity),]
  dataforplot$CumEvents <- cumsum(dataforplot$Events)
  cdf <- seq(1,nrow(dataforplot))/nrow(dataforplot)
  dataforplot <- cbind(dataforplot,cdf)
  
  # Convert cum events to percentage
  dataforplot$CumEvents <- dataforplot$CumEvents/sum(dataforplot$Events)
  
  return(dataforplot)
}

## Function to make surveillance plots
plot.surveillance <- function(pi, lscv, lscv.RN, bcv, ns, name) {
  # Make Plot
  png(paste0(output,'/',name,'.png'))
  par(mar=c(4,4,2,2))
  
  # - Plot all lines
  plot(pi$cdf, pi$CumEvents, type="l", xlab="", ylab="", xaxt="n", ylim=c(0,1.05), yaxt="n")
  lines(lscv$cdf, lscv$CumEvents, type="l", col="blue")
  lines(lscv.RN$cdf, lscv.RN$CumEvents, type="l", col="red")
  lines(bcv$cdf, bcv$CumEvents, type="l", col="green")
  lines(ns$cdf, ns$CumEvents, type="l", col="orange")
  lines(c(0,1),c(0,1))
  
  # - Add Axis Labels
  title(xlab = "Area Covered", line = 2.0, cex.lab=1.20)
  axis(1, at=c(0,.2,.4,.6,.8,1), labels=c("0%","20%","40%","60%","80%","100%"), cex.axis=1.10)
  
  title(ylab = "2015 Events Covered", line = 2.0, cex.lab=1.20)
  axis(2, at=c(0,.2,.4,.6,.8,1), labels=c("0%","20%","40%","60%","80%","100%"), cex.axis=1.10)
  
  # - Add Legend
  legend(.55, .2, c("Plug-In","LSCV","LSCV w/ Random Noise","BCV","NS"), lwd=c(2,2,2,2), col=c("black","blue","red","green","orange"))
  
  dev.off()
}

## Function to calculate AUC
calc.auc <- function(dataforauc) {
  dataforauc$ones <- rep(1,nrow(dataforauc))
  dataforauc$width <- dataforauc$ones/sum(dataforauc$ones)
  dataforauc$auccomp <- dataforauc$width*dataforauc$CumEvents
  sum(dataforauc$auccomp)
}

## Function to combine the above
results.surveillance <- function(pointsbygridID, estimates.pi, estimates.lscv, estimates.lscv.RN, estimates.bcv, estimates.ns, name) {
  
  # Change data to surveillance plot format
  data.pi <- data.surveillance(pointsbygridID,estimates.pi)
  data.lscv <- data.surveillance(pointsbygridID,estimates.lscv)
  data.lscv.RN <- data.surveillance(pointsbygridID,estimates.lscv.RN)
  data.bcv <- data.surveillance(pointsbygridID,estimates.bcv)
  data.ns <- data.surveillance(pointsbygridID,estimates.ns)
  
  # Plot
  plot.surveillance(data.pi, data.lscv, data.lscv.RN, data.bcv, data.ns,name)
  
  # AUC Calculation
  pi.auc <- calc.auc(data.pi)
  lscv.auc <- calc.auc(data.lscv)
  lscv.auc.RN <- calc.auc(data.lscv.RN)
  bcv1.auc <- calc.auc(data.bcv)
  ns.auc <- calc.auc(data.ns)
  aucs <- c(pi.auc,lscv.auc,lscv.auc.RN,bcv1.auc,ns.auc)
  return(aucs)
}

## All Data
# Load data
load(file=paste0(temp,"/EstimatesAllPI.R"))
load(file=paste0(temp,"/EstimatesAllLSCV.R"))
load(file=paste0(temp,"/EstimatesAllLSCVRN.R"))
load(file=paste0(temp,"/EstimatesAllBCV1.R"))
load(file=paste0(temp,"/EstimatesAllNS.R"))

# Surveillance Plot & AUC Calculation
all.auc <- results.surveillance(points.all, estimates.all.pi, estimates.all.lscv, estimates.all.lscv.RN, estimates.all.bcv1, estimates.all.ns, "AllData")

## Battles
# Load data
load(file=paste0(temp,"/EstimatesBattlesPI.R"))
load(file=paste0(temp,"/EstimatesBattlesLSCV.R"))
load(file=paste0(temp,"/EstimatesBattlesLSCVRN.R"))
load(file=paste0(temp,"/EstimatesBattlesBCV1.R"))
load(file=paste0(temp,"/EstimatesBattlesNS.R"))

# Surveillance Plot & AUC Calculation
battles.auc <- results.surveillance(points.all, estimates.b.pi, estimates.b.lscv, estimates.b.lscv.RN, estimates.b.bcv1, estimates.b.ns, "Battles")

## Riots
# Load data
load(file=paste0(temp,"/EstimatesRiotsPI.R"))
load(file=paste0(temp,"/EstimatesRiotsLSCV.R"))
load(file=paste0(temp,"/EstimatesRiotsLSCVRN.R"))
load(file=paste0(temp,"/EstimatesRiotsBCV1.R"))
load(file=paste0(temp,"/EstimatesRiotsNS.R"))

# Surveillance Plot & AUC Calculation
riots.auc <- results.surveillance(points.all, estimates.r.pi, estimates.r.lscv, estimates.r.lscv.RN, estimates.r.bcv1, estimates.r.ns, "Riots")

## Violence Against Civilians
# Load data
load(file=paste0(temp,"/EstimatesVACPI.R"))
load(file=paste0(temp,"/EstimatesVACLSCV.R"))
load(file=paste0(temp,"/EstimatesVACLSCVRN.R"))
load(file=paste0(temp,"/EstimatesVACBCV1.R"))
load(file=paste0(temp,"/EstimatesVACNS.R"))

# Surveillance Plot & AUC Calculation
vac.auc <- results.surveillance(points.all, estimates.v.pi, estimates.v.lscv, estimates.v.lscv.RN, estimates.v.bcv1, estimates.v.ns, "VAC")

### Combine AUC Calculations & Save
combined.auc <- rbind(all.auc, battles.auc, riots.auc, vac.auc)
colnames(combined.auc) <- c("Plug-In","LSCV","LSCVRN","BCV2","NS")
write.xlsx(combined.auc, file=paste0(output,"/AUC Calculations.xlsx"), sheetName="AUC", col.names=TRUE, row.names=TRUE, append=TRUE)
