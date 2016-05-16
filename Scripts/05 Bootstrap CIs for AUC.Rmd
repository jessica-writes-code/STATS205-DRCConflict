### Set up coding environment
# Clear Data Sets & Plots
rm(list=ls())
set.seed(205)

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
load(file=paste0(temp,"/acleddrc.R"))
load(file=paste0(temp,"/drcgrid.R"))

# Subset data
acled.drc2015 <- subset(acled.drc, YEAR==2015)

### Function Definition
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

## Function to calculate AUC
calc.auc <- function(dataforauc) {
  dataforauc$ones <- rep(1,nrow(dataforauc))
  dataforauc$width <- dataforauc$ones/sum(dataforauc$ones)
  dataforauc$auccomp <- dataforauc$width*dataforauc$CumEvents
  sum(dataforauc$auccomp)
}

## Function to Perform Boostrap
bootstrap.auc <- function(event.type="all", estimates) {
  auclist <- c()
  for (i in 1:1000) {
    # Subset data, if necessary
    if (event.type =="B") {
      acled.drc2015 <- subset(acled.drc2015, event_type_std=="Battle")
    }
    if (event.type == "R") {
      acled.drc2015.r <- subset(acled.drc2015, event_type_std=="Riots/Protests")
    }
    if (event.type == "V") {
      acled.drc2015.v <- subset(acled.drc2015, event_type_std=="Violence against civilians")
    }
    
    # Draw bootstrap sample
    ind <- sample(length(acled.drc2015), length(acled.drc2015), replace=TRUE)
    bs.samp <- acled.drc2015[ind,]
    row.names(bs.samp) <- seq(1,length(bs.samp))
    
    # Count Events
    points <- over(bs.samp, drcgrid)
    points.df <- as.data.frame(table(points$ID))
    names(points.df)[names(points.df)=="Var1"] <- "ID"
  
    # Calculate AUC
    surv.data <- data.surveillance(points.df, estimates)
    new.auc <- calc.auc(surv.data)
    auclist <- c(auclist,new.auc)
  }
  lb <- quantile(auclist, probs = .025)
  avg <- mean(auclist)
  ub <- quantile(auclist, probs = .975)
  return(c(lb, avg, ub))
}

### AUC Calculations
## All Data
# Load data
load(file=paste0(temp,"/EstimatesAllPI.R"))
load(file=paste0(temp,"/EstimatesAllLSCV.R"))
load(file=paste0(temp,"/EstimatesAllLSCVRN.R"))
load(file=paste0(temp,"/EstimatesAllBCV1.R"))
load(file=paste0(temp,"/EstimatesAllNS.R"))

# Bootstrap CIs
ci.all.pi <- bootstrap.auc(event.type="all", estimates.all.pi)
ci.all.lscv <- bootstrap.auc(event.type="all", estimates.all.lscv)
ci.all.lscv.RN <- bootstrap.auc(event.type="all", estimates.all.lscv.RN)
ci.all.bcv1 <- bootstrap.auc(event.type="all", estimates.all.bcv1)
ci.all.ns <- bootstrap.auc(event.type="all", estimates.all.ns)

# Combine AUC Calculations & Save
cis.all.combined <- rbind(ci.all.pi, ci.all.lscv, ci.all.lscv.RN, ci.all.bcv1, ci.all.ns)
colnames(cis.all.combined) <- c("LB: 2.5%", "Average","UB: 97.5%")
rownames(cis.all.combined) <- c("Plug-In","LSCV","LSCVRN","BCV2","NS")
write.xlsx(cis.all.combined, file=paste0(output,"/AUC Calculations.xlsx"), sheetName="CIs-All", col.names=TRUE, row.names=TRUE, append=TRUE)

## Battles
# Load data
load(file=paste0(temp,"/EstimatesBattlesPI.R"))
load(file=paste0(temp,"/EstimatesBattlesLSCV.R"))
load(file=paste0(temp,"/EstimatesBattlesLSCVRN.R"))
load(file=paste0(temp,"/EstimatesBattlesBCV1.R"))
load(file=paste0(temp,"/EstimatesBattlesNS.R"))

# Bootstrap CIs
ci.b.pi <- bootstrap.auc(event.type="all", estimates.b.pi)
ci.b.lscv <- bootstrap.auc(event.type="all", estimates.b.lscv)
ci.b.lscv.RN <- bootstrap.auc(event.type="all", estimates.b.lscv.RN)
ci.b.bcv1 <- bootstrap.auc(event.type="all", estimates.b.bcv1)
ci.b.ns <- bootstrap.auc(event.type="all", estimates.b.ns)

# Combine AUC Calculations & Save
cis.b.combined <- rbind(ci.b.pi, ci.b.lscv, ci.b.lscv.RN, ci.b.bcv1, ci.b.ns)
colnames(cis.b.combined) <- c("LB: 2.5%", "Average","UB: 97.5%")
rownames(cis.b.combined) <- c("Plug-In","LSCV","LSCVRN","BCV2","NS")
write.xlsx(cis.b.combined, file=paste0(output,"/AUC Calculations.xlsx"), sheetName="CIs-Battles", col.names=TRUE, row.names=TRUE, append=TRUE)

## Riots
# Load data
load(file=paste0(temp,"/EstimatesRiotsPI.R"))
load(file=paste0(temp,"/EstimatesRiotsLSCV.R"))
load(file=paste0(temp,"/EstimatesRiotsLSCVRN.R"))
load(file=paste0(temp,"/EstimatesRiotsBCV1.R"))
load(file=paste0(temp,"/EstimatesRiotsNS.R"))

# Bootstrap CIs
ci.r.pi <- bootstrap.auc(event.type="all", estimates.r.pi)
ci.r.lscv <- bootstrap.auc(event.type="all", estimates.r.lscv)
ci.r.lscv.RN <- bootstrap.auc(event.type="all", estimates.r.lscv.RN)
ci.r.bcv1 <- bootstrap.auc(event.type="all", estimates.r.bcv1)
ci.r.ns <- bootstrap.auc(event.type="all", estimates.r.ns)

# Combine AUC Calculations & Save
cis.r.combined <- rbind(ci.r.pi, ci.r.lscv, ci.r.lscv.RN, ci.r.bcv1, ci.r.ns)
colnames(cis.r.combined) <- c("LB: 2.5%", "Average","UB: 97.5%")
rownames(cis.r.combined) <- c("Plug-In","LSCV","LSCVRN","BCV2","NS")
write.xlsx(cis.r.combined, file=paste0(output,"/AUC Calculations.xlsx"), sheetName="CIs-Riots", col.names=TRUE, row.names=TRUE, append=TRUE)

## Violence Against Civilians
# Load data
load(file=paste0(temp,"/EstimatesVACPI.R"))
load(file=paste0(temp,"/EstimatesVACLSCV.R"))
load(file=paste0(temp,"/EstimatesVACLSCVRN.R"))
load(file=paste0(temp,"/EstimatesVACBCV1.R"))
load(file=paste0(temp,"/EstimatesVACNS.R"))

# Bootstrap CIs
ci.v.pi <- bootstrap.auc(event.type="all", estimates.v.pi)
ci.v.lscv <- bootstrap.auc(event.type="all", estimates.v.lscv)
ci.v.lscv.RN <- bootstrap.auc(event.type="all", estimates.v.lscv.RN)
ci.v.bcv1 <- bootstrap.auc(event.type="all", estimates.v.bcv1)
ci.v.ns <- bootstrap.auc(event.type="all", estimates.v.ns)

# Combine AUC Calculations & Save
cis.v.combined <- rbind(ci.v.pi, ci.v.lscv, ci.v.lscv.RN, ci.v.bcv1, ci.v.ns)
colnames(cis.v.combined) <- c("LB: 2.5%", "Average","UB: 97.5%")
rownames(cis.v.combined) <- c("Plug-In","LSCV","LSCVRN","BCV2","NS")
write.xlsx(cis.v.combined, file=paste0(output,"/AUC Calculations.xlsx"), sheetName="CIs-VAC", col.names=TRUE, row.names=TRUE, append=TRUE)