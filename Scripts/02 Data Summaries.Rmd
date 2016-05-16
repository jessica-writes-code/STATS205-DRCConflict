### Set up coding environment
# Clear Data Sets & Plots
rm(list=ls())

# Load libraries
library(maptools)
library(xlsx)

# Map paths
setwd('/Users/jmoore523/Dropbox/Graduate School/Q3 - Spring 2016/STATS205/Project/STATS205-DRCConflict')
input <- file.path(".", "Input")
output <- file.path(".", "Output")
temp <- file.path(".", "Temp")

# Load data
load(paste0(temp,"/acleddrc.R"))
load(paste0(temp,"/drc.R"))

### Summary Statistics
acled.drc$count <- 1 

## No. Events Per Month (2014-2015)
acled.drc1415 <- subset(acled.drc, YEAR==2014 | YEAR==2015)
acled.drc1415.counts <- aggregate(cbind(count, FATALITIES)~YEAR+month_std+event_type_std, data=acled.drc1415, sum)
names(acled.drc1415.counts) <- c("year","month_std","event_type_std", "event_count","fatalities")
acled.drc1415.counts$month_order_std <- acled.drc1415.counts$month_std
acled.drc1415.counts$month_order_std[acled.drc1415.counts$year==2015] <- acled.drc1415.counts$month_order_std[acled.drc1415.counts$year==2015] + 12
acled.drc1415.counts <- acled.drc1415.counts[order(acled.drc1415.counts$month_order_std),]

# Table
write.xlsx(acled.drc1415.counts, file=paste0(output,"/Data Summaries.xlsx"), sheetName="Event Count 1415", col.names=TRUE, row.names=FALSE, append=FALSE)

# Graph - Counts
battles <- subset(acled.drc1415.counts, event_type_std == "Battle")
riots <- subset(acled.drc1415.counts, event_type_std == "Riots/Protests")
vacs <- subset(acled.drc1415.counts, event_type_std == "Violence against civilians")

png(paste0(output,'/EventCount20142015.png'))
par(mar=c(3,3.5,4.1,2.1))
par(las=1)
plot(battles$month_order_std, battles$event_count, type="l", lwd=2, xlab = "Month", ylim=c(0,55), xaxt='n', col="red", ann=FALSE)
axis(1, at=c(1,6,12,18,24), labels=c("Jan.2014","Jun.2014","Dec.2014","Jun.2015","Dec.2015"), cex.axis=1.10)
title(ylab = "Event Count", line = 2.0, cex.lab=1.20)
lines(riots$month_order_std, riots$event_count, type="l", lwd=2, col="blue")
lines(vacs$month_order_std, vacs$event_count, lwd=2, col="green")
par(xpd=TRUE)
legend(8, 65, c("Battles","Riots/Protests","Violence Against Civilians"), lwd=c(2,2,2), col=c("red","blue","green"))
par(xpd=FALSE)
dev.off()

## No. Events Per Year (2010-2015)
acled.drc1015 <- subset(acled.drc, YEAR>=2010 & YEAR<=2015)
acled.drc1015.counts <- aggregate(cbind(count, FATALITIES)~YEAR+event_type_std, data=acled.drc1015, sum)
names(acled.drc1015.counts) <- c("year","event_type_std", "event_count","fatalities")

# Table
write.xlsx(acled.drc1015.counts, file=paste0(output,"/Data Summaries.xlsx"), sheetName="Event Count 1015", col.names=TRUE, row.names=FALSE, append=TRUE)

# Graph - Counts
battles <- subset(acled.drc1015.counts, event_type_std == "Battle")
riots <- subset(acled.drc1015.counts, event_type_std == "Riots/Protests")
vacs <- subset(acled.drc1015.counts, event_type_std == "Violence against civilians")

png(paste0(output,'/EventCount20102015.png'))
par(mar=c(3,3.5,4.1,2.1))
par(las=1)
plot(battles$year, battles$event_count, type="l", lwd=2, xlab = "Year", ylim=c(0,500), col="red", ann=FALSE)
title(ylab = "Event Count", line = 2.4, cex.lab=1.20)
lines(riots$year, riots$event_count, type="l", lwd=2, col="blue")
lines(vacs$year, vacs$event_count, lwd=2, col="green")
par(xpd=TRUE)
legend(2011.5, 575, c("Battles","Riots/Protests","Violence Against Civilians"), lwd=c(2,2,2), col=c("red","blue","green"))
par(xpd=FALSE)
dev.off()

### Visual Summaries
par(mar=c(1,1,1,2))

## All Conflicts
acled.drc2014 <- subset(acled.drc, YEAR==2014)
acled.drc2015 <- subset(acled.drc, YEAR==2015)

# 2014
png(paste0(output,'/AllConflicts2014.png'))
plot(drc, xlim=c(21,21.025), ylim=c(-13,6))
plot(acled.drc2014, pch=20, cex=.25, add=TRUE)
dev.off()

# 2015
png(paste0(output,'/AllConflicts2015.png'))
plot(drc, xlim=c(21,21.025), ylim=c(-13,6))
plot(acled.drc2015, pch=20, cex=.25, add=TRUE)
dev.off()

## Battles
acled.drc2014.battles <- subset(acled.drc2014, event_type_std=="Battle")
acled.drc2015.battles <- subset(acled.drc2015, event_type_std=="Battle")

# 2014
png(paste0(output,'/Battles2014.png'))
plot(drc, xlim=c(21,21.025), ylim=c(-13,6))
plot(acled.drc2014.battles, pch=20, cex=.25, add=TRUE, col="red")
dev.off()

# 2015
png(paste0(output,'/Battles2015.png'))
plot(drc, xlim=c(21,21.025), ylim=c(-13,6))
plot(acled.drc2015.battles, pch=20, cex=.25, add=TRUE, col="red")
dev.off()

## Riots/Protests
acled.drc2014.riots <- subset(acled.drc2014, event_type_std=="Riots/Protests")
acled.drc2015.riots <- subset(acled.drc2015, event_type_std=="Riots/Protests")

# 2014
png(paste0(output,'/Riots2014.png'))
plot(drc, xlim=c(21,21.025), ylim=c(-13,6))
plot(acled.drc2014.riots, pch=20, cex=.25, add=TRUE, col="blue")
dev.off()

# 2015
png(paste0(output,'/Riots2015.png'))
plot(drc, xlim=c(21,21.025), ylim=c(-13,6))
plot(acled.drc2015.riots, pch=20, cex=.25, add=TRUE, col="blue")
dev.off()

## Violence against civilians
acled.drc2014.vac <- subset(acled.drc2014, event_type_std=="Violence against civilians")
acled.drc2015.vac <- subset(acled.drc2015, event_type_std=="Violence against civilians")

# 2014
png(paste0(output,'/VAC2014.png'))
plot(drc, xlim=c(21,21.025), ylim=c(-13,6))
plot(acled.drc2014.vac, pch=20, cex=.25, add=TRUE, col="green")
dev.off()

# 2015
png(paste0(output,'/VAC2015.png'))
plot(drc, xlim=c(21,21.025), ylim=c(-13,6))
plot(acled.drc2015.vac, pch=20, cex=.25, add=TRUE, col="green")
dev.off()

### Saving
save(acled.drc2014, file=paste0(temp,"/acled.drc2014.R"))
save(acled.drc2015, file=paste0(temp,"/acled.drc2015.R"))

save(acled.drc2014.battles, file=paste0(temp,"/acleddrc2014battles.R"))
save(acled.drc2015.battles, file=paste0(temp,"/acleddrc2015battles.R"))

save(acled.drc2014.riots, file=paste0(temp,"/acleddrc2014riots.R"))
save(acled.drc2015.riots, file=paste0(temp,"/acleddrc2015riots.R"))

save(acled.drc2014.vac, file=paste0(temp,"/acleddrc2014vac.R"))
save(acled.drc2015.vac, file=paste0(temp,"/acleddrc2015vac.R"))
