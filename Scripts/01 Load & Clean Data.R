### Set up coding environment
# Clear Data Sets & Plots
rm(list=ls())

# Load libraries
library(GISTools)
library(maptools)
library(rgdal)
library(raster)
library(rgeos)
library(dismo)
library(psych)
library(ks)

# Map paths
setwd('/Users/jmoore523/Dropbox/Graduate School/Q3 - Spring 2016/STATS205/Project/STATS205-DRCConflict')
input <- file.path(".", "Input")
output <- file.path(".", "Output")
temp <- file.path(".", "Temp")

### Load & Clean up Data
# Load Data
acled <- readShapePoints(paste0("input/ACLED/ACLED_Version6_Shpfile.shp"))
drc <- readShapePoly(paste0("input/DRC Outlines/COD_adm0.shp"))

### ACLED
# Subset to only events in the DRC
acled.drc <- subset(acled, COUNTRY=="Democratic Republic of Congo")

# Create month variable
acled.drc$month_std <- as.numeric(format(acled.drc$EVENT_DATE, "%m"))

# Eliminate minor event types
acled.drc <- subset(acled.drc, acled.drc$EVENT_TYPE!="Non-violent transfer of territory" & acled.drc$EVENT_TYPE!="Headquarters or base established" & acled.drc$EVENT_TYPE!="Remote violence" & acled.drc$EVENT_TYPE!="Strategic development")

# Categorize event types
acled.drc$event_type_std <- as.character(acled.drc$EVENT_TYPE)
acled.drc$event_type_std[acled.drc$EVENT_TYPE=="Battle-Government regains territory" | acled.drc$EVENT_TYPE=="Battle-No change of territory" | acled.drc$EVENT_TYPE=="Battle-Non-state actor overtakes territory"] <- "Battle"

### DRC
## Grid Africa
grid <- raster(extent(drc))
res(grid) <- .1
proj4string(grid) <- proj4string(drc)
gridpolygon <- rasterToPolygons(grid)
drcgrid <- intersect(drc, gridpolygon)
drcgrid$ID <- seq(1,nrow(drcgrid))

# Center Points
drcgrid.centroids = gCentroid(drcgrid,byid=TRUE)

### Save data
save(acled.drc,file=paste0(temp,"/acleddrc.R"))
save(drc,file=paste0(temp,"/drc.R"))
save(drcgrid,file=paste0(temp,"/drcgrid.R"))
save(drcgrid.centroids,file=paste0(temp,"/drcgridcentroids.R"))
