## using tsne to general a nice plot with the new dataset----------
rm(list = ls())
setwd("C:/Main/AQM/Translink_Project/Exploration")
library(Rtsne)
library(rgl)
library(plotly)
library(ggplot2)
library(dbscan)
library(tibble)
library(stats)
library(graphics)

#require(plotly)
# load and clean data
tripsdf <- readRDS("C:/Main/AQM/Translink_Project/diceR/DelayEvents_2016_Normalizedv2.rds")
tripsdf <- tripsdf[,6:ncol(tripsdf)]

#colnames(tripsdf)[1:20] <- paste("Var", 1:20, sep="")

#delaysdf <- trips_145_summarised
cSub <- tripsdf[c("duration", "max_delay", "mean_delay", "avg_TravelTime", "avg_ArriveLoad", "median_delay", "n_stops", "TotalTimingPoints", "avg_OnsLoadCompensated", "avg_OffsLoadCompensated", "avg_LeaveLoadCompensated", "avg_WCLiftActivated", "avg_BikeLoaded", "avg_DwellTime", "avg_OnAndOffsCompensated", "onset_time_seconds", "avg_distance", "total_distance", "avg_tripportion")]

#cSub$duration <- as.numeric(cSub$duration)
cSub <- cSub[complete.cases(cSub),]
cSub <- cSub[!duplicated(cSub),]

tsneGroups <- Rtsne(as.matrix(cSub), dim=3, perplexity = 30)

#save(tsneGroups, file = "tsneGroups.Rds")
load("C:/Main/AQM/Translink_Project/Exploration/tsneGroups.Rds")

## run this when done
#save(tsneGroups, file = "tsneGroups.Rds")
#tsneAnswer <- tsneGroups----------------
tripsdf <- readRDS("C:/Main/AQM/Translink_Project/diceR/DelayEvents_2016_Normalizedv2.rds")
tripsdf <- tripsdf[,6:ncol(tripsdf)]

#delaysdf <- trips_145_summarised
cSub <- tripsdf[c("duration", "max_delay", "mean_delay", "avg_TravelTime", "avg_ArriveLoad", "median_delay", "n_stops", "TotalTimingPoints", "avg_OnsLoadCompensated", "avg_OffsLoadCompensated", "avg_LeaveLoadCompensated", "avg_WCLiftActivated", "avg_BikeLoaded", "avg_DwellTime", "avg_OnAndOffsCompensated", "onset_time_seconds", "avg_distance", "total_distance", "avg_tripportion")]

#cSub$duration <- as.numeric(cSub$duration)
cSub <- cSub[complete.cases(cSub),]
cSub <- cSub[!duplicated(cSub),]

load("C:/Main/AQM/Translink_Project/Exploration/tsneGroups.Rds")

devtools::install_version("plotly", version = "4.5.6", 
                          repos = "http://cran.us.r-project.org")
# plot raw data
tsnedf <- as.data.frame(tsneGroups$Y)

library(scatterplot3d)
scatterplot3d( x = tsnedf[,1], y = tsnedf[,2], z = tsnedf[,3])

plot_ly(data = tsnedf, x = ~V1, y = ~V2, z = ~V3)
plot_ly(data = tsnedf, x = ~V1, y = ~V2, z = ~V3, type="scatter3d", mode = "markers")

db <- dbscan(tsneGroups$Y, eps = 4, minPts = 10) # eps = 2.3, minPts = 10
tsnelinked <- tsneGroups$Y
tsnelinked <- data.frame(tsnelinked)
tsnelinked$cluster <- db$cluster

table(tsnelinked$cluster)

table(tsneGroups$cluster)

# plot raw data
tsnedf <- as.data.frame(tsneGroups$Y)
plot_ly(data = tsnedf, x = ~V1, y = ~V2, z = ~V3, type="scatter3d", mode = "markers")
plot_ly(x = tsnedf[,1], y = tsnedf[,2], z = tsnedf[,3])

plot_ly(x = tsneFull[,1], y = tsneFull[,2], z = tsneFull[,3], color = tsneFull[,4], colors = palette(rainbow(12)))
