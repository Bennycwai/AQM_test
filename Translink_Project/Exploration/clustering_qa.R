#---------------------------------------------------------------------
#-----------------------------TSNE with translink---------------------
#---------------------------------------------------------------------

rm(list = ls())
setwd("C:/Main/AQM/Translink_Project/Exploration")


delaysdf <- read.csv('line_41_ArriveDelays.csv', header = TRUE)
cSub <- delaysdf[c("max_delay","mean_delay","std_dev_delay","avg_Temp","avg_TravelTime", "avg_ArriveLoad", "avg_Humidity", "avg_Visibility", "avg_WindSpeed", "duration", "n_stops")]

cSub <- cSub[!duplicated(cSub),]

cSub <- cSub[complete.cases(cSub),]

library(Rtsne)
#ecb = function(x,y){ plot(x,t='n'); text(x,labels=cSub$pre_cGPA) }
z <- Rtsne(as.matrix(cSub), dim=2, perplexity = 30)

#cols<-brewer.pal(n=4,name="Set1")?
plot(z$Y, main = "R t-sne clustering on delay variables, perplexity = 30", xlab = "t-sne dimension reduction vector 1",ylab = "t-sne dimension reduction vector 2")

tsne30 <- z

#save(tsne30, file = "tsne30.Rds")



##use dbscan from the R t-sne 2D data
load("C:/Main/AQM/Translink_Project/Exploration/tsne30.Rds")

library("dbscan")

db <- dbscan(tsne30$Y, eps = 2.22, minPts = 10) # eps = 2.3, minPts = 10
tsnelinked <- tsne30$Y
tsnelinked <- data.frame(tsnelinked)
tsnelinked$cluster <- db$cluster

table(tsnelinked$cluster)

plot(tsnelinked$X1,tsnelinked$X2, col = tsnelinked$cluster)

## Try this again using Rtsne for 3D output --------------------------------------------

rm(list = ls())
setwd("C:/Main/AQM/Translink_Project/Exploration")
library(tsne)
library(rgl)
library(plotly)
library(ggplot2)
library("dbscan")

# load and clean data
delaysdf <- read.csv('line_41_ArriveDelays.csv', header = TRUE)
cSub <- delaysdf[c("max_delay","mean_delay","std_dev_delay","avg_Temp","avg_TravelTime", "avg_ArriveLoad", "avg_Humidity", "avg_Visibility", "avg_WindSpeed", "duration", "n_stops")]
cSub <- cSub[!duplicated(cSub),]
cSub <- cSub[complete.cases(cSub),]

z <- Rtsne(as.matrix(cSub), dim=3, perplexity = 30)

tsne30 <- z

db <- dbscan(tsne30$Y, eps = 5, minPts = 10) # eps = 2.3, minPts = 10
tsnelinked <- tsne30$Y
tsnelinked <- data.frame(tsnelinked)
tsnelinked$cluster <- db$cluster

table(tsnelinked$cluster)

#3D Scatterplot
plot_ly(x = tsnelinked[,1], y = tsnelinked[,2], z = tsnelinked[,3], color = tsnelinked[,4], colors = palette(rainbow(14)))

#3D Scatterplot without "outliers" group 0
tsneFull <- tsnelinked[tsnelinked$cluster != 0,]
plot_ly(x = tsneFull[,1], y = tsneFull[,2], z = tsneFull[,3], color = tsneFull[,4], colors = palette(rainbow(12)))

#save(tsne30, file = "tsne30.Rds")
#save(tsnelinked, file = "tsnelinked.Rds")
#save(cSub, file = "cSub.Rds")


## analysing the clusters -------------------------------------------
rm(list = ls())
library(dplyr)
library(fmsb)
library(radarchart)
library(tsne)
library(rgl)
library(plotly)
library(ggplot2)
library("dbscan")

setwd("C:/Main/AQM/Translink_Project/Exploration")
load("C:/Main/AQM/Translink_Project/Exploration/tsne30.Rds")
load("C:/Main/AQM/Translink_Project/Exploration/tsnelinked.Rds")
load("C:/Main/AQM/Translink_Project/Exploration/cSub.Rds")
load("C:/Main/AQM/Translink_Project/Exploration/delaydf_cluster.Rds")
load("C:/Main/AQM/Translink_Project/Exploration/tsneFull.Rds")

delaydf_clust <- cbind(cSub,tsnelinked)

clusterTable <- delaydf_clust %>% 
  group_by(cluster) %>% 
  summarise(total.count=n(), max_delay = mean(max_delay), mean_delay = mean(mean_delay), std_dev_delay = mean(std_dev_delay), avg_Temp = mean(avg_Temp), avg_TravelTime = mean(avg_TravelTime), avg_ArriveLoad = mean(avg_ArriveLoad), avg_Humidity = mean(avg_Humidity), avg_Visibility = mean(avg_Visibility), avg_WindSpeed = mean(avg_WindSpeed), duration = mean(duration), n_stops = mean(n_stops))

# Get rid of the outlier row
clusterTable <- clusterTable[-1,]
# acquire max value for each column
clusterMaxes <- clusterTable %>%
  summarise_each(funs(max))

clusterDF <- rbind(clusterMaxes, rep(0,13), clusterTable)
# create radar chart to summarise clusters visually 

radarchart(df = clusterDF, pcol = palette(rainbow(7))) #cglcol = palette(rainbow(7))

#save(delaydf_clust, file = "delaydf_clust.Rds")
#save(tsneFull, file = "tsneFull.Rds")

#3D Scatterplot without "outliers" group 0
plot_ly(x = tsneFull[,1], y = tsneFull[,2], z = tsneFull[,3], color = tsneFull[,4], colors = palette(rainbow(12)))

