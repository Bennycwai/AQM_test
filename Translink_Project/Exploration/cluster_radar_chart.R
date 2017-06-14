# Analysing clusters with radar chart
rm(list = ls())
setwd("C:/Main/AQM/Translink_Project/Exploration")
library(rgl)
library(plotly)
library(dbscan)
library(clusterSim)
library(dplyr)
library(fmsb)
library(radarchart)

#load in full dataset (complete only)
load("C:/Main/AQM/Translink_Project/Exploration/delayDF.Rds")

#load in csv with cluster numbers (cbind only one specific column!)
#clusters <- read.csv("dbscan_cluster.csv", header = TRUE)
clusters <- read.csv("output_cluster_results.csv", header = TRUE)
clusterDF <- cbind(delayDF,clusters[4])
clusterDF$clusters <- clusterDF[,36]
clusterDF[,36] <- NULL

## get means of all variables by cluster
clusterTable <- aggregate(. ~clusters, data= clusterDF, mean)

## Get rid of the outlier row (if needed) !!!!!!!!!!!!!
#clusterTable <- clusterTable[-1,]

for (i in 1:5)
{
  clusterTable$count[i] <- sum(clusterDF$clusters == i) 
}

# acquire max value for each column
clusterMaxes <- clusterTable %>%
  summarise_each(funs(max))

clusterDF <- rbind(clusterMaxes, rep(0,13), clusterTable)
clusterDF$clusters[2]<- 1
clusterDF$count[2]<- 0

#split into two to make it easier
clusterDF1 <- clusterDF[,1:20]
clusterDF2 <- clusterDF[,c(1,21:37)]

# create radar chart to summarise clusters visually 
par(mfrow=c(1,2))
radarchart(df = clusterDF1, pcol = palette(rainbow(5)))
radarchart(df = clusterDF2, pcol = palette(rainbow(5)))
