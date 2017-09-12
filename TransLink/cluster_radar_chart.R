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
clusters <- read.csv("dbscan_cluster.csv", header = TRUE)
#clusters <- read.csv("output_cluster_results.csv", header = TRUE)
clusterDF <- cbind(delayDF,clusters)
clusterDF$clusters <- clusterDF[,36]
clusterMethod <- colnames(clusterDF)[36]
clusterDF[,36] <- NULL

## get means of all variables by cluster
clusterTable <- aggregate(. ~clusters, data= clusterDF, mean)

## Get rid of the outlier row (if needed) !!!!!!!!!!!!!
clusterTable <- clusterTable[-1,]

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
#clusterDF[1,] <- clusterDF[1,]*1.1

# seperated radar charts by cluster
#clusterDF <- clusterDF[c(1:2,3),]

clusterDF <- clusterDF %>%
  mutate_all(funs(signif), digits = 3)

#split into two to make it easier
clusterDF1 <- clusterDF[,1:20]
clusterDF2 <- clusterDF[,c(1,21:37)]

# create radar chart to summarise clusters visually 
#par(mfrow=c(1,2))
#radarchart(df = clusterDF1, pcol = palette(rainbow(5)), axistype = 2)
#radarchart(df = clusterDF2, pcol = palette(rainbow(5)), axistype = 2)


# save a bunch of pictures
filename <- "ClusterDiagrams"

groups <- 5

title=paste(getwd(),'/',filename,'/','output_', sep="")
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2', '#33F4FF', '#F9FF33', '#7EFF33')
for(i in c(1:groups)){
  png(paste(title,clusterMethod,'_',i,'_',Sys.Date(),'.png', sep=""), units="in", width=16, height=10, res=270)
  par(xpd = TRUE,mfrow=c(1,2))
  radarchart(clusterDF1[c(1,2,2+i),], axistype=2, seg=4, pcol = pretty_palette[i],plwd=2, plty=1,vlcex=0.7)
  radarchart(clusterDF2[c(1,2,2+i),], axistype=2, seg=4, pcol = pretty_palette[i],plwd=2, plty=1,vlcex=0.7)
  dev.off()
}