## find outliers via dbscan

## reading the files --------------------------
#load("C:/Main/AQM/Translink_Project/Exploration/delays_summarised.Rds")

# load the 2016 data sets for delay and early events
#deDF <- read.csv("DelayEvents_2016.csv", header = TRUE)
#earDF <- read.csv("TooEarlyEvents_2016.csv", header = TRUE)

#save(deDF, file = "deDF.Rds")
#save(earDF, file = "earDF.Rds")

## dbscan clustering to find outliers --------------------------
rm(list = ls())
setwd("C:/Main/AQM/Translink_Project/Exploration")

load("C:/Main/AQM/Translink_Project/Exploration/deDF.rds")
load("C:/Main/AQM/Translink_Project/Exploration/earDF.rds")

library(dbscan)

delaysdf <- delays_summarised

# load and clean data
cSub <- subset(delaysdf, select = c(-OperationDate,-Trip, -Line, -weather.Mainlyclear, -weather.Mostlycloudy, -delayID))
cSub <- cSub[complete.cases(cSub),]

# normalizing columns individually
for (i in 1:ncol(cSub))
{
  minner <- min(cSub[,i])
  cSub[,i] <- cSub[,i] - minner
  maxxer <- max(cSub[,i])
  cSub[,i] <- cSub[,i]/maxxer
}

#cNorm <- data.Normalization(cSub,type="n8",normalization="column")

db <- dbscan(cSub, eps = 0.985, minPts = 3) # eps = 2.3, minPts = 10
table(db$cluster)

# check on cluster status
table(db$cluster)


delays_outlier <- delays_summarised
delays_outlier <- subset(delays_outlier, select = c(-OperationDate,-Trip, -Line, -weather.Mainlyclear, -weather.Mostlycloudy, -delayID))
delays_outlier <- delays_outlier[complete.cases(delays_outlier),]

delays_outlier$cluster <- db$cluster

delays_outlier$outlier <- 0

# from the table of clusters, make all elements in small clusters (16 or less) as outlier points
delays_outlier$outlier[delays_outlier$cluster %in% c(0,10,11,13,14,18,20,21,22)] <- 1

#save(delays_outlier, file = "delays_outlier.Rds")

## analysis the outliers ---------
require(ggplot2)
require(reshape2)
require(dplyr)

rm(list = ls())
setwd("C:/Main/AQM/Translink_Project/Exploration")
load("C:/Main/AQM/Translink_Project/Exploration/delays_summarised.Rds")
load("C:/Main/AQM/Translink_Project/Exploration/delays_outlier.Rds")

# normalizing columns individually
for (i in 1:ncol(delays_outlier))
{
  minner <- min(delays_outlier[,i])
  delays_outlier[,i] <- delays_outlier[,i] - minner
  maxxer <- max(delays_outlier[,i])
  delays_outlier[,i] <- delays_outlier[,i]/maxxer
}

## get outliers only, and get rid of cluster row
outdf <- delays_outlier[delays_outlier$outlier == 1,]
outdf$cluster <- NULL

indf <- delays_outlier[delays_outlier$outlier == 0,]
indf$cluster <- NULL

## seperate into subsets to make analysis feasible
outdf1 <- outdf[,c(37,1:10)]
outdf2 <- outdf[,c(37,11:19)]
outdf3 <- outdf[,c(37,20:28)]
outdf4 <- outdf[,c(37,29:36)]

indf1 <- indf[,c(37,1:10)]
indf2 <- indf[,c(37,11:19)]
indf3 <- indf[,c(37,20:28)]
indf4 <- indf[,c(37,29:36)]

#in1 <- indf1 %>%
#  reshape2::melt(id.vars = "outlier") %>%
#  group_by(variable) %>%
#  ggplot(aes(x=variable, y=value, colour=variable)) +
#  geom_boxplot()
#in1

out1 <- outdf1 %>%
  reshape2::melt(id.vars = "outlier") %>%
  group_by(variable) #%>%
  #ggplot(aes(x=variable, y=value, colour=variable)) +
 # geom_point()

in1 <- indf1 %>%
  reshape2::melt(id.vars = "outlier") %>%
  group_by(variable) #%>%
  #ggplot(aes(x=variable, y=value, colour=variable)) +
  #geom_boxplot(outlier.shape = NA) +
  #geom_point(out1,aes(x=variable, y=value, colour=variable))

outlier_graph1 <- ggplot(in1,aes(x=variable, y=value, colour=variable)) +
  geom_boxplot(outlier.shape = NA) +
  #geom_point(data = out1,aes(x=variable, y=value)) +
  geom_jitter(data = out1, aes(x=variable, y=value), height = 0.01, width = 0.1)

out1 <- outdf1 %>%
  reshape2::melt(id.vars = "outlier") %>%
  group_by(variable) #%>%
#ggplot(aes(x=variable, y=value, colour=variable)) +
# geom_point()

in1 <- indf1 %>%
  reshape2::melt(id.vars = "outlier") %>%
  group_by(variable) #%>%
#ggplot(aes(x=variable, y=value, colour=variable)) +
#geom_boxplot(outlier.shape = NA) +
#geom_point(out1,aes(x=variable, y=value, colour=variable))

outlier_graph1 <- ggplot(in1,aes(x=variable, y=value, colour=variable)) +
  geom_boxplot(outlier.shape = NA) +
  #geom_point(data = out1,aes(x=variable, y=value)) +
  geom_jitter(data = out1, aes(x=variable, y=value), height = 0.01, width = 0.1)

## 2

out2 <- outdf2 %>%
  reshape2::melt(id.vars = "outlier") %>%
  group_by(variable) #%>%
#ggplot(aes(x=variable, y=value, colour=variable)) +
# geom_point()

in2 <- indf2 %>%
  reshape2::melt(id.vars = "outlier") %>%
  group_by(variable) #%>%
#ggplot(aes(x=variable, y=value, colour=variable)) +
#geom_boxplot(outlier.shape = NA) +
#geom_point(out1,aes(x=variable, y=value, colour=variable))

outlier_graph2 <- ggplot(in2,aes(x=variable, y=value, colour=variable)) +
  geom_boxplot(outlier.shape = NA) +
  #geom_point(data = out1,aes(x=variable, y=value)) +
  geom_jitter(data = out2, aes(x=variable, y=value), height = 0.01, width = 0.1)

## 3

out3 <- outdf3 %>%
  reshape2::melt(id.vars = "outlier") %>%
  group_by(variable) #%>%
#ggplot(aes(x=variable, y=value, colour=variable)) +
# geom_point()

in3 <- indf3 %>%
  reshape2::melt(id.vars = "outlier") %>%
  group_by(variable) #%>%
#ggplot(aes(x=variable, y=value, colour=variable)) +
#geom_boxplot(outlier.shape = NA) +
#geom_point(out1,aes(x=variable, y=value, colour=variable))

outlier_graph3 <- ggplot(in3,aes(x=variable, y=value, colour=variable)) +
  geom_boxplot(outlier.shape = NA) +
  #geom_point(data = out1,aes(x=variable, y=value)) +
  geom_jitter(data = out3, aes(x=variable, y=value), height = 0.01, width = 0.1)


## 4

out4 <- outdf4 %>%
  reshape2::melt(id.vars = "outlier") %>%
  group_by(variable) #%>%
#ggplot(aes(x=variable, y=value, colour=variable)) +
# geom_point()

in4 <- indf4 %>%
  reshape2::melt(id.vars = "outlier") %>%
  group_by(variable) #%>%
#ggplot(aes(x=variable, y=value, colour=variable)) +
#geom_boxplot(outlier.shape = NA) +
#geom_point(out1,aes(x=variable, y=value, colour=variable))

outlier_graph4 <- ggplot(in4,aes(x=variable, y=value, colour=variable)) +
  geom_boxplot(outlier.shape = NA) +
  #geom_point(data = out1,aes(x=variable, y=value)) +
  geom_jitter(data = out4, aes(x=variable, y=value), height = 0.01, width = 0.1)

outlier_graph4

## analyze how many outliers are outside n standard deviations for at least one variable-------
require(ggplot2)
require(reshape2)
require(dplyr)

rm(list = ls())
setwd("C:/Main/AQM/Translink_Project/Exploration")
load("C:/Main/AQM/Translink_Project/Exploration/delays_summarised.Rds")
load("C:/Main/AQM/Translink_Project/Exploration/delays_outlier.Rds")

delays_outlier <- subset(delays_outlier,select=-c(DayType,start_lat, start_long, end_lat, end_long, start_TimingPoint, end_TimingPoint))

outdf <- delays_outlier[delays_outlier$outlier == 1,]
outdf$cluster <- NULL
outdf$outlier <- NULL

indf <- delays_outlier[delays_outlier$outlier == 0,]
indf$cluster <- NULL
indf$outlier <- NULL

outdf$out1 <-0
outdf$out2 <-0
outdf$out3 <-0
outdf$out4 <-0
outdf$out5 <-0
outdf$out6 <-0

stdValues <- data.frame(matrix(ncol = ncol(indf), nrow = 2))

stdValues[1,] <- apply(indf, 2, mean) + 3*apply(indf, 2, sd)
stdValues[2,] <- apply(indf, 2, mean) - 3*apply(indf, 2, sd)

for (i in 1:)