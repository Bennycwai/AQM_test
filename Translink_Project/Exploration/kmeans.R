
#User: Benny Wai
#Latest Date: September 26th 2016
rm(list = ls())
setwd("C:/Main/Machine_Learning_Hobby/Home_Edition/Student_Grades/Clustering")

#---------------------------------------------------------------------
#------------------------Make subset for clustering-------------------
#---------------------------------------------------------------------

load("C:/Main/Machine_Learning_Hobby/Home_Edition/Student_Grades/Clustering/gdSub.RData")

#NOTE: pastBadGrades = F,N,D,C-

#make a subset only containing columns that will used as clustering variables
cSub <- gdSub[c("pre_cGPA","pastBadGrades","num.Diff","num.Easy","prereqSemBtwn","prereqValue")]

#get rid of outlier
cSub <- cSub[!grepl(44,cSub$prereqSemBtwn),]

#normalize the data by dividing by the total amount
for (i in 1:6){
  cSub[i] <- cSub[i]/max(cSub[i])
}

save(cSub, file = "cSub.RData")



##MACM316 analysis color and tsne---------
load("C:/Main/Machine_Learning_Hobby/Home_Edition/Student_Grades/Clustering/gdSub.RData")

gM316 <- gdSub[gdSub$Course == "MACM316",]

for (j in 1:length(gM316$Course)){
  if (identical("F", gM316$CrsGrade[j])){
    gM316$gcolor[j] <- 1
  }
  if (identical("N", gM316$CrsGrade[j])){
    gM316$gcolor[j] <- 2
  }
  if (identical("D", gM316$CrsGrade[j])){
    gM316$gcolor[j] <- 3
  }
  if (identical("C-", gM316$CrsGrade[j])){
    gM316$gcolor[j] <- 4
  }
  if (identical("C", gM316$CrsGrade[j])){
    gM316$gcolor[j] <- 5
  }
  if (identical("C+", gM316$CrsGrade[j])){
    gM316$gcolor[j] <- 6
  }
  if (identical("B-", gM316$CrsGrade[j])){
    gM316$gcolor[j] <- 7
  }
  if (identical("B", gM316$CrsGrade[j])){
    gM316$gcolor[j] <- 8
  }
  if (identical("B+", gM316$CrsGrade[j])){
    gM316$gcolor[j] <- 9
  }
  if (identical("A-", gM316$CrsGrade[j])){
    gM316$gcolor[j] <- 10
  }
  if (identical("A", gM316$CrsGrade[j])){
    gM316$gcolor[j] <- 11
  }
  if (identical("A+", gM316$CrsGrade[j])){
    gM316$gcolor[j] <- 12
  }
}

save(gM316, file = "gM316.RData")

#make a subset only containing columns that will used as clustering variables
#c316 <- gM316[c("pre_cGPA","pastBadGrades","num.Diff","num.Easy","prereqSemBtwn","prereqValue")]
c316 <- gM316[c("pre_cGPA","pastBadGrades","prereqSemBtwn","prereqValue")]

#normalize the data by dividing by the total amount
for (i in 1:4){
  c316[i] <- c316[i]/max(c316[i])
}

save(c316, file = "c316.RData")

load("gM316.RData")
load("c316.RData")

#run tsne
#start timing
library(tsne)

ptm <- proc.time()

#ecb = function(x,y){ plot(x,t='n'); text(x,labels=cSub$pre_cGPA) }
z <- tsne(c316, max_iter = 500)

#stop timing, and output
proc.time() - ptm

tsneOut <- z
#add a color into tsne output
tsneOut <- cbind(z, gM316$gcolor)
tsneOut <- data.frame(tsneOut)

#with color
library(RColorBrewer)

#all palette available from RColorBrewer
display.brewer.all()
#we will select the first 4 colors in the Set1 palette
cols<-brewer.pal(n=4,name="Set1")
#cols contain the names of four different colors
#create a color vector corresponding to levels in the T1 variable in dat
cols_t1<-cols[tsneOut$X3]
#plot
plot(tsneOut[,1:2],col=cols_t1, main = "t-sne on MACM 316")


##Try Rtsne instead------
setwd("C:/Main/Machine_Learning_Hobby/Home_Edition/Student_Grades/Clustering")
load("gM316.RData")
load("c316.RData")

library(Rtsne)

#number of dimensions to run tsne on
t = 4

gM316 <- gM316[c("pre_cGPA","pastBadGrades","prereqSemBtwn","prereqValue", "gcolor")]
#gM316 <- gM316[c("pre_cGPA","pastBadGrades","num.Diff","num.Easy","prereqSemBtwn","prereqValue", "gcolor")]
gM316 <- gM316[!duplicated(gM316[,1:t]),]


#only values to work on
c316 <- gM316[c("pre_cGPA","pastBadGrades","prereqSemBtwn","prereqValue")]
#c316 <- gM316[c("pre_cGPA","pastBadGrades","num.Diff","num.Easy","prereqSemBtwn","prereqValue")]

#normalize the data by dividing by the total amount
for (i in 1:t){
  c316[i] <- c316[i]/max(c316[i])
}


#with color
library(RColorBrewer)


#ecb = function(x,y){ plot(x,t='n'); text(x,labels=cSub$pre_cGPA) }
z <- Rtsne(as.matrix(c316), dim=2, perplexity = 30)

#cols<-brewer.pal(n=4,name="Set1")
cols <- c("gray8","firebrick","steelblue1","steelblue3","steelblue4")
cols_t1<-cols[gM316$gcolor]
plot(z$Y,col=cols_t1, main = "Rtsne on 4 variables for MACM 316, perplexity = 30")

#color plot maker-----------------------------------------------------------------
setwd("C:/Main/Machine_Learning_Hobby/Home_Edition/Student_Grades/Clustering")
load("gM316.RData")
load("c316.RData")

library(Rtsne)
#ecb = function(x,y){ plot(x,t='n'); text(x,labels=cSub$pre_cGPA) }
z <- Rtsne(as.matrix(c316), dim=2, perplexity = 30)

#cols<-brewer.pal(n=4,name="Set1")?
cols <- c("firebrick1","firebrick1","firebrick1","chartreuse1","chartreuse1","chartreuse1","black","black","black","steelblue3","steelblue3","steelblue3")
cols_t1<-cols[gM316$gcolor]
plot(z$Y,col=cols_t1, main = "Rtsne on 4 variables for MACM 316, perplexity = 30", xlab = "t-sne dimension reduction vector 1",ylab = "t-sne dimension reduction vector 2")
legend(x=23,y=36, bty = "n", text.col=c("firebrick1","chartreuse1"), c("F,N,D", "C-,C,C+"), cex=.9)
legend(x=29,y=36, bty = "n", text.col=c("black","steelblue3"), c("B-,B,B+","A-,A,A+"), cex=.9)
legend(x=21,y=39, bty = "n", text.col=c("gray3"), c("Course grades by color"), cex=1)


#running dbscan on the output of Rtsne----------------------------------------------



#Kmeans Cluster---------------------------------------------------------------------


load("C:/Main/Machine_Learning_Hobby/Home_Edition/Student_Grades/Clustering/cSub.RData")

#clustering
clusterSubset.results <- kmeans(cSub, 4)
#make an analysis table
clusterTableSubset<-data.frame(clusterSubset.results$size,clusterSubset.results$centers)
clusterTableSubset$size <- clusterTableSubset$clusterSubset.results.size
clusterTableSubset$clusterSubset.results.size<-NULL
round(clusterTableSubset, digits = 3)

#Getting cluster data sets
dataMainSubset$group <- clusterSubset.results$cluster
cluster1 <- dataMainSubset[dataMainSubset$group == 1,]
cluster2 <- dataMainSubset[dataMainSubset$group == 2,]
cluster3 <- dataMainSubset[dataMainSubset$group == 3,]
cluster4 <- dataMainSubset[dataMainSubset$group == 4,]

#analysis of the groups
mean(cluster1$CUM_GPA)
mean(cluster2$CUM_GPA)
mean(cluster3$CUM_GPA)
mean(cluster4$CUM_GPA)

#---------------------------------------------------------------------
#-----------------------------Running T-SNE---------------------------
#---------------------------------------------------------------------
library(tsne)

load("C:/Main/Machine_Learning_Hobby/Home_Edition/Student_Grades/Clustering/cSub.RData")

##sprinkle white noise in it, makes it continuous and hopefully inproves speed
#for (j in 1:6){
#  for (i in 1:length(cSub$pre_cGPA)){
#    cSub[i,j] <- cSub[i,j] + runif(1,min = 0, max = 0.001)
#  }
#}

#Try to look at 4 values instead of 6
cSub1 <- cSub[c("pre_cGPA","pastBadGrades","prereqSemBtwn","prereqValue")]

#take a sample of data and run, as tsne takes a long time to run
mysample <- cSub[sample(1:nrow(cSub), 2500, replace=FALSE),]

#start timing
ptm <- proc.time()

#ecb = function(x,y){ plot(x,t='n'); text(x,labels=cSub$pre_cGPA) }
z <- tsne(cSub)

#stop timing, and output
proc.time() - ptm

plot(z)

#---------------------------------------------------------------------
#-----------------------------Try 3D Plotting-------------------------
#---------------------------------------------------------------------
library(rgl)
library(plotly)
library(ggplot2)

load("C:/Main/Machine_Learning_Hobby/Home_Edition/Student_Grades/Clustering/cSub.RData")

#3D Scatterplot
plot_ly(cSub, x = ~pre_cGPA, y = ~prereqSemBtwn, z = ~prereqValue, main = 'title') %>% add_markers(text = ~paste("Past Bad Grades", pastBadGrades))

#---------------------------------------------------------------------
#-----------------------------TSNE with translink---------------------
#---------------------------------------------------------------------

rm(list = ls())
setwd("C:/Main/AQM/Assignments/Winter_Break_Challenge")

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
load("C:/Main/AQM/Assignments/Winter_Break_Challenge/tsne30.Rds")

library("dbscan")

db <- dbscan(tsne30$Y, eps = 2.22, minPts = 10) # eps = 2.3, minPts = 10
tsnelinked <- tsne30$Y
tsnelinked <- data.frame(tsnelinked)
tsnelinked$cluster <- db$cluster

table(tsnelinked$cluster)

plot(tsnelinked$X1,tsnelinked$X2, col = tsnelinked$cluster)

## Try this again using Rtsne for 3D output --------------------------------------------

rm(list = ls())
setwd("C:/Main/AQM/Assignments/Winter_Break_Challenge")
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

