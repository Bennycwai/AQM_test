## See if cluster on gdp data would be a good way to seperate countries by life expectancy--------
# prepare a data frame of all countries
setwd("C:/Main/AQM/Assignments/Challenge1")

require(plyr)
require(dplyr)
require(gapminder)
require(ggplot2)

# create gdpPercap change variable
deltadf <- gapminder %>%
  mutate(change = 100*((gdpPercap - lag(gdpPercap)))/gdpPercap) %>%
  filter(year > 1952)
deltadf <- deltadf %>% mutate_if(is.factor, as.character)
uContries <- unique(deltadf$country)

# do simple linear regression
simpLM <- lm(gdpPercap~lifeExp, data = gapminder)
simpLM

# calculating intercept and slope of life expectancy
modsLE = dlply(deltadf, .(country), lm, formula = lifeExp ~ year)
coefsLE = ldply(modsLE, coef)
coefsLE

# calculating intercept and slope of gdpPercap
modsGDP = dlply(deltadf, .(country), lm, formula = gdpPercap ~ year)
coefsGDP = ldply(modsGDP, coef)
coefsGDP

avg.Variables <- ddply(deltadf, .(country), summarise, ave.LE = mean(lifeExp), ave.gdp = mean(gdpPercap), ave.delta = mean(change), ave.pop = mean(pop),max.pop = max(pop), max.lifeExp = max(lifeExp))
avg.Variables

# create data frame of all cluster variables and data for analysis after clustering (measurements)
preDf<- merge(coefsLE, coefsGDP, by = "country")
preDf<- merge(preDf, avg.Variables, by = "country")
colnames(preDf)<- c("country","LE_intercept","LE_slope","gdpPercap_intercept","gdpPercap_slope","avg.LE","avg.gdpPercap","avg.change.gdp","avg.pop","final.pop","final_LE")
preDf <- preDf[,c(1,4,5,7,2,3,6,11,8,9,10)]

# create data frame of clustering variables only
clusterVar <- preDf[,3:4]
# normalize the columns
clusterVar[,1] <- clusterVar[,1]/ (max(clusterVar[,1])-min(clusterVar[,1]))
clusterVar[,2] <- clusterVar[,2]/ (max(clusterVar[,2])-min(clusterVar[,2]))

# save(clusterVar, file = "clusterVar.RData")
# save(preDf, file = "preDf.RData")

# run hierarchical clustering method to determine the number of cluster k-means requires ------------------
# norm as euclidean
setwd("C:/Main/AQM/Assignments/Challenge1")

load("clusterVar.RData")
load("preDf.RData")
###

preDist <- dist(clusterVar)
hcluster<- hclust(preDist, method="ward.D2")

plot(hcluster, xlab = "countries (4 branches)",main = "Hierarchical clustering to identify # of clusters")
# add rectangles the 4 clusters 
rect.hclust(hcluster, k=4, border="blue")

# save(clusterVar, file = "clusterVar.RData")
# save(deltadf, file = "deltadf.RData")

# run k-means clustering with the chosen 4 clusters-------------------
setwd("C:/Main/AQM/Assignments/Challenge1")

load("clusterVar.RData")
load("preDf.RData")

###

kcluster<- kmeans(clusterVar,4)
str(kcluster)
plot(kcluster$cluster)

# combine the clustered information together with our previous information
kclusterDf <- data.frame(preDf, kcluster$cluster)
colnames(kclusterDf)[12]<- c("cluster")

# get summary information of all 4 clusters
kcsummary <- ddply(kclusterDf, .(cluster), summarise, N=length(cluster), lifeExp.Slope = mean(LE_slope), lifeExp.avg = mean(avg.LE),lifeExp.final = mean(final_LE),gdpPerCap.Slope = mean(gdpPercap_slope), gdpPerCap.avg = mean(avg.gdpPercap), pop.avg = mean(avg.pop), pop.final = mean(final.pop))
kcsummary

# save(kcsummary, file = "kcsummary.RData")
# save(kclusterDf, file = "kclusterDf.RData")

# dataset for clustering

plotDf <- merge(deltadf,kclusterDf[,c(1,12)])
colnames(plotDf)[8]<- c("cluster")

# save(plotDf, file = "plotDf.RData")

# plots for cluster analysis------------------
load("plotDf.RData")
load("kclusterDf.RData")
load("kcsummary.RData")
require(gridExtra)

setwd("C:/Main/AQM/Assignments/Challenge1")
require(ggplot2)

# graph gdpPercap~year again grouped by continent
qplot(year, gdpPercap, data = deltadf, color = continent,geom = c("point","smooth"))
# Kuwait is undoubtly an outlier in the data set

# graph gdpPercap~year again grouped by cluster
qplot(year, gdpPercap, data = plotDf, color = as.factor(cluster),geom = c("point","smooth"), main = "GDP per capita vs year by clusters", ylab = "GDP per capita", xlab = "year of occurance")
# looks pretty standard as the gdpPercap was one of the variable used to cluster

# graph lifeExp vs year by cluster
qplot(year,lifeExp,data = plotDf, color = as.factor(cluster),geom = c("point","smooth"), main = "Life expectancy vs year by clusters", ylab = "Life expectancy (years)",xlab = "year of occurance")

# graph boxplot by clusters - average life expectancy
qplot(as.factor(cluster),avg.LE,data = kclusterDf,geom = "boxplot",color = as.factor(cluster), main = "box plot of average life expectancy by clusters", xlab = "clusters", ylab = "average life expectancy")

# graph boxplot by year and cluster - average life expectancy
qplot(as.factor(year),lifeExp,data = plotDf,color = as.factor(cluster),geom = "boxplot", main = "box plot of life expectancy by year and clusters", xlab = "year of occurance", ylab = "life expectancy")

#histogram
plot3<-qplot(as.factor(cluster), data = kcsummary, geom = "bar", weight = lifeExp.avg,fill = as.factor(cluster), ylab = "", xlab = "clusters", main = "average life expectancy")
plot4<-qplot(as.factor(cluster), data = kcsummary, geom = "bar", weight = pop.avg,fill = as.factor(cluster), ylab = "", xlab = "clusters", main = "population average")
plot2<-qplot(as.factor(cluster), data = kcsummary, geom = "bar", weight = gdpPerCap.Slope,fill = as.factor(cluster), ylab = "", xlab = "clusters", main = "change (slope) of GDP per capita")
plot1<-qplot(as.factor(cluster), data = kcsummary, geom = "bar", weight = gdpPerCap.avg,fill = as.factor(cluster), ylab = "", xlab = "clusters", main = "average GDP per capita")
grid.arrange(plot1, plot2,plot3,plot4, ncol=2, nrow =2)