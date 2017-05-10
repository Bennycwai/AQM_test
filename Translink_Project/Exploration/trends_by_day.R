# try to visualize the translink data in terms of days for number of people (ridership measured by on's variable)
rm(list = ls())
setwd("C:/Main/AQM/Assignments/Winter_Break_Challenge")

# translink set
transdf <- readRDS("trips_2011.Rds")

# let us start by only look at the 84 and 99 bus lines
transdf.84 <- transdf[as.character(transdf$Line) == "084",]
transdf.99 <- transdf[as.character(transdf$Line) == "099",]

# space is limited, let's delete what we don't need for now
rm(transdf)

# get rid of variables I don't need as my computer memory is very limited
transdf.84 <- transdf.84[,c("OperationDate","DayType","VehicleNo","OnsLoadCompensated")]
transdf.99 <- transdf.99[,c("OperationDate","DayType","VehicleNo","OnsLoadCompensated")]

# get rid of incomplete rows
transdf.84 <- transdf.84[complete.cases(transdf.84),]
transdf.99 <- transdf.99[complete.cases(transdf.99),]
## for the record, no changes

# lets save 'em
#saveRDS(transdf.84, file = "transdf.84.Rds")
#saveRDS(transdf.99, file = "transdf.99.Rds")


### exploritory and tables------------------------------------------------
require(dplyr)
require(plyr)
require(ggplot2)

transdf.84 <- readRDS("transdf.84.Rds")
transdf.99 <- readRDS("transdf.99.Rds")

# input your bus line of interest here
mydf <- transdf.84

# take a look at the number of days
#tableDates <- table(mydf$OperationDate)
## goes from 2nd of September 2011 to 30th of December 2011

# number of unique dates
#dim(tableDates)
## 94 for line 084, 113 for 099 (118 is complete number of days)

# get sums of ons compensated by date
onsGrouped <- ddply(mydf,~OperationDate,summarise,total_Ons=sum(OnsLoadCompensated))

# now merge it with the day types
dayLabels <- mydf[,c("OperationDate","DayType")]
dayLabels <- unique(dayLabels)
onsGrouped <- merge(onsGrouped, dayLabels, by = "OperationDate")
rm(dayLabels)

### plots-----------------------------------------
require(dplyr)
require(plyr)
require(ggplot2)

transdf.84 <- readRDS("transdf.84.Rds")
transdf.99 <- readRDS("transdf.99.Rds")

# input your bus line of interest here
mydf <- transdf.99

# plot by month to keep things nice

mydf$month <- factor(format(mydf$OperationDate, "%B"), levels = month.name)
mydf$day <- factor(format(mydf$OperationDate, "%d"))

# for a particular month
mydf.sub <- mydf[mydf$month == "September" ,]

# lets plot total number of "ons" by dates, colored by day type
qplot(as.factor(day), data=mydf.sub, geom="bar", fill=DayType, main = "Total passengers 'ons' per day in September for bus line 099", ylab = "Total 'ons'", xlab = "Day in the month")



## Only look at november ---------------------------------------------------
require(dplyr)
require(plyr)
require(ggplot2)
rm(list = ls())
setwd("C:/Main/AQM/Assignments/Winter_Break_Challenge")

# translink set
# plot by month to keep things nice

transdf$month <- factor(format(transdf$OperationDate, "%B"), levels = month.name)
transdf$day <- factor(format(transdf$OperationDate, "%d"))

# for a particular month
novdf <- transdf[transdf$month == "November" ,]

novdf <- novdf[complete.cases(novdf),]

#saveRDS(novdf, file = "novdf.Rds")

# filter a new subset only based on bay
novdf.STN <- filter(novdf, grepl('STN', StopName))

novdf.else <- filter(novdf, !grepl('STN', StopName))


#get rid of columns we don't need
novSub.STN <- novdf.STN[,c("Line","Hour","StopName","Conditions","OnsLoadCompensated")]

novSub.else <- novdf.else[,c("Line","Hour","StopName","Conditions","OnsLoadCompensated")]


# working with STNs only
# get sums of ons compensated by date
onsGrouped.STN <- ddply(novSub.STN,~Hour,summarise,total_Ons=sum(OnsLoadCompensated))

# now merge it with the day types
dayLabels <- novdf[,c("Hour","DayType")]
dayLabels <- unique(dayLabels)
onsGrouped.STN <- merge(onsGrouped.STN, dayLabels, by = "Hour")
colnames(onsGrouped.STN) <- c("Hour","total_Ons_STN","DayType")

# working with elses only
# get sums of ons compensated by date
onsGrouped.else <- ddply(novSub.else,~Hour,summarise,total_Ons=sum(OnsLoadCompensated))
colnames(onsGrouped.else) <- c("Hour","total_Ons_else")


onsGrouped.merged <- merge(onsGrouped.STN, onsGrouped.else, by = "Hour")

## seperate this into two groups
#onsMerged.Down <- onsGrouped.merged[onsGrouped.merged$Hour == "2011-11-12 05:00:00" | onsGrouped.merged$Hour == "2011-11-12 06:00:00" | onsGrouped.merged$Hour == "2011-11-12 07:00:00" | onsGrouped.merged$Hour == "2011-11-12 08:00:00" | onsGrouped.merged$Hour == "2011-11-12 09:00:00" | onsGrouped.merged$Hour == "2011-11-12 10:00:00" | onsGrouped.merged$Hour == "2011-11-12 11:00:00" | onsGrouped.merged$Hour == "2011-11-08 11:00:00" | onsGrouped.merged$Hour == "2011-11-08 12:00:00" | onsGrouped.merged$Hour == "2011-11-08 13:00:00" | onsGrouped.merged$Hour == "2011-11-08 14:00:00",]
onsMerged.Down <- onsGrouped.merged[onsGrouped.merged$Hour == "2011-11-12 05:00:00" | onsGrouped.merged$Hour == "2011-11-12 06:00:00" | onsGrouped.merged$Hour == "2011-11-12 07:00:00" | onsGrouped.merged$Hour == "2011-11-12 08:00:00" | onsGrouped.merged$Hour == "2011-11-12 09:00:00" | onsGrouped.merged$Hour == "2011-11-12 10:00:00" | onsGrouped.merged$Hour == "2011-11-12 11:00:00",]

#onsMerged.Up <- subset(onsGrouped.merged, !(Hour == "2011-11-12 05:00:00" | Hour == "2011-11-12 06:00:00" | Hour == "2011-11-12 07:00:00" | Hour == "2011-11-12 08:00:00" | Hour == "2011-11-12 09:00:00" | Hour == "2011-11-12 10:00:00" | Hour == "2011-11-12 11:00:00" | Hour == "2011-11-08 11:00:00" | Hour == "2011-11-08 12:00:00" | Hour == "2011-11-08 13:00:00" | Hour == "2011-11-08 14:00:00"))

onsMerged.Up <- subset(onsGrouped.merged, !(Hour == "2011-11-12 05:00:00" | Hour == "2011-11-12 06:00:00" | Hour == "2011-11-12 07:00:00" | Hour == "2011-11-12 08:00:00" | Hour == "2011-11-12 09:00:00" | Hour == "2011-11-12 10:00:00" | Hour == "2011-11-12 11:00:00"))

## calcualate the ratio

onsMerged.Down$Ratio <- onsMerged.Down$total_Ons_STN/onsMerged.Down$total_Ons_else
onsMerged.Up$Ratio <- onsMerged.Up$total_Ons_STN/onsMerged.Up$total_Ons_else

mean(onsMerged.Down$Ratio)
mean(onsMerged.Up$Ratio)

mean(onsMerged.Down$total_Ons_STN)
mean(onsMerged.Up$total_Ons_STN)

mean(onsMerged.Down$total_Ons_else)
mean(onsMerged.Up$total_Ons_else)
## plots
# lets plot total number of "ons" by dates, colored by day type
qplot(as.factor(Hour), data= onsMerged.Down, geom="bar", fill=DayType, main = "Total passengers 'ons' per day in September for bus line 099", ylab = "Total 'ons'", xlab = "Day in the month")

qplot(as.factor(Hour), data= onsMerged.Up, geom="bar", fill=DayType, main = "Total passengers 'ons' per day in September for bus line 099", ylab = "Total 'ons'", xlab = "Day in the month")
