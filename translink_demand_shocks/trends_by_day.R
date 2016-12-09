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
