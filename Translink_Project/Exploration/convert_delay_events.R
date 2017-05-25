## converts trips_2011 data to delay event dataframe

library(magrittr)
library(dplyr)

rm(list = ls())
setwd("C:/Main/AQM/Translink_Project/Exploration")

## read in apc_2011 file [[WARNING: LARGE]]--------------

tripsdf <- readRDS("C:/Main/AQM/Translink_Project/Exploration/trips_2011.Rds")

# take complete cases only
tripsdf_complete <- tripsdf[complete.cases(tripsdf),]
#saveRDS(tripsdf_complete, file = "tripsdf_complete.Rds")

# extracting only for 1 line (145)
trips_145 <- tripsdf_complete[tripsdf_complete$Line == "145",]

#saveRDS(trips_145, file = "trips_145.Rds")


##load line 145 data ----------------------------------
library(magrittr)
library(dplyr)

rm(list = ls())
setwd("C:/Main/AQM/Translink_Project/Exploration")

trips_145 <- readRDS("C:/Main/AQM/Translink_Project/Exploration/trips_145.rds")

trips_145_arriveDelays <- trips_145 %>%
  filter(ArriveDelay >= 2.00) %>%
  arrange(OperationDate, Trip, TripLeg) %>%
  mutate(delayID = cumsum(c(0,diff(TripLeg)) != 1))

trips_145_arriveDelays$minutes <- as.numeric(substr(trips_145_arriveDelays$ActualArriveTime,12,13))*60+as.numeric(substr(trips_145_arriveDelays$ActualArriveTime,15,16))

trips_145_summarised <- trips_145_arriveDelays %>%
  group_by(OperationDate, Trip, delayID) %>%
  summarise(DayType=first(DayType), Pattern=first(Pattern), Line=first(Line), first_Arrival = first(minutes), avg_ArriveLoad = mean(ArriveLoadCompensated), std_ArriveLoad = sd(ArriveLoadCompensated), avg_Temp = mean(Temp), avg_Humidity = mean(Humidity), avg_Visibility = mean(Visibility), avg_WindSpeed = mean(Wind.Speed), avg_TravelTime=mean(TravelTime),onset_time = min(ActualArriveTime), end_time = max(ActualArriveTime), duration = max(ActualArriveTime)-min(ActualArriveTime), max_delay = max(ArriveDelay), mean_delay = mean(ArriveDelay), std_dev_delay = sd(ArriveDelay), n_stops = n(), start_lat=first(DestinationLat), start_long=first(DestinationLong),end_lat=last(DestinationLat),end_long=last(DestinationLong)) %>%
  mutate(std_dev_delay = ifelse(is.na(std_dev_delay),0,std_dev_delay), std_ArriveLoad = ifelse(is.na(std_ArriveLoad),0,std_ArriveLoad))

write.csv(trips_145_summarised, "line_145_ArriveDelays.csv")



##alternate data format which separates start and stop-------------------
trips_41_summarised_start <- trips_41_arriveDelays %>%
  group_by(OperationDate, Trip, delayID) %>%
  summarise(StartEnd = 'Start', DayType=first(DayType), Pattern=first(Pattern), Line=first(Line), avg_ArriveLoad = mean(ArriveLoadCompensated), avg_Temp = mean(Temp), avg_Humidity = mean(Humidity), avg_Visibility = mean(Visibility), avg_WindSpeed = mean(Wind.Speed), avg_TravelTime=mean(TravelTime),time = min(ActualArriveTime), duration = max(ActualArriveTime)-min(ActualArriveTime), max_delay = max(ArriveDelay), mean_delay = mean(ArriveDelay), std_dev_delay = sd(ArriveDelay), n_stops = n(), lat=first(DestinationLat), long=first(DestinationLong)) %>%
  na.omit()

trips_41_summarised_end <- trips_41_arriveDelays %>%
  group_by(OperationDate, Trip, delayID) %>%
  summarise(StartEnd = 'End', DayType=first(DayType), Pattern=first(Pattern), Line=first(Line), avg_ArriveLoad = mean(ArriveLoadCompensated), avg_Temp = mean(Temp), avg_Humidity = mean(Humidity), avg_Visibility = mean(Visibility), avg_WindSpeed = mean(Wind.Speed), avg_TravelTime=mean(TravelTime), time = max(ActualArriveTime), duration = max(ActualArriveTime)-min(ActualArriveTime), max_delay = max(ArriveDelay), mean_delay = mean(ArriveDelay), std_dev_delay = sd(ArriveDelay), n_stops = n(), lat=last(DestinationLat), long=last(DestinationLong)) %>%
  na.omit()

write.csv(trips_41_summarised_start, "line_41_ArriveDelays_start.csv")
write.csv(trips_41_summarised_end, "line_41_ArriveDelays_end.csv")