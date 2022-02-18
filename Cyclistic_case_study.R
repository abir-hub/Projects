### Opening the library
library(tidyverse)
library(janitor)
library(here)
library(lubridate)
library(skimr)
library(dplyr)
library(modeest)
library(DescTools)
###
setwd("C:/Users/HP/Desktop/Notes/DATA ANALYST/Project/Bike-ride/Bike-ride") # Set working directory
getwd() # displays your working directory
##
##Uploading the csv files for 12 months, from October to September
Oct_2020_tripdata <- read_csv("202010-divvy-tripdata.csv")
Nov_2020_tripdata <- read_csv("202011-divvy-tripdata.csv")
Dec_2020_tripdata <- read_csv("202012-divvy-tripdata.csv")
Jan_2021_tripdata <- read_csv("202101-divvy-tripdata.csv")
Feb_2021_tripdata <- read_csv("202102-divvy-tripdata.csv")
Mar_2021_tripdata <- read_csv("202103-divvy-tripdata.csv")
Apr_2021_tripdata <- read_csv("202104-divvy-tripdata.csv")
May_2021_tripdata <- read_csv("202105-divvy-tripdata.csv")
Jun_2021_tripdata <- read_csv("202106-divvy-tripdata.csv")
Jul_2021_tripdata <- read_csv("202107-divvy-tripdata.csv")
Aug_2021_tripdata <- read_csv("202108-divvy-tripdata.csv")
Sep_2021_tripdata <- read_csv("202109-divvy-tripdata.csv")
##
##PREPARING THE DATA
##The file follows ROCCC analysis
##Data wrangling
str(Oct_2020_tripdata)
str(Nov_2020_tripdata)
str(Dec_2020_tripdata)
str(Jan_2021_tripdata)
str(Feb_2021_tripdata)
str(Mar_2021_tripdata)
str(Apr_2021_tripdata)
str(May_2021_tripdata)
str(Jun_2021_tripdata)
str(Jul_2021_tripdata)
str(Aug_2021_tripdata)
str(Sep_2021_tripdata)
###
###
Oct_2020_tripdata <-  mutate(Oct_2020_tripdata, start_station_id = as.character(start_station_id)
                             ,end_station_id = as.character(end_station_id)
                             ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
                             ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))
Nov_2020_tripdata <-  mutate(Nov_2020_tripdata, start_station_id = as.character(start_station_id)
                             ,end_station_id = as.character(end_station_id)
                             ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
                             ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))
Dec_2020_tripdata <-  mutate(Dec_2020_tripdata, start_station_id = as.character(start_station_id)
                             ,end_station_id = as.character(end_station_id)
                             ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
                             ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))
Jan_2021_tripdata <-  mutate(Jan_2021_tripdata, start_station_id = as.character(start_station_id)
                             ,end_station_id = as.character(end_station_id)
                             ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
                             ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))
Feb_2021_tripdata <-  mutate(Feb_2021_tripdata, start_station_id = as.character(start_station_id)
                             ,end_station_id = as.character(end_station_id)
                             ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
                             ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))
Mar_2021_tripdata <-  mutate(Mar_2021_tripdata, start_station_id = as.character(start_station_id)
                             ,end_station_id = as.character(end_station_id)
                             ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
                             ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))
Apr_2021_tripdata <-  mutate(Apr_2021_tripdata, start_station_id = as.character(start_station_id)
                             ,end_station_id = as.character(end_station_id)
                             ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
                             ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))
May_2021_tripdata <-  mutate(May_2021_tripdata, start_station_id = as.character(start_station_id)
                             ,end_station_id = as.character(end_station_id)
                             ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
                             ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))
Jun_2021_tripdata <-  mutate(Jun_2021_tripdata, start_station_id = as.character(start_station_id)
                             ,end_station_id = as.character(end_station_id)
                             ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
                             ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))
Jul_2021_tripdata <-  mutate(Jul_2021_tripdata, start_station_id = as.character(start_station_id)
                             ,end_station_id = as.character(end_station_id)
                             ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
                             ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))
Aug_2021_tripdata <-  mutate(Aug_2021_tripdata, start_station_id = as.character(start_station_id)
                             ,end_station_id = as.character(end_station_id)
                             ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
                             ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))
Sep_2021_tripdata <-  mutate(Sep_2021_tripdata, start_station_id = as.character(start_station_id)
                              ,end_station_id = as.character(end_station_id)
                              ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
                              ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))
###
###Binding all data frames
all_trips <- bind_rows(Oct_2020_tripdata, Nov_2020_tripdata, Dec_2020_tripdata, 
                       Jan_2021_tripdata, Feb_2021_tripdata, Mar_2021_tripdata,
                       Apr_2021_tripdata, May_2021_tripdata, Jun_2021_tripdata,
                       Jul_2021_tripdata, Aug_2021_tripdata, Sep_2021_tripdata)
###
###Remove lat, long
###We use select(-data_frame/column_name) for removing
all_trips <- all_trips %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng))
##
##DATA CLEANING
##
colnames(all_trips)  #List of column names
dim(all_trips)  #Dimensions of the data frame
#the first number show the nrow and second show the ncol of data frame.
#5136261      13
head(all_trips)
#
summary(all_trips)  #Statistical summary of data. Mainly for numerics
#
glimpse(all_trips)
#Checking the Null value. If there is.
colSums(is.na(all_trips))
#Listwise deletion
all_trips_cleaned <- all_trips[complete.cases(all_trips), ]#removing the Null containing row
colSums(is.na(all_trips_cleaned))
##
##Filtering the data for started_at value having lesser than ended at
all_trips_cleaned <- all_trips_cleaned %>% 
  filter(all_trips_cleaned$started_at < all_trips_cleaned$ended_at)
#Or you can write
#all_trips_cleaned <- filter(all_trips_cleaned, all_trips_cleaned$started_at < all_trips_cleaned$ended_at) 
##
##New columns to list the date, month, day, and year of each ride
all_trips_cleaned$date <- as.Date(all_trips_cleaned$started_at, format= "%m/%d/%Y %H:%M")
all_trips_cleaned$month <- format(as.Date(all_trips_cleaned$date), "%m")
all_trips_cleaned$day <- format(as.Date(all_trips_cleaned$date), "%d")
all_trips_cleaned$year <- format(as.Date(all_trips_cleaned$date), "%Y")
all_trips_cleaned$day_of_week <- format(as.Date(all_trips_cleaned$date), "%A")
#Display some elements of the new table to see if everything is as expected
glimpse(all_trips_cleaned)
#Adding new column to calculate each ride length in mins
all_trips_cleaned$ride_length <- difftime(all_trips_cleaned$ended_at, all_trips_cleaned$started_at)
##Inspecting the structure of the columns
is.factor(all_trips_cleaned$ride_length)
##Convert "ride_length" from Factor to numeric so we can run calculations on the data
all_trips_cleaned$ride_length <- as.numeric(as.character(all_trips_cleaned$ride_length))
is.numeric(all_trips_cleaned$ride_length)
#
all_trips_v2 <- all_trips_cleaned[!(all_trips_cleaned$start_station_name == "HQ QR" | all_trips_cleaned$ride_length<0),]
###
###Data Analysis
###
summary(all_trips_v2$ride_length)#the time is in second
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1     433     762    1359    1379 3356649 
##Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
#1                     casual                2012.3765
2                     member                 821.9262
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
#1                     casual                     1033
2                     member                      606
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
#1                     casual                  3356649
2                     member                   573467
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)
#1                     casual                        1
#2                     member                        1
##
##See the average ride time by each day for members vs casual users
# Arranging the days of the week accordingly
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
# all_trips_v2$member_casual all_trips_v2$day_of_week all_trips_v2$ride_length
# 1                      casual                   Sunday                2322.5672
# 2                      member                   Sunday                 936.1720
# 3                      casual                   Monday                1992.7622
# 4                      member                   Monday                 787.7697
# 5                      casual                  Tuesday                1802.6559
# 6                      member                  Tuesday                 775.0020
# 7                      casual                Wednesday                1752.4906
# 8                      member                Wednesday                 777.9496
# 9                      casual                 Thursday                1723.6577
# 10                     member                 Thursday                 771.2929
# 11                     casual                   Friday                1931.6675
# 12                     member                   Friday                 808.9091
# 13                     casual                 Saturday                2167.0959
# 14                     member                 Saturday                 917.5835
#ctrl + shift + C for multi line comment
#Analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()                            #calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>%         # calculates the average duration
  arrange(member_casual, weekday)                                # sorts
# A tibble: 14 x 4
# Groups:   member_casual [2]
# member_casual weekday number_of_rides average_duration
# <chr>         <ord>             <int>            <dbl>
#   1 casual        Sun              381824            2323.
# 2 casual        Mon              219104            1993.
# 3 casual        Tue              203610            1803.
# 4 casual        Wed              208587            1752.
# 5 casual        Thu              222581            1724.
# 6 casual        Fri              278722            1932.
# 7 casual        Sat              449381            2167.
# 8 member        Sun              295641             936.
# 9 member        Mon              324047             788.
# 10 member        Tue              351332             775.
# 11 member        Wed              366051             778.
# 12 member        Thu              362053             771.
# 13 member        Fri              345847             809.
# 14 member        Sat              341952             918.
###
#Visual for number of rides grouped by rider type
###
all_trips_v2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(), .groups = 'drop') %>% 
  #arrange(member_casual, day_of_week) %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + scale_y_continuous(labels = scales::comma) +
  labs(x = "Day of Week", y = "Number of Rides", fill = "Member/Casual",
       title = "Average Number of Rides by Day: Members vs. Casual Riders")
##
##Visual for average duration
##
all_trips_v2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(average_duration = mean(ride_length), .groups = 'drop') %>% 
  #arrange(member_casual, day_of_week) %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(x = "Day of Week", y = "Average Duration (min)", 
       fill = "Member/Casual",
       title = "Average Riding Duration by Day: Members vs. Casual Riders")
##
##Average Number of Rides by Month: Casual Riders
##
all_trips_v2 %>% 
  group_by(month, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  filter(member_casual == 'casual') %>%
  drop_na() %>%
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) + 
  geom_bar(position = 'dodge', stat = 'identity') + scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45)) + 
  labs(x = "Month", y = "Number of Rides", 
       fill = "Member/Casual",
       title = "Average Number of Rides by Month: Casual Riders")
##
##MORE DESCRIPTIVE DATA ANALYSIS
##
# Combine start and end stations
# Removing entries with no station name
# Separate the data frame by rider type
all_stations <- bind_rows(data.frame("stations" = all_trips_v2$start_station_name, 
                                     "member_casual" = all_trips_v2$member_casual),
                          data.frame("stations" = all_trips_v2$end_station_name,
                                     "member_casual" = all_trips_v2$member_casual))
all_stations_v2 <- all_stations[!(all_stations$stations == "" | is.na(all_stations$stations)),]
all_stations_member <- all_stations_v2[all_stations_v2$member_casual == 'member',]
all_stations_casual <- all_stations_v2[all_stations_v2$member_casual == 'casual',]

# Get the top 10 popular stations all, members, and casual riders
top_10_station <- all_stations_v2 %>% 
  group_by(stations) %>% 
  summarise(station_count = n()) %>% 
  arrange(desc(station_count)) %>% 
  slice(1:10)

top_10_station_member <- all_stations_member %>% 
  group_by(stations) %>% 
  summarise(station_count = n()) %>% 
  arrange(desc(station_count)) %>% 
  head(n=10)

top_10_station_casual <- all_stations_casual %>% 
  group_by(stations) %>% 
  summarise(station_count = n()) %>% 
  arrange(desc(station_count)) %>% 
  head(n=10)

# Comparing general bike type preference between members and casual riders
all_trips_v2 %>% 
  group_by(rideable_type, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop')
# 1 classic_bike  casual                1116253
# 2 classic_bike  member                1626931
# 3 docked_bike   casual                 406805
# 4 docked_bike   member                 267339
# 5 electric_bike casual                 440751
# 6 electric_bike member                 492653
# average number of rides by hour (casual riders)
all_trips_v2$started_at_hour <- as.POSIXct(all_trips_v2$started_at, "%Y-%m-%d %H:%M")      
str(all_trips_v2)
##
##
ggplot(data = top_10_station_member) +
  geom_col(aes(x = reorder(stations, station_count), y = station_count), fill = "thistle") +
  labs(title = "Top 10 Used Stations by Members", y = "Number of Rides", x = "") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  theme_minimal()
##
##
ggplot(data = top_10_station_casual) +
  geom_col(aes(x = reorder(stations, station_count), y = station_count), fill = "lightsalmon") +
  labs(title = "Top 10 Used Stations by Casual Riders", x = "", y = "Number of Rides") + 
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  theme_minimal() 
##
##Visual for Average Number of Rides by Hour (Member riders)
all_trips_v2 %>%
  filter(member_casual == 'member') %>%
  group_by(hour_of_day = hour(round_date(started_at_hour, 'hour'))) %>% 
  group_by(hour_of_day, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  arrange(-number_of_rides) %>% 
  ggplot(aes(x = hour_of_day, y = number_of_rides, fill = member_casual)) +
  geom_bar(position = 'dodge', stat = 'identity') + scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)) +
  labs(x = "Time of the Day (h)", y = "Number of Rides", 
       fill = "Member/Casual",
       title = "Average Number of Rides by Hour: Member Riders") 
##
##Visual for average number of rides by hour (casual riders)
##
options(repr.plot.width = 12, repr.plot.height = 8)
all_trips_v2 %>%
  filter(member_casual == 'casual') %>%
  group_by(hour_of_day = hour(round_date(started_at_hour, 'hour'))) %>% 
  group_by(hour_of_day, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  arrange(-number_of_rides) %>% 
  ggplot(aes(x = hour_of_day, y = number_of_rides, fill = member_casual)) +
  geom_bar(position = 'dodge', stat = 'identity') + scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)) +
  labs(x = "Time of the Day (h)", y = "Number of Rides", 
       fill = "Member/Casual",
       title = "Average Number of Rides by Hour: Casual Riders") 
##
##
##
options(repr.plot.width = 14, repr.plot.height = 10)
all_trips_v2 %>% 
  group_by(month, member_casual, rideable_type) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  drop_na() %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  facet_grid(member_casual~rideable_type) +
  labs(x = "Month", y = "Number of Rides", fill = "Member/Casual",
       title = "Average Number of Rides by Month") +
  theme(axis.text.x = element_text(angle = 90))







