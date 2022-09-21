# Google-Data-Analytics-Capstone-Bike-Share
Cyclistic Bike Sharing Analysis
# This R environment comes with many helpful analytics packages installed
# It is defined by the kaggle/rstats Docker image: https://github.com/kaggle/docker-rstats

# Loading the libraaries required for installing, Cleansing and Visualization
library(tidyverse) # metapackage of all tidyverse packages
library(dplyr)
library(geosphere)
library(lubridate)
library(ggplot2)

# Uploading and Saving Dataset
aug <- read_csv("../input/august/202208-divvy-tripdata.csv")
jul <- read_csv("../input/july22/202207-divvy-tripdata.csv")
jun <- read_csv("../input/june22/202206-divvy-tripdata.csv")
may <- read_csv("../input/may2022/202205-divvy-tripdata.csv")
mar <- read_csv("../input/march22/202203-divvy-tripdata.csv")
feb <- read_csv("../input/feb2022/202202-divvy-tripdata.csv")
jan <- read_csv("../input/jan2022/202201-divvy-tripdata.csv")
dec <- read_csv("../input/dec2021/202112-divvy-tripdata.csv")
nov <- read_csv("../input/nov2021/202111-divvy-tripdata.csv")
oct <- read_csv("../input/oct2022/202110-divvy-tripdata.csv")
sep <- read_csv("../input/sep2022/202109-divvy-tripdata.csv")

# Converting and unifying datatype
aug <-  mutate(aug, start_station_id = as.character(start_station_id)
               ,end_station_id = as.character(end_station_id)
               ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
               ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))


jul <-  mutate(jul, start_station_id = as.character(start_station_id)
               ,end_station_id = as.character(end_station_id)
               ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
               ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))

jun <-  mutate(jun, start_station_id = as.character(start_station_id)
               ,end_station_id = as.character(end_station_id)
               ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
               ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))

may <-  mutate(may, start_station_id = as.character(start_station_id)
               ,end_station_id = as.character(end_station_id)
               ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
               ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))

mar <-  mutate(mar, start_station_id = as.character(start_station_id)
               ,end_station_id = as.character(end_station_id)
               ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
               ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))

feb <-  mutate(feb, start_station_id = as.character(start_station_id)
               ,end_station_id = as.character(end_station_id)
               ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
               ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))

jan <-  mutate(jan, start_station_id = as.character(start_station_id)
               ,end_station_id = as.character(end_station_id)
               ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
               ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))

dec <-  mutate(dec, start_station_id = as.character(start_station_id)
               ,end_station_id = as.character(end_station_id)
               ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
               ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))

nov <-  mutate(nov, start_station_id = as.character(start_station_id)
               ,end_station_id = as.character(end_station_id)
               ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
               ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))

oct <-  mutate(oct, start_station_id = as.character(start_station_id)
               ,end_station_id = as.character(end_station_id)
               ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
               ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))

sep <-  mutate(sep, start_station_id = as.character(start_station_id)
               ,end_station_id = as.character(end_station_id)
               ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
               ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))

apr <-  mutate(apr, start_station_id = as.character(start_station_id)
               ,end_station_id = as.character(end_station_id)
               ,started_at= as.POSIXct(started_at, format= "%m/%d/%Y %H:%M")
               ,ended_at= as.POSIXct(ended_at, format= "%m/%d/%Y %H:%M"))

# Adding all the files to merge
bikedata <- bind_rows(aug,jul,jun,may,mar,feb,jan,dec,nov)


## Clean dataset by removing unnessesary colons
ridedata <- ridedata %>% select(-c(start_station_id, end_station_id, start_lat, start_lng, end_lat, end_lng))


# inspecting the new file
colnames(bikedata)
str(bikedata)
head(ridedata)


# Adding all the files to merge
bikedata1 <- bind_rows(aug,jul,jun,may,apr,mar,feb,jan,dec,nov)


## Clean dataset by removing unnessesary colons
bikedata2 <- bikedata1 %>% select(-c(start_station_id, end_station_id, start_lat, start_lng, end_lat, end_lng))


# inspecting the new file
colnames(bikedata2)
str(bikedata2)
head(bikedata2)


# Add date, month, day and trip length columns all at the same time
bikedata2$date <- as.Date(bikedata2$started_at)
bikedata2$month <- format(as.Date(bikedata2$date), "%B")
bikedata2$day <- format(as.Date(bikedata2$date), "%A")
bikedata2$trip_length <- difftime(bikedata2$ended_at, bikedata2$started_at)

# remove empty rows, dataset trimmed down from3489748 rows to 3294691
colSums(is.na(bikedata2)) #returns a summary of empty colons on each variable
cleanbikedata2 <- bikedata2 [complete.cases(bikedata2), ]


# Remove all negative and zero trip length as well as NA/null values
cleanbikedata2 <- subset(cleanbikedata2, trip_length > 0)

# Convert ride length to integer
cleanbikedata2$trip_length <- as.integer(cleanbikedata2$trip_length)

#DESCRIPTIVE ANALYSIS
# Aggregating Data to determine average trip length of riders for month of the year
aggregate(trip_length ~ member_casual + month, cleanbikedata2, mean)

# Aggregate data to determine average trip length of riders on each day of the week
aggregate(trip_length ~ day + member_casual, cleanbikedata2, mean)


# Aggregate data to determine how riders use the different bike types available
aggregate(trip_length ~ rideable_type + member_casual, cleanbikedata2, mean)


# DATA VISUALIZATION
# 1.Group and visualize data by rider type and monthly trip length
mc_month <- cleanbikedata2 %>% group_by(month, member_casual) %>%
  count()
ggplot(data=mc_month)+
  geom_col(mapping=aes(x=month, y=n, fill=member_casual))+
  facet_wrap(~member_casual)+
  labs(y="Trip length", x="Rider type", fill="Member/Casual",
       title="Ride length by month")+
  theme(axis.text.x=element_text(angle=45))


# 2.Group and visualize rider type by day of the week
mc_week <- cleanbikedata2 %>% group_by(day, member_casual) %>%
  count()
ggplot(data=mc_week)+
  geom_col(mapping=aes(x=day, y=n, fill=member_casual))+
  facet_grid(~member_casual)+
  labs(y="Trip length", x="Rider type", fill="Member/Casual",
       title="Ride length by day of the week")+
  theme(axis.text.x=element_text(angle=45))


# 3.Group, summarize and visualize data to determine ride count by rider type
mcdata <- cleanbikedata2 %>% group_by(member_casual) %>% count()
ggplot(data=mcdata)+
  geom_col(mapping=aes(x=member_casual, y=n, fill=member_casual))+
  labs(y="Trip length", x="Rider type", fill="Member/Casual",
       title="Number of trips by different rider types")


# 4.Summarize and visualize data by rider type and average trip length to ascertain lenght of ride taken by different rider types
trip_len <- cleanbikedata2 %>% group_by(member_casual) %>% summarise(mean_trip_length = mean(trip_length, na.rm = TRUE))
ggplot(data=trip_len)+
  geom_col(mapping=aes(x=member_casual, y=mean_trip_length, fill=member_casual))+
  labs(y="Trip length", x="Rider type", fill="Member/Casual",
       title="Lenght of ride taken by different rider types")


# 5.Group and visualize data to determine prefered type of bike by riders
# Group and visualize data by member_casual and rideable_type to determine prefered type of bike by riders
ride_type <- cleanbikedata2 %>% group_by(member_casual, rideable_type) %>%
  count()
ggplot(data=ride_type)+
  geom_col(mapping=aes(x=rideable_type, y=n, fill=member_casual))+
  facet_wrap(~member_casual)+
  labs(y="Number of rides", x="Type of bike", fill="Member/Casual",
       title="Most prefered type of bike by riders")

# TOP 10 STATIONS

# Creat a new data frame for all stations
all_stations <- bind_rows(data.frame("stations" = cleanbikedata2$start_station_name, 
                                     "member_casual" = cleanbikedata2$member_casual),
                          data.frame("stations" = cleanbikedata2$end_station_name,
                                     "member_casual" = cleanbikedata2$member_casual))


# Exclude entries with no station name
all_stations_v2 <- all_stations[!(all_stations$stations == "" | is.na(all_stations$stations)),]


# Separate the data frame by rider type
all_stations_member <- all_stations_v2[all_stations_v2$member_casual == 'member',]
all_stations_casual <- all_stations_v2[all_stations_v2$member_casual == 'casual',]


# Get the top 10 popular stations all, members and casual riders
top_10_station <- all_stations_v2 %>% 
  group_by(stations) %>% 
  summarise(station_count = n()) %>% 
  arrange(desc(station_count)) %>% 
  slice(1:10)


# Viz
ggplot(data=top_10_station)+
  geom_col(mapping=aes(x=stations,y=station_count, fill=stations))+
  labs(title="Top 10 stations", subtitle = "Most populous bike stations.")+
  theme(axis.text = element_text(angle=45))


# Get the top 10 popular stations for members
top_10_stations_member <- all_stations_member %>% 
  group_by(stations) %>% 
  summarise(station_count = n()) %>% 
  arrange(desc(station_count)) %>% 
  head(n=10)


# Viz
ggplot(data=top_10_stations_member)+
  geom_col(mapping=aes(x=stations,y=station_count, fill=stations))+
  labs(title="Top 10 stations", subtitle = "Most populous bike stations amongst members.")+
  theme(axis.text = element_text(angle=45))


# Get the top 10 popular stations for members
top_10_stations_casual <- all_stations_casual %>% 
  group_by(stations) %>% 
  summarise(station_count = n()) %>% 
  arrange(desc(station_count)) %>% 
  head(n=10)


## Viz
ggplot(data=top_10_stations_casual)+
  geom_col(mapping=aes(x=stations,y=station_count, fill=stations))+
  labs(title="Top 10 stations", subtitle = "Most populous bike stations amongst members.")+
  theme(axis.text = element_text(angle=45))
