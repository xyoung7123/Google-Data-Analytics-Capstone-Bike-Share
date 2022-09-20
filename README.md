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

# Clean dataset by removing unnessesary colons
bikedata <- bikedata %>% select(-c(start_station_id, end_station_id, start_lat, start_lng, end_lat, end_lng))

# Input data files are available in the read-only "../input/" directory
# For example, running this (by clicking run or pressing Shift+Enter) will list all files under the input directory

list.files(path = "../input")

# You can write up to 20GB to the current directory (/kaggle/working/) that gets preserved as output when you create a version using "Save & Run All" 
# You can also write temporary files to /kaggle/temp/, but they won't be saved outside of the current session
