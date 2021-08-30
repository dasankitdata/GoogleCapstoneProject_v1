### The Full_Year_Analysis of Cyclistic Data sets for 2019

## Installing the required packages

library(tidyverse)
library(lubridate)
library(ggplot2)
getwd()

## Setting the working directory and verifying

setwd("C:\\Users\\Ankit Das\\Desktop\\Courses and certificates\\Google analytics\\Course Materials\\PortfolioProjects\\CaseStudy_1_Cyclistic\\DatasetsManipulationWith_R")
getwd()

##Uploading the csv files for 2019 to R

q1_2019 <- read.csv("Divvy_Trips_2019_Q1.csv")
q2_2019 <- read.csv("Divvy_Trips_2019_Q2.csv")
q3_2019 <- read.csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read.csv("Divvy_Trips_2019_Q4.csv")
q1_2020 <- read.csv("Divvy_Trips_2020_Q1.csv")

## Verifying the column names
colnames(q1_2020)
colnames(q1_2019)
colnames(q2_2019)
colnames(q3_2019)
colnames(q4_2019)

## Renaming the column names for 2019 as per 2020
q1_2019 <- rename(q1_2019, ride_id=trip_id, rideable_type=bikeid, started_at=start_time, ended_at=end_time, start_station_name=from_station_name, start_station_id=from_station_id, end_station_name=to_station_name, end_station_id=to_station_id, member_casual=usertype)
q2_2019 <- rename(q2_2019, ride_id=X01...Rental.Details.Rental.ID, rideable_type=X01...Rental.Details.Bike.ID, started_at=X01...Rental.Details.Local.Start.Time, ended_at=X01...Rental.Details.Local.End.Time, start_station_name=X03...Rental.Start.Station.Name, start_station_id=X03...Rental.Start.Station.ID, end_station_name=X02...Rental.End.Station.Name, end_station_id=X02...Rental.End.Station.ID, member_casual=User.Type)
q3_2019 <- rename(q3_2019, ride_id=trip_id, rideable_type=bikeid, started_at=start_time, ended_at=end_time, start_station_name=from_station_name, start_station_id=from_station_id, end_station_name=to_station_name, end_station_id=to_station_id, member_casual=usertype)
q4_2019 <- rename(q4_2019, ride_id=trip_id, rideable_type=bikeid, started_at=start_time, ended_at=end_time, start_station_name=from_station_name, start_station_id=from_station_id, end_station_name=to_station_name, end_station_id=to_station_id, member_casual=usertype)

## Inspect the data frames and look for incongruities
str(q1_2020)
str(q4_2019)
str(q3_2019)
str(q2_2019)
str(q1_2019)

## Convert ride_id and rideable_type to character so that they can stack correctly
q4_2019 <- mutate(q4_2019, ride_id = as.character(ride_id),rideable_type = as.character(rideable_type)) 
q3_2019 <- mutate(q3_2019, ride_id = as.character(ride_id),rideable_type = as.character(rideable_type)) 
q2_2019 <- mutate(q2_2019, ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
q1_2019 <- mutate(q1_2019, ride_id = as.character(ride_id),rideable_type = as.character(rideable_type)) 

# Stack individual quarter's data frames into one big data frame
all_trips <- bind_rows(q1_2019, q2_2019, q3_2019, q4_2019, q1_2020)


# Remove lat, long, birthyear, and gender fields as this data was dropped beginning in 2020
all_trips <- all_trips %>%
  select(-c(start_lat,start_lng,end_lat,end_lng,X01...Rental.Details.Duration.In.Seconds.Uncapped,X05...Member.Details.Member.Birthday.Year,birthyear,gender,tripduration))

all_trips <- all_trips %>%
  select(-c(Member.Gender))

## Verifying the column names, dimension and other properties of the of the all_trips data set
colnames(all_trips)
nrow(all_trips)
dim(all_trips)
head(all_trips)
str(all_trips)
summary(all_trips)

### Cleaning the data set and adding calculations

## Cleaning the member_casual column
# Checking for inconsistency...

table(all_trips$member_casual)

# Reassigning with values as per 2020
all_trips <- all_trips %>%
  mutate(member_casual = recode(member_casual, "Subscriber"="member", "Customer"="casual"))

table(all_trips$member_casual)

## Creating rows for the months, years, day, date, day of the week of each ride
# Formatting the started_at column to capture the above mentioned details

all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

## Ride length calculation
# Duration of the ride is calculated based on the started at and ended at column

all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)
# The difftime() function calculates the difference of the two date or date time arguments passed and returns the Time interval in seconds 

str(all_trips)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
#Verifying the type conversion
is.numeric(all_trips$ride_length)

### Capturing rides with ride length is less than ZERO and the start staion name as HQ QR
### Those data point denote the bike being unfit for use and sent to the headquaters for quality check and repairs

##Creating a seperate dataframe for all such bikes and removing them from the main dataframe

all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

## all_trips_v2 is cleaned from all BAD data, where the bikes are sent for repairs are not accounted for

###-----------------------------------------------------------------------------------------------------###

### Conducting the descriptive analysis

## Capturing the mean, median, max, min from the ride lengths (in seconds)

summary(all_trips_v2$ride_length)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

#Capturing the average ride time by each day of the week for members vs casual users
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)


#Analyzing the rides data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at,label = TRUE)) %>% 
  group_by(member_casual,weekday) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% 
  arrange(member_casual,weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual))+
  geom_col(position = "dodge")

# Let's create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")


####Exporting summary file for further analysis

counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = 'C://Users//Ankit Das//Desktop//Courses and certificates//Google analytics//Course Materials//PortfolioProjects//CaseStudy_1_Cyclistic//DatasetsManipulationWith_R//avg_ride_length.csv')
