### Hi there 👋

<!--
**annyannyanny/annyannyanny** is a ✨ _special_ ✨ repository because its `README.md` (this file) appears on your GitHub profile.

Here are some ideas to get you started:

- 🔭 I’m currently working on ...
- 🌱 I’m currently learning ...
- 👯 I’m looking to collaborate on ...
- 🤔 I’m looking for help with ...
- 💬 Ask me about ...
- 📫 How to reach me: ...
- 😄 Pronouns: ...
- ⚡ Fun fact: ...
-->
#Import csv files 
setwd("/Users/qianzhaoyi/Downloads")
tripdata_202201 <- read.csv("202201-divvy-tripdata.csv")
tripdata_202202 <- read.csv("202202-divvy-tripdata.csv")
tripdata_202203 <- read.csv("202203-divvy-tripdata.csv")
tripdata_202204 <- read.csv("202204-divvy-tripdata.csv")
tripdata_202205 <- read.csv("202205-divvy-tripdata.csv")
tripdata_202206 <- read.csv("202206-divvy-tripdata.csv")
tripdata_202207 <- read.csv("202207-divvy-tripdata.csv")
tripdata_202208 <- read.csv("202208-divvy-tripdata.csv")
tripdata_202209 <- read.csv("202209-divvy-publictripdata.csv")
tripdata_202210 <- read.csv("202210-divvy-tripdata.csv")
tripdata_202211 <- read.csv("202211-divvy-tripdata.csv")
tripdata_202212 <- read.csv("202212-divvy-tripdata.csv")

#Investigate each dataset
#No unaligned data type
str(tripdata_202201)
str(tripdata_202202)
str(tripdata_202203)
str(tripdata_202204)
str(tripdata_202205)
str(tripdata_202206)
str(tripdata_202207)
str(tripdata_202208)
str(tripdata_202209)
str(tripdata_202210)
str(tripdata_202211)
str(tripdata_202212)

#Concate 12 files into 1 file
tripdata_2022 <- rbind(tripdata_202201, tripdata_202202, tripdata_202203, tripdata_202204, 
                       tripdata_202205, tripdata_202206, tripdata_202207, tripdata_202208, 
                       tripdata_202209,tripdata_202210, tripdata_202211, tripdata_202212)

#Confirm the number of rows and get the sum number of total rows
rowtotal <- sum(
  nrow(tripdata_202201),
  nrow(tripdata_202202),
  nrow(tripdata_202203),
  nrow(tripdata_202204),
  nrow(tripdata_202205),
  nrow(tripdata_202206),
  nrow(tripdata_202207),
  nrow(tripdata_202208),
  nrow(tripdata_202209),
  nrow(tripdata_202210),
  nrow(tripdata_202211),
  nrow(tripdata_202212)
)

print(rowtotal)
#View the structure and first 6 rows of tripdata_2022
str(tripdata_2022)
head(tripdata_2022)

#Data cleaning
summary(tripdata_2022)
#There is no missing value but we can still run the function just to show. And remove duplicate value.
tripdata_2022 <- drop_na(tripdata_2022)
trip_2022_clean <- tripdata_2022[!duplicated(tripdata_2022$ride_id),]
print(paste("Remove", nrow(tripdata_2022) - nrow(trip_2022_clean), "duplicate rows"))

#Create a new column of ride duration with unit "mins"
trip_2022_clean$started_at <- as.POSIXct(trip_2022_clean$started_at, "%Y-%m-%d %H:%M:%S")
trip_2022_clean$ended_at <- as.POSIXct(trip_2022_clean$ended_at, "%Y-%m-%d %H:%M:%S")

names(trip_2022_clean)

trip_2022_clean <- trip_2022_clean %>%
  mutate(ride_duration = as.numeric(trip_2022_clean$ended_at - trip_2022_clean$started_at) / 60)
summary(trip_2022_clean$ride_duration)

#Filter out the negative ride duration
nrow(trip_2022_clean[trip_2022_clean$ride_duartion < 0,])
trip2022 <- trip_2022_clean[!trip_2022_clean$ride_duartion <0,]
glimpse(trip2022)

#Seperate the year and the month 
trip2022 <- trip2022 %>%
  mutate(year_month = paste(strftime(trip2022$started_at, "%Y"),
                            "-",
                            strftime(trip2022$ended_at, "%m"),
                            paste("(", strftime(trip2022$started_at, "%b"), ")", sep="")))

#Get the number of casual riders ane annual mebers
rider_type_total <- table(trip2022$member_casual)
View(rider_type_total)

#Statistical analysis
trip_stats <- trip2022 %>% 
  group_by(member_casual) %>% 
  summarise(avg_ride_duration = mean(ride_duration), 
            stdev_ride_duration = sd(ride_duration), 
            median_ride_duration = median(ride_duration), 
            min_ride_duration = min(ride_duration), 
            max_ride_duration = max(ride_duration))
head(trip_stats)

#method2
trip2022 <- trip2022 %>%
  mutate(ride_duration_m = as.numeric(trip2022$ended_at - trip2022$started_at) / 60)
summary(trip2022$ride_time_m)

#Determine the Mode for the day of the week
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

weekday_mode <- getmode(trip2022$day_of_week)

print(weekday_mode)

#Determine the most poplular day by rider type
popular_month <- trip2022 %>% 
  group_by(month) %>% 
  summarise(number_of_rides = n(), avg_duration = mean(ride_duration)) %>% 
  arrange(-number_of_rides)

View(popular_month)


#Determine the most popular start station
station_mode <- getmode(trip2022$start_station_name)
print(station_name)

#Determine the most popular start station for (members)
pop_start_station_member <- trip2022 %>% 
  filter(member_casual == "member") %>% 
  group_by(start_station_name) %>% 
  summarise(num_starts_station = n()) %>% 
  filter(start_station_name!= "") %>% 
  arrange(-num_starts_station)
head(pop_start_station_member)

#Determine the most popular start station for (casual)
pop_start_station_casual <- trip2022 %>% 
  filter(member_casual == "casual") %>% 
  group_by(start_station_name) %>% 
  summarise(num_starts_station = n()) %>% 
  filter(start_station_name!= "") %>% 
  arrange(-num_starts_station)
head(pop_start_station_casual)

##From the analysis we can determine that:

There were more members(2009756) than casual riders(1278811) in the year 2020.

Casual riders(48.27 mins) had a higher average ride length than members(15.75 mins).

Casual riders had the longest ride time being 156450.40 mins or 2608 hours.

The most popular day for bike sharing was Saturday.

The most popular day for casual riders was Saturday with an average ride length of 49.18 mins.

The most popular day for members was Wednesday with an average ride length of 14.73 mins

The most popular month was August with 605746 rides with an average ride duration of 29.45 mins.

The least popular month was December with 23295 rides with an average ride duration of 17.90 mins.

The most popular start station overall was "Streeter Dr & Grand Ave".

The most popular start station for members was "Clark St & Elm St" with 19195 trips starting from here.

The most popular start station for casual riders was "Streeter Dr & Grand Ave" with 25790 trips starting from here.

##Were you able to answer the question of how annual members and casual riders use Cyclistic bikes differently?

Yes, we were able to determine a number of differences between casual riders and annual members.

● What story does your data tell?
  
  The data tells us that there are a large number of casual riders who have a higher average of ride duration. This is a potential target for the digital marketing campaign.

● How do your findings relate to your original question?
  
  The data answered all the original questions.

How do annual members and casual riders use Cyclistic bikes differently?
  We determined a variety of differences between the two groups.

Why would casual riders buy Cyclistic annual memberships?
  Casual members would buy the annual membership as they on average use the bikes for longer than members and the longest individual ride was also by a casual member.

How can Cyclistic use digital media to influence casual riders to become members?
  A digital media campaign with a focus on the benefits of a membership aimed at the casual rider who are using the bikes for long durations.

##Data visualisation in R
library(ggplot2)

trip2022 %>% 
  group_by(member_casual) %>% 
  summarise(total_rider_type = n()) %>% 
  ggplot(aes(x = member_casual, y = total_rider_type, fill = member_casual)) + 
  geom_col(position = "dodge") + geom_text(aes(label = total_rider_type, vjust = -0.25))


#Visualisation of the 2 types riders's ride duration
rider_type_avg_duartion <- trip2022 %>% 
  group_by(member_casual) %>% 
  summarize(avg_ride_duration = mean(ride_duration))

rider_type_avg_duartion %>% 
  ggplot(aes(x = member_casual, y = avg_ride_duration, fill = member_casual)) +
  geom_col(position = "dodge") + geom_text(aes(label = avg_ride_duration, vjust = -0.25))


#Visualiasation of the usage(avg_duration) by members and casual riders by the weekdays

trip2022 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(num_of_rides = n(), avg_duration = mean(ride_duration)) %>% 
  arrange(member_casual,day_of_week) %>% 
  ggplot(aes(x = day_of_week, y = avg_duration, fill = mamber_casual)) +
  geom_col(position = "dodge")

#Visulisation of the number of the trips(num_of_ride) by members and casual riders by the weekdays

trip2022 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(num_of_rides = n(), avg_duration = mean(ride_duration)) %>% 
  arrange(member_casual,day_of_week) %>% 
  ggplot(aes(x = day_of_week, y = num_of_rides, fill = mamber_casual)) +
  geom_col(position = "dodge")

#Visualisation of the usage by members and casual riders by the month
