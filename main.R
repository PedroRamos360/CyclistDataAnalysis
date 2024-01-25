library(tidyverse)

# Total number of csv files that need to be read
num_files = 12

# Create an empty list to store the data frames
data_list = list()

# Loop through file names and read the CSV files
for (i in 1:num_files) {
  if (i < 10) {
    file_name = paste0("20230", i, "-divvy-tripdata.csv")
  } else {
    file_name = paste0("2023", i, "-divvy-tripdata.csv")
  }
  data_list[[i]] <- read.csv(file_name)
}

# Concatenate the data of all months of 2023
cyclistic_2023 = do.call(rbind, data_list)
# Free memory allocated
rm(data_list)

# Preview of the data
head(cyclistic_2023)

# 1) Verifying the integrity of the data:
# Make sure all essential data is present in each row:
empty_counts = sapply(cyclistic_2023, function(x) sum(is.null(x) | x == ""))
print(empty_counts)
# With that is possible to conclude that there are lots of rows if missing values
# for the station names and ids (for the end and start), also end_lat and end_lng
# returned NA which should be further investigated.
# Using the following code:
empty_counts = sapply(cyclistic_2023, function(x) sum(is.na(x)))
print(empty_counts)
rm(empty_counts)
# We can verify that there are 239 rows for which there is no end_lat and end_lng
# which are likely bikes that were broken or stolen before reaching their destinations:
na_end_positions = cyclistic_2023 %>%
  filter(is.na(end_lat))
head(na_end_positions)
# Graph comparing the amount of problematic bikes from casual members x anual members
ggplot(na_end_positions, aes(
  x = member_casual,
  fill=member_casual,
)) + geom_bar() + labs(title="Anual members with bike problems x\nCasual members with bike problems")
# Graph comparing the total amount of anual members against total casual riders
ggplot(cyclistic_2023, aes(
  x = member_casual,
  fill=member_casual
)) + geom_bar() + labs(title="Total anual members x\nTotal casual members")
# With these graphs is clear that more bike problems occur in casual riders than
# members, since there are more members in total but more casual riders with bike
# problems. Nevertheless, with the data provided it is not clear exaclty what happens
# to the bikes or why, they might have broken, stolen or maybe the casual riders simply
# did not understood correctly how to retrieve the bikes.

# 2) Manipulating the data
# To further discover differences between casual riders and members we can analyze
# calculated columns like total_trip_time and total_trip_distance
# Total trip seconds:
library(lubridate)
library(dplyr)
manipulated_data = cyclistic_2023
rm(cyclistic_2023)
manipulated_data$started_at <- ymd_hms(manipulated_data$started_at)
manipulated_data$ended_at <- ymd_hms(manipulated_data$ended_at)
manipulated_data$total_trip_seconds = as.numeric(manipulated_data$ended_at - manipulated_data$started_at)
# Convert the numeric column to the 'mm:ss' format for better visualization
manipulated_data = manipulated_data %>%
  mutate(formatted_time = seconds_to_period(total_trip_seconds) %>%
           as.character() %>% 
           sprintf("%M:%S"))

View(manipulated_data)

# Add weekday
manipulated_data = manipulated_data %>%
  mutate(weekday_start = weekdays(started_at), weekday_end = weekdays(ended_at))

# Check if there are trips that goes over midnight
count_different_start_end = manipulated_data %>%
  filter(weekday_start != weekday_end)

# There are lots of trips that goes over midnight
print(count_different_start_end)

View(manipulated_data)

# Distance from bike retrieval to delivery:
# Function to calculate Haversine distance
haversine_distance = function(delta_latitude, delta_longitude) {
  # Radius of the Earth in kilometers
  earth_radius = 6371
  
  # Convert latitude and longitude differences to radians
  delta_lat_rad = delta_latitude * pi / 180
  delta_lon_rad = delta_longitude * pi / 180
  
  # Haversine formula
  a = sin(delta_lat_rad/2)^2 + cos((0) * pi / 180) * cos((0 + delta_latitude) * pi / 180) * sin(delta_lon_rad/2)^2
  c = 2 * atan2(sqrt(a), sqrt(1 - a))
  distance = earth_radius * c
  
  return(distance)
}

distance_time_trips = manipulated_data %>%
  # Removing unfinished trips since they were already analyzed previously
  filter(!is.na(end_lat) & !is.na(end_lng)) %>%
  mutate(delta_lat = abs(end_lat - start_lat), delta_lng = abs(end_lng - start_lng)) %>%
  mutate(trip_distance_km = haversine_distance(delta_lat, delta_lng))

rm(manipulated_data)
View(distance_time_trips)

# 3) Use additional calculated data to compare casual riders and members:
# Total trip time comparison:
grouped_data = distance_time_trips %>%
  group_by(member_casual) %>%
  summarize(average_time = mean(total_trip_seconds))

View(grouped_data)
ggplot(grouped_data, aes(
  x = member_casual,
  y = average_time,
  fill = member_casual
)) + geom_bar(stat = "identity")

# Total distance trip comparison:
grouped_data = distance_time_trips %>%
  group_by(member_casual) %>%
  summarize(average_distance = mean(trip_distance_km))

ggplot(grouped_data, aes(
  x = member_casual,
  y = average_distance,
  fill = member_casual
)) + geom_bar(stat = "identity")

# Count of riders that started and ended in the same station
trips_0_distance = distance_time_trips %>%
  filter(trip_distance_km == 0)

ggplot(trips_0_distance, aes(
  x = member_casual,
  fill = member_casual
)) + geom_bar()
rm(trips_0_distance)
# Comparison of trips that started on weekdays between groups
trips_in_weekdays = distance_time_trips %>%
  group_by(member_casual) %>%
  summarize(
    total_trips = n(),  # Count total trips
    weekday_trips = sum(!(weekday_start %in% c("Saturday", "Sunday"))),
    weekend_trips = sum(weekday_start %in% c("Saturday", "Sunday"))
  )

print(trips_in_weekdays)

casual_data <- subset(trips_in_weekdays, member_casual == "casual")
member_data <- subset(trips_in_weekdays, member_casual == "member")

print(trips_in_weekdays)
casual_data = c(casual_data$weekday_trips, casual_data$weekend_trips)
member_data = c(member_data$weekday_trips, member_data$weekday_trips)
labels =
  c("Weekdays", "Weekends")
colors = c("orange", "blue")

par(mfrow = c(1, 2))
pie(casual_data, labels = labels, col = colors, main = "Casual riders weekdays x\nweekends distribution")
pie(member_data, labels = labels, col = colors, main = "Members weekdays x\nweekends distribution")
