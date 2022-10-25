``` r
# Install required packages and libraries
```

``` r
#tidyverse for metapackage of all tidyverse packages
#lubridate for date functions
#ggplot2 for visualization
#scales for the values in a vector or data frame
#dplyr provides a uniform set of verbs, helping to resolve the most frequent data manipulation hurdles
```

``` r
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("scales")
install.packages("dplyr")
```

``` r
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(dplyr)
```

``` r
#=====================
# STEP 1: COLLECT DATA
#=====================
# Upload Divvy datasets (csv files) here

Aug21 <- read_csv("C:/Capstone Project _cyclistic_trip_data_08_21_ to_07_22.csv/.CSV/Aug21.csv")
Sept21 <-read_csv("C:/Capstone Project _cyclistic_trip_data_08_21_ to_07_22.csv/.CSV/Sept21.csv")
Oct21 <- read_csv("C:/Capstone Project _cyclistic_trip_data_08_21_ to_07_22.csv/.CSV/Oct21.csv")
Nov21 <- read_csv("C:/Capstone Project _cyclistic_trip_data_08_21_ to_07_22.csv/.CSV/Nov21.csv")
Dec21 <- read_csv("C:/Capstone Project _cyclistic_trip_data_08_21_ to_07_22.csv/.CSV/Dec21.csv")
Jan22 <- read_csv("C:/Capstone Project _cyclistic_trip_data_08_21_ to_07_22.csv/.CSV/Jan22.csv")
Feb22 <- read_csv("C:/Capstone Project _cyclistic_trip_data_08_21_ to_07_22.csv/.CSV/Feb22.csv")
Mar22 <- read_csv("C:/Capstone Project _cyclistic_trip_data_08_21_ to_07_22.csv/.CSV/Mar22.csv")
April22 <-read_csv("C:/Capstone Project _cyclistic_trip_data_08_21_ to_07_22.csv/.CSV/April22.csv")
May22 <- read_csv("C:/Capstone Project _cyclistic_trip_data_08_21_ to_07_22.csv/.CSV/May22.csv")
Jun22 <- read_csv("C:/Capstone Project _cyclistic_trip_data_08_21_ to_07_22.csv/.CSV/Jun22.csv")
July22 <- read_csv("C:/Capstone Project _cyclistic_trip_data_08_21_ to_07_22.csv/.CSV/July22.csv")
```

``` r
#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================

# Compare column names each of the files
# While the names don't have to be in the same order, they DO need to match perfectly before we can use a command to join them into one file
colnames(Aug21)
colnames(Sept21)
colnames(Oct21)
colnames(Nov21)
colnames(Dec21)
colnames(Jan22)
colnames(Feb22)
colnames(Mar22)
colnames(April22)
colnames(May22)
colnames(Jun22)
colnames(July22)
```

``` r
# While cleaning the raw data in Excel, I renamed the columns to make them consistent 

ride_id = trip_id
rideable_type = bikeid 
started_at = start_time  
ended_at = end_time  
start_station_name = from_station_name 
start_station_id = from_station_id 
end_station_name = to_station_name 
end_station_id = to_station_id 
member_casual = usertype
```

``` r
# Inspect the dataframes and look for incongruencies
str(Aug21)
str(Sept21)
str(Oct21)
str(Nov21)
str(Dec21)
str(Jan22)
str(Feb22)
str(Mar22)
str(April22)
str(May22)
str(Jun22)
str(July22)
```

``` r
# Convert ride_id and rideable_type to character so that they can stack correctly

Aug21 <-  mutate(Aug21, trip_id = as.character(trip_id)
                   ,bikeid = as.character(bikeid))
Sept21 <-  mutate(Sept21, trip_id = as.character(trip_id)
                 ,bikeid = as.character(bikeid)) 
Oct21 <-  mutate(Oct21, trip_id = as.character(trip_id)
                 ,bikeid = as.character(bikeid)) 
Nov21 <-  mutate(Nov21, trip_id = as.character(trip_id)
                 ,bikeid = as.character(bikeid)) 
Dec21 <-  mutate(Dec21, trip_id = as.character(trip_id)
                 ,bikeid = as.character(bikeid)) 
Jan22 <-  mutate(Jan22, trip_id = as.character(trip_id)
                 ,bikeid = as.character(bikeid)) 
Feb22 <-  mutate(Feb22, trip_id = as.character(trip_id)
                 ,bikeid = as.character(bikeid)) 
Mar22 <-  mutate(Mar22, trip_id = as.character(trip_id)
                 ,bikeid = as.character(bikeid)) 
April22 <-  mutate(April22, trip_id = as.character(trip_id)
                 ,bikeid = as.character(bikeid)) 
May22 <-  mutate(May22, trip_id = as.character(trip_id)
                 ,bikeid = as.character(bikeid)) 
Jun22 <-  mutate(Jun22, trip_id = as.character(trip_id)
                 ,bikeid = as.character(bikeid)) 
July22 <-  mutate(July22, trip_id = as.character(trip_id)
                 ,bikeid = as.character(bikeid)) 
```

``` r
# Stack individual quarter's data frames into one big data frame
all_trips <- bind_rows(Aug21, Sept21, Oct21, Nov21, Dec21, Jan22, Feb22, Mar22, April22, May22, Jun22, July22)

#check all_trips for errors
colnames(all_trips)
str(all_trips)

# Remove lat, long fields
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng))
```

``` r
#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# I Inspected the new table that has been created
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips) #Statistical summary of data. Mainly for numerics

#I cleaned and removed any NA's and duplicates in Excel before uploading to R

# There are a few problems I will need to fix:
# (1) In the "member_casual" column, there are two names for members ("member" and "Subscriber") and two names for casual riders ("Customer" and "casual"). I need to consolidate that from four to two labels.
# (2) The data can only be aggregated at the ride-level, which is too granular. I want to add some additional columns of data -- such as day, month, year -- that provide additional opportunities to aggregate the data.
# (3) I want to add a calculated field for length of ride since the 2020Q1 data did not have the "tripduration" column. We will add "ride_length" to the entire dataframe for consistency.
# (4) There are some rides where tripduration shows up as negative, including several hundred rides where Divvy took bikes out of circulation for Quality Control reasons. I want to delete these rides.

# In the "member_casual" column, I replaced "Subscriber" with "member" and "Customer" with "casual"
# Divvy used different labels for these two types of riders ... I want to make our dataframe consistent with their current nomenclature
# N.B.: "Level" is a special property of a column that is retained even if a subset does not contain any values from a specific level

# Begin by seeing how many observations fall under each usertype
(all_trips$usertype)

# Reassign to the desired values (we will go with the current 2020 labels)
all_trips <-  all_trips %>% 
  mutate(usertype = recode(usertype
                                ,"member" = "Subscriber"
                                ,"casual" = "Customer"))

# Check to make sure the proper number of observations were reassigned
table(all_trips$usertype)

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
all_trips$date <- as.Date(all_trips$start_time) 
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Add a "ride_length" calculation to all_trips (in seconds)
all_trips$start_time <- as.POSIXlt(all_trips$start_time, format = "%m/%d/%Y %H:%M", tz="EST")
all_trips$end_time <- as.POSIXlt(all_trips$end_time, format = "%m/%d/%Y %H:%M", tz="EST")
all_trips$ride_length <- difftime(all_trips$end_time, all_trips$start_time)

# Inspect the structure of the columns
str(all_trips)
tibble [4,632,305 × 15] (S3: tbl_df/tbl/data.frame)
 $ trip_id          : chr [1:4632305] "99103BB87CC6C1BB" "EAFCCCFB0A3FC5A1" "9EF4F46C57AD234D" "5834D3208BFAF1DA" ...
 $ bikeid           : chr [1:4632305] "electric_bike" "electric_bike" "electric_bike" "electric_bike" ...
 $ start_time       : POSIXlt[1:4632305], format: "2021-08-01 00:00:00" "2021-08-01 00:00:00" "2021-08-01 00:00:00" "2021-08-01 00:00:00" ...
 $ end_time         : POSIXlt[1:4632305], format: "2021-08-01 00:13:00" "2021-08-01 00:05:00" "2021-08-01 00:05:00" "2021-08-01 00:12:00" ...
 $ from_station_name: chr [1:4632305] "Clark St & Wrightwood Ave" "Racine Ave & Fullerton Ave" "Wells St & Huron St" "Halsted St & Dickens Ave" ...
 $ from_station_id  : chr [1:4632305] "TA1305000014" "TA1306000026" "TA1306000012" "13192" ...
 $ to_station_name  : chr [1:4632305] "Ashland Ave & Wrightwood Ave" "Halsted St & Wrightwood Ave" "Wells St & Evergreen Ave" "Wilton Ave & Belmont Ave" ...
 $ to_station_id    : chr [1:4632305] "13296" "TA1309000061" "TA1308000049" "TA1307000134" ...
 $ usertype         : chr [1:4632305] "Subscriber" "Subscriber" "Customer" "Subscriber" ...
 $ date             : Date[1:4632305], format: "2021-08-01" "2021-08-01" "2021-08-01" "2021-08-01" ...
 $ month            : chr [1:4632305] "08" "08" "08" "08" ...
 $ day              : chr [1:4632305] "01" "01" "01" "01" ...
 $ year             : chr [1:4632305] "2021" "2021" "2021" "2021" ...
 $ day_of_week      : chr [1:4632305] "Sunday" "Sunday" "Sunday" "Sunday" ...
 $ ride_length      : 'difftime' num [1:4632305] 780 300 300 720 ...
  ..- attr(*, "units")= chr "secs"
  
# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
# I will create a new version of the dataframe (v2) since data is being removed

all_trips_v2 <- all_trips[!(all_trips$from_station_name == "HQ QR" | all_trips$ride_length<0),]
```

``` r
#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride

# I condensed the four lines above to one line using summary() on the specific attribute
summary(all_trips_v2$ride_length)

# Compared subscriber and customer users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype, FUN = mean)
all_trips_v2$usertype all_trips_v2$ride_length
1              Customer                1596.9974
2            Subscriber                 756.2138

aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype, FUN = median)
all_trips_v2$usertype all_trips_v2$ride_length
1              Customer                      900
2            Subscriber                      540

aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype, FUN = max)
all_trips_v2$usertype all_trips_v2$ride_length
1              Customer                  2497740
2            Subscriber                    90000

aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype, FUN = min)
all_trips_v2$usertype all_trips_v2$ride_length
1              Customer                        0
2            Subscriber                        0

# The average ride time by each day for subscriber vs customer users
aggregate (all_trips_v2$ride_length ~ all_trips_v2$usertype + all_trips_v2$day_of_week, FUN = mean)
   all_trips_v2$usertype all_trips_v2$day_of_week all_trips_v2$ride_length
1               Customer                   Friday                1479.2096
2             Subscriber                   Friday                 735.5357
3               Customer                   Monday                1659.4393
4             Subscriber                   Monday                 732.9114
5               Customer                 Saturday                1742.0033
6             Subscriber                 Saturday                 853.6923
7               Customer                   Sunday                1837.1096
8             Subscriber                   Sunday                 860.7055
9               Customer                 Thursday                1404.7698
10            Subscriber                 Thursday                 724.2263
11              Customer                  Tuesday                1391.5868
12            Subscriber                  Tuesday                 705.2034
13              Customer                Wednesday                1368.1549
14            Subscriber                Wednesday                 713.3479

# I noticed that the days of the week are out of order. I fixed it
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Analyze ridership data by type and weekday
all_trips_v2 %>% mutate(weekday = wday(start_time,label = TRUE)) %>% 
group_by(usertype, weekday) %>%
summarise(number_of_rides = n(),
average_duration = mean(ride_length)) %>% 
arrange(usertype, weekday)

# A tibble: 14 × 4
# Groups:   usertype [2]
   usertype   weekday number_of_rides average_duration
   <chr>      <ord>             <int>            <dbl>
 1 Customer   Sun              380935            1837.
 2 Customer   Mon              229489            1659.
 3 Customer   Tue              204756            1392.
 4 Customer   Wed              210892            1368.
 5 Customer   Thu              237805            1405.
 6 Customer   Fri              264012            1479.
 7 Customer   Sat              422972            1742.
 8 Subscriber Sun              328661             861.
 9 Subscriber Mon              377689             733.
10 Subscriber Tue              420197             705.
11 Subscriber Wed              419031             713.
12 Subscriber Thu              414687             724.
13 Subscriber Fri              365380             736.
14 Subscriber Sat              355724             854.

# Turned off scientific notation so that the entire number is displayed on the ggplot for numerical distribution

options(scipen=999)
```

``` r
#=================================================
# STEP 5: Visualizations
#=================================================

##The number of rides by usertype in a week %\>% mutate(weekday = wday(start_time, label = TRUE)) %\>% group_by(usertype, weekday) %\>% summarise(number_of_rides = n(), average_duration = mean(ride_length)) %\>% arrange(usertype, weekday) %\>% ggplot(aes(x = weekday, y = number_of_rides, fill = usertype)) + geom_col(position = "dodge") + labs(title="Total Number of Rides by UserType in A Week", x = "Week Day", y = "Number of Rides") + theme(axis.text.x = element_text(angle = 60, hjust = 1))
```
According to this graph, Monday - Friday, customers are less likely to ride bikes in comparison to subscribers. However, on Saturday and Sunday, customers are more privy to ride bikes.

![](https://github.com/sam-is-curious/Google-Data-Analytics-Capstone-Project-Cyclistic-Bike-Share/blob/main/data-ana-images/total%20num%20of%20rides%20by%20usertype%20in%20a%20week.png?raw=true)

``` r
#Let's create a visualization for average duration all_trips_v2 %\>% mutate(weekdays (start_time, label = TRUE)) %\>% group_by(usertype, weekday) %\>% summarise(number_of_rides = n(), average_duration = mean(ride_length)) %\>% arrange(usertype, weekday) %\>% ggplot(aes(x = weekday, y = average_duration, fill = usertype)) + geom_col(position = "dodge")+ labs(title="Average Duration by UserType", x = "Week Day", y = "Average Duration") + theme(axis.text.x = element_text(angle = 60, hjust = 1))
```
The average duration during the week is higher for customers in comparison to subscribers.

![](https://github.com/sam-is-curious/Google-Data-Analytics-Capstone-Project-Cyclistic-Bike-Share/blob/main/data-ana-images/average%20duration%20by%20usertype.png?raw=true)

``` r
`#Let's visualize the number of rides by bike id all_trips %\>% mutate(weekday = wday(start_time, label = TRUE)) %\>% group_by(bikeid, usertype) %\>% summarise(number_of_rides = n(), average_duration = mean(ride_length)) %\>% arrange(bikeid, usertype) %\>% ggplot(aes(x = bikeid, y = number_of_rides, fill = usertype)) + geom_col(position = "dodge") + labs(title="Total Number of Rides by BikeId", x = "Week Day", y = "Number of Rides") + theme(axis.text.x = element_text(angle = 60, hjust = 1))
```
 For the type of bikes used during the weekday, it seems that customers and subscribers enjoy the classic bike with the electric bike coming in second
 
![](https://github.com/sam-is-curious/Google-Data-Analytics-Capstone-Project-Cyclistic-Bike-Share/blob/main/data-ana-images/total%20num%20of%20rides%20by%20bikeid.png?raw=true)

``` r
`#Average Ride Duration For Each User Type For One Year all_trips_v2 %\>% group_by(usertype, month) %\>% summarise(average_duration = mean(ride_length)) %\>% arrange(usertype, month) %\>% ggplot(aes(x = month, y = average_duration, group = usertype)) + geom_line(aes(color = usertype)) + geom_point() + scale_x\_discrete(labels = c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul")) + labs(title = "Average Ride Duration For Each User Type In One Year", x = "Month", y = "Average Duration (sec)", color = "User Type")
```
For ride duration for a full year, the peak for customers is in the months of Dec and May. The lowest month for customers is in Feb. In comparison, the peak for subscribers is also in Dec followed by April, May, and June. The lowest months for subscribers are Aug, Nov, Jan, and Mar.

![](https://github.com/sam-is-curious/Google-Data-Analytics-Capstone-Project-Cyclistic-Bike-Share/blob/main/data-ana-images/average%20ride%20duration%20for%20each%20user%20type%20in%20one%20year.png?raw=true)

``` r
`#Number Of Rides For Each User Type For One Year all_trips_v2 %\>% group_by(usertype, month) %\>% summarise(number_of_rides = n()) %\>% arrange(usertype, month) %\>% ggplot(aes(x = month, y = number_of_rides, group = usertype)) + geom_line(aes(color = usertype)) + geom_point() + scale_y\_continuous(labels = label_number(suffix = " K", scale = 1e-4)) + scale_x\_discrete(labels = c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul")) + labs(title = "Number of Rides For Each User Type In One Year", x = "Month", y = "Number of Rides", color = "User Type")
```
According to this graph, customers were steady during the months of Aug - Nov. Displaying a slight increase in Nov into Dec. With a huge decline in January. 
Subscribers were at a peak in Aug followed by a decline in the months of Sept - Oct. 
There was an increase in Nov/Dec followed by another decline in Jan.
Both customers and subscribers shared a high peak in April and May.

![](https://github.com/sam-is-curious/Google-Data-Analytics-Capstone-Project-Cyclistic-Bike-Share/blob/main/data-ana-images/number%20of%20rides%20for%20each%20usertype%20in%20one%20year.png?raw=true)

```r
#Top 20 station by number of rides booked for subscribed members all_trips_v2 %\>% filter(!is.na(from_station_name)) %\>% filter(usertype == "Subscriber") %\>% group_by(from_station_name) %\>% summarise(number_of_rides = n(), avg_ride_length = mean(ride_length), avg_ride_length_min = mean(ride_length) / 60) %\>% arrange(-number_of_rides) %\>% head(20)
```
The top station for the #of rides booked for subscribed members is, Kingsbury St & Kinzie St.

![](https://github.com/sam-is-curious/Google-Data-Analytics-Capstone-Project-Cyclistic-Bike-Share/blob/main/data-ana-images/Top%2020%20station%20by%20number%20of%20rides%20booked%20for%20subscriber%20members.png?raw=true)

``` r
#Top 20 station by number of rides booked for customer members all_trips_v2%\>% filter(!is.na(from_station_name)) %\>% filter(usertype == "Customer") %\>% group_by(from_station_name) %\>% summarise(number_of_rides = n(), avg_ride_length = mean(ride_length), avg_ride_length_min = mean(ride_length) / 60) %\>% arrange(-number_of_rides) %\>% head(20)
```
The top station for the #of rides booked for customers is Streeter Dr & Grand Ave.

![](https://github.com/sam-is-curious/Google-Data-Analytics-Capstone-Project-Cyclistic-Bike-Share/blob/main/data-ana-images/Top%2020%20station%20by%20number%20of%20rides%20booked%20for%20customer%20members.png?raw=true)

``` r
#Total number of rides by customer and subscribers ggplot(all_trips_v2, aes(x = fct_infreq(usertype)))+ geom_bar(width = 0.5)+ labs(x = NULL, y = "Number of rides", title = "Total rides of Subscribers vs Customers")+ scale_y\_continuous(labels = unit_format(unit = "M", scale = 1e-6))
```
For the total number of rides of subscribers and customers, the subscribers are in the lead with over 1 million subscribers. and customers with a little over 0.75 million.

![](https://github.com/sam-is-curious/Google-Data-Analytics-Capstone-Project-Cyclistic-Bike-Share/blob/main/data-ana-images/total%20rides%20of%20subscribers%20vs%20customers.png?raw=true)

``` r
#=================================================
# EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
counts \<- aggregate(all_trips\$ride_length \~ all_trips\$usertype + all_trips\$day_of_week, FUN = mean)

write.csv(counts, file = 'C:/Desktop/Divvy_Exercise/Capstone Project_1.csv'
```
