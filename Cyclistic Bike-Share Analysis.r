#Step 1: setting up my environment:

#Set working directory
setwd("~/Capstone Project") 

##Load required R libraries
library(tidyverse) #Calculations
library(janitor)
library(lubridate) #Dates
library(knitr)
library(skimr) 
library(dplyr)
library(readr) #CSV Files import
library(hms) #time
library(data.table) #exporting data frame

#Step 2: Preparing the files

#Import the 12 months trip data into RStudio
jan_tripdata <- read_csv("jan_tripdata.csv")
feb_tripdata <- read_csv("feb_tripdata.csv")
mar_tripdata <- read_csv("mar_tripdata.csv")
apr_tripdata <- read_csv("apr_tripdata.csv")
may_tripdata <- read_csv("may_tripdata.csv")
jun_tripdata <- read_csv("jun_tripdata.csv")
jul_tripdata <- read_csv("jul_tripdata.csv")
aug_tripdata <- read_csv("aug_tripdata.csv")
sep_tripdata <- read_csv("sep_tripdata.csv")
oct_tripdata <- read_csv("oct_tripdata.csv")
nov_tripdata <- read_csv("nov_tripdata.csv")
dec_tripdata <- read_csv("dec_tripdata.csv")

#Compare the column names of all the 12 csv files and return any missmatch
compare_df_cols(jan_tripdata,feb_tripdata,mar_tripdata,mar_tripdata,may_tripdata,jun_tripdata,jul_tripdata,aug_tripdata,sep_tripdata,oct_tripdata,nov_tripdata,dec_tripdata,  return = "mismatch")
#[1] column_name    jan_tripdata		feb_tripdata	mar_tripdata	mar_tripdata	may_tripdata	jun_tripdata	jul_tripdata	aug_tripdata	sep_tripdata	oct_tripdata	nov_tripdata	dec_tripdata
#<0 rows> (or 0-length row.names)

#Combine the 12 months tripdata into one big dataframe
trip_data_2021 <- bind_rows(jan_tripdata,feb_tripdata,mar_tripdata,mar_tripdata,may_tripdata,jun_tripdata,jul_tripdata,aug_tripdata,sep_tripdata,oct_tripdata,nov_tripdata,dec_tripdata)

#remove the 12 datasets after combining them in order to free up the memory
remove(jan_tripdata,feb_tripdata,mar_tripdata,mar_tripdata,may_tripdata,jun_tripdata,jul_tripdata,aug_tripdata,sep_tripdata,oct_tripdata,nov_tripdata,dec_tripdata)

#After combining the datasets into one big dataframe 'trip_data_2021', Use the code below to view the dataframe
View(trip_data_2021)
#From the above code, the trip_data_2021 dataframe has a total of 5,595,063 rows and 13 colums

#Exported the trip_data_2021 dataframe to a csv for safe keeping while I continue to work on the dataframe
write.csv(trip_data_2021, "trip_data_2021.csv")

str(trip_data_2021)
# spec_tbl_df [5,595,063 × 13] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
# $ ride_id           : chr [1:5595063] "E19E6F1B8D4C42ED" "DC88F20C2C55F27F" "EC45C94683FE3F27" "4FA453A75AE377DB" ...
# $ ride_type     : chr [1:5595063] "electric_bike" "electric_bike" "electric_bike" "electric_bike" ...
# $ started_at        : POSIXct[1:5595063], format: "2021-01-23 16:14:19" "2021-01-27 18:43:08" ...
# $ ended_at          : POSIXct[1:5595063], format: "2021-01-23 16:24:44" "2021-01-27 18:47:12" ...
# $ start_station_name: chr [1:5595063] "California Ave & Cortez St" "California Ave & Cortez St" "California Ave & Cortez St" "California Ave & Cortez St" ...
# $ start_station_id  : chr [1:5595063] "17660" "17660" "17660" "17660" ...
# $ end_station_name  : chr [1:5595063] NA NA NA NA ...
# $ end_station_id    : chr [1:5595063] NA NA NA NA ...
# $ start_lat         : num [1:5595063] 41.9 41.9 41.9 41.9 41.9 ...
# $ start_lng         : num [1:5595063] -87.7 -87.7 -87.7 -87.7 -87.7 ...
# $ end_lat           : num [1:5595063] 41.9 41.9 41.9 41.9 41.9 ...
# $ end_lng           : num [1:5595063] -87.7 -87.7 -87.7 -87.7 -87.7 ...
# $ member_casual     : chr [1:5595063] "member" "member" "member" "member" ...
# - attr(*, "spec")=
#   .. cols(
#     ..   ...1 = col_skip(),
#     ..   ride_id = col_character(),
#     ..   ride_type = col_character(),
#     ..   started_at = col_datetime(format = ""),
#     ..   ended_at = col_datetime(format = ""),
#     ..   start_station_name = col_character(),
#     ..   start_station_id = col_character(),
#     ..   end_station_name = col_character(),
#     ..   end_station_id = col_character(),
#     ..   start_lat = col_double(),
#     ..   start_lng = col_double(),
#     ..   end_lat = col_double(),
#     ..   end_lng = col_double(),
#     ..   member_casual = col_character()
#     .. )
# - attr(*, "problems")=<externalptr> 


#Step 3: Processing stage of the analysis process (data cleaning process)

colnames(trip_data_2021)
skimr::skim_without_charts(trip_data_2021)
glimpse(trip_data_2021)
head(trip_data_2021)

#With the code below, there are 2,869,497 missing values in this dataset #
sum(is.na(trip_data_2021))

#The code below returns the column names where there are missing values (ride_length, station names/id and end_lat/lng columns).
colSums(is.na(trip_data_2021))

##Dropping the missing values from the dataset##
trip_data_2021 <- na.omit(trip_data_2021)
#The original dataframe had 5,595,063 and after dropping missing values of 1,006,761 (17.99%)
#The new dataset has 4,588,302 rows.


#For proper analysis, there is need to creat additional columns
trip_data_2021$ride_date <- as.Date(trip_data_2021$started_at)	# Create column for date
trip_data_2021$year <- format(as.Date(trip_data_2021$ride_date), "%Y") #Create column for Year
trip_data_2021$month <- format(as.Date(trip_data_2021$ride_date), "%B") #Create column to hold month of year (i.e January, February, etc)
trip_data_2021$day_of_week <- format(as.Date(trip_data_2021$ride_date), "%A") #Create column for Day of week (short form eg. Sunday, Monday, etc)
trip_data_2021$ride_length <- difftime(trip_data_2021$ended_at, trip_data_2021$started_at, units = "mins") #Column for ride_length
trip_data_2021$ride_length <- round(trip_data_2021$ride_length,1) #Round ride_length to 1 decimal place

trip_data_2021$hour <- hour(trip_data_2021$started_at) #create new column for hour which will enable us know which time of the day the ride started

#create column for different time_of_day: Night, Morning, Afternoon, Evening
trip_data_2021 <- trip_data_2021 %>% mutate(time_of_day = 
                        case_when(hour == "0" ~ "Night",
                                  hour == "1" ~ "Night",
                                  hour == "2" ~ "Night",
                                  hour == "3" ~ "Night",
                                  hour == "4" ~ "Night",
                                  hour == "5" ~ "Night",
                                  hour == "6" ~ "Morning",
                                  hour == "7" ~ "Morning",
                                  hour == "8" ~ "Morning",
                                  hour == "9" ~ "Morning",
                                  hour == "10" ~ "Morning",
                                  hour == "11" ~ "Morning",
                                  hour == "12" ~ "Afternoon",
                                  hour == "13" ~ "Afternoon",
                                  hour == "14" ~ "Afternoon",
                                  hour == "15" ~ "Afternoon",
                                  hour == "16" ~ "Afternoon",
                                  hour == "17" ~ "Afternoon",
                                  hour == "18" ~ "Evening",
                                  hour == "19" ~ "Evening",
                                  hour == "20" ~ "Evening",
                                  hour == "21" ~ "Evening",
                                  hour == "22" ~ "Evening",
                                  hour == "23" ~ "Evening")
)

trip_data_2021$month_number <- format(as.Date(trip_data_2021$ride_date), "%m") #Create month_number to enable us create season

trip_data_2021 <- trip_data_2021 %>% mutate(season = 
              case_when(month_number == "03" ~ "Spring",
                        month_number == "04" ~ "Spring",
                        month_number == "05" ~ "Spring",
                        month_number == "06"  ~ "Summer",
                        month_number == "07"  ~ "Summer",
                        month_number == "08"  ~ "Summer",
                        month_number == "09" ~ "Fall",
                        month_number == "10" ~ "Fall",
                        month_number == "11" ~ "Fall",
                        month_number == "12" ~ "Winter",
                        month_number == "01" ~ "Winter",
                        month_number == "02" ~ "Winter")
)

#Remove duplicate rows 
trip_data_2021 <- distinct(trip_data_2021) 


# The dataframe includes entries with negative ride_length
trip_data_2021 <- trip_data_2021[!(trip_data_2021$ride_length<1),]
#With the above code 58,087 rides with ride_length less than 1 minute were deleted
#We now have 4,530,215 to work with

#Rename Columns
trip_data_2021 <- trip_data_2021 %>% 
  rename(
      ride_type = ride_type,
      start_time = started_at,
      end_time =ended_at,
      from_station_name = start_station_name,
      to_station_name = end_station_name,
      rider_type = member_casual
    )

#Remove columns not needed: ride_id, start_station_id, end_station_id, hour, start_lat, start_lng, end_lat, end_lng
trip_data_2021 <- trip_data_2021 %>%  
  select(-c(ride_id, from_station_id, to_station_id, hour, start_lat, start_lng, end_lat, end_lng))

#Statistical summary of data. Mainly for numeric
summary(trip_data_2021) 

#Dimensions of the data frame
dim(trip_data_2021) 

#get summary of data, check missing data
skim(trip_data_2021_new) 


# Step 4: Conducting descriptive analysis

# Descriptive analysis on ride_length (all figures in minutes)
mean(trip_data_2021$ride_length) #Minimum ride_length
median(trip_data_2021$ride_length) #midpoint number in the ascending array of ride lengths
max(trip_data_2021$ride_length) #longest ride
min(trip_data_2021$ride_length) #shortest ride

# Compare members and casual users
aggregate(trip_data_2021$ride_length ~ trip_data_2021$rider_type, FUN = mean)
aggregate(trip_data_2021$ride_length ~ trip_data_2021$rider_type, FUN = median)
aggregate(trip_data_2021$ride_length ~ trip_data_2021$rider_type, FUN = max)
aggregate(trip_data_2021$ride_length ~ trip_data_2021$rider_type, FUN = min)



#---------------- FOR FURTHER ANALYSIS------------------------


#-----------------------------------------TOTAL RIDES--------------------------------------

#total number of rides
nrow(trip_data_2021)

#-----------------MEMBER TYPE---------------------
trip_data_2021 %>%
  group_by(rider_type) %>% 
  count(rider_type)

#----------------TYPE OF BIKE---------------------

#total rides by member type 
trip_data_2021 %>%
  group_by(rider_type, ride_type) %>% 
  count(ride_type)

#total rides 
trip_data_2021 %>%
  group_by(ride_type) %>% 
  count(ride_type)

#-------------------HOUR--------------------------

#total rides by member type 
trip_data_2021 %>%
  group_by(rider_type) %>% 
  count(hour) %>% 
  print(n = 48) #lets you view the entire tibble

#total rides
trip_data_2021 %>%
  count(hour) %>% 
  print(n = 24) #lets you view the entire tibble

#----------------------TIME OF DAY-----------------------

#-----morning-------
#total rides by member type 
trip_data_2021 %>%
  group_by(rider_type) %>% 
  filter(time_of_day == "Morning") %>% 
  count(time_of_day)

#total rides
trip_data_2021 %>%
  filter(time_of_day == "Morning") %>% 
  count(time_of_day)

#-----afternoon-------
#total rides by member type 
trip_data_2021 %>%
  group_by(rider_type) %>% 
  filter(time_of_day == "Afternoon") %>% 
  count(time_of_day)

#total rides 
trip_data_2021 %>%
  filter(time_of_day == "Afternoon") %>% 
  count(time_of_day)

#-----evening-------
#total rides by member type
trip_data_2021 %>%
  group_by(rider_type) %>% 
  filter(time_of_day == "Evening") %>% 
  count(time_of_day)

#total rides
trip_data_2021 %>%
  filter(time_of_day == "Evening") %>% 
  count(time_of_day)

#-----night-------
#number of rides by member type
trip_data_2021 %>%
  group_by(rider_type) %>% 
  filter(time_of_day == "Night") %>% 
  count(time_of_day)

#number of rides 
trip_data_2021 %>%
  filter(time_of_day == "Night") %>% 
  count(time_of_day)

#---all times of day----
#total rides by member type 
trip_data_2021 %>%
  group_by(rider_type) %>% 
  count(time_of_day)

#number of rides
trip_data_2021 %>%
  group_by(time_of_day) %>% 
  count(time_of_day)

#----------------DAY OF THE WEEK------------------

#total rides by member type
trip_data_2021 %>%
  group_by(rider_type) %>% 
  count(day_of_week)

#total rides 
trip_data_2021 %>%
  count(day_of_week)

#----------------DAY OF THE MONTH-----------------

#total rides by member type
trip_data_2021 %>%
  group_by(rider_type) %>% 
  count(day) %>% 
  print(n = 62) #lets you view the entire tibble

#total rides
trip_data_2021 %>%
  count(day) %>% 
  print(n = 31) #lets you view the entire tibble

#---------------------MONTH-----------------------

#total rides by member type 
trip_data_2021 %>%
  group_by(rider_type) %>% 
  count(month) %>% 
  print(n = 24) #lets you view the entire tibble

#total rides
trip_data_2021 %>%
  count(month) 

#--------------------SEASON-----------------------

#-----spring-------

#total rides by member type 
trip_data_2021 %>%
  group_by(rider_type) %>% 
  filter(season == "Spring") %>% 
  count(season)

#total rides
trip_data_2021 %>%
  filter(season == "Spring") %>% 
  count(season)

#-----summer-------

#total rides by member type
trip_data_2021 %>%
  group_by(rider_type) %>% 
  filter(season == "Summer") %>% 
  count(season)

#total rides
trip_data_2021 %>%
  filter(season == "Summer") %>% 
  count(season)

#-----fall-------

#total rides by member type
trip_data_2021 %>%
  group_by(rider_type) %>% 
  filter(season == "Fall") %>% 
  count(season)

#total rides
trip_data_2021 %>%
  filter(season == "Fall") %>% 
  count(season)

#-----winter-------

#total rides by member type
trip_data_2021 %>%
  group_by(rider_type) %>% 
  filter(season == "Winter") %>% 
  count(season)

#total rides 
trip_data_2021 %>%
  filter(season == "Winter") %>% 
  count(season)

#-----all seasons-------

#total rides by member type
trip_data_2021 %>%
  group_by(season, rider_type) %>% 
  count(season)

#total rides
trip_data_2021 %>%
  group_by(season) %>% 
  count(season)

#------------------------------------AVERAGE RIDE LENGTH-----------------------------------

#average of ride_length
cyclistic_avgRide <- mean(trip_data_2021$ride_length)
print(cyclistic_avgRide)

#------------------MEMBER TYPE--------------------

#average ride_length
trip_data_2021 %>% group_by( rider_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----------------TYPE OF BIKE---------------------

#total rides by member type 
trip_data_2021 %>% group_by(rider_type, ride_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride_length
trip_data_2021 %>% group_by(ride_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----------------------HOUR-------------------------

#average ride_length by member type
trip_data_2021 %>% group_by(hour, rider_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=48) #lets you view entire tibble

#average ride_length
trip_data_2021 %>% group_by(hour) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=24) #lets you view entire tibble

#--------------------TIME OF DAY---------------------

#----morning----

#average ride length by member type
trip_data_2021 %>% 
  group_by(rider_type) %>% 
  filter(time_of_day == "Morning") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
trip_data_2021 %>% 
  filter(time_of_day == "Morning") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----afternoon----

#average ride length by member type
trip_data_2021 %>% 
  group_by(rider_type) %>% 
  filter(time_of_day == "Afternoon") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
trip_data_2021 %>% 
  filter(time_of_day == "Afternoon") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----evening----

#average ride length by member type
trip_data_2021 %>% 
  group_by(rider_type) %>% 
  filter(time_of_day == "Evening") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
trip_data_2021 %>% 
  filter(time_of_day == "Evening") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----night----

#average ride length by member type 
trip_data_2021 %>% 
  group_by(rider_type) %>% 
  filter(time_of_day == "Night") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
trip_data_2021 %>% 
  filter(time_of_day == "Night") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#---all times of day---

#average ride length by member type
trip_data_2021 %>% 
  group_by(time_of_day, rider_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
trip_data_2021 %>% 
  group_by(time_of_day) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-------------------DAY OF THE WEEK-----------------

#average ride_length by member type
trip_data_2021 %>% group_by(rider_type, day_of_week) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride_length 
trip_data_2021 %>% group_by(day_of_week) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----------------DAY OF THE MONTH------------------

#average ride_length by member type
trip_data_2021 %>% group_by(day, rider_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=62)  #lets you view entire tibble

#average ride_length
trip_data_2021 %>% group_by(day) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=31)  #lets you view entire tibble

#---------------------MONTH--------------------------

#average ride_length by member type
trip_data_2021 %>% group_by(month, rider_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=24)  #lets you view entire tibble

#average ride_length
trip_data_2021 %>% group_by(month) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----------------------SEASON-------------------------

#-----spring------

#average ride length by member type
trip_data_2021 %>% 
  group_by(rider_type) %>% 
  filter(season == "Spring") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
trip_data_2021 %>% 
  filter(season == "Spring") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----summer------

#average ride length by member type for summer 
trip_data_2021 %>% 
  group_by(rider_type) %>% 
  filter(season == "Summer") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length for summer 
trip_data_2021 %>% 
  filter(season == "Summer") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----fall------

#average ride length by member type
trip_data_2021 %>% 
  group_by(rider_type) %>% 
  filter(season == "Fall") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
trip_data_2021 %>% 
  filter(season == "Fall") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----winter-----

#average ride length by member type
trip_data_2021 %>% 
  group_by(rider_type) %>% 
  filter(season == "Winter") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
trip_data_2021 %>% 
  filter(season == "Winter") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----all seasons----

#average ride length by member type
trip_data_2021 %>% 
  group_by(season, rider_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length 
trip_data_2021 %>% 
  group_by(season) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))



#After the data cleaning and transformation I am left with 4530215 rows of data for analysis.
write.csv(trip_data_2021_cleaned, "trip_data_2021_cleaned.csv", row.names = FALSE) #Export Dataframe to CSV file for visualization in PowerBI
