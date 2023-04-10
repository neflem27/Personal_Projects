# Uber Pickups in New York City
setwd("C:/Users/domin/OneDrive/Escritorio/Github Files/Uber Dataset")

# Data Description, Project goal, link to data, 

# Part 1: Data Preparation

# Libraries Used
library(ggplot2)
library(tidyverse)
library(DT)

# Read in Data (May - Sep 2016)
apr <- read.csv("uber-raw-data-apr14.csv")
may <- read.csv("uber-raw-data-may14.csv")
jun <- read.csv("uber-raw-data-jun14.csv")
jul <- read.csv("uber-raw-data-jul14.csv")
aug <- read.csv("uber-raw-data-aug14.csv")
sep <- read.csv("uber-raw-data-sep14.csv")

# Structure of the data
str(apr)
str(may)
str(jun)
str(jul)
str(aug)
str(sep)

# Notes: By looking at the initial structure of the data, the number of observations 
  #increase by month and are 4 intial variables date/time,Lat,Lon,Base.

# Part 2: Data Cleaning 

# Combining Data
data_2014 <- rbind(apr, may, jun, jul, aug, sep)
# NA Check
sum(is.na(data_2014)) # None


# Simplifying time frames (To uncover service trends)
data_2014$Date.Time <- as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S")
data_2014$Time <- format(as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")
data_2014$Date.Time <- ymd_hms(data_2014$Date.Time)
data_2014$day <- factor(day(data_2014$Date.Time))
data_2014$month <- factor(month(data_2014$Date.Time, label = TRUE))
data_2014$year <- factor(year(data_2014$Date.Time))
data_2014$dayofweek <- factor(wday(data_2014$Date.Time, label = TRUE))
data_2014$hour <- factor(hour(hms(data_2014$Time)))
data_2014$minute <- factor(minute(hms(data_2014$Time)))
data_2014$second <- factor(second(hms(data_2014$Time)))
head(data_2014)

#Note 2: # We have created new variables out of the original data to view the distribution
  # of uber pickups in different time frames 

# EDA

#Frequency of trips by hours in a day
options(scipen = 999)
library(scales)

trips_hour <- data_2014 %>% 
  group_by(hour) %>% 
  summarise(total = n())

# Barplot
ggplot(trips_hour, aes(x = hour, y = total)) + 
  geom_bar(stat = "identity", fill = "lightgreen", color = "black", width = 0.8,cex = 1 ) +
  ggtitle("Trips Every Hour") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)+
  xlab("Hour") +
  ylab("Total Trips")

#Chart of busiest hours
datatable(trips_hour %>% arrange(desc(total)))


# Frequency of pickups by Month
month_hour <- data_2014 %>%
  group_by(month, hour) %>%
  summarise(Total = n(), .groups = "keep")

ggplot(month_hour, aes(hour, Total, fill = month)) + 
  geom_bar( stat = "identity", width = 0.8) +
  theme_minimal() +
  ggtitle("Trips by Hour and Month") +
  scale_y_continuous(labels = comma) +
  xlab("Hour") +
  ylab("Total Trips")

# Montly Trip Comparison
ggplot(month_hour, aes(x=hour, y=Total, group = month, color = month)) +
  geom_line(linewidth = 1)+
  geom_point() +
  theme_minimal() +
  ggtitle("Monthly Trip Comparison") +
  scale_y_continuous(labels = comma) +
  xlab("Hour") +
  ylab("Total Trips")

# Trips By Day of the Month

day_group <- data_2014 %>%
  group_by(day) %>%
  dplyr::summarize(Total = n()) 

datatable(day_group)

# Histogram of Trips By day
ggplot(day_group, aes(day, Total)) + 
  geom_histogram( stat = "identity", fill = "orange", color = "black", width = 0.8) +
  ggtitle("Histogram of Trips By Day") +
  theme_minimal() +
  scale_y_continuous(labels = comma)

# Trips By Day of the Week
 month_weekday <- data_2014 %>%
   group_by(month, dayofweek) %>%
   dplyr::summarize(Total = n())
 
 ggplot(month_weekday, aes(month, Total, fill = dayofweek)) + 
   geom_bar( stat = "identity", position = "dodge", width = 0.8) +
   theme_minimal() +
   ggtitle("Trips by Day and Month") +
   scale_y_continuous(labels = comma)

# Part 3: Spatial Data
library(mapview)
# Random sample (100,000 only)
rsamp <- sample.int(nrow(data_2014), size = 100000, replace = F)
map_data <- data_2014[rsamp,]
mapview(map_data, xcol = "Lon", ycol = "Lat", zcol = "month",
        grid = F,
        crs = 4326,
        cex = 3,
        alpha.regions = 0.8)

