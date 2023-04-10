# Data Visualization Top 50,000 IMBD TV and Web Series World Wide
setwd("C:/Users/domin/OneDrive/Escritorio/Github Files")
tvseries <- read.csv("TV Series.csv")
library(tidyverse)
library(ggplot2)

dim(tvseries) # 50,000 7
str(tvseries) # Raw data has 7 character variables
names(tvseries)
sum(is.na(tvseries)) # Data has no NA's but some observations are missing whatsoever, 

# Data Cleaning

# Unique instances check (Series.Title)
length(which(table(tvseries$Series.Title) >2))
  #Note: 78 title obs are repeated as their title freq > 2

# Example
bb <- which(tvseries$Series.Title == "Breaking Bad")
breaking_bad_instances <- tvseries[bb,]
dim(breaking_bad_instances) # 801 instances

# Question: Are these 801 instances equal?
identical(breaking_bad_instances,breaking_bad_instances)
# A: yes, we can therefore proceed with the duplicate() function to remove 
  # repeated elements in the entire data set

#Remove duplicates by single column(Series.Title)
tvseries2 <- tvseries[!duplicated(tvseries$Series.Title), ]
length(tvseries2) # 9644 different titles

# Release.year

# Extracting release year from "Release.Year" and converting char to num
# Note: For the sake of simplicity I will keep only those with actual dates
head(tvseries2$Release.Year) # Note: date has year from - to, lets extract from year 
tvseries2$Release.Year <- substr(tvseries2$Release.Year, start = 2, stop = 5)

# Removing entries with wrong format dates
tvseries2$Release.Year <- as.numeric(tvseries2$Release.Year)
# Note: non-year entries coerced to NA, will deal with them later
str(tvseries2)

# Runtime (minutes)
tvseries2$Runtime <- as.numeric(substr(tvseries2$Runtime, start = 1 , stop = 3 ))
sum(is.na(tvseries2$Runtime))

# Genre
tvseries2$Genre <- as.factor(tvseries2$Genre)

# Rating
tvseries2$Rating <- as.numeric(tvseries2$Rating)

str(tvseries2)
dim(tvseries2)
sum(is.na(tvseries2)) # There are 2498 missing values

# Lets rank them by rating
tvseries2r <- tvseries2[order(tvseries2$Rating, decreasing = T),]
top100 <- head(tvseries2r, 10)
ggplot(top100, aes(x=Series.Title, y = Rating))
  + geom_bar(stat='identity') 
  + labs(title = "Top 10 Rated Shows", xlab = "Series")

