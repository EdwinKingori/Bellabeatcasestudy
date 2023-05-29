# Bellbeat_Case_study
# Loading packages
library(tidyverse)
library(lubridate)
library(skimr)
library(dplyr)
library(janitor)
library(ggplot2)
library(selectr)

#(A) Preparing Data
#importing files

activity <- read.csv("dailyActivity_merged.csv")
calories <- read.csv("dailyCalories_merged.csv")
intensities <- read.csv("dailyIntensities_merged.csv")
steps <- read.csv("dailySteps_merged.csv")
sleepday <- read.csv("sleepDay_merged.csv")
Weight <- read.csv("weightLogInfo_merged.csv")

head(activity)
head(calories)
head(intensities)
head(steps)
head(sleepday)
head(Weight)

#data cleaning
#cleaning the  column names
activity1 <- janitor::clean_names(activity)
calories1 <- janitor::clean_names(calories)
intensities1 <- janitor::clean_names(intensities)
steps1 <- janitor::clean_names(steps)
sleepday1 <- janitor::clean_names(sleepday)
Weight1 <- janitor::clean_names(Weight)

colnames(activity1)
colnames(calories1)
colnames(intensities1)
colnames(sleepday1)
colnames(steps1)

# (B) Analyzing Data
#Summarizing data

#Checking sample size to decide whether to use for analysis
n_distinct(activity$Id)
n_distinct(calories$Id)
n_distinct(intensities$Id)
n_distinct(steps$Id)
n_distinct(sleepday$Id)
n_distinct(Weight$Id)

# Conclusion 
# 8 participants in the weight data sets are not a significant number to make a comprehensive analytic decision.

#Checking the summary statistics of the data sets.
activity1 %>%
  select(total_steps,
         total_distance,
         sedentary_minutes, calories) %>%
  summary()
#summarizing the number of active minutes per category
activity1 %>%
  select(very_active_minutes,
         fairly_active_minutes,
         lightly_active_minutes) %>%
  summary()
#calories summary
calories1 %>%
  select(calories) %>%
  summary()
#Intensities 
#summarizing the number of active minutes per category
intensities1 %>%
  select(very_active_minutes,
         fairly_active_minutes,
         lightly_active_minutes) %>%
  summary()
#Sleep
sleepday1 %>%
  select(total_sleep_records,
         total_minutes_asleep,
         total_time_in_bed) %>%
  summary()

# Discoveries made from the summary
#1: Most participants are lightly active
#2: Most participants spend an approximated 7 hours of sleep
#3: Most participants average around 7400 steps, resulting them to burn around 2300 calories a day
#4: the average sedentary time is 991, an equivalent of 16 hours which is lower than the medically required 4-8hours a day (https://bmcpublichealth.biomedcentral.com/articles/10.1186/s12889-023-15029-8). 

# formatting 
sleepday1$sleep_day <- as.Date(sleepday1$sleep_day, format = "%m/%d/%Y")
activity1$activity_date <- as.Date(activity1$activity_date, format = "%m/%d/%Y")
head(sleepday1)
head(activity1)



#Data Aggregation
#Merging Data
merged_data <- merge (x = sleepday1, y =  activity1, by = c('id', 'sleep_day'))
merged_data <- distinct(merged_data, .keep_all = TRUE)
head(merged_data)
merged_data2 <- merge(x = steps1, y =  activity1, by = 'id')
head(merged_data2)

#Visualization 
ggplot(data  = activity1)+
  geom_point(mapping = aes(x = total_steps, y = calories), color = 'blue')+
  geom_smooth(mapping = aes(x = total_steps, y = calories), method = 'loess')+
  labs(title = "Total Steps Vs Calories")

ggplot(data = merged_data, aes(x = total_steps, y = calories))+
  geom_point(color = 'darkblue')+
  geom_smooth()+
  labs(title = 'Steps vs Calories')

#Calories burnt by steps and distance.

# Relationship Between Calories and Total Steps by Activity Date
ggplot(data = merged_data, aes(x=calories, y= total_steps))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~activity_date)+
  labs(title = "Relationship Between Calories and Total Steps by Activity Date")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = merged_data, aes(x=calories, y= total_distance))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~activity_date)+
  labs(title = "Relationship Between Calories and Total Distance by Activity Date")+
  theme(plot.title = element_text(hjust = 0.5))

#Exploring the relationship between steps taken in a day and sedentary minutes

ggplot(data = merged_data, aes(x = total_steps, y = sedentary_minutes))+
  geom_point()+
  geom_smooth()+
  labs(title = "Sedentary Minutes vs Total steps Taken")+
  theme(plot.title = element_text(hjust = 0.5))
# the plot shows the need for the company to sensitize its users to start  walking more

#Exploring the Relationship Between Minutes Asleep and Time in Bed
ggplot(data = sleepday1, aes(x= total_minutes_asleep, y = total_time_in_bed))+
  geom_point()+
  geom_smooth()+
  labs(title = "Relationship Between Minutes Asleep and Time in Bed")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = merged_data, aes(x= total_minutes_asleep, y = sedentary_minutes))+
  geom_point(color ="darkblue")+
  geom_smooth()+
  labs(title = "Relationship Between Minutes Asleep and Time in Bed")+
  theme(plot.title = element_text(hjust = 0.5))
# the plot shows that for users to improve their sleep, the company can recommend reducing sedentary time






  