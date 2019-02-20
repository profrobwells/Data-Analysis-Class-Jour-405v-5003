# "Twitter Data Exploration - Feb 4 2019"
# Rob Wells

#KEY
#KEY

# ------- Get Organized --------- #  

###Set Working Directory. My directory "Student-Loan-Data-R" is an example

getwd()
setwd("")


library (tidyverse)
library (readxl)
library (dplyr)

#-------------------------------------------------------------------#
#Load the Ocasio-Cortez Twitter data 
#-------------------------------------------------------------------#
#Pull in Alexandra Ocasio-Cortez Twitter feed
#Load Data
AOC <- rio::import("https://raw.githubusercontent.com/profrobwells/Data-Analysis-Class-Jour-405v-5003/master/Data/AOC.csv")

#Clean labels
AOC <- janitor::clean_names(AOC)

#Look at table
View(AOC)

ncol(AOC)
nrow(AOC)
colnames(AOC)
str(AOC)

#Question: How many rows?
#Question: Earliest tweet?
#Answer - Sort headers in table
#2018-04-17 16:55:53


#What do these columns mean?
https://developer.twitter.com/en/docs/tweets/data-dictionary/overview/user-object

#Question #1: What is the most popular tweet, what did it say and what day did it happen?
#Build a Smaller Table with four elements: 
#the date of the tweet, the text, count of favorites, count of retweets
#and sort by number of favorites

#Answer
library(dplyr)
AOC2 <- AOC %>%select(created_at, text, favorite_count, retweet_count) %>%arrange(desc(favorite_count)) 
#I hear the GOP thinks women dancing are scandalous. Wait till they find out Congresswomen dance too! 💃🏽 Have a great weekend everyone :) https://t.co/9y6ALOw4F6
#Retweeted 785,382 times
#Jan 4, 2019

#Question #2: What is the third most popular retweet, what did it say and what day did it happen?

#Answer
AOC3 <- AOC %>%select(created_at, text, favorite_count, retweet_count) %>%arrange(desc(retweet_count)) 
#Oct 18, 2018
#President @BarackObama doesn't have time for these 7 excuses not to vote. https://t.co/2Etpm6taTq 
#--111,234 times

#Question #3:
#Using %>% count(COLUMN NAME) function in dplyr, find out the most commonly used stand-alone screen names in her tweets.
#Bonus nerd question: Then arrange it in dplyr descending
#Super mega bonus nerd question: Disaggregate all of the screen names and count them up 

AOC4 <- AOC %>% select(created_at, text, mentions_screen_name) %>% count(mentions_screen_name)%>%arrange(desc(n)) 
