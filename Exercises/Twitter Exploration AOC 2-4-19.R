# "Twitter Data Exploration - Feb 4 2019"
# Rob Wells


# ------- Get Organized --------- #  

###Set Working Directory. My directory "Student-Loan-Data-R" is an example

getwd()
setwd("")

library(ggplot2)
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
#Question: How many rows?
#Question: Earliest tweet?

#What do these columns mean? Read this:
https://developer.twitter.com/en/docs/tweets/data-dictionary/overview/user-object

#Question #1: 
#
#What is the most popular tweet, what did it say and what day did it happen?
#
#Build a Smaller Table with four elements: 
#the date of the tweet, the text, count of favorites, count of retweets
#and sort by number of favorites
#Use your dplyr powers! See the course web page and earlier exercises for details
#Also consult this: How Do I?
  https://smach.github.io/R4JournalismBook/HowDoI.html

  
#Question #2: What is the third most popular retweet, what did it say and what day did it happen?


#Question #3:
#Using %>% count(COLUMN NAME) function in dplyr, find out the most commonly used stand-alone screen names in her tweets.
#Bonus nerd question: Then arrange it in dplyr descending
#Super mega bonus nerd question: Disaggregate all of the screen names and count them up 
  
  