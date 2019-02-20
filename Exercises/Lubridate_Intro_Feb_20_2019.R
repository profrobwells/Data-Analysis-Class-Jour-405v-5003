#Introduction to Lubridate
#Tutorials Based on Andrew Ba Tran dealing-with-dates.R

#Install lubridate if you haven't already
#install.packages("lubridate")

#Load some data

some_date <- "12-31-1999"


# NOTE: IF YOU GET AN ERROR ABOUT NOT HAVING A PACKAGE CALLED stringi
# UNCOMMENT AND RUN THE LINES BELOW IF YOU HAVE A MAC MACHINE

#install.packages("glue", type="mac.binary")
#install.packages("stringi", type="mac.binary")
#install.packages("stringr", type="mac.binary")
#install.packages("lubridate", type="mac.binary")

#Ok, here we go

library(lubridate)

mdy(some_date)

data <- data.frame(First=c("Charlie", "Lucy", "Peppermint"),
                   Last=c("Brown", "van Pelt", "Patty"),
                   birthday=c("10-31-06", "2/4/2007", "June 1, 2005"))

data$DOB <- mdy(data$birthday)

data

data$year <- year(data$DOB)
data$month <- month(data$DOB, label=TRUE)
data$day <- day(data$DOB)
data$weekday <- wday(data$DOB, label=TRUE, abbr=FALSE)

data

#------------------------------------------------------#
#Now, apply this to the AOC data
#------------------------------------------------------#


#Load the AOC tweets
AOC <- rio::import("./Data/AOC.csv")
AOC <- janitor::clean_names(AOC)
colnames(AOC)
str(AOC)

#Load dplyr
library(dplyr)
#Let's trim down AOC to just date, text, hashtags
AOCdates <- ???????

str(AOCdates)
head(AOCdates)

#------------------------------------------------------#
#Question: What is the data type of the date field?
#------------------------------------------------------#

#Separate into new date and time columns
library(dplyr)
library(tidyr)

#Separate into new columns
#Thanks to https://stackoverflow.com/questions/4350440/split-data-frame-string-column-into-multiple-columns

AOCdates2 <- AOCdates %>% 
  separate(created_at, c("date", "seconds"), " ")

#------------------------------------------------------#
#Question 2
#------------------------------------------------------#
#Create Separate Year Field



#Create Separate Month Column


#Create Separate Day Column


#------------------------------------------------------#
#Question 3
#------------------------------------------------------#

#Calculate Total Tweets by Day

#Calculate Total Twwets by Month


#------------------------------------------------------#
#Question 4
#------------------------------------------------------#



#Graph Tweets by Month
library(ggplot2)

ggplot(AOCmonth????