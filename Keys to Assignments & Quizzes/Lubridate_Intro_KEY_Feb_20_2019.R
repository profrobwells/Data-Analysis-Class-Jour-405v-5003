#Introduction to Lubridate - Answer KEY
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

#Now, apply this to the AOC data
#Load the AOC tweets
AOC <- rio::import("./Data/AOC.csv")
AOC <- janitor::clean_names(AOC)
colnames(AOC)
str(AOC)

#Load dplyr
library(dplyr)
#Let's trim down AOC to just date, text, hashtags
AOCdates <- AOC %>% select(created_at, text, hashtags)

str(AOCdates)
head(AOCdates)

#Question: What is the data type of the date field?

#Separate into new date and time columns
library(dplyr)
library(tidyr)

#Separate into new columns
#Thanks to https://stackoverflow.com/questions/4350440/split-data-frame-string-column-into-multiple-columns

AOCdates2 <- AOCdates %>% 
  separate(created_at, c("date", "seconds"), " ")

#Create Separate Year Field
#Create random date
library(lubridate)
some_date <- "2019-01-01"
ymd(some_date)


#New separate date field
AOCdates2$date2 <- ymd(AOCdates2$date)
str(AOCdates2)

AOCdates2$year <- year(AOCdates2$date2)
AOCdates2$month <- month(AOCdates2$date2, label=TRUE)
AOCdates2$day <- day(AOCdates2$date2)
AOCdates2$weekday <- wday(AOCdates2$date2, label=TRUE, abbr=FALSE)

#Total Tweets by Day
AOCDaytotal <- AOCdates2 %>% 
  count(date) %>% 
  group_by(date) %>% 
  arrange(desc(n))


#Total Tweets by Month
AOCmonth <- AOCdates2 %>% 
  count(month) %>% 
  group_by(month) %>% 
  arrange(desc(n))


library(ggplot2)

ggplot(AOCmonth, aes(x=month, y=n, color = month, fill=month)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.2, hjust = 2.1)) +
  labs(y="tweets", title="Alexandria Ocasio_Cortez Twitter Activity", caption="Source: Twitter, 2018")+
  theme(legend.position="none") 

#option 2
tweetplot <- ggplot(AOCmonth, aes(x=month, y=n, fill=month)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Monthly Tweets", x="month", y="tweets", caption = "Source: Twitter") +
  theme_bw() 

plot(tweetplot)   

#Solve the Date Problem
#
# What does R need to the graphic sort properly?
#
#Use this as a template
#https://stackoverflow.com/questions/46691933/r-sort-by-year-then-month-in-ggplot2

#Once you figured this out, open Splitting Hashtags 2-25-19



