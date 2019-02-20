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



#End of Lesson for Now

# We're going to use the now() function which brings in the date for today

today <- now()
data$age <- difftime(today, data$DOB)

data

data$age_years <- as.numeric(data$age) / 365.25 #.25 because of leap years

data


#Andrew Ba Tran 
#Lubridate Exercise from Learn 4 Ch. 4



# If you don't have readr installed yet, uncomment and run the line below
install.packages("readr")

library(readr)

temps <- read_csv("For lecture/Lubridate/110-tavg-all-5-1895-2018.csv", skip=4)
head(temps)

# install.packages("lubridate")

# If you don't have lubridate installed yet uncomment the line below and run it
#install.packages("lubridate")

# NOTE: IF YOU GET AN ERROR ABOUT NOT HAVING A PACKAGE CALLED stringi
# UNCOMMENT AND RUN THE LINES BELOW IF YOU HAVE A WINDOWS MACHINE

#install.packages("glue", type="win.binary")
#install.packages("stringi", type="win.binary")
#install.packages("stringr", type="win.binary")
#install.packages("lubridate", type="win.binary")

# UNCOMMENT AND RUN THE LINES BELOW IF YOU HAVE A MAC MACHINE

#install.packages("glue", type="mac.binary")
#install.packages("stringi", type="mac.binary")
#install.packages("stringr", type="mac.binary")
#install.packages("lubridate", type="mac.binary")

library(lubridate)

# Converting the date into a date format R recognizes
# This requires using paste0() to add a day to the date, so 189501 turns into 18950101

temps$Date <- ymd(paste0(temps$Date, "01"))

# Extracting the year
temps$Year <- year(temps$Date)

# Extracting the month
temps$month <- month(temps$Date)
temps$month <- as.numeric(temps$month)
temps$month_label <- month(temps$Date, label=T)

# Creating a column with rounded numbers
temps$rounded_value <- round(temps$Value, digits=0)

# Turning the year into a factor so it'll chart easier
temps$Year <- as.factor(as.character(temps$Year))



head(temps)

# If you don't have readr installed yet, uncomment and run the line below
# install.packages("ggplot2")

library(ggplot2)

ggplot(temps, aes(x=month, y=Value, group=Year)) +
  geom_line(alpha=.5) +
  scale_x_continuous(breaks=seq(1,12,1), limits=c(1,12)) +
  theme_minimal() +
  labs(y="average temperature", title="Monthly temperature since 1895", caption="Source: NOAA")


# If you don't have readr installed yet, uncomment and run the line below
install.packages("gghighlight")

library(gghighlight)

# adding some alpha to the line so there's some transparency
ggplot(temps, aes(x=month, y=Value, color=Year)) +
  geom_line(alpha=.8) +
  scale_x_continuous(breaks=seq(1,12,1), limits=c(1,12)) +
  theme_minimal() +
  labs(y="average temperature", title="Monthly temperature since 1895", caption="Source: NOAA") +
  # NEW CODE BELOW
  gghighlight(max(as.numeric(Year)), max_highlight = 4L)



