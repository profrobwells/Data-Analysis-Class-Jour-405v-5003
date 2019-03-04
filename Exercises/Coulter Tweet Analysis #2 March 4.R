#Coulter Tweet Analysis #2
#March 4 2019
# Thanks to Mohamed M'Bareck for these ideas
# Shout out to Alex Nicoll for graphics material 

#Load 'er up, Bob!
library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)
library(tidytext)
library(stringr)
library(kableExtra)
library(knitr)

#Load the Coulter tweets
#Coulter <- rio::import("./Data/Coulter.csv")
Coulter <- rio::import("Coulter.csv")
Coulter <- janitor::clean_names(Coulter)
colnames(Coulter)
str(Coulter)

#Mohamed's elegant way to create a Tweets by Month, Year Table
Coulter_y_Month <- Coulter %>%
  mutate(created_at = ymd_hms(created_at)) %>%
  mutate(yearmon= format(created_at, "%Y-%m")) %>%
  group_by(yearmon) %>%
  count(yearmon)

# Alex Nicoll's publication-ready graphic - charting extras
ggplot(Coulter_y_Month, aes(x = yearmon, y = n, color = yearmon, fill=yearmon))  +
  geom_bar(stat = "identity") +
  geom_text(aes(label=n), colour="black", vjust=-0.3, size=3.5) +
  expand_limits(y = c(0,900)) +
  scale_x_discrete(breaks=c("2018-09","2018-10","2018-11","2018-12","2019-01"),
                   labels=c("Sept. 2018", "Oct. 2018", "Nov. 2018", "Dec. 2018", "Jan. 2019")) +
  scale_y_continuous(breaks=c(150,300, 450, 600, 750, 900))+
  labs(title = "Number of Tweets per Month", 
       subtitle = "Ann Coulter Twitter Feed",
       caption = "Source: Twitter 
       Graphic by Alex Nicoll",
       x="Month",
       y="Number of Tweets") +
  theme(legend.position="none")


#Let's break this down
#Line 50 adds the numeric labels
#Line 51 Gives us more vertical room on the chart
#Lines 52-53 Convert the month labels to text
#Line 54 gives a nice background grid for Y axis

#Let's look at geom_label and geom_text
?? geom_label

#Examine the exercise
p <- ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars)))
p + geom_text()
# Avoid overlaps
p + geom_text(check_overlap = TRUE)
# Labels with background
p + geom_label()
# Change size of the label
p + geom_text(size = 10)
# Set aesthetics to fixed value
p + geom_point() + geom_text(hjust = 0, nudge_x = 0.05)
p + geom_point() + geom_text(vjust = 0, nudge_y = 0.5)
p + geom_point() + geom_text(angle = 45)
p + geom_text(family = "Times New Roman")


#Stack the various options
ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars))) +
  geom_label(size =2.5) 

#Points on chart, reduced overlap
ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars))) +  
  geom_point() + 
  geom_text(size = 2.5, hjust = 0, nudge_x = 0.05, check_overlap = TRUE)

#scale the text by its weight
p + geom_text(aes(size = wt))
#scale by height
p + geom_text(aes(size = wt)) + scale_radius(range = c(3,6))



#---------------------------------------------------------------------#
# Exercise #1: Chart frequency of Trump tweets #
#---------------------------------------------------------------------#

# Create a table with tweets that mention Trump

#Process the dates for year_mo

#Chart using the Nicoll labels etc

#---------------------------------------------------------------------#
#Part 2: Let's Analyze News and Twitter Narratives #
#---------------------------------------------------------------------#

#Formating the Tweets in kable


#Save as html file using export function in Plots viewer

#---------------------------------------------------------------------#
# Exercise #2: Measure how much engagement (likes) Coulter gets over time
#---------------------------------------------------------------------#
# Measure how much engagement (likes) Coulter gets over time
# Construct a table based on favorite count, chart it
# Favorite count
#plot

#---------------------------------------------------------------------#
# Measuring Engagement
#---------------------------------------------------------------------#

# TK
