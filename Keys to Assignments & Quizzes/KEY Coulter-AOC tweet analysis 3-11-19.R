 #Coulter Tweet Analysis #2
#March 11 2019
# Thanks to Mohamed M'Bareck for these ideas
# Shout out to Alex Nicoll for graphics material 
# Thanks to my class for their patience

#Load 'er up, Bob!

library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)
library(tidytext)
library(stringr)
library(kableExtra)
library(knitr)
install.packages("rio")


#Load the Coulter tweets
Coulter <- rio::import("./Data/Coulter.csv")
Coulter <- janitor::clean_names(Coulter)
colnames(Coulter)
str(Coulter)

#Process Tweets Lubidate - New YearMon Column
Coulter <- Coulter %>%
  mutate(created_at = ymd_hms(created_at)) %>%
  mutate(yearmon= format(created_at, "%Y-%m"))

#Simplified table Tweets by Month, Year Table
Coulter2 <- Coulter %>%
  select(created_at, yearmon, text, screen_name, favorite_count)
 
# Here's an elegant way to create a Tweets by Month, Year Table
Coulter_y_Month <- Coulter %>%
  select(created_at, yearmon, text, screen_name) %>%   
  group_by(yearmon) %>%
  count(yearmon)


#Load the AOC tweets
AOC <- rio::import("./Data/AOC.csv")
AOC <- janitor::clean_names(AOC)
colnames(AOC)
str(AOC)

#Process Tweets Lubidate - New YearMon Column
AOC <- AOC %>%
  mutate(created_at = ymd_hms(created_at)) %>%
  mutate(yearmon= format(created_at, "%Y-%m"))

#Simplified table Tweets by Month, Year Table
AOC2 <- AOC %>%
  select(created_at, yearmon, text, screen_name, favorite_count)

# Here's an elegant way to create a Tweets by Month, Year Table
AOC_y_Month <- Coulter %>%
  select(created_at, yearmon, text, screen_name) %>%   
  group_by(yearmon) %>%
  count(yearmon)

#------------------
# Alex Nicoll graphic
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

#Nicoll Graphic with Charting Extras
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
# Exercise #1: Chart frequency of Coulter-Trump tweets #
#---------------------------------------------------------------------#

# Create a table with tweets that mention Trump
Coulter_Trump <- filter(Coulter, grepl ("Trump", text)) %>% 
  select(created_at, text, is_retweet, hashtags, urls_expanded_url)

#Process the dates for year_mo
CoulterTrump_yearmo <- Coulter_Trump %>%
  mutate(created_at = ymd_hms(created_at)) %>%
  mutate(yearmon= format(created_at, "%Y-%m")) %>%
  group_by(yearmon) %>%
  count(yearmon)

#Chart using the Nicoll labels etc
ggplot(CoulterTrump_yearmo, aes(x = yearmon, y = n, color = yearmon, fill=yearmon))  +
  geom_bar(stat = "identity") +
  geom_text(aes(label=n), colour="black", vjust=-0.3, size=3.5) +
  expand_limits(y = c(0,125)) +
  scale_x_discrete(breaks=c("2018-09","2018-10","2018-11","2018-12","2019-01"),
                   labels=c("Sept. 2018", "Oct. 2018", "Nov. 2018", "Dec. 2018", "Jan. 2019")) +
  scale_y_continuous(breaks=c(25,50,75,100,125))+
  labs(title = "Trump Tweets per Month", 
       subtitle = "Ann Coulter Twitter Feed",
       caption = "Source: Twitter 
       Graphic by YOU AND Alex Nicoll",
       x="Month",
       y="Number of Tweets") +
  theme(legend.position="none")

#---------------------------------------------------------------------#
#Part 2: Let's Analyze News and Twitter Narratives #
#---------------------------------------------------------------------#

#Formating the Tweets in kable
TweetText_Table <- Coulter %>%
  select(created_at, text) %>% 
  kable("html") %>%
  kable_styling("striped")
#Save as html file using export function in Plots viewer

#---------------------------------------------------------------------#
# Exercise #2: Measure how much engagement (likes) Coulter gets over time
#---------------------------------------------------------------------#
# Construct a table based on favorite count, chart it
#
# Measure how much engagement (likes) Coulter gets over time
# Favorite count
CoulterFav <- Coulter2 %>%
  select(screen_name, favorite_count, created_at, yearmon) %>% 
  arrange(desc(favorite_count))

#Construct table with Fav
Fav2 <- Coulter2 %>%
  select(favorite_count, yearmon) %>% 
  group_by(yearmon) %>% 
  summarize(fav_sum = sum(favorite_count))

#plot Favs
ggplot(CoulterFav)+
  aes(x = yearmon, y = favorite_count, fill = yearmon)+
  geom_col(show.legend = F) +
  theme_bw()+
  labs(title = "Ann Coulter Twitter engagement", 
       subtitle = "Ann Coulter Twitter Feed",
       caption = "Source: Twitter 2019",
       x="Year/Month",
       y="Tweets Favorites") 


#---------------------------------------------------------------------#
# Exercise #3, #4, #5: Measure how much engagement (likes) AOC, Coulter get over time
#---------------------------------------------------------------------#
# Measure how much engagement (likes) AOC gets over time
# Construct a table based on favorite count, chart it
# Favorite count
#plot


Fav_Coulter <- Coulter %>%
  select(screen_name, favorite_count, created_at, yearmon) %>% 
  arrange(desc(favorite_count))

# Favorite count
Fav_AOC <- AOC %>%
  select(screen_name, favorite_count, created_at, yearmon) %>% 
  arrange(desc(favorite_count))

#Construct table with Fav
AOCFav2 <- Fav_AOC %>%
  select(screen_name, favorite_count, yearmon) %>% 
  group_by(yearmon, screen_name) %>% 
  summarize(fav_sum = sum(favorite_count))

CoulterFav2 <- Fav_Coulter %>%
  select(screen_name, favorite_count, yearmon) %>% 
  group_by(yearmon, screen_name) %>% 
  summarize(fav_sum = sum(favorite_count))


#Combine dataframes using rbind
FAVS <- rbind(AOCFav2, CoulterFav2)


#plot: deal with scales
#https://stackoverflow.com/questions/14563989/force-r-to-stop-plotting-abbreviated-axis-labels-e-g-1e00-in-ggplot2
## displays as you require
#require(scales)
#p + scale_x_continuous(labels = comma)
require(ggplot2)
require(scales)

#plot
ggplot(FAVS)+
  aes(x = yearmon, y = fav_sum, fill = screen_name)+
  scale_y_continuous(labels = scales::comma) +
  geom_col(position = "dodge") +
  theme_bw()+
  labs(title = "AOC-Coulter engagement", 
       subtitle = "AOC Twitter Feed",
       caption = "Source: Twitter 2019",
       x="Year/Month",
       y="Tweets Favorites") 

#---------------------------------------------------------------------#
# Exercise #6: Build Table with AOC, Coulter Immigration Narrative
# Plot over time
#---------------------------------------------------------------------#

AOC_immigration <- filter(AOC, grepl("immigration|wall|mexico|border|immigrant|immigrants|Mexican",text, ignore.case = TRUE)) %>% 
  select(created_at, text, screen_name, yearmon)


Coulter_immigration <- filter(Coulter, grepl("immigration|wall|mexico|border|immigrant|immigrants|Mexican",text, ignore.case = TRUE)) %>% 
  select(created_at, text, screen_name, yearmon)

#Combine dataframes using rbind
Immig_AOC_Coulter <- rbind(AOC_immigration, Coulter_immigration)

Immig_sum <- Immig_AOC_Coulter %>%
  select(screen_name, yearmon) %>% 
  summarize(fav_sum = sum(yearmon))


#Summary table with dates and screen name
Immig_sum <- Immig_AOC_Coulter %>%
  select(screen_name, yearmon) %>% 
  group_by(yearmon, screen_name) %>%
  count(yearmon)


#plot
ggplot(Immig_sum)+
  aes(x = yearmon, y = n, fill = screen_name)+
  geom_col(position = "dodge") +
  theme_bw()+
  labs(title = "AOC-Coulter Immigration Tweets", 
       subtitle = "AOC-Coulter Twitter Feeds",
       caption = "Source: Twitter 2019",
       x="Year/Month",
       y="Tweets Per month") 


