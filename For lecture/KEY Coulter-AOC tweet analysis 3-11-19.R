#Coulter Tweet Analysis #2
#March 10 2019
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
install.packages("rio")


#Load the Coulter tweets
Coulter <- read_csv("./Data/Coulter.csv")

Coulter <- rio::import("./Data/Coulter.csv")
Coulter <- janitor::clean_names(Coulter)
colnames(Coulter)
str(Coulter)


 
# Here's an elegant way to create a Tweets by Month, Year Table
Coulter_y_Month <- Coulter %>%
  mutate(created_at = ymd_hms(created_at)) %>%
  mutate(yearmon= format(created_at, "%Y-%m")) %>%
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
Fav1 <- Coulter %>%
  select(screen_name, favorite_count, created_at) %>% 
  arrange(desc(favorite_count))

NotFav <- Coulter %>%
  select(screen_name, favorite_count, created_at) %>% 
  filter(favorite_count == 0)

Fav <- Coulter %>%
  select(screen_name, favorite_count, created_at) %>%
  filter(favorite_count > 0) %>%
  mutate(created_at = ymd_hms(created_at)) %>%
  mutate(yearmon= format(created_at, "%Y-%m")) %>%
  group_by(yearmon) %>%
  arrange(desc(favorite_count))

#Construct table with Fav
Fav2 <- Fav %>%
  summarize(fav_sum = sum(favorite_count)) 

#Bonus - Construct table with no Favs
NotFav2 <- Coulter %>%
  select(screen_name, favorite_count, created_at) %>%
  filter(favorite_count ==0) %>%
  mutate(created_at = ymd_hms(created_at)) %>%
  mutate(yearmon= format(created_at, "%Y-%m")) %>%
  group_by(yearmon) 

#Total No Favs
No_Fav_Total <- NotFav2 %>%
  group_by(yearmon) %>%
  count(yearmon) 


#plot Favs
ggplot(Fav)+
  aes(x = yearmon, y = favorite_count, fill = yearmon)+
  geom_col(show.legend = F) +
  theme_bw()+
  labs(title = "Ann Coulter Twitter engagement", 
       subtitle = "Ann Coulter Twitter Feed",
       caption = "Source: Twitter 2019",
       x="Year/Month",
       y="Tweets Favorites") 

#plot Not Favs
ggplot(No_Fav_Total)+
  aes(x = yearmon, y = n, fill = yearmon)+
  geom_col(show.legend = F) +
  theme_bw()+
  labs(title = "Ann Coulter's Lack of Twitter engagement", 
       subtitle = "Ann Coulter Twitter Feed",
       caption = "Source: Twitter 2019",
       x="Year/Month",
       y="Tweets Favorites")



#---------------------------------------------------------------------#
# Exercise #3: Measure how much engagement (likes) AOC gets over time
#---------------------------------------------------------------------#
# Measure how much engagement (likes) AOC gets over time
# Construct a table based on favorite count, chart it
# Favorite count
#plot

#Load the AOC tweets
AOC <- read_csv("./Data/AOC.csv")

AOC <- rio::import("./Data/AOC.csv")
AOC <- janitor::clean_names(AOC)
colnames(AOC)
str(AOC)

#
Fav_AOC <- AOC %>%
  select(screen_name, favorite_count, created_at) %>% 
  arrange(desc(favorite_count))

NotFav <- AOC %>%
  select(screen_name, favorite_count, created_at) %>% 
  filter(favorite_count == 0)

Fav_AOC <- AOC %>%
  select(screen_name, favorite_count, created_at) %>%
  filter(favorite_count > 0) %>%
  mutate(created_at = ymd_hms(created_at)) %>%
  mutate(yearmon= format(created_at, "%Y-%m")) %>%
  group_by(yearmon) %>%
  arrange(desc(favorite_count))
#plot
ggplot(Fav_AOC)+
  aes(x = yearmon, y = favorite_count, fill = yearmon)+
  geom_col(show.legend = F) +
  theme_bw()+
  labs(title = "AOC Twitter engagement", 
       subtitle = "AOC Twitter Feed",
       caption = "Source: Twitter 2019",
       x="Year/Month",
       y="Tweets Favorites") 

#deal with scales
#https://stackoverflow.com/questions/14563989/force-r-to-stop-plotting-abbreviated-axis-labels-e-g-1e00-in-ggplot2
## displays as you require
#require(scales)
#p + scale_x_continuous(labels = comma)

require(ggplot2)

require(scales)


ggplot(Fav_AOC)+
  aes(x = yearmon, y = favorite_count, fill = yearmon)+
  scale_y_continuous(labels = scales::comma) +
  geom_col(show.legend = F) +
  theme_bw()+
  labs(title = "AOC Twitter engagement", 
       subtitle = "AOC Twitter Feed",
       caption = "Source: Twitter 2019",
       x="Year/Month",
       y="Tweets Favorites") 

#---------------------------------------------------------------------#
# Exercise #5: Build Table with AOC, Coulter Engagement (likes)
# Plot over time
#---------------------------------------------------------------------#

#Build AOC monthly Tweets Table

AOC_y_Month <- AOC %>%
  mutate(created_at = ymd_hms(created_at)) %>%
  mutate(yearmon= format(created_at, "%Y-%m")) %>%
  group_by(yearmon) %>%
  count(yearmon)


#Add identifying label to the AOC and Coulter dataframes
#data$newcolumn<-"your text"
Coulter_y_Month$Name <- "Coulter"
AOC_y_Month$Name <- "AOC"

#Combine dataframes using rbind
AOC_Coulter_Mo <- rbind(AOC_y_Month, Coulter_y_Month)

#plot
ggplot(AOC_Coulter_Mo)+
  aes(x = yearmon, y = n, fill = Name)+
  geom_col() +
  theme_bw()+
  labs(title = "AOC-Coulter Tweets", 
       subtitle = "AOC-Coulter Twitter Feeds",
       caption = "Source: Twitter 2019",
       x="Year/Month",
       y="Tweets Per month") 

#---------------------------------------------------------------------#
# Exercise #6: Build Table with AOC, Coulter Immigration Narrative
# Plot over time
#---------------------------------------------------------------------#


#what is the capitalize =- false?

AOC_Immigration <- filter(AOC, grepl ("immigration", text,ignore.case=TRUE)) %>% 
  select(created_at, text)

Coulter_Immigration <- filter(Coulter, grepl ("immigration|ICE", text)) %>% 
  select(created_at, text)

Immig_AOC_y_Month <- AOC_Immigration %>%
  mutate(created_at = ymd_hms(created_at)) %>%
  mutate(yearmon= format(created_at, "%Y-%m")) %>%
  group_by(yearmon) %>%
  count(yearmon)

Immig_Coulter_y_Month <- Coulter_Immigration %>%
  mutate(created_at = ymd_hms(created_at)) %>%
  mutate(yearmon= format(created_at, "%Y-%m")) %>%
  group_by(yearmon) %>%
  count(yearmon)

#Add identifying label to the AOC and Coulter dataframes
#data$newcolumn<-"your text"
Immig_Coulter_y_Month$Name <- "Coulter"
Immig_AOC_y_Month$Name <- "AOC"

#Combine dataframes using rbind
Immig_AOC_Coulter <- rbind(Immig_Coulter_y_Month, Immig_AOC_y_Month)


#plot
ggplot(Immig_AOC_Coulter)+
  aes(x = yearmon, y = n, fill = Name)+
  geom_col() +
  theme_bw()+
  labs(title = "AOC-Coulter Immigration Tweets", 
       subtitle = "AOC-Coulter Twitter Feeds",
       caption = "Source: Twitter 2019",
       x="Year/Month",
       y="Tweets Per month") 


#---------------------------------------------------------------------#
# Notes below here


#Measuring Engagement
#---------------------------------------------------------------------#

# this code shows people and entities that coulter comment on the most on Twitter

quotes <- Coulter %>%
  count(quoted_name, sort = T) %>%
  na.exclude() %>%
  top_n(20)
ggplot(quotes)+
  aes(x = reorder(quoted_name,-n), y = n, fill = quoted_name)+
  geom_col(show.legend = F) +
  coord_flip()+
  labs(title = "Ann Coulter Quoted Sources", 
       subtitle = "Ann Coulter Twitter Feed",
       caption = "Source: Twitter 2019",
       x="Quoted Name",
       y="Count") 
# this code shows us who is engaging her in their conversations, soupposing she relpies to those who mention her on Twitter
engagement1 <- Coulter %>%
  count(reply_to_screen_name)%>%
  arrange(reply_to_screen_name)

#---------------------------------------------------------------------#
#this code shows who she is trying to intreact with the most (as an ally)
#---------------------------------------------------------------------#

mentions <- Coulter %>%
  count(mentions_screen_name, sort = T) %>%
  arrange(desc(n)) %>%
  na.exclude() %>%
  top_n(10)
ggplot(mentions)+
  aes(x = reorder(mentions_screen_name, -n), y = n, fill = mentions_screen_name)+
  geom_col(show.legend = F)+
  theme_bw()+
  coord_flip()+
  labs(title = "Coulter's Most Common Name Mentions on Twitter", y = "Count", x = "Name"  )

#---------------------------------------------------------------------#
# this code shows  how many times Coulter's Tweets whave been liked by others
#---------------------------------------------------------------------#

engagement2 <- Coulter %>%
  select(favorite_count, created_at) %>%
  filter(favorite_count >0) %>%
  mutate(created_at = ymd_hms(created_at)) %>%
  mutate(yearmon= format(created_at, "%Y-%m")) %>%
  group_by(yearmon) %>%
  arrange(favorite_count)
ggplot(engagement2)+
  aes(x = yearmon, y = favorite_count, fill= yearmon)+
  geom_col(show.legend = F)+
  labs(title = "Times Coulter's Tweets whave been liked by Ohers")
# this code shows many time Coulter's Tweets have been retweeted by others
engagement3 <- Coulter %>%
  select(retweet_count, created_at) %>%
  mutate(created_at = ymd_hms(created_at)) %>%
  mutate(yearmon= format(created_at, "%Y-%m")) %>%
  group_by(yearmon) %>%
  arrange(retweet_count)
ggplot(engagement3)+
  aes(x = yearmon, y = retweet_count, fill= yearmon)+
  geom_col(show.legend = F)+
  labs(title = "Coulter's Retweets Count")



#---------------------------------------------------------------------#
# Specific Twitter Narratives by Month
#---------------------------------------------------------------------#

#Coulter Twitter activity increased significantly in October and November
# what was she tweeting about in during this period
remove_reg <- "&amp;|&lt;|&gt;"
OctNov <- Coulter %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]")) %>%
  mutate(created_at = month(created_at, label = T)) %>%
  filter(created_at == c("Oct", "Nov")) %>%
  group_by(created_at) %>%
  count(word, sort = T) %>%
  top_n(10) %>%
  arrange(desc(n))

#Modifying to Sort by month and number descending
OctNov <- Coulter %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]")) %>%
  mutate(created_at = month(created_at, label = T)) %>%
  filter(created_at == c("Oct", "Nov")) %>%
  group_by(created_at) %>%
  count(word, sort = T) %>%
  top_n(10) %>%
  arrange(created_at, -n)


#plot
ggplot(OctNov)+
  aes(x = reorder(word,-n), y= n, fill= created_at)+
  geom_col()+
  theme_bw()+
  labs(
    title = "Ann Coulter top Subjects During October/Novemebr",
    y = "Count",
    x  = "word",
    legend = "Month"
  )

#Let's Break This Down!
#unnest_tokens
#filter stop word
#remove reg
#ggplot fill = created_at gives us two toned visualization




#---------------------------------------------------------------------#

# Hashtags
Hash <- Coulter %>%
  count(hashtags, sort = T) %>%
  arrange(desc(n)) %>%
  na.exclude()
# clean hashtags
Coulter2 <- Coulter %>%
  select(text, hashtags)
Coulter2$hashtag1 <- gsub("\\(", "", Coulter2$hashtags) 
Coulter2$hashtag1 <- gsub ("\\)", "", Coulter2$hashtag1)
Coulter2$hashtag1 <- gsub ("\"", "&", Coulter2$hashtag1)
Coulter2$hashtag1 <- gsub ("c&&", "", Coulter2$hashtag1)
Coulter2$hashtag1 <- gsub ("&&", "", Coulter2$hashtag1)
# seperate hashtags
Coulter2 <- separate(Coulter2, hashtag1, 
         c('hashtag1', 'hashtag2', 'hashtag3', 'hashtag4'), 
         sep=',', remove=TRUE)

#table with only hashtags 
Coulter2 <- select(Coulter2, hashtag1:hashtag4)
count <- table(unlist(Coulter2))
Hashtags <- as.data.frame(count)
#rename columns
colnames(Hashtags)[1] <- "hashtag"
colnames(Hashtags)[2] <- "count"

#Total by Column, Summarize by String#
hash_df <- Hashtags %>%
  separate_rows(hashtag, sep = ' ') %>%
  group_by(hashtag = tolower(hashtag)) %>%
  summarise(Count = n(), 
            ScoreSum = sum(count))

Hashtags <- select(hash_df, hashtag, ScoreSum) %>%
  arrange(desc(ScoreSum)) %>%
  top_n(15)

# omit the first row and avoid redundants 
Hashtags <- Hashtags[2:16,]
# plot
ggplot(Hashtags, aes(x = reorder(hashtag, -ScoreSum), y = ScoreSum, fill=hashtag))  +
  geom_bar(stat = "identity", show.legend = F) +
  coord_flip() +
  labs(title = "Top 15 hashtags In Ann Coulter Twitter Feed", 
       subtitle = "Ann Coulter Twitter Feed",
       caption = "Source: Twitter 2019",
       x="Word",
       y="Count of the hashtag usage") +
  
  theme_bw()

#----
# how often does she retweet others?
#retweet count VS actual tweets
ggplot(Coulter)+
  aes(x = is_retweet, fill = is_retweet)+
  geom_histogram(stat = "count", show.legend = F)+
  theme_bw()+
  labs(title = "Ann Coulter Tweets vs retweets", 
       subtitle = "Ann Coulter Twitter Feed",
       caption = "Source: Twitter 2019",
       x="Retweets",
       y="Tweets") 

# quoted_names
quotes <- Coulter %>%
  count(quoted_name, sort = T) %>%
  na.exclude() %>%
  top_n(20)
ggplot(quotes)+
  aes(x = reorder(quoted_name,-n), y = n, fill = quoted_name)+
  geom_col(show.legend = F) +
  coord_flip()+
  labs(title = "Ann Coulter Quoted Sources", 
       subtitle = "Ann Coulter Twitter Feed",
       caption = "Source: Twitter 2019",
       x="Quoted Name",
       y="Count") 


#Joining data frames              


#Another common issue for data journalists is having to join two datasets together 
#on one or more common fields/columns. Just like SQL, you can do an inner_join
#(return only matching records) or left_join (return all records from the 
#first table listed and only those that match from the second table) 
#using the dplyr package. Here's the official documentation on joining.
#In the code below you can replace the two instances of "fieldname" with 
#the correct names of columns in your two tables that match. 
#And replace "table1" and "table2" with the names of the two data frames 
#you are joining. 
#In my example, "newtable" is the name of the new data frame I'm creating 
#from this join.

#newtable <- inner_join(table1, table2, by=c("fieldname"="fieldname"))

AOC_y_Month <- as.data.frame(AOC_y_Month)
Coulter_y_Month <- as.data.frame(Coulter_y_Month)


AOC_Coulter_Mo <- inner_join(AOC_y_Month, Coulter_y_Month, by=c("yearmon"="yearmon"))

df2 <- right_join(AOC_y_Month, Coulter_y_Month, by="yearmon")


#Cross join: merge(x = df1, y = df2, by = NULL)
df1 <- merge(x=AOC_y_Month, y=Coulter_y_Month, by.x="yearmon", by.y="yearmon")

