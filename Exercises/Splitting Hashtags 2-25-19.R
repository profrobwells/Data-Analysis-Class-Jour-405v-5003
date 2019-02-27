#Splitting out hashtags
#Feb 25 2019


#Load the AOC tweets
AOC <- rio::import("./Data/AOC.csv")
AOC <- janitor::clean_names(AOC)
colnames(AOC)
str(AOC)


#------------------------------------------------#
#CREATE SMALLER TABLE WITH HASHTAGS AND URLS
#------------------------------------------------#
#Create a subset
library(dplyr)
library(tidyr)
AOC2 <- select(AOC, user_id, created_at, text, is_retweet, hashtags, urls_expanded_url)
View(AOC2)
colnames(AOC2)

#Why are we doing this?
#Total the hashtag column
hashtags <- AOC %>% count(hashtags, sort=TRUE) 

View(hashtags)

#We have a problem!   

#Data Cleaning - Split the URLS out#

#Consulted This Tutorial https://trendct.org/2015/06/12/r-for-beginners-how-to-transition-from-excel-to-r/#chapterTOC8 #

#Step 1 - Delete stray charcters  c( "" ) etc# 
#Put it in a new Column Called URL1#
AOC2$hashtag1 <- gsub("\\(", "", AOC2$hashtags) 
AOC2$hashtag1 <- gsub ("\\)", "", AOC2$hashtag1)
AOC2$hashtag1 <- gsub ("\"", "&", AOC2$hashtag1)
AOC2$hashtag1 <- gsub ("c&&", "", AOC2$hashtag1)
AOC2$hashtag1 <- gsub ("&&", "", AOC2$hashtag1)

#function{x <- gsub("\\(", "", x), return(x)}

#also to remove quotes: noquote(YOUR TEXT STRING)


#Step 2 - Separate hashtag1, which has the comma delimiters and dump in five new colums#
library(tidyr)
AOC3 <- separate(AOC2, hashtag1, 
                     c('hashtag1', 'hashtag2', 'hashtag3', 'hashtag4', 'hashtag5', 
                       'hashtag6', 'hashtag7', 'hashtag8', 'hashtag9', 'hashtag10'), 
                     sep=',', remove=TRUE)
View(AOC3)

#table with only hashtags 
AOC4 <- select(AOC3, hashtag1:hashtag10)

count <- table(unlist(AOC4))
hashtag2 <- as.data.frame(count)
#rename columns
colnames(hashtag2)[1] <- "hashtag"
colnames(hashtag2)[2] <- "count"

#Total by Column, Summarize by String#
df7 <- hashtag2 %>%
  separate_rows(hashtag, sep = ' ') %>%
  group_by(hashtag = tolower(hashtag)) %>%
  summarise(Count = n(), 
            ScoreSum = sum(count))

hashtags3 <- select(df7, hashtag, ScoreSum) %>% 
  arrange(desc(ScoreSum))

write.csv(hashtags, "AOC_hashtags.csv")

#Examine the table - what are the major narratives?

#Create a table with the original tweets by hashtag
#Filtered Table with abolishIce in Tweets
AOC_ICE <- filter(AOC2, grepl ("abolishICE", hashtag1)) %>% 
  select(created_at, text, hashtag1)

#abolishIce
#Look up AP on ProQuest in NYT when that narrative started

#pub.Exact("New York Times") AND abolishICE




#end of lesson for Wednesday
