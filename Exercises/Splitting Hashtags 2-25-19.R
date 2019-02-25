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
AOC2 <- select(AOC, user_id, created_at, text, is_retweet, hashtags, urls_expanded_url)
View(AOC2)
colnames(AOC2)
#Data Cleaning - Split the URLS out#

#Consulted This Tutorial https://trendct.org/2015/06/12/r-for-beginners-how-to-transition-from-excel-to-r/#chapterTOC8 #

#Step 1 - Delete stray charcters  c( "" ) etc# 
#Put it in a new Column Called URL1#
AOC2$hashtag1 <- gsub("\\(", "", AOC2$hashtags) 
AOC2$hashtag1 <- gsub ("\\)", "", AOC2$hashtag1)
AOC2$hashtag1 <- gsub ("\"", "&", AOC2$hashtag1)
AOC2$hashtag1 <- gsub ("c&&", "", AOC2$hashtag1)
AOC2$hashtag1 <- gsub ("&&", "", AOC2$hashtag1)

#also to remove quotes: noquote(YOUR TEXT STRING)


#Step 2 - Separate hashtag1, which has the comma delimiters and dump in five new colums#
library(tidyr)
AOC3 <- separate(AOC2, hashtag1, 
                     c('hashtag1', 'hashtag2', 'hashtag3', 'hashtag4', 'hashtag5', 
                       'hashtag6', 'hashtag7', 'hashtag8', 'hashtag9', 'hashtag10'), 
                     sep=',', remove=TRUE)
View(AOC3)

#table with only hashtags and dates
AOC4 <- select(AOC3, hashtag1:hashtag10)

count <- table(unlist(AOC4))
df6 <- as.data.frame(count)
#rename columns
colnames(df6)[1] <- "hashtag"
colnames(df6)[2] <- "count"

#Total by Column, Summarize by String#
df7 <- df6 %>%
  separate_rows(hashtag, sep = ' ') %>%
  group_by(hashtag = tolower(hashtag)) %>%
  summarise(Count = n(), 
            ScoreSum = sum(count))

hashtags <- select(df7, hashtag, ScoreSum) %>% 
  arrange(desc(ScoreSum))

write.csv(hashtags, "AOC_hashtags.csv")

#end of lesson for Monday
