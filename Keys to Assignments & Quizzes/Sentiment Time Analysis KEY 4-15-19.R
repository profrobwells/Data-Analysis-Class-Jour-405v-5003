# Sentiment Analysis Over Time
#  April 15, 2019
# Updated with Coulter exercises
#Based on these tutorials: http://varianceexplained.org/r/trump-tweets/ 
#https://rtweet.info/
#https://rtweet.info/articles/auth.html


#Set Working Directory#
setwd("Your directory")
#load packages needed for this; needs to be done every time
#these are all part of tidyverse

#Install packages if you haven't already
#install.packages("twitteR")
#install.packages("rtweet")
#install.packages("stringr")
#install.packages("tidytext") 
## install httpuv if not already
if (!requireNamespace("httpuv", quietly = TRUE)) {
  install.packages("httpuv")
}

#Call libraries
library(twitteR)
library(rtweet)
library(stringr)
library(tidytext)
library(tidyverse)
library(lubridate)
library(ggthemes) 
library(purrr)
library(rio)
library(tidyr)

#------------------------------------------------------------------------------#
#                       Load and Process Tables of Twitter Data                #
#------------------------------------------------------------------------------#

#
#Import Twitter data
AOC <- rio::import("./data/AOC.csv")
#
#Smaller table for this exercise
#
AOC2 <- select(AOC, V1, user_id, created_at, screen_name, 
               text, source, is_retweet, favorite_count, 
               retweet_count, hashtags)
#
#Process with lubridate
#
AOC2 <- AOC2 %>% 
  separate(created_at, c("date", "seconds"), " ")
#
some_date <- "2019-01-01"
ymd(some_date)
#
#New separate date field
AOC2$date2 <- ymd(AOC2$date)
str(AOC2)
#
#Disaggregate by day, month, weekday
#
AOC2$year <- year(AOC2$date2)
AOC2$month <- month(AOC2$date2, label=TRUE)
AOC2$day <- day(AOC2$date2)
AOC2$weekday <- wday(AOC2$date2, label=TRUE, abbr=FALSE)

#------------------------------------------------------------------------------#
#                       ANALYZE WORDS                       #
#------------------------------------------------------------------------------#
#from book: https://www.tidytextmining.com/tidytext.html #
#from tutorial: http://varianceexplained.org/r/trump-tweets/

#Define characters
reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
#
#
AOC3 <- AOC2 %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))



#------------------------------------------------------------------------------#
#                               Sentiment Analysis
#------------------------------------------------------------------------------#
#Based on these tutorials: http://varianceexplained.org/r/trump-tweets/ 
# http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm
#install.packages("sentimentr")
library(sentimentr)

#The sentiments dataset
library(tidytext)
sentiments

get_sentiments("afinn")

#Sentiments
#AFINN rates words positive - negative: https://rdrr.io/cran/corpus/man/sentiment_afinn.html
AFINN <- get_sentiments("afinn")
#
#Tidy table, date for AOC - Twitter data
AOC_tidy <- AOC3 %>%
  group_by(date2) %>%
  mutate(linenumber = row_number(),
         tweet_number = V1)

#Joins the Twitter data to sentiment scoring
AOC_afinn <- AOC3 %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(V1) %>% 
  summarise(sentiment = sum(score)) 
#
#Using date as grouping variable
#
AOC_Date <- AOC3 %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(V1, date2) %>% 
  summarise(sentiment = sum(score)) %>% 
  arrange(desc(sentiment))
#
#Adding sentiment score to the full corpus of Tweets
#This is a big deal
#Full join: Gives us 3193 observations
AOC4 <- AOC_Date %>%
  full_join(AOC2) %>% 
  group_by(V1, date2)
#
#Question: Find the most negative tweet.
#Hint: kable might be useful for reading the text
#
#Formating the Tweets in kable
library(kableExtra)
#
TweetText_Table <- AOC2 %>%
  select(V1, date, text)

TweetText_Table %>% 
  kable() %>%
  kable_styling("striped")
#

#Question: Create a table with tweets with a rating more negative than -10.
#Question: Create a table with tweets with a rating more positive than 10.
#Hint: Dplyr is your buddy
#
library(dplyr)
#Using Mohamed's Top N function
TopAOCNegative <- AOC4 %>% 
  filter(sentiment < -10)

TopAOCPositive <- AOC4 %>% 
  filter(sentiment > 10)

#
#Here it is, plotting sentiment over time
ggplot(AOC_Date, aes(date2, sentiment, color = sentiment, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  labs(title = "AOC Tweets: Sentiment", 
       subtitle = "Postive, Negative Sentiment in AOC Tweets, 2018-2019",
       caption = "Source: Twitter 
       Graphic by Rob Wells",
       x="Month",
       y="Sentiment, Positive or Negative") +
  theme(legend.position="none")
#
#Question: Which time periods had the most negative sentiment?
#Filter those time periods
#
AOC_Dec <- filter(AOC_Date, date2 > "2018-12-01" & date2 < "2018-12-15" & sentiment < -4)
#
#Attach to full table for a filter
AOC_Dec1 <- AOC_Dec %>%
  inner_join(AOC2) %>% 
  group_by(V1, date2) %>% 
  arrange(-desc(sentiment))

AOC_Dec1 %>% 
  kable() %>%
  kable_styling("striped")

#-----------------------------------------------------------------#
# Do same thing with Ann Coulter's tweets
#-----------------------------------------------------------------#
#
#Import Twitter data
Coulter <- rio::import("./data/Coulter.csv")
#
#Smaller table for this exercise
#
Coulter2 <- select(Coulter, V1, user_id, created_at, screen_name, 
               text, source, is_retweet, favorite_count, 
               retweet_count, hashtags)
#
#Process with lubridate
#
Coulter2 <- Coulter2 %>% 
  separate(created_at, c("date", "seconds"), " ")
#
some_date <- "2019-01-01"
ymd(some_date)
#
#New separate date field
Coulter2$date2 <- ymd(Coulter2$date)
str(Coulter2)
#
#Disaggregate by day, month, weekday
#
Coulter2$year <- year(Coulter2$date2)
Coulter2$month <- month(Coulter2$date2, label=TRUE)
Coulter2$day <- day(Coulter2$date2)
Coulter2$weekday <- wday(Coulter2$date2, label=TRUE, abbr=FALSE)

#------------------------------------------------------------------------------#
#                       ANALYZE WORDS- Coulter3                       #
#------------------------------------------------------------------------------#
#from book: https://www.tidytextmining.com/tidytext.html #
#from tutorial: http://varianceexplained.org/r/trump-tweets/

#Define characters
reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
#
#
Coulter3 <- Coulter2 %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))


#------------------------------------------------------------------------------#
#                               Sentiment Analysis
#------------------------------------------------------------------------------#
#Based on these tutorials: http://varianceexplained.org/r/trump-tweets/ 
# http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm
#install.packages("sentimentr")
library(sentimentr)

#The sentiments dataset
library(tidytext)
sentiments

get_sentiments("afinn")


#Sentiments
#AFINN rates words positive - negative: https://rdrr.io/cran/corpus/man/sentiment_afinn.html
AFINN <- get_sentiments("afinn")
#
#Tidy table, date for AOC - Twitter data
Coulter_tidy <- Coulter3 %>%
  group_by(date2) %>%
  mutate(linenumber = row_number(),
         tweet_number = V1)

#Joins the Twitter data to sentiment scoring
Coulter_afinn <- Coulter3 %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(V1) %>% 
  summarise(sentiment = sum(score)) 
#
#Using date as grouping variable
#
Coulter_Date <- Coulter3 %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(V1, date2) %>% 
  summarise(sentiment = sum(score)) %>% 
  arrange(desc(sentiment))
#
#Adding sentiment score to the full corpus of Tweets
#This is a big deal
#
Coulter4 <- Coulter_Date %>%
  full_join(Coulter2) %>% 
  group_by(V1, date2)
#
#Question: Find the most negative tweet.
#2724
#Hint: kable might be useful for reading the text
#
#Formating the Tweets in kable
library(kableExtra)
#
TweetText_Table <- Coulter4 %>%
  select(V1, date, text)

TweetText_Table %>% 
  kable() %>%
  kable_styling("striped")
#

#Question: Create a table with tweets with a rating more negative than -10.
#Question: Create a table with tweets with a rating more positive than 10.
#Hint: Dplyr is your buddy
#
library(dplyr)
#Using Mohamed's Top N function
TopCoulterNegative <- Coulter4 %>% 
  filter(sentiment < -10)

TopCoulterPositive <- Coulter4 %>% 
  filter(sentiment > 10)

#
#Here it is, plotting sentiment over time
ggplot(Coulter_Date, aes(date2, sentiment, color = sentiment, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Coulter Tweets: Sentiment", 
       subtitle = "Postive, Negative Sentiment in Coulter Tweets, 2018-2019",
       caption = "Source: Twitter 
       Graphic by Rob Wells",
       x="Month",
       y="Sentiment, Positive or Negative") +
  theme(legend.position="none")
#------------------------------------------------------------------------------#
#
#Filter out retweets and outliers
#
#------------------------------------------------------------------------------#
#
Coulter_Originals <- Coulter4 %>% filter(is_retweet == "FALSE", sentiment > -63)
#
#Table drops from 1106 to 571 observations. Why? The filter on sentiment knocks out the NA responses.
#

ggplot(Coulter_Originals, aes(date2, sentiment, color = sentiment, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Coulter Original Tweets: Sentiment", 
       subtitle = "Postive, Negative Sentiment in Coulter's original Tweets, 2018-2019",
       caption = "Source: Twitter 
       Graphic by Rob Wells",
       x="Month",
       y="Sentiment, Positive or Negative") +
  theme(legend.position="none")

#------------------------------------------------------------------------------#

#Question: Which time periods had the most negative sentiment?
#Filter those time periods
#
Coulter_Oct <- filter(Coulter_Originals, date2 > "2018-10-10" & date2 < "2018-10-25" & sentiment < -4)
#
#Oct 18 - she is on a tear
#
Coulter_Oct %>% 
  kable() %>%
  kable_styling("striped")

#Line chart of sentiment

  ggplot(Coulter_Originals, aes(x=date2, y=sentiment, color = sentiment, fill=sentiment)) +
    geom_line() +
    ggplot2::theme_minimal()+
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
    labs(title = "Coulter Original Tweets: Sentiment", 
         subtitle = "Postive, Negative Sentiment in Coulter's original Tweets, 2018-2019",
         caption = "Source: Twitter 
         Graphic by Rob Wells",
         x="Month",
         y="Sentiment, Positive or Negative") +
    theme(legend.position="none")

#------------------------------------------------------------------------------#
#Compare AOC, Coulter Original Tweets for Sentiment in a Single Chart  
#------------------------------------------------------------------------------#
  
  
#Filter AOC for originals
AOC_Originals <- AOC4 %>% filter(is_retweet == FALSE)
AOC_Originals <- as.data.frame(AOC_Originals)  

colnames(AOC_Originals)
colnames(Coulter_Originals)
Coulter_Originals <- as.data.frame(Coulter_Originals)

#Combine dataframes using rbind
AOC_Coulter_Sentiment <- rbind(AOC_Originals, Coulter_Originals)
#
#Filter from Sept 8 forward when Coulter's feed begins
AOC_Coulter_Sentiment <- filter(AOC_Coulter_Sentiment, date2 >= "2018-09-08")


#plot
ggplot(AOC_Coulter_Sentiment)+
  aes(x = date2, y = sentiment, fill = screen_name)+
  scale_y_continuous(labels = scales::comma) +
  geom_col(position = "dodge") +
  theme_bw()+
  labs(title = "AOC-Coulter Sentiment", 
       subtitle = "AOC-Coulter Twitter Feed",
       caption = "Source: Twitter 2019",
       x="Year/Month",
       y="Sentiment Postive-Negative") 

#Revel in your nerd powers



