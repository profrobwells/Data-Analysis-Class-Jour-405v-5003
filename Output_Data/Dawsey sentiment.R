#Dawsey Sentiment Analysis 
# April 8, 2019

#Based on these tutorials: http://varianceexplained.org/r/trump-tweets/ 
#https://rtweet.info/
#https://rtweet.info/articles/auth.html


#Set Working Directory#
setwd("~YOUR DIRECTORY")
#load packages needed for this; needs to be done every time
#these are all part of tidyverse

#Install packages if you haven't already
install.packages("twitteR")
install.packages("rtweet")
install.packages("stringr")
install.packages("tidytext") 
install.packages("ggthemes")

## install httpuv if not already
if (!requireNamespace("httpuv", quietly = TRUE)) {
  install.packages("httpuv")
}

#Call libraries
library(twitteR)
library(rtweet)
library(stringr)
library(tidytext)
library(ggthemes)
library(tidyverse)
library(lubridate)
library(ggthemes) 
library(purrr)
library(rio)

#------------------------------------------------------------------------------#
#                       LOAD PREVIOUSLY CREATED TABLES                         #
#------------------------------------------------------------------------------#


##Import Josh
Josh <- rio::import("./data/Josh.csv")


#------------------------------------------------------------------------------#
#                       ANALYZE WORDS                       #
#------------------------------------------------------------------------------#
#from book: https://www.tidytextmining.com/tidytext.html #
#from tutorial: http://varianceexplained.org/r/trump-tweets/

#Define characters
reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
#
#
Joshtweet_words <- Josh %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

tweet_words

Joshwords <- Joshtweet_words %>%
  count(word, source) %>%
  filter(sum(n) >= 5) %>%
  spread(source, n, fill = 0) %>%
  ungroup()

#Don't count as separate devices#
Joshwords <- Joshtweet_words %>%
  count(word) %>%
  filter(sum(n) >= 5) %>%
  ungroup()


#------------------------------------------------------------------------------#
#                               Sentiment Analysis
#------------------------------------------------------------------------------#
#Based on these tutorials: http://varianceexplained.org/r/trump-tweets/ 
# http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm
#NRC Word-Emotion Association lexicon, available from the tidytext package
#Associates words with 10 sentiments: positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, and trust.

#Create a sentiment analysis table called nrc
nrc <- sentiments %>%
  filter(lexicon == "nrc") %>%
  dplyr::select(word, sentiment)

nrc

#------------------------------------------------------------------------------#  
#Count the number of words by screen_name:
#------------------------------------------------------------------------------#  
screen_name <- tweet_words %>%
  group_by(screen_name) %>%
  mutate(total_words = n()) %>%
  ungroup() %>%
  distinct(user_id, screen_name, total_words)

#------------------------------------------------------------------------------#
# Create a Table of Words and Sentiments
#------------------------------------------------------------------------------#

sentiment_words <- tweet_words %>%
  count(word) %>%
  inner_join(nrc, by = "word") %>% 
  select(word, sentiment, n) %>% 
  arrange(desc(n))
#
#Export output this file to a CSV or Excel  write.csv or write.excel
write.csv(sentiment_words,"Josh_sentiment_words.csv")

#------------------------------------------------------------------------------#
#                 Summarizing Sentiments Based on Josh Screen Name
#------------------------------------------------------------------------------#

total_sentiment <- tweet_words %>%
  inner_join(nrc, by = "word") %>%
  count(sentiment, user_id) %>%
  ungroup() %>%
  complete(sentiment, user_id, fill = list(n = 0)) %>%
  inner_join(screen_name) %>%
  group_by(screen_name, sentiment, total_words) %>%
  summarize(words = sum(n)) %>%
  arrange(desc(words)) %>% 
  ungroup()

head(total_sentiment)

#Export output this file to a CSV or Excel  write.csv or write.excel
write.csv(total_sentiment,"Josh_total_sentiment.csv")

#------------------------------------------------------------------------------#
#      Create a Chart With Top Sentiments
#------------------------------------------------------------------------------#

library(forcats)
library(ggthemes)
#this works
ggplot(total_sentiment,
       aes(x=words, y=fct_reorder(sentiment, words, desc=TRUE))) +
  geom_point() +
  labs(x="Count of Words in Twitter Feed", y="Words", 
       title = "Sentiment In Josh Twitter 2018-2019",
       subtitle = "Chart: 4-8-19",
       caption = "Data analysis by Rob Wells") +
  theme_wsj() +
  geom_text(aes(label=words), hjust=-.4) 



#------------------------------------------------------------------------------#
# Anoter Version Count the number of words in each category of phone:
#------------------------------------------------------------------------------#

sources <- tweet_words %>%
  group_by(source) %>%
  mutate(total_words = n()) %>%
  ungroup() %>%
  distinct(user_id, source, total_words)

by_source_sentiment <- tweet_words %>%
  inner_join(nrc, by = "word") %>%
  count(sentiment, user_id) %>%
  ungroup() %>%
  complete(sentiment, user_id, fill = list(n = 0)) %>%
  inner_join(sources) %>%
  group_by(source, sentiment, total_words) %>%
  summarize(words = sum(n)) %>%
  ungroup()

head(by_source_sentiment)


#------------------------------------------------------------------------------#
# Analytics
#------------------------------------------------------------------------------#

library(broom)

sentiment_differences <- total_sentiment %>%
  group_by(sentiment) %>%
  do(tidy(poisson.test(.$words, .$total_words)))

sentiment_differences

#Export output this file to a CSV or Excel  write.csv or write.excel
write.csv(sentiment_differences,"Josh_sentiment_differences.csv")

ggsave("Collins Sentiment 10-26-18.png", width=30, height=20, units="cm")
