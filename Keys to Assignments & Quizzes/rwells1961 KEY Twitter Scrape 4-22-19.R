#Scraping the @rwells1961 Twitter feed
#April 22, 2019#

#Based on Mike Kearey's tutorials
#https://rtweet.info/
#https://rtweet.info/articles/auth.html

#Set Working Directory#
setwd("YOUR DIRECTORY")
#Install procedure if you don't have them already
#install.packages("twitteR")
#install.packages("rtweet")
#install.packages("stringr")
#install.packages("tidytext") 
#
#load packages 
library(tidyverse)
library(readr) 
library(ggplot2)  
library(lubridate)
library(ggthemes) 
library(purrr)
library(twitteR)
library(rtweet)
library(tidytext)

## install httpuv if not already
if (!requireNamespace("httpuv", quietly = TRUE)) {
  install.packages("httpuv")
}

# This Section Activates The Twitter API Call Sequence 
#
# api keys
consumer_key <- "Your Keys Here - Obtain this from Twitter"
consumer_secret <- "Your Keys Here - Obtain this from Twitter"
access_token <- "Your Keys Here - Obtain this from Twitter"
access_secret <- "Your Keys Here - Obtain this from Twitter"

# web browser method: create token and save it as an environment variable
#
#token <- create_token(
#  app = "NAME OF YOUR TWITTER APP",
#  consumer_key = "YOUR CONSUMER KEY HERE",
#  consumer_secret = "YOUR CONSUMER SECRET KEY HERE")

token <- create_token(
  app = "Your Keys Here - Obtain this from Twitter",
  consumer_key = "Your Keys Here - Obtain this from Twitter",
  consumer_secret = "Your Keys Here - Obtain this from Twitter")


## name of twitter app
app_name <- "YOUR TWITTER APP NAME HERE: MINE IS robwells_app"

# Create Token
# create_token(app=...,consumer_key=..., consumer_secret=..., access_token=..., access_secret=...)
#create_token(app_name, consumer_key, consumer_secret, ACCESS_TOKEN, ACCESS_SECRET)
create_token(app_name, consumer_key, consumer_secret, access_token, access_secret)

token <- create_token(app_name, consumer_key, consumer_secret, access_token, access_secret)
token

#-----------------------------------------------------------#
#     Save Token
#-----------------------------------------------------------#
# Save token to home directory. Assign and save R object

path_to_token <- file.path(path.expand("~"), ".twitter_token.rds")
saveRDS(token, path_to_token)

#create env variable TWITTER_PAT (with path to saved token)
env_var <- paste0("TWITTER_PAT=", path_to_token)

# save as .Renviron file (or append if the file already exists)
cat(env_var, file = file.path(path.expand("~"), ".Renviron"),
    fill = TRUE, append = TRUE)

# refresh .Renviron variables
readRenviron("~/.Renviron")

#---------------------------------------------------------------#
#                  Download Twitter Data
#---------------------------------------------------------------#

#Get timeline
Wells <- get_timeline("@rwells1961", n = 3200)

# Coerce the data.frame to all-character
df_Wells = data.frame(lapply(Wells, as.character), stringsAsFactors=FALSE)

#Write Export output this file to a CSV or Excel  write.csv or write.excel
write.csv(df_Wells,"Wells.csv")

#get status IDs of Wells' friends
fdsWells <- get_friends("rwells1961")

# get Wells' followers
kmwWells <- get_followers("rwells1961")

#Write Export output this file to a CSV or Excel  write.csv or write.excel
write.csv(fdsWells,"fdsWells.csv")
write.csv(kmwWells,"kmwWells.csv")

#-----------------------------------------------------------------------------#
#   Process Data
#-----------------------------------------------------------------------------#
Wells <- rio::import("./Data/rwells.csv")

#Create a subset
Wells2 <- Wells
Wells_sub <- select(Wells2, user_id, created_at, text, is_retweet, hashtags, urls_expanded_url, source)
View(Wells_sub)

#-----------------------------------------------------------------------------#
# ANALYZE WORDS 
#-----------------------------------------------------------------------------#
#
#From this:
# 1) Produce a table with the top 10 common words in my Twitter feed
# 2) Produce a table with the top 10 common words and distinguish by source of the device
# 3) Determine the negative or positive sentiment of my feed using the AFINN sentiment analysis dictionary

#Use tidytext to divide into individual words
#using the unnest_tokens function, remove common “stopwords”
#install.packages("tidytext") 
library(tidytext)

reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
tweet_words <- Wells_sub %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

tweet_words

#Filter out some garbage
junk <- c("http", "mt", "rt")

tweet_words <- tweet_words %>%
  filter(!word %in% junk)

CommonWellsWords <- tweet_words %>%
  filter(!word %in% junk)

#------------------------------------------------------------------------#
#        Build Table of Common Wells Words 
#------------------------------------------------------------------------#
CommonWellsWords <- CommonWellsWords %>%
  count(word) %>%
  arrange(desc(n)) %>% 
  top_n(10)


#Write to CSV
write.csv(CommonWellsWords, "CommonWellsWords.csv")
#
#Bonus!
#Filter out Retweets
#
Wells_Real_Tweets <- tweet_words %>% 
  filter(is_retweet == "FALSE")
#
RealWellsWords <- Wells_Real_Tweets %>%
  count(word) %>%
  arrange(desc(n)) %>% 
  top_n(10)

#------------------------------------------------------------------------#
# Top 10 words and distinguished by source of device
#------------------------------------------------------------------------#
#For reference, the top devices

source_ratios <- tweet_words %>%
  count(source) %>%
  arrange(desc(n))
#
tweet_device <- tweet_words %>%
  count(word, source) %>%
  filter(n > 10) %>% 
  spread(source, n, fill = 0) %>% 
  top_n(10) %>% 
  arrange(desc(TweetDeck))

#------------------------------------------------------------------------#
# Sentiments
#------------------------------------------------------------------------#
#AFINN rates words positive - negative: https://rdrr.io/cran/corpus/man/sentiment_afinn.html
AFINN <- get_sentiments("afinn")
#
#
#Filter out Retweets
#
Wells_Real_Tweets <- tweet_words %>% 
  filter(is_retweet == "FALSE")
#
Wells_scored1 <- Wells_Real_Tweets %>%
  mutate(created_at = ymd_hms(created_at))
#
#Joins the Twitter data to sentiment scoring
Wells_scored1 <- Wells_scored1 %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(created_at) %>% 
  summarise(sentiment = sum(score)) 

#Basic plot according to Tweet in sequence
#
# 3) Determine the negative or positive sentiment of my feed using the AFINN sentiment analysis dictionary

ggplot(Wells_scored1, aes(created_at, sentiment, color = sentiment, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Sentiment of Wells Tweets", 
       subtitle = "Postive, Negative Sentiment in Wells Tweets, 2015-2019",
       caption = "Measured by Tweet. Source: Twitter 
       Graphic by Rob Wells",
       x="Year",
       y="Sentiment, Positive or Negative") +
  theme(legend.position="none")

