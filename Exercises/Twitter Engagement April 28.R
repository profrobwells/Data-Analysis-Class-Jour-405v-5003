#Measuring Twitter Engagement
#April 28 2019
# Thanks to Mohamed M'Bareck

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
Coulter <- rio::import("./Data/Coulter.csv")
Coulter <- janitor::clean_names(Coulter)
#Date processing
Coulter <- Coulter %>%
  mutate(created_at = ymd_hms(created_at))
#
AOC <- rio::import("./Data/AOC.csv")
AOC <- janitor::clean_names(AOC)
#Date processing
AOC <- AOC %>%
  mutate(created_at = ymd_hms(created_at))
#Measuring Engagement
#---------------------------------------------------------------------#
#Who are AOC/ Coulter Quoting?
#
#quoted_name is the field identifying people and entities a user quotes. 
#Construct a table with the top 20 people quoted by AOC and Coulter
#Create a table that compares the two
#Draw from Coulter Tweet Analysis #2 March 4.R
#
#Question: Are these two political figures reading the same thing?
#

Annquotes <- Coulter %>%
  count(quoted_name, sort = T) %>%
  na.exclude() %>%
  top_n(20)
#
#Add identifying label to Coulter dataframe
Annquotes$Name <- "Coulter"

AOCquotes <- AOC %>%
  count(quoted_name, sort = T) %>%
  na.exclude() %>%
  top_n(20)
#Add identifying label dataframe
AOCquotes$Name <- "AOC"

#Combine dataframes using rbind
AOC_Coulter_Quotes <- rbind(AOCquotes, Annquotes)

ggplot(AOC_Coulter_Quotes)+
  aes(x = reorder(quoted_name,-n), y = n, fill = Name)+
  geom_col(show.legend = T) +
  coord_flip()+
  labs(title = "Ann Coulter - Acasio Cortez Quoted Sources", 
       subtitle = "Ann Coulter- AOC Twitter Feed",
       caption = "Source: Twitter 2019",
       x="Quoted Name",
       y="Count") 

#What was the sentiment of the most popular Tweets?
#Build table of favorites
AOCfavorites <- AOC %>%
  select(favorite_count, v1) %>%
  filter(favorite_count >0) %>% 
  arrange(desc(favorite_count))
#------------------------------------------------------------------------------#
#                       ANALYZE WORDS                       #
#------------------------------------------------------------------------------#
#from book: https://www.tidytextmining.com/tidytext.html #
#from tutorial: http://varianceexplained.org/r/trump-tweets/
#Define characters
reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
#
#
AOCtoken <- AOC %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

#------------------------------------------------------------------------------#
#                               Sentiment Analysis
#------------------------------------------------------------------------------#
AFINN <- get_sentiments("afinn")
#
#A table with all words scored
#from Sentiment and Time Analysis 4-9-19.r
AOCsentiment <- AOCtoken %>%
  inner_join(AFINN, by = c(word = "word")) %>%
  group_by(v1, created_at) %>% 
  summarise(sentiment = sum(score))


#
#Join favorites
#
AOCfavorites1 <- AOCfavorites %>% 
  inner_join(AOCsentiment, by = c( v1 = "v1")) %>% 
  top_n(30, favorite_count) %>% 
  arrange(desc(favorite_count)) 
#
AOCfavorites1 <- AOCfavorites1 %>%
  mutate(ymd = format(created_at, "%Y-%m-%d"))
#Chart tweet by favorite Count, color by Sentiment
#
ggplot(AOCfavorites1)+
  aes(x = ymd, y = favorite_count, fill = sentiment) +
  geom_col(show.legend = T) +
  scale_fill_distiller(palette ="RdBu", direction = +1)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(limits = c(0,300000), breaks = seq(0,300000,50000)) +
  labs(title = "Acasio Cortez Favorites and Sentiment", 
       subtitle = "AOC Twitter Feed",
       caption = "Source: Twitter 2019",
       x="tweets",
       y="Most popular tweets") 

#Class Assignment
#Now do that with Coulter
#Upload your code and graphic to GitHub
#
#And if you finished that, create a dual chart with AOC and Coulter sentiment and engagement
