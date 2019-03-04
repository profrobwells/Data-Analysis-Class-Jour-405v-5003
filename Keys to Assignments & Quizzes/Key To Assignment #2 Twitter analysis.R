#Key to Assignment #2
#March 4 2019#

#Visualization of Twitter data.  Due Feb 27, 11:59 p.m.   
#Students will produce publication-ready graphics from Twitter data. 
#Data dictionary required  
#Details:  
#  With the assigned dataset, Coulter.csv, students will produce the following tables:
#--Most common words used in Twitter text field   
#--Most common hashtags    
#--A chart of the volume of Twitter messages by date    
#Based on this information, write a 300-500 word report, following AP style, 
#that describes potential newsworthy trends in this Twitter feed and 
#insights from your analysis.

#Load Data

#Load the Coulter tweets
Coulter <- rio::import("./Data/Coulter.csv")
Coulter <- janitor::clean_names(Coulter)
colnames(Coulter)
str(Coulter)

#--------------------------------------#
# Question #1: Common Words
#--------------------------------------#
#
#----------  ANALYZE WORDS ---------------#
#from book: https://www.tidytextmining.com/tidytext.html #
#from tutorial: http://varianceexplained.org/r/trump-tweets/
# using tidytext: https://cran.r-project.org/web/packages/tidytext/
#https://cran.r-project.org/web/packages/tidytext/vignettes/tidytext.html
#https://cran.r-project.org/web/packages/tidytext/tidytext.pdf
#Comparison of words

library(tidytext)
library(stringr)
library(dplyr)

#Create reg variable with various text characters
#
#reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
#
#Filter sequence
#tweet_words <- tweets %>%
#  filter(!str_detect(text, '^"')) %>%
#  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
#  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
#  filter(!word %in% stop_words$word,
#         str_detect(word, "[a-z]"))

#tweet_words

reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
tweet_words <- Coulter %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

tweet_words

#Count the Words
CommonCoulterWords <- tweet_words %>%
  count(word) %>%
  filter(sum(n) >= 5) %>%
  ungroup() %>% 
  arrange(desc(n))

#Write to CSV
write.csv(CommonCoulterWord, "CommonCoulterWord.csv")

#Filter to top 15 occurrences
#Use top_n function

Top15Coulter <- CommonCoulterWords %>%
  top_n(15, n) %>%
  arrange(desc(n))

#Convert to data frame
Top15Coulter <- as.data.frame(Top15Coulter)

#Visualize it!
library(ggplot2)

#Create graphic
ggplot(Top15Coulter, aes(x = reorder(word, -n), y = n, color = word, fill=word))  +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top Words In Coulter Twitter Feed", 
       subtitle = "Coulter Twitter Feed",
       caption = "Graphic by Rob Wells",
       x="Word",
       y="Count of the word usage") +
  theme(legend.position="none") 

#Another version
ggplot(Top15Coulter, aes(x = reorder(word, -n), y = n, color = word, fill=word))  +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 70, vjust = 1.2, hjust = 1.0)) +
  labs(title = "Top Words In Coulter Twitter Feed", 
       subtitle = "Coulter Twitter Feed",
       caption = "Graphic by Rob Wells",
       x="Word",
       y="Count of the word usage") +
  theme(legend.position="none")


#------------------------------------------------#
# Question 2: Hashtags
#------------------------------------------------#


#Create a subset
Coulter2 <- select(Coulter, user_id, created_at, text, is_retweet, hashtags, urls_expanded_url)
View(Coulter2)

#Data Cleaning - Split the URLS out#
#Consulted This Tutorial https://trendct.org/2015/06/12/r-for-beginners-how-to-transition-from-excel-to-r/#chapterTOC8 #
#function{x <- gsub("\\(", "", x), return(x)}
#also to remove quotes: noquote(YOUR TEXT STRING)

# Separate hashtag1, which has the comma delimiters and dump in five new colums#
library(tidyr)
Coulter3 <- separate(Coulter2, hashtag1, 
                 c('hashtag1', 'hashtag2', 'hashtag3', 'hashtag4', 'hashtag5'), 
                 sep=',', remove=TRUE)

#table with only hashtags 
Coulter4 <- select(Coulter3, hashtag1:hashtag5)

count <- table(unlist(Coulter4))
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

#Remove row with NA
hashtags3 <- hashtags3[-c(1), ] 
View(hashtags3)

#Convert to data frame
hashtags3 <- as.data.frame(hashtags3)


#Filter to top 15 occurrences
Top15Hashtags <- hashtags3 %>%
  top_n(15, ScoreSum) %>%
  arrange(desc(ScoreSum))

#Save your work
write.csv(hashtags3, "Coulter_hashtags.csv")

#Create the chart
ggplot(Top15Hashtags, aes(x = reorder(hashtag, ScoreSum), y = ScoreSum, color = hashtag, fill=hashtag))  +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top Hashtags In Coulter Twitter Feed", 
       subtitle = "Coulter Twitter Feed",
       caption = "Graphic by Rob Wells",
       x="Word",
       y="Count of the hashtag frequency") +
  theme(legend.position="none") 

#--------------------------------------------------------------------#
#    Question 3:     Dates  
#--------------------------------------------------------------------#

library(lubridate)

#Thanks to https://stackoverflow.com/questions/4350440/split-data-frame-string-column-into-multiple-columns
#Let's trim down Coulter to just date, text, hashtags
Coulterdates <- Coulter %>% select(created_at, text, hashtags)

#Bust out the date field - separate seconds#
Coulterdates2 <- Coulterdates %>% 
  separate(created_at, c("date", "seconds"), " ")

#Create Separate Year Field
#Create random date
some_date <- "2019-01-01"
ymd(some_date)


#New separate date field
Coulterdates2$date2 <- ymd(Coulterdates2$date)
str(Coulterdates2)

Coulterdates2$year <- year(Coulterdates2$date2)
Coulterdates2$month <- month(Coulterdates2$date2, label=TRUE)
Coulterdates2$day <- day(Coulterdates2$date2)
Coulterdates2$weekday <- wday(Coulterdates2$date2, label=TRUE, abbr=FALSE)

#Total Tweets by Day
CoulterDaytotal <- Coulterdates2 %>% 
  count(date) %>% 
  group_by(date) %>% 
  arrange(desc(n))

#Total Tweets by Month
Coultermonth <- Coulterdates2 %>% 
  count(month) %>% 
  group_by(month) %>% 
  arrange(desc(n))

#Deal with the chronology - have 2018 flow into 2019
#Glue the years and months into a new field
#Thanks nerds: https://stackoverflow.com/questions/46691933/r-sort-by-year-then-month-in-ggplot2

Coulteryear_mo <- Coulterdates2 %>%
  mutate(year = format(date2, "%Y"), yearmon = format(date2, "%Y-%m")) %>%
  group_by(year, yearmon) %>%
  count(yearmon) 

#An Elegant solution by Mohamed to the preceding lines
#Addresses portions of Lines 188-89, Replaces 222-225
# Tweets by Month, Sorted by Year
Coulter_y_Month <- Coulter %>%
  mutate(created_at = ymd_hms(created_at)) %>%
  mutate(yearmon= format(created_at, "%Y-%m")) %>%
  group_by(yearmon) %>%
  count(yearmon)

#library(ggplot2)

ggplot(Coulteryear_mo, aes(x=yearmon, y=n, color = yearmon, fill=yearmon)) +
  geom_col() +
  theme_bw() +
  labs(y="tweets", title="Ann Coulter Twitter Activity", caption="Source: Twitter, 2018")+
  theme(legend.position="none") 
