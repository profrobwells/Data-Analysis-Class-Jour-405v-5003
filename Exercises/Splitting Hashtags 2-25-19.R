#Splitting out hashtags
#------------------------------------------------#
#CREATE SMALLER TABLE WITH HASHTAGS AND URLS
#------------------------------------------------#
#Create a subset
AOC2 <- select(AOC, user_id, created_at, text, is_retweet, hashtags, urls_expanded_url)
View(AOC2)
colnames(AOC2)
#Data Cleaning - Split the URLS out#

#Consulted This Tutorial https://trendct.org/2015/06/12/r-for-beginners-how-to-transition-from-excel-to-r/#chapterTOC8 #

#Step 1 - Replace the / with a comma. You can't split on / .# 
#Put it in a new Column Called URL1#
AOC2$hashtag1 <- gsub("\\(", "", AOC2$hashtags) 
AOC2$hashtag1 <- gsub ("\\)", "", AOC2$hashtag1)
AOC2$hashtag1 <- gsub ("\"", "&", AOC2$hashtag1)
AOC2$hashtag1 <- gsub ("c&&", "", AOC2$hashtag1)
AOC2$hashtag1 <- gsub ("&&", "", AOC2$hashtag1)

#also to remove quotes: noquote(YOUR TEXT STRING)


#Step 2 - Create New DF#
#Separate URL1, which has the comma delimiters and dump in five new colums#
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


hashtags<- data.frame(table(unlist(df6$hashtag), " "))


hashtags <- df6 %>% 
  group_by(hashtag) %>% 
  summarise(count2 = n())

mpg %>% 
  group_by(model, manufacturer) %>% 
  summarise(sum_drv = sum(drv))

Hashtags <- df6 %>%
  select(hashtag, count) %>%
  group_by(hashtag) %>% 
  count(count)


df7 <- df6 %>% select(hashtag) %>% group_by(hashtag) %>% 
  summarize(count)





df7 <- group_by(df6, hashtag) %>% 
  summarize(total=count()) %>%    
  arrange(desc(total)) 
View(df7)

#for later
count <- table(unlist(AOC4))
perc <- 100*count/sum(count)
AOC5 <- data.frame(code = sprintf("%03d", as.integer(names(count))),
                   count = as.integer(count), perc = as.numeric(perc))



df5 <- AOC3 %>%
  count(hashtag1,hashtag2,hashtag3) %>%
  filter(sum(n) >= 5)


#Total by Column, Summarize by String#
df3 <- group_by(AOC3, hashtag1) %>% 
  summarize(total=n()) %>%    
  arrange(desc(total)) 
View(df4)


#Delete Now Useless Columns URL1 and URL2#
# sample code: df = subset(mydata, select = -c(x,z) )
Collins5 <- subset(Collins4, select = -c(URL1, URL2))