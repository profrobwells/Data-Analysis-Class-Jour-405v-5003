continue hashtag exercise




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