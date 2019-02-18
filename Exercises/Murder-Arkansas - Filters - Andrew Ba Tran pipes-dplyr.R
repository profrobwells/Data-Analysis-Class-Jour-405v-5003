#Filtering and Sorting Murder Database
#Feb 18 2019 - update from class


#Additional exercises based on Andrew Ba Tran's pipes-dplyr.rmd exercise

ArkMurders <- filter(murders, State=="Arkansas")

sex <- filter(ArkMurders, VicSex_label=="Male")

df3 <- filter(murders, Relationship_label %in% c("Husband", "Boyfriend"))

#count murders by state
library(dplyr)
library(tidyr)
library(tidytext)
MurderState <- murders %>% 
  count(State) %>% 
  group_by(State) %>% 
  spread(State, n, fill=0)

View(MurderState)
#We Want states as rows. 
#flip table with this example
#m1 <- t(df1)
#d2 <- data.frame(r1= row.names(m1), m1, row.names=NULL)

m1 <- t(MurderState)
MurderState2 <- data.frame(state= row.names(m1), m1, row.names=NULL )
View(MurderState2)


#Haile's Filter and Age Range
Ages1 <- filter(murders, State=="Arkansas", VicAge== 19:22, Year==2016)

#filter for null
https://community.rstudio.com/t/filtering-the-data-when-input-is-null-dplyr-and-shiny/9920/2
http://www.cookbook-r.com/Basics/Working_with_NULL_NA_and_NaN/
df %>% filter(!is.na(a))



#Mohamed's nifty table automatically calculates percentages
#Handles two-way tables.
#Check this out!
https://cran.r-project.org/web/packages/janitor/vignettes/tabyls.html

library(janitor)
MurderState3 <- murders %>% 
  tabyl(State)

#Two-Way tabyl
MurderState3 <- murders %>% 
  tabyl(State, VicSex_label)

#By weapons
MurderState3 <- murders %>% 
  tabyl(State, Weapon_label)

#NWA counties - Benton, Washington, Sebastian, Newton
#Construct with grepl
NWA <- filter(ArkMurders, grepl ("Benton|Washington|Sebastian|Madison", Agency))





#VICRACE

murders$VicRace_label

whitevics <- filter(murders, State=="Arkansas", VicRace_label=="White", Solved_label=="No", Year==2016)

#Asian or Pacific Islander
asianvics <- filter(murders, State=="Arkansas", VicRace_label=="Asian or Pacific Islander", Year==2016)

#multiple results
#%in% c("Husband", "Boyfriend")
unsolvedwhiteblack <- filter(murders, State=="Arkansas", VicRace_label %in% c("White", "Black"), Solved_label=="No", Year==2016)



unsolvedwhiteblack <- filter(murders, State=="Arkansas", VicRace_label %in% c("White", "Black"), Solved_label=="No", Year==2016)

counties2 <- filter(murders, State=="Arkansas", MSA_label=="Fayetteville-Springdale-Rogers, AR-MO", Year==2016)

murders$Agency


ArkMurders <- filter(murders, State=="Arkansas")



#Let's Look at GREP
??grepl
#Pattern Matching and Replacement
#grep, grepl, regexpr, gregexpr and regexec search for matches to argument pattern within each element of a character vector: they differ in the format of and amount of detail in the results.
#grepl(pattern, x, ignore.case = FALSE, perl = FALSE,
#           fixed = FALSE, useBytes = FALSE)
#Another resource: https://www.rdocumentation.org/packages/base/versions/3.5.2/topics/grep

#The Power of Grep!
#Or
#filter_for_value<-CO2[grep("non", CO2$Treatment), ]
#head(filter_for_value)
Test1 <-AOC[grep("Trump", AOC$text), ]
head(Test1)

#Mohamed's Graphs.

library(ggplot2)
library(dplyr)

source("import_murders.R")

# in the last 5 decades, what year was the most violent?
AR1 <- murders %>%
  filter(State == "Arkansas") %>%
  group_by(Year) %>%
  count(VicSex_label) %>%
  arrange(desc(n)) 

library(ggplot2)

# murders in AR in 2016 were the highest since 1998, which indicates that the crime rates are raising agin
ggplot(AR1) +
  aes(Year, n, fill = Year) +
  geom_col(show.legend = F)+
  labs(title = "Murders in AR",
       subtitle = "From 1976 to 2016")+
  ylab("Number of Murders Per Year")+
  xlab("Year")

Unknowns <-AR1 %>%
  filter(VicSex_label =="Unknown")

# what gender was the most victimized in AR over the year?
ggplot(AR1) +
  aes(Year, n, fill = VicSex_label=="Unknown") +
  geom_col() +
  labs(title = "Murders in AR",
       subtitle = "Based on Victim Sex")+
  ylab("Number of Murders Per Year")+
  xlab("Year")

ggplot(Unknowns) +
  aes(Year, n, fill = VicSex_label) +
  geom_col() +
  labs(title = "Murders in AR",
       subtitle = "Based on Victim Sex")+
  ylab("Number of Murders Per Year")+
  xlab("Year")



# What race was the most victimized over the years?  
AR2 <- murders %>%
  filter(State == "Arkansas") %>%
  select(Year, VicRace_label, Relationship_label) %>%
  group_by(Year) %>%
  count(VicRace_label)
#Answer = BLACK!!
ggplot(AR2) +
  aes(Year, n, fill = VicRace_label) +
  geom_col() +
  labs(title = "Murders in AR in 2016",
       subtitle = "Based on Victim Race")+
  ylab("Number of Murders")+
  xlab("Year")

# who committed the highest murders in AR in 2016?
AR3 <- murders %>%
  filter(State == "Arkansas", Year ==2016) %>%
  count(Relationship_label, Month_label, sort = T) %>%
  group_by(Relationship_label)

# answer = the type "UNDETERMINED"
ggplot(AR3) +
  aes(x= Month_label, n, fill = Relationship_label) +
  geom_col()+
  labs(title = "Murders in AR in 2016",
       subtitle = "Based on Relationship")+
  ylab("Number of Murders")+
  xlab("2016 Year")
# for more analysis: the graph indicates that in all cases, the offender somehow know his victime!!
# to prove this, lets look at the exact numbers for each category
murders %>%
  filter(State == "Arkansas", Year ==2016) %>%
  count(Relationship_label, sort = T) %>%
  group_by(Relationship_label) 
# this shows that the number of murders committed by a stranger is relatively small compared to other categories



# what is the highest cause of murders in AR?
weapons <- murders %>%
  filter(State == "Arkansas") %>%
  count(Weapon_label, Year) %>%
  group_by(Weapon_label) %>%
  arrange(desc(n))
# Handgun is historically the highest cause of murders in AR
ggplot(weapons)+
  aes(Year, n, fill = Weapon_label, color= Weapon_label)+
  geom_col()+
  geom_point()+
  labs(title = "Murders in AR From 1976 to 2016",
       subtitle = "Based on Weapon Type")+
  ylab("Number of Murders")+
  xlab("Year")


# murders in AR by county
Cities <- murders %>%
  filter(State == "Arkansas") %>%
  count(MSA_label, Year, sort = T) %>%
  group_by(MSA_label)
# Texarkana and Jonesboro have been historically the least violent
ggplot(Cities) +
  aes(Year, n, fill = MSA_label)+
  geom_col()+
  labs(title = "Murders in AR From 1976 to 2016",
       subtitle = "Based on City")+
  ylab("Number of Murders per Year")+
  xlab("Year")


              