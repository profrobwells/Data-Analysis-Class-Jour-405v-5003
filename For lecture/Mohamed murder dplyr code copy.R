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

