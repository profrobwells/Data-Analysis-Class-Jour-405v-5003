#title: "Assignment#1_KEY_Static_Graphic"
# 2/9/2019"

#THIS IS THE KEY#
  
#Assignment #1 - Data Analysis for Journalists - Jour 405v
###Static Graphic - Managing Data in R.   Due Feb 6
  
#Students will use R Studio to gather, analyze and visualize FBI Uniform Crime data for Arkansas.    

#1. Load the appropriate R software packages to analyze and calculate data  
###Set Working Directory. 
getwd()
setwd("~/YOUR FOLDER PATH NAME HERE")

#load packages needed for this; needs to be done every time
#these are all part of tidyverse
#for importing csv file
library(readr) 
#for analysis
library(dplyr) 
#for creating charts
library(ggplot2)  
#themes for data viz
library(ggthemes) 

#2. Import FBI Uniform Crime data for Arkansas for 2017: arkansas_crime.xls 
ArkCrime2017 <- rio::import("Data/arkansas_crime.xls", skip=4)
#Cut the crud at the end of the table
ArkCrime2017 <- ArkCrime2017[-c(187),]
View(ArkCrime2017)

#3. Clean labels, convert appropriate columns to numeric data    
ArkCrime2017 <- janitor::clean_names(ArkCrime2017)

#Rename state to city
colnames(ArkCrime2017)[1] <- "city"

#Check the labels and data types
str(ArkCrime2017)
colnames(ArkCrime2017)

#If you needed to convert to numeric, use this
#ArkCrime2017$violent_crime <- as.numeric(ArkCrime2017$violent_crime)

#4: Provide the R code for a summary of the statistics, the number of rows, number of columns   
View(ArkCrime2017)
str(ArkCrime2017)  
nrow(ArkCrime2017)
summary(ArkCrime2017)

#5: Create a new crime rate column with violent crime per 10,000 residents   

ArkCrime2017$CrimePer10000 <- (ArkCrime2017$violent_crime / ArkCrime2017$population) *10000 
View(ArkCrime2017)

#6: Sort the table descending with the highest crime rate on top   

# Sort by largest percentage change
ArkCrime2017 <- ArkCrime2017[order(-ArkCrime2017$CrimePer10000),]

OR
ArkCrime2017a <- ArkCrime2017%>%select(city, CrimePer10000)%>%arrange(desc(CrimePer10000))


#7: Create separate table with just the top five counties' crime rate
Top5 <- ArkCrime2017 %>% select(city, CrimePer10000) %>% filter(CrimePer10000 >= 163)
                     View(Top5)  

#8: Export these two tables as .csv files    
write.csv(ArkCrime2017, "ArkCrime2017.csv")
write.csv(Top5, "Top5.csv")

#9: Visualize findings, create a simple chart using ggplot that shows the top 5 counties' crime rate.   
# If you haven't installed the ggplot2 package yet, uncomment and run the line below
# install.packages("ggplot2")
library(ggplot2)
library(forcats)


#SAMPLE CODE: Loanchart<- ggplot(NationalLoans, 
#               aes(x=TotalDisbursements_Mil., y=fct_reorder(State, TotalDisbursements_Mil., desc=TRUE))) +

#Build a basic percentage change chart  
Top5Chart <- ggplot(Top5, aes(x = CrimePer10000, y=fct_reorder(city, CrimePer10000, desc=TRUE))) +
  geom_col () +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "Arkansas Cities With Top Crime Rates Per 10,000, 2017", 
       subtitle = "FBI Uniform Crime Report Data: https://ucr.fbi.gov/crime-in-the-u.s/2017/crime-in-the-u.s.-2017/tables/table-8/table-8-state-cuts/arkansas.xls",
       caption = "Graphic by Rob Wells",
       x="Crime Rate Per 10,000 People",
       y="City")

plot(Top5Chart)


library(ggplot2)
Top5Chart <- ggplot(Top5, aes(x = reorder(city, -CrimePer10000), y = CrimePer10000))  +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Arkansas Cities With Top Crime Rates Per 10,000", 
       subtitle = "FBI Uniform Crime Report Data, 2017",
       caption = "Graphic by Rob Wells",
       x="City",
       y="Crime Rate Per 10,000 People")
plot(Top5Chart)

#10: Upload the R script, .csv files and .jpg file to Blackboard.

#-----------------------------------#
# Extra touches from your colleagues
#-----------------------------------#

#Do the Katie Serrano special and add some killer colors
ColorTop5Chart <- ggplot(Top5, aes(x = reorder(city, -CrimePer10000), y = CrimePer10000, color = city, fill=city))  +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Arkansas Cities With Top Crime Rates Per 10,000", 
       subtitle = "FBI Uniform Crime Report Data, 2017",
       caption = "Graphic by Rob Wells",
       x="City",
       y="Crime Rate Per 10,000 People")
plot(ColorTop5Chart)


#Alex Nichol decimal rounding:
CrimeData$ViolentCrimePer10000<-round(CrimeData$ViolentCrimePer10000, 2)


#Mohamed and the top n function
#Create separate table with just the top five counties' crime rate: dplyr has a "top_n" function that i find handy
Top_5_county_rates <- Arkansas_Crime %>%
  select(County, violent_crime_rate_per10k) %>%
  top_n(5, violent_crime_rate_per10k) %>%
  arrange(desc(violent_crime_rate_per10k))

#Mohamed and rio for export
rio::export(Top_5_county_rates, "Top_5_violent_crimes.csv")

#Mohamed and the graphic
#Visualize findings, create a simple bar chart using ggplot that shows the top 5 counties' crime rate. 

library(ggplot2)
ggplot(Top_5_county_rates) +
  aes(reorder(County, violent_crime_rate_per10k), y = violent_crime_rate_per10k, fill = County) +
  geom_col() +
  labs(title = "Arkansas Crime Rate Per 10k",  
       subtitle = "Top 5 Counties",
       caption = "Graphic by Mohamed",
       x="County",
       y="Crime Rate")
