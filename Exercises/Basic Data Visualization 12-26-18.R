#Title: Machlis - Basic Data Visualization
#UpdatedJan 30 2019


#load software - Select NO when asked to restart
install.packages("ggplot2")
install.packages("dplyr")
install.packages("usethis")
install.packages("forcats")

#call software into memory
library(ggplot2)
library(dplyr)
library(usethis)
library(forcats)

#Basic demo
#You will run the commands from the Console below
demo(topic="graphics")

library(dplyr)

#Tutorial
#Import Data, Create Dataframe, Rename Columns
snowdata <- rio::import("data/BostonChicagoNYCSnowfalls.csv")
bostonsnow <- select(snowdata, Winter, Boston)
names(bostonsnow)[2] <- "TotalSnow"

#Doing the same thing but with pipe function
bostonsnow2 <- select(snowdata, Winter, Boston) %>%
  rename(TotalSnow = Boston)

#Doing the same thing but more efficiently
bostonsnow3 <- select(snowdata, Winter, TotalSnow = Boston) 

#Basic graphs
plot(bostonsnow$TotalSnow)

hist(bostonsnow$TotalSnow)
boxplot(bostonsnow$TotalSnow)
barplot(bostonsnow$TotalSnow)
barplot(sort(bostonsnow$TotalSnow, decreasing = TRUE))

#qplot
qplot(data=bostonsnow, y = TotalSnow)
qplot(y = bostonsnow$TotalSnow)

#basic ggplot2 - boxplot
ggplot(data=snowdata) + 
  geom_boxplot(aes(x = "Boston", y = Boston))

#dual box plots
ggplot(data=snowdata) + 
  geom_boxplot(aes(x = "Boston", y = Boston)) +
  geom_boxplot(aes(x = "Chicago", y = Chicago))

#bring in snowdata tidy
snowdata_tidy <- rio::import("data/snowdata_tidy.csv")

#view a tidy table
View(snowdata_tidy)

#Boxplot with ggplot
ggplot(snowdata_tidy, aes(x = City, y = TotalSnow)) +
  geom_boxplot()

#Line graphs
ggplot(snowdata_tidy, aes(x = Winter, y = TotalSnow, group = City)) +
  geom_line()

#ggplot with colors and points

ggplot(snowdata_tidy, aes(x = Winter, y = TotalSnow, group = City, color = City)) +
  geom_line()


#ggplot with colors and points
ggplot(snowdata_tidy, aes(x = Winter, y = TotalSnow, group = City, color = City)) +
  geom_line() +
  geom_point() 

#Filtered for two years, 1999 and 2000
snowdata_tidy21 <- filter(snowdata_tidy, Winter >= "1999-2000")
ggplot(snowdata_tidy21, aes(x = Winter, y = TotalSnow, group = City, color = City)) +
  geom_line() +
  geom_point()

#Barplots
ggplot(data = snowdata_tidy21, aes(x = Winter, y = TotalSnow, group = City, color = City)) +
  geom_col() 

#Not so ugly bars
ggplot(data = snowdata_tidy21, aes(x = Winter, y = TotalSnow, group = City, fill = City)) +
  geom_col(position = "dodge") 

#-------------------------------------------------------------------#
#Build a chart - Snow
#-------------------------------------------------------------------#

library(ggplot2)
SnowChartBoston <- ggplot(bostonsnow, aes(x = reorder(Winter, TotalSnow), y = TotalSnow))  +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Snow", 
       subtitle = "lots of it",
       caption = "Graphic by Rob Wells",
       x="Years",
       y="snow in inches")
plot(SnowChartBoston)


#Exercise:
#Pull in ArkCo_Income_2017
#1. Create a bar chart with the top 10 counties with the greatest percentage of low-income population
#Your answer will look like this


https://github.com/profrobwells/Data-Analysis-Class-Jour-405v-5003/Images/LowWageHouseholds.png

#2. Create a bar chart with the top 10 counties with the greatest percentage of upper-income population


#-------------------------------------------------------------------------#
#Stop Here Wednesday #
#-------------------------------------------------------------------------#



#Code snippets
snippet myg_barplot_grouped
  ggplot(${1:mydataframe}, aes(${2:xcolname}, ${3:ycolname}, group = ${4:groupbycolname},
                               fill = ${4:groupbycolname})) +
  geom_col(position = "dodge")

ggplot(mydataframe, aes(xcolname, ycolname, group = groupbycolname, 
fill = groupbycolname)) +
geom_col(position = "dodge")

boston10 <- bostonsnow %>%
  top_n(10, TotalSnow) %>%
  arrange(desc(TotalSnow))

ggplot(data = boston10, aes(x = Winter, y = TotalSnow)) + 
  geom_col(fill = "rainbow") +
  theme_minimal() 

ggplot(data = boston10, aes(x = Winter, y = TotalSnow)) + 
geom_col(fill = "dodgerblue4") +
theme_minimal() +
labs(title = " XXXX Any Title Here",
subtitle = "XXXX Any subtitle", 
caption = "Source: XXXXX")

#With ordered bars
ggplot(boston10, aes(x=fct_reorder(Winter, TotalSnow), y=TotalSnow)) + 
  geom_col()

#month factor
month_factor <- factor(month.name, levels = month.name, ordered = TRUE) 
month_factor


#Adjust the axes
myplot + theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

#---------------------------------------------------------------------#
#Exercise Answer:
#---------------------------------------------------------------------#

https://github.com/profrobwells/Data-Analysis-Class-Jour-405v-5003/Images/LowWageHouseholds.png


#Pull in ArkCo_Income_2017
#Load Data
ArkCo_Income_2017 <- rio::import("https://raw.githubusercontent.com/profrobwells/Data-Analysis-Class-Jour-405v-5003/master/Data/ArkCo_Income_2017.csv", skip=1)

#Clean labels
ArkCo_Income_2017 <- janitor::clean_names(ArkCo_Income_2017)
View(ArkCo_Income_2017)

#Rename Columns
library(data.table)
data.table::setnames(ArkCo_Income_2017, old = c('id', 'id2', 'geography', 'households_estimate_total', 
                                                'households_estimate_less_than_10_000', 'households_estimate_10_000_to_14_999', 
                                                'households_estimate_15_000_to_24_999', 'households_estimate_25_000_to_34_999', 
                                                'households_estimate_35_000_to_49_999', 'households_estimate_50_000_to_74_999', 
                                                'households_estimate_75_000_to_99_999', 'households_estimate_100_000_to_149_999', 
                                                'households_estimate_150_000_to_199_999', 'households_estimate_200_000_or_more',
                                                'households_estimate_median_income_dollars', 'households_estimate_mean_income_dollars',
                                                'households_estimate_percent_allocated_household_income_in_the_past_12_months',
                                                'households_estimate_percent_allocated_family_income_in_the_past_12_months',
                                                'households_estimate_percent_allocated_nonfamily_income_in_the_past_12_months'),
                     new = c('id','id2','geography','households_estimate_total','less10_000','10k_to_14_999','15k_to_24_999',
                             '25k_to_34_999', '35k_to_49_999','50k_to_74_999','75k_to_99_999','100k_to_149_999',
                             '150k_to_199_999','200k_plus','median_income','mean_income',
                             'pct_allocated_household_income','pct_allocated_family_income','pct_allocated_nonfamily_income'))

#load dplyr to use the mutate function and create groups
library(dplyr)

#Create Groups
#Low Wage
ArkCo_Income_2017 <- ArkCo_Income_2017 %>%
  replace(is.na(.), 0) %>%
  mutate(Low_Wage_Households = rowSums(.[5:7]))

#Working Class households: $25,000 to $50,000
ArkCo_Income_2017 <- ArkCo_Income_2017 %>%
  replace(is.na(.), 0) %>%
  mutate(WorkingClass = rowSums(.[8:9]))

#Middle class households: $50,000 to $150,000
ArkCo_Income_2017 <- ArkCo_Income_2017 %>%
  replace(is.na(.), 0) %>%
  mutate(MiddleClass = rowSums(.[10:12]))

#Upper income households: More than $150,000
ArkCo_Income_2017 <- ArkCo_Income_2017 %>%
  replace(is.na(.), 0) %>%
  mutate(UpperIncome = rowSums(.[13:14]))

#Using these percentages, create new columns for low-wage, working class, middle class, and upper income 
# and calculate the actual number of people in each income group
ArkCo_Income_2017$LowWagePop <- ((ArkCo_Income_2017$households_estimate_total*ArkCo_Income_2017$Low_Wage_Households)/100)
ArkCo_Income_2017$WorkingClassPop <- ((ArkCo_Income_2017$households_estimate_total*ArkCo_Income_2017$WorkingClass)/100)
ArkCo_Income_2017$MiddleClassPop <- ((ArkCo_Income_2017$households_estimate_total*ArkCo_Income_2017$MiddleClass)/100)
ArkCo_Income_2017$UpperIncomePop <- ((ArkCo_Income_2017$households_estimate_total*ArkCo_Income_2017$UpperIncome)/100)

#Save this table
Write Export output this file to a CSV or Excel  write.csv or write.excel
write.csv(ArkCo_Income_2017,"ArkCo_Income_2017_Modified.csv") 

#1. Create a bar chart with the top 10 counties with the greatest percentage of low-income population
#Find Top10Counties
#Use Dplyr - arrange
#https://dplyr.tidyverse.org/reference/arrange.html

Top10_LowWage <- ArkCo_Income_2017%>%select(geography, Low_Wage_Households)%>%arrange(desc(Low_Wage_Households))

#The cutoff for the top 10 is 39%
#Filter table to just that
Top10_LowWage <- Top10_LowWage%>%filter(Low_Wage_Households > 38.9)

#Chart It
library(ggplot2)
Top10_LowWage_Chart <- ggplot(Top10_LowWage, aes(x = reorder(geography, Low_Wage_Households), y = Low_Wage_Households))  +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Arkansas Counties With Most Low Wage Households", 
       subtitle = "Source: U.S. Census Data, 2017",
       caption = "Graphic by Rob Wells",
       x="Counties",
       y="Percentage Households Less $25,000")
plot(Top10_LowWage_Chart)


#2. Create a bar chart with the top 10 counties with the greatest percentage of upper-income population


