#Title: Machlis - Basic Data Visualization
#UpdatedJan 29 2019


#load software - Select NO when asked to restart
install.packages("ggplot")
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



#Tutorial
#Import Data, Create Dataframe, Rename Columns
snowdata <- rio::import("data/BostonChicagoNYCSnowfalls.csv")
bostonsnow <- select(snowdata, Winter, Boston)
names(bostonsnow)[2] <- "TotalSnow"

bostonsnow <- select(snowdata, Winter, Boston) %>%
  rename(TotalSnow = Boston)

bostonsnow <- select(snowdata, Winter, TotalSnow = Boston) 

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


#Exercise:
#Pull in ArkCo_Income_2017
#1. Create a bar chart with the top 10 counties with the greatest percentage of low-income population
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


