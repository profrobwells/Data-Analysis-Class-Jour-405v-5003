# "Introduction to R - Spring 2018"
# Rob Wells, PhD
# 1/15/2019

# ------- Get Organized --------- #  

###Set Working Directory. My directory "~/Dropbox/Classes/Data-Analysis-Class-Jour-405v-5003" is an example

getwd()
setwd("~/Dropbox/Classes/Data-Analysis-Class-Jour-405v-5003")

#  Here is some basic information about R  
#  There are four main windows:  

# Script writing, R Markdown, Table Viewer: Upper Left  
# Environment - data loaded in R: Upper Right  
# Console - write commands in R: Lower Left  
# File Manager and Html table viewer: Bottom Right  



#  In the Console window, type:
demo()
help()
help.start()


### Create a folder for this project on your Desktop  

#Create a script to put all your code -- top left window. 
#File > New File > R Script  

# Download Census data into that folder
# Create an R Project in that folder


#Install software to grab data
##rio handles more than two dozen formats including tab-separated data (with the extension .tsv), 
#JSON, Stata, and fixed-width format data (.fwf).
install.packages("rio") 
install.packages("tidyverse")
install.packages("readxl")
install.packages("dplyr")
install.packages("janitor")

### Load Libraries  

Libraries are bits of software you will have to load each time into R to make things run. Here, we will **load library(tidyverse) and library (readxl)**:  
library (tidyverse)
library (readxl)
library (dplyr)
library(rio)
library(janitor)


#Get census data
#Check the website: 
https://factfinder.census.gov/bkmk/table/1.0/en/PEP/2017/PEPANNRES/0400000US05.05000


#Load table - ArkCensus.csv - 
download.file("https://bit.ly/2FxLJHj", "ArkCensus.csv")

#It downloaded it to your current working directory. Use this command to find where it is
getwd() 

#Import using Rio. Make sure ArkCensus is in your working directory!
ArkCensus <- rio::import("ArkCensus.csv")

#Look at the table
View(ArkCensus)

#Note that this data table has sortable headers. Pretty goddamned nice.  

# How many rows?  
nrow(ArkCensus)

# How many columns?
ncol(ArkCensus)

# Let's look at the first six rows
head(ArkCensus)

#Check data types
glimpse(ArkCensus)

#Here is a quick way to view the range of your data  
summary(ArkCensus)

# Clean up column names to they are R friendly
ArkCensus <- janitor::clean_names(ArkCensus)
View(ArkCensus)



# another method Rename a specific column
colnames(ArkCensus)[5] <- "BaseEstimate"
View(ArkCensus)


#Create a New Column and a Formula: Percents of Washington County as Whole of the state of Arkansas

ArkCensus$Pct2017 <- ((ArkCensus$x2017-ArkCensus$x2016)/(ArkCensus$x2016))

#To quickly format into percents, load
install.packages("formattable")
library(formattable)

ArkCensus$Pct2017 <- percent(ArkCensus$Pct2017)

View(ArkCensus)


# Sort to see biggest-smallest descending in population from 2016-2017 

ArkCensus <- ArkCensus[order(-ArkCensus$Pct2017),]
View(ArkCensus)

# What is the average population change?

mean(ArkCensus$Pct2017)
median(ArkCensus$Pct2017)

#Find all counties with population growth above 1%

Above1pct <- ArkCensus%>%select(county, Pct2017)%>%filter(Pct2017 > .01)

#Build a table with places with upper quantile of crime
quantile(ArkCensus$Pct2017)

# the Upper Quantile is 0.74% to 2.94%
TopGrowth <- ArkCensus%>%select(county, Pct2017)%>%filter(Pct2017 > 0.0074)

#Find all places with below average crime

MajorLosers <- ArkCensus%>%select(county, Pct2017)%>%filter(Pct2017 < -0.0093)


#-------------------------------------------------------------------#
#Build a chart - Total loan disbursement
#-------------------------------------------------------------------#

library(ggplot2)

TopGrowthChart <- ggplot(TopGrowth, aes(x = county, y=Pct2017)) +
geom_bar(stat = "identity") +
coord_flip() +     #this makes it a horizontal bar chart instead of vertical
labs(title = "Your title here", 
subtitle = "Subtitle and source ",
caption = "Graphic by Rob Wells",
x="County",
y="Population growth 2016-2017")
plot(TopGrowthChart)


#-------------------------------------------------------------------#
#      What You Have Learned So Far
#-------------------------------------------------------------------#  

# How to navigate in R studio
# How to install libraries and packages 
# How to import a .csv file into R: read.csv
# How to obtain summary statistics (summary)
# How to create a new calculated field



