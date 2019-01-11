# "Introduction to R - Spring 2018"
# Rob Wells, PhD
# 1/8/2019

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
q()

### Create a folder for this project on your Desktop  

#Create a script to put all your code -- top left window. 
#File > New File > R Script  

# Download FBI Crime data into that folder
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
https://www.census.gov/quickfacts/fact/table/washingtoncountyarkansas,ar,US/PST045217


#Load table - XXXXX
options(header=FALSE, stringsAsFactors = FALSE,fileEncoding="utf-8")
Census <- read.csv(file.choose(), stringsAsFactors = FALSE)

#Look at the table
View(Census)

#Note that this data table has sortable headers. Pretty goddamned nice.  

# How many rows?  
nrow(Census)

# Let's look at the first six rows
head(Census)

#Check data types
glimpse(Census)


#More data cleaning #

###Here is a quick way to view the range of your data  
summary(Census1)

##Why no math on this?

#Remove rows with NA
newdata <- Census[-c(1,3,5), ] 
View(newdata)

#Once everything is ok, rename it back to Census
Census1 <- newdata

#Convert numbers to "numeric" data
Census1[2:4] <- lapply(Census1[2:4], as.numeric)
View(Census1)

###Summary Statistics
summary(Census1)

# Clean up column names to they are R friendly
Census1 <- janitor::clean_names(Census1)
View(Census1)


# another method Rename a specific column
colnames(Census1)[2] <- "WashCo_Ark"
colnames(Census1)[4] <- "USA"
View(Census1)


#Create a New Column and a Formula: Percents of Washington County as Whole of the state of Arkansas

Census1$Wash_Pct_Ark <- ((Census1$WashCo_Ark)/(Census1$Arkansas))
View(Census1)



### Let's Look Closely At These Numbers
### The program just calculated the minimum, maximum value
### It calculated the average (mean) and the median
### Let's store this somewhere so we can refer to it as we build a chart


XXXXXSTOPPED THURSDAY 3:30 PM XXXXX


# Sort by column descending  
  
Census <- Census[order(-Census$Murder),]
View(Census)





##What Just Happened:
### -- We created a new calculated field: Per 10,000 residents
### -- We divided Total Recipients into Total Disbursements and multiplied by 10,000


### Type in R code to sort Violent Crime rate descending
Census <- Census[order(-Census$Per10000),]
View(Census)

#Calculations on columns 
#Here we will calculate the total violent crimes and create a new variable to store the results.

TotalViolent2017 <- sum(Census$Violent_Crime)
AvgViolent2017 <- mean(Census$Violent_Crime)
MedianViolent2017 <- median(Census$Violent_Crime)

#To See the Results

TotalViolent2017
AvgViolent2017 
MedianViolent2017 


summary(Census)

#Standard Deviation
sd(Census$Per10000, na.rm=TRUE)

#Find all counties with above average crime rates of 42.91 per 10,000

Above_Average <- Census%>%select(Place, Per10000)%>%filter(Per10000 > 42.91)

#Build a table with places with upper quantile of crime
quantile(Census$Per10000)

# the Upper Quantile is 55.83 to 226.27
TopCrime <- Census%>%select(Place, Per10000)%>%filter(Per10000 > 55.83)

#Find all places with below average crime

Below_Average <- Census%>%select(Place, Per10000)%>%filter(Per10000 < 42.91)




#-------------------------------------------------------------------#
#Build a chart - Total loan disbursement
#-------------------------------------------------------------------#

library(ggplot2)

StudentDebt_chart <- ggplot(NationalLoans, aes(x = State, y=TotalDisbursements_Mil.)) +
geom_bar(stat = "identity") +
coord_flip() +     #this makes it a horizontal bar chart instead of vertical
labs(title = "Total disbursements U.S. colleges, grants and loans 2017", 
subtitle = "2017 data. Source: U.S. Department of Education. https://studentaid.ed.gov/sa/about/data-center/student/title-iv ",
caption = "Graphic by Rob Wells",
x="State",
y="All grants and loans disbursed in 2017. ")
plot(StudentDebt_chart)


#-------------------------------------------------------------------#
#      What You Have Learned So Far
#-------------------------------------------------------------------#  

# How to navigate in R studio
# How to install libraries and packages 
# How to import a .csv file into R: read.csv
# How to obtain summary statistics (summary)
# How to create a new calculated field

#-------------------------------------------------------------------#
#      Resources
#-------------------------------------------------------------------# 

### See this slide deck about R 
<p><a href="https://docs.google.com/presentation/d/1zICxR7qDM3RQ2Nxi5CqHlM3H8I7qoVkNtqcNcnbbDCw/edit#slide=id.gb2f956a38_0_14">This tutorial from Andrew Ba Tran, data journalist at the Washington Post</a> gives a good overview of R.</p>

###Time Out: More Introductory Material  
> Here is a great slide deck on R 
<p><a href="https://docs.google.com/presentation/d/1O0eFLypJLP-PAC63Ghq2QURAnhFo6Dxc7nGt4y_l90s/edit#slide=id.g1bc441664e_0_93">.Rddj</a>, Excellent R introduction.</p>
  > <>  
  
#Digging Deeper
#Material from MaryJo Webster, Minn. Star Tribune
<p><a href="https://github.com/mjwebster/R_tutorials/blob/master/Using_R.Rmd</p>
<p><a href="https://www.computerworld.com/article/2497143/business-intelligence/business-intelligence-beginner-s-guide-to-r-introduction.html">Beginner's guide to R, by Sharon Machlis</a>. This covers a lot of basic ground and provides a ton of useful links</p>
<p><a href="https://github.com/Caelainn/R3_NICAR18">This tutorial from NICAR18 by Caelinn Barr</a> covers how to scrape data from a website and some other slightly more advanced concepts.</p>
<p><a href="https://rddj.info/">.Rddj</a>, resources for doing data journalism with R.</p>
<p><a href="https://github.com/dhmontgomery/r-data-for-beginners/blob/master/r_for_beginners.md">Ron Campbell's Intro to R hands-on class from NICAR18.</a> This one is a little more advanced and uses more Base R than the others.</p>



#-------------------------------------------------------------------#
#      MISC
#-------------------------------------------------------------------# 


### Read the Data: Load the student loan data into R and check it out.

### Read file - Standard Method Defining File Path
### To Copy File Path, use Finder and highlight the file name
### Cntl + click file name. See option to Copy file
### Continue to hold Cntl + click file name. Press Option key. See copy file path

Alternate Downloading sequence

options(header=FALSE, stringsAsFactors = FALSE,fileEncoding="utf-8")
NationalLoans <- read.csv(file.choose(), stringsAsFactors = FALSE) 

