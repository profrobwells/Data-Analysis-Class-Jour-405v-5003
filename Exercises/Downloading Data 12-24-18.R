#Title: Downloading data
#Jan 22 2019 with Census Exercise

#rio
install.packages("rio") 

#rio handles more than two dozen formats including tab-separated data (with the extension .tsv), 
#JSON, Stata, and fixed-width format data (.fwf).

StudentLoans <- rio::import("AR2016_SMALL.csv")
glimpse(StudentLoans)

# Number columns
ncol(StudentLoans)


#Alternate download option: Use read.table() works for importing data:      

<img src="Images/ImportingDataTip.jpg", width="300" height="100" />
  

#--------------------------------------------------------------------#
#Converting character strings into numeric
#--------------------------------------------------------------------#
  
#What is the character type?
  
#Install tibble for the glimpse function
library (tibble)

glimpse(StudentLoans)  
  
#Convert numbers to "numeric" data
#We want to turn all columns after HMC2 into numeric
#HMC2 is Column #9
StudentLoans[9:102] <- lapply(StudentLoans[9:102], as.numeric)
View(StudentLoans)
glimpse(StudentLoans)  
summary(StudentLoans)

#Do some math - average number of white students
mean(StudentLoans$UGDS_WHITE)

#Why the NA result? NA= Missing Value

# list rows of data that have missing values 
#mydata[!complete.cases(mydata),]

StudentLoans[!complete.cases(StudentLoans)]

#Doing math on columns with missing values

sum(StudentLoans$UGDS_WHITE, na.rm=TRUE)
mean(StudentLoans$TUITIONFEE_IN, na.rm=TRUE)



#--------------------------------------------------------------------#
# Loading Data from Scratch
#--------------------------------------------------------------------#

Loading data
RSQlite - read data from a database
xlsx - read in Excel spreadsheets

#Import Income data from US Census
#INCOME IN THE PAST 12 MONTHS (IN 2017 INFLATION-ADJUSTED DOLLARS) 
#2013-2017 American Community Survey 5-Year Estimates. S1901. All Arkansas Counties

https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_17_5YR_S1901&prodType=table

#Load Data
ArkCo_Income_2017 <- rio::import("Data/ArkCo_Income_2017.csv")

#Look at the table
View(ArkCo_Income_2017)

# How many rows?  
nrow(ArkCo_Income_2017)

# How many columns?
ncol(ArkCo_Income_2017)

#Install tibble for the glimpse function
library (tibble)
    
#Check data types
glimpse(ArkCo_Income_2017)

#What is the issue? (Don't read ahead and spoil the fun)

#You are reading ahead
#Caught you.



#What is the issue? 




#Delete First Row Headers
#read.csv(.... , skip=1)

ArkCo_Income2_2017_copy <- rio::import("Data/ArkCo_Income2_2017_copy.csv", skip=1)

#Clean Headers - Janitor package
library(janitor)

# Clean up column names to they are R friendly
ArkCo_Income_2017 <- janitor::clean_names(ArkCo_Income_2017)
View(ArkCo_Income_2017)
     

# Still need to fix column names
colnames(ArkCo_Income_2017)

#Use setnames from the data.tablepackage will work on data.frames or data.tables
#Example
#library(data.table)
#setnames(d, old = c('a','d'), new = c('anew','dnew'))
#d


#New Names
library(data.table)
setnames(ArkCo_Income_2017, old = c('id', 'id2', 'geography', 'households_estimate_total', 
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

View(ArkCo_Income_2017)  

Manipulating data
dplyr - fast data work
stringr - work with strings

Data Management
mutate Create new column(s) in the data, or change existing column(s).
rename Rename column(s).  
bind_rows Merge two data frames into one, combining data from columns with the same name.


#Other data cleaning tricks
#Change column to number format (first you have to strip out the $)  
--The $ is a special character  
-- earnings$TOTAL.EARNINGS <- gsub("\\$", "", earnings$TOTAL.EARNINGS) 

--Export data 
Write Export output this file to a CSV or Excel  write.csv or write.excel
write.csv(AR2016_SMALL,"AR2016_SMALL.csv") 


#--------------------------------------------------------------------#
#More Advanced Section from Machlis Book, Ch. 4
#--------------------------------------------------------------------#


#get data for tutorial
download.file("http://bit.ly/BostonSnowfallCSV", "BostonWinterSnowfalls.csv")

#load into memory
snowdata <- rio::import("BostonWinterSnowfalls.csv")



#Data Cleaning install own function in my own rmiscutils package 
#turns “character strings” -- numbers with commas back into numbers
pacman::p_load_gh("smach/rmiscutils")

#more software
install.packages("remotes")
install.packages("githubinstall")
githubinstall::gh_install_packages("rmiscutils")

install.packages("htmltab")
library(htmltab)
citytable <- htmltab(
  "https://en.wikipedia.org/wiki/List_of_United_States_cities_by_population", 
  which = 5)
colnames(citytable)

library(rmiscutils)
citytable$PopEst2017 <- number_with_commas(citytable$`2017estimate`)

#parsing numbers with readr
#After installing readr, you could generate numbers from the 
#2017 estimate column with readr:
  
citytable$PopEst2017 <- readr::parse_number(citytable$`2017 estimate`)


#-----------------------------#
#Install pacman adventure
#-----------------------------#

install.packages("devtools")
install.packages("tsibble")
library(devtools)

## Make sure your current packages are up to date
update.packages()
#ERROR: this R is version 3.4.3, package 'pacman' requires R >= 3.5.0
install.packages("devtools")
devtools::install_github("AndreaCirilloAC/updateR")
updateR(admin_password = "taxi66") 
# Where "PASSWORD" stands for your system password
#https://www.r-statistics.com/2018/04/r-3-5-0-is-released-major-release-with-many-new-features/
  

## devtools is required
library(devtools)
install_github("trinker/pacman")

install.packages("pacman", repos="https://cran.rstudio.com/mirrors.html 576", dependencies=TRUE)


#-----------------------------#
# Notes on Ch. 4
#-----------------------------#  

Ch 4 Import Data into R
* rio
* htmltab
* readxl
* googlesheets
* pacman
* janitor
* rmiscutils (GitHub) or readr
* tibble

rio
install.packages("rio") 
--rio handles more than two dozen formats including tab-separated data (with the extension .tsv), JSON, Stata, and fixed-width format data (.fwf).
Chr to Numeric
This is one of R’s small annoyances: R generally doesn’t understand that 8,550 is a number. I dealt with this problem myself by writing my own function in my own rmiscutils package to turn all those “character strings” that are really numbers with commas back into numbers. Anyone can download the package from GitHub and use it.
Skip rows
Beginning rows that aren’t part of the data. If you know that the first few rows of an Excel spreadsheeet don’t have data you want, you can tell rio to skip one or more lines. The syntax is rio::import("mySpreadsheet.xlsx", skip=3) to exclude the first three rows. skip takes an integer.
Import and create new column names
Or, use a syntax such as rio::import("mySpreadsheet.xlsx", col_names = c("City", "State", "Population")) to set your own column names.
Import second tab of SS
If there are multiple tabs in your spreadsheet, the which argument will override the default of reading in the first worksheet. rio::import("mySpreadsheet.xlsx", which = 2) reads in the second worksheet.
Slick trick to add a column and do a conversion
It’s easy to add a column to a data frame. Currently, the Total column shows winter snowfall in inches. To add a column showing totals in Meters, you can use this format:
  snowdata$Meters <- snowdata$Total * 0.0254
4.5 Convert zip codes into usable data. Boston zip codes with leading zeroes.


