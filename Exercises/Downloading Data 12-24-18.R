#Title: Downloading data
#Jan 22 2019 with Census Exercise

#rio
install.packages("rio") 

#rio handles more than two dozen formats including tab-separated data (with the extension .tsv), 
#JSON, Stata, and fixed-width format data (.fwf).

StudentLoans <- rio::import('./Data/AR2016_SMALL.csv')
View(StudentLoans)

# Number columns
ncol(StudentLoans)

#vignettes: Learn about packages and commands
browseVignettes("rio")
??rio


#--------------------------------------------------------------------#
#Converting character strings into numeric
#--------------------------------------------------------------------#
  
#What is the character type?
library(dplyr)
    
#Tibble or Dplyr for the glimpse function
dplyr::glimpse(StudentLoans)
tibble::glimpse(StudentLoans)  

#chr stands for character vectors, or strings.
#int stands for integers.
#dbl stands for doubles, or real numbers.
#dttm stands for date-times (a date + a time).

#Convert numbers to "numeric" data
#We want to turn all columns after HMC2 into numeric
#HMC2 is Column #10

#Check you have the right names you want to convert
colnames(StudentLoans[10:102])
  

StudentLoans[10:102] <- lapply(StudentLoans[10:102], as.numeric)
glimpse(StudentLoans)  
#Data changed from <chr> to <dbl>


#Run stats
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

#Loading data
#RSQlite - read data from a database
#xlsx - read in Excel spreadsheets

#Import Income data from US Census
#INCOME IN THE PAST 12 MONTHS (IN 2017 INFLATION-ADJUSTED DOLLARS) 
#2013-2017 American Community Survey 5-Year Estimates. S1901. All Arkansas Counties

#https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_17_5YR_S1901&prodType=table

#Load Data
ArkCo_Income_2017 <- rio::import("Data/ArkCo_Income_2017.csv")

#Look at the table
View(ArkCo_Income_2017)

# How many rows?  
nrow(ArkCo_Income_2017)

# How many columns?
ncol(ArkCo_Income_2017)

#Install dplyr or tibble for the glimpse function if you haven't already
#library (tibble)
    
#Check data types
glimpse(ArkCo_Income_2017)

#What is the issue? (Don't read ahead and spoil the fun)

#You are reading ahead
#Caught you.



#What is the issue? 




#Delete First Row Headers
#Reimport the data and skip the first row
#read.csv(.... , skip=1)

ArkCo_Income_2017 <- rio::import("Data/ArkCo_Income_2017.csv", skip=1)
View(ArkCo_Income_2017)

#Clean Headers - Janitor package
#library(janitor)

# Clean up column names to they are R friendly
ArkCo_Income_2017 <- janitor::clean_names(ArkCo_Income_2017)
View(ArkCo_Income_2017)
     

# Still need to fix column names
colnames(ArkCo_Income_2017)

#You can do it one at a time
#Column 4 households_estimate_total renamed to household_income
colnames(ArkCo_Income_2017)[4] <- "household_income"
colnames(ArkCo_Income_2017)

#change it back
colnames(ArkCo_Income_2017)[4] <- "households_estimate_total"
colnames(ArkCo_Income_2017)

#------------------------------------------#
#Rename a whole slug of columns at once!
#So the following is a *little intense*
#------------------------------------------#

#Use setnames from the data.tablepackage will work on data.frames or data.tables
#Example
#library(data.table)
#setnames(d, old = c('a','d'), new = c('anew','dnew'))
#d


#We are changing all of the old column names to new ones
#That's 19 column names we are changing.

#New Names
#library(data.table)
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

View(ArkCo_Income_2017)  

#Manipulating data
#dplyr - fast data work
#stringr - work with strings

#Data Management
#mutate - Create new column(s) in the data, or change existing column(s).
#mutate() adds new variables and preserves existing;
# Newly created variables are available immediately

#An example:
mtcars <- as.data.frame(mtcars)
View(mtcars)

mtcars2 <- mtcars %>% as_tibble() %>% mutate(
  cyl2 = cyl * 2,
  cyl4 = cyl2 * 2
)

# window functions are useful for grouped mutates
mtcars %>%
  group_by(cyl) %>%
  mutate(rank = min_rank(desc(mpg)))

#Use mutate to add together the percentages of low-wage households
ArkCo_Income_2017 <- ArkCo_Income_2017 %>%
  replace(is.na(.), 0) %>%
  mutate(Low_Wage_Households = rowSums(.[5:7]))

--Export data 
Write Export output this file to a CSV or Excel  write.csv or write.excel
write.csv(ArkCo_Income_2017,"ArkCo_Income_2017.csv") 



#Exercises
# 1) Create a column for working class households: $25,000 to $50,000
# 2) Create a column for middle class households: $50,000 to $150,000
# 3) Create a column for upper income households: More than $150,000
# 4) Using these percentages, create new columns for low-wage, working class, middle class, and upper income 
# and calculate the actual number of people in each income group
# This will require looking at the table data structure, so go to the census.gov link provided above


#Answers
# 1) Create a column for working class households: $25,000 to $50,000
ArkCo_Income_2017 <- ArkCo_Income_2017 %>%
  replace(is.na(.), 0) %>%
  mutate(WorkingClass = rowSums(.[8:9]))


# 2) Create a column for middle class households: $50,000 to $150,000
ArkCo_Income_2017 <- ArkCo_Income_2017 %>%
  replace(is.na(.), 0) %>%
  mutate(MiddleClass = rowSums(.[10:12]))


# 3) Create a column for upper income households: More than $150,000
ArkCo_Income_2017 <- ArkCo_Income_2017 %>%
  replace(is.na(.), 0) %>%
  mutate(UpperIncome = rowSums(.[13:14]))


# 4) Using these percentages, create new columns for low-wage, working class, middle class, and upper income 
# and calculate the actual number of people in each income group
# This will require looking at the table data structure, so go to the census.gov link provided above

#Copied this as a test
#ArkCensus$Pct2017 <- ((ArkCensus$x2017-ArkCensus$x2016)/(ArkCensus$x2016))


ArkCo_Income_2017$LowWagePop <- ((ArkCo_Income_2017$households_estimate_total*ArkCo_Income_2017$Low_Wage_Households)/100)

ArkCo_Income_2017$WorkingClassPop <- ((ArkCo_Income_2017$households_estimate_total*ArkCo_Income_2017$WorkingClass)/100)

ArkCo_Income_2017$MiddleClassPop <- ((ArkCo_Income_2017$households_estimate_total*ArkCo_Income_2017$MiddleClass)/100)

ArkCo_Income_2017$UpperIncomePop <- ((ArkCo_Income_2017$households_estimate_total*ArkCo_Income_2017$UpperIncome)/100)

#For amusement, see if they all add up
ArkCo_Income_2017 <- ArkCo_Income_2017 %>%
  replace(is.na(.), 0) %>%
  mutate(SumPop = rowSums(.[24:27]))


#Eyeball the two columns, household_estimate_total and our SumPop
#df1 <- select(AR2016ALL, V4:V8, V10:20)
PopCheck <- select(ArkCo_Income_2017, households_estimate_total, SumPop) 
       
#which ones varied the most?

PopCheck$variance <- (ArkCo_Income_2017$households_estimate_total- ArkCo_Income_2017$SumPop) 

#nerdy checking individual
ArkCo_Income_2017 <- ArkCo_Income_2017 %>%
  +   replace(is.na(.), 0) %>%
  +   mutate(SumIndivdPct = rowSums(.[5:14]))

#more sum groups
ArkCo_Income_2017 <- ArkCo_Income_2017 %>%
  replace(is.na(.), 0) %>%
  mutate(SumGroupPct = rowSums(.[20:23]))


PopCheck <- select(ArkCo_Income_2017, households_estimate_total, SumPop, SumIndivdPct, SumGroupPct) 


#Other tools

#rename - Rename column(s).  
#bind_rows - Merge two data frames into one, combining data from columns with the same name.


#Other data cleaning tricks
#Change column to number format (first you have to strip out the $)  
--The $ is a special character  
-- earnings$TOTAL.EARNINGS <- gsub("\\$", "", earnings$TOTAL.EARNINGS) 


#Quick Data Viz
#Basic graphs
plot(ArkCo_Income_2017$median_income)

hist(ArkCo_Income_2017$median_income)  
boxplot(ArkCo_Income_2017$median_income)
barplot(ArkCo_Income_2017$median_income)
barplot(sort(ArkCo_Income_2017$median_income, decreasing = TRUE))


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


