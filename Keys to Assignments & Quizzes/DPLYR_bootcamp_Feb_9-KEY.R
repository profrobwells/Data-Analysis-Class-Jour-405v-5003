# DPLYR Bootcamp KEY KEY
# Rob Wells - Feb 11 2019


library(dplyr)

#Remember from Dplyr Presentation 
#Five basic verbs in Dplyr
#• filter() • select() • arrange() • mutate() • summarize() group_by()

#-----------------------------------------------#
#Task #1: Build yourself a dplyr cheat sheet. 

#Search the tutorials and exercises so far. That would be Intro_to_R, Downloading Data, Basic Data Visualization, Answer to Monday Feb 4 Exercise.

#Find examples of all of these (except summarise, which I provided). Copy those examples on this grid below. 

#This is boot camp! Do it now!!


#filter()

#select()

#arrange()

#mutate()

#summarize()
#summarise()
summarize(murders, average_victim_age=mean(VicAge))

#group_by()

#Pipes: pipe %>% 

#Find the vignette on dplyr - copy the URL for the "Introduction to dplyr" file

#----------------------------------------------------------#
#Answer to Task #1
#----------------------------------------------------------#
#filter() filter() to select cases based on their values.

Above1pct <- ArkCensus%>%select(county, Pct2017)%>%filter(Pct2017 > .01)


#select() select() and rename() to select variables based on their names.

Above1pct <- ArkCensus%>%select(county, Pct2017)%>%filter(Pct2017 > .01)


#arrange() arrange() to reorder the cases.
Top10_LowWage <- ArkCo_Income_2017%>%select(geography, Low_Wage_Households)%>%arrange(desc(Low_Wage_Households))


#mutate() mutate() and transmute() to add new variables that are functions of existing variables.
ArkCo_Income_2017 <- ArkCo_Income_2017 %>%
  replace(is.na(.), 0) %>%
  mutate(Low_Wage_Households = rowSums(.[5:7]))


#summarise()
summarise(flights,
          delay = mean(dep_delay, na.rm = TRUE)
)


#group_by() It breaks down a dataset into specified groups of rows.
mtcars %>%
  group_by(cyl) %>%
  mutate(rank = min_rank(desc(mpg)))

#Pipes: pipe %>% 

CMD +  Shift + M   

#Get Help on Dplyr 
browseVignettes("dplyr")

#http://127.0.0.1:27455/library/dplyr/doc/dplyr.html

#----------------------------------------------------------#

#Task #2
#With those examples, use the DPLYR vignette to include basic definitions of these operations. For example:
#  summarise() =  to condense multiple values to a single value.

#This is boot camp! Hurry!!

#Hand it in end of the class. Blackboard.

#----------------------------------------------------------#
#Task 3
#Use your cheat sheet to answer the following questions
#from this dataset: 
https://github.com/profrobwells/Data-Analysis-Class-Jour-405v-5003/blob/master/Data/ArkCampusCrime.xlsx

ArkCampusCrime <- rio::import('./Data/ArkCampusCrime.xlsx') 

ArkCampusCrime <- janitor::clean_names(ArkCampusCrime)
colnames(ArkCampusCrime)
str(ArkCampusCrime)
write.csv(ArkCampusCrime, "ArkCampusCrime.csv")


#1: Filter the entire table for just UA Fayetteville
#Put results in dataframe called Fayetteville
Fayetteville <- ArkCampusCrime%>%filter(campus == "Fayetteville")

#2: Filter entire ArkCampusCrime table for Fayetteville and Little Rock
#Put results in dataframe called FAYLR
#Hint ??filter, look for dplyr. See multiple criteria
FAYLR <- ArkCampusCrime%>%filter(campus == "Fayetteville" | campus == "Little Rock")



#3: Select a table with limited columns
#Table with university_college, campus, enrollment, violent and property crime
#Result in table called Main

Main <- ArkCampusCrime%>%select(university_college, campus, student_enrollment1, violent_crime, property_crime)

#4: Arrange table to sort descending by property crime.
#Dump results into PropertyCrime

PropertyCrime <- ArkCampusCrime%>%arrange(desc(property_crime))

#5: Mutuate - Create New Column with Total Crimes
#Put results in table TotalCrime
#Hint: see vignette on how to use actual column names to add things

#mutate() mutate() and transmute() to add new variables that are functions of existing variables.
TotalCrime <- ArkCampusCrime %>%
   mutate(AllCrime = violent_crime + property_crime)


#6: Calculate a violate crime rate per 1,000 students. 
#Create a table selecting campuses, enrollment and crime rates 
#just for the top 5 crime rates
ArkCampusCrime$ViolentRate <- (ArkCampusCrime$violent_crime/ArkCampusCrime$student_enrollment1)*1000

TopCrime <- ArkCampusCrime%>%
  select(campus, student_enrollment1, ViolentRate)%>%
  filter(ViolentRate > .34)

#Or Using Mohamed's Top N function
TopCrime <- ArkCampusCrime%>%select(campus, student_enrollment1, ViolentRate)%>%
  top_n(6, ViolentRate) %>%
  arrange(desc(ViolentRate))





