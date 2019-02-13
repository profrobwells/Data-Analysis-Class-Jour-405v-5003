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

PropertyCrime <- ArkCampusCrime%>%
  arrange(desc(property_crime))

#4a: Combine #3 and #4:
Main <- ArkCampusCrime%>%
  select(university_college, campus, student_enrollment1, violent_crime, property_crime) %>% 
  arrange(desc(property_crime))

#5: Mutuate - Create New Column with Total Crimes
#Put results in table TotalCrime
#Hint: see vignette on how to use actual column names to add things

#mutate() mutate() and transmute() to add new variables that are functions of existing variables.
TotalCrime <- ArkCampusCrime %>%
   mutate(AllCrime = violent_crime + property_crime)


#6: Calculate a violate crime rate per 1,000 students. 
#Create a table selecting campuses, enrollment and crime rates 
#just for the top 5 crime rates

#Halie's method is correct
ArkCampusCrime$violentrate <- 
  (ArkCampusCrime$violent_crime/ArkCampusCrime$student_enrollment1)*1000

TopCrime <- ArkCampusCrime%>%
  select(campus, student_enrollment1, violentrate)%>%
  filter(violentrate > .34)

#Revision
ArkCampusCrime2 <- ArkCampusCrime %>% 
  mutate(violentrate = (violent_crime / student_enrollment1)*1000) %>% 
  top_n(5, violentrate) %>%
  arrange(desc(violentrate))



# Using Mohamed's Top N function
TopCrime <- ArkCampusCrime%>%select(campus, student_enrollment1, ViolentRate)%>%
  top_n(6, violentrate) %>%
  arrange(desc(violentrate))

#--------------------------------------#
#     Now We Will Graph                #
#--------------------------------------#

library(ggplot2)

#Refer to Basic Data Visualization 2-4-19.R for the following

#1: Create a histogram of campus violentrate
hist(ArkCampusCrime$violentrate)

#Basic plot of violentrate
plot(ArkCampusCrime$violentrate)

#Histogram of property crime
hist(ArkCampusCrime$property_crime)

#Box Plot
boxplot(ArkCampusCrime$violentrate)

#Dual box plots
ggplot(data=ArkCampusCrime) + 
  geom_boxplot(aes(x = "violentrate", y = violentrate)) +
  geom_boxplot(aes(x = "property_crime", y = property_crime))

#Use a top 5 chart

#Do the Katie Serrano special and add some killer colors
Top5Chart <- ggplot(ArkCampusCrime2, aes(x = reorder(university_college, -violentrate), 
                                         y = violentrate, color = university_college, 
                                         fill=university_college))+
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Arkansas Campuses With Top Crime Rates Per 1,000", 
       subtitle = "FBI Uniform Crime Report Data, 2017",
       caption = "Graphic by Rob Wells",
       x="City",
       y="Crime Rate Per 10,000 People")
plot(Top5Chart)

#Dual Chart Violent and Property Crime
ggplot(data=ArkCampusCrime, aes(campus)) + 
  geom_point(aes(y = violent_crime, colour = "violent_crime")) + 
  geom_point(aes(y = property_crime, colour = "property_crime"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 1.2, hjust = 1.1))

ggplot(data=ArkCampusCrime, aes(campus)) + 
  geom_col(aes(y = violent_crime, fill = "violent_crime")) + 
  geom_point(aes(y = property_crime, colour = "property_crime", size = "property_crime"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 1.2, hjust = 1.1))