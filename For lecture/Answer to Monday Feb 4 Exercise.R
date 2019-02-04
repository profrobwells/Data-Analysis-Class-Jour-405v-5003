#Answer to Monday Feb 4 Exercise

#---------------------------------------------------------------------#
#Exercise Answer:
#---------------------------------------------------------------------#

https://bit.ly/2BgPmyo

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

Top10_LowWage_Chart <- ggplot(data = Top10_LowWage) + geom_point(mapping
                                                                 =aes(x = reorder(geography, Low_Wage_Households), y = Low_Wage_Households,
                                                                      color = Low_Wage_Households, size = Low_Wage_Households)) + 
  coord_flip() +
  labs(title = "Arkansas Counties With Most Low Wage Households", 
       subtitle = "Source: U.S. Census Data, 2017",
       caption = "Graphic by Rob Wells",
       x="Counties",
       y="Percentage Households Less $25,000")
plot(Top10_LowWage_Chart)


Top10_LowWage_Chart <- ggplot(data = Top10_LowWage) + geom_point(mapping
                                                                 =aes(x = reorder(geography, Low_Wage_Households), y = Low_Wage_Households,
                                                                      color = Low_Wage_Households < 42, size = Low_Wage_Households)) + 
  coord_flip() +
  labs(title = "Arkansas Counties With Most Low Wage Households", 
       subtitle = "Source: U.S. Census Data, 2017",
       caption = "Graphic by Rob Wells",
       x="Counties",
       y="Percentage Households Less $25,000")
plot(Top10_LowWage_Chart)



#Done with bar graph
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


#-------------------------------------------------------------------------#
#Stop Here Monday#
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