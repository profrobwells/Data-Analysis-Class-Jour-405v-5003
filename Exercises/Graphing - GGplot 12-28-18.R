
#-------------------------------------
#9 Graphing by Group
#-------------------------------------
install.packages('pacman')
install.packages('geofacet')
install.packages('RColorBrewer')
library(geofacet)
library(RColorBrewer)


Load this file
ny <- rio::import("https://raw.githubusercontent.com/profrobwells/Data-Analysis-Class-Jour-405v-5003/master/Data/ny.csv")


#Exercise #2: Exercise 2: What was the percent of cancelled flights each day 
#among flights that were supposed to leave during the Sandy time period? 
#Create a data frame called departing_cancellations from the ny data. 
#Hint: You first want to filter for flights leaving JFK, LGA, and EWR; 
#group by day; summarize the number of cancellations and total flights; 
#and then calculate percents. (Answer is at the end of this chapter.)


departing_cancellations <- ny %>%
  filter(ORIGIN %in% c("JFK", "LGA", "EWR")) %>%
  filter(FL_DATE >= "2012-10-27", FL_DATE <= "2012-11-03",
  ) %>%
  group_by (FL_DATE, ORIGIN) %>%
  summarize(
    TotalCancelled = sum(CANCELLED),
    TotalFlights = n(),
    PctCancelled = (TotalCancelled / TotalFlights) * 100
  )

ggplot(departing_cancellations, aes(x=FL_DATE, y=PctCancelled, fill=ORIGIN)) + 
  geom_col(position="dodge") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.2, hjust = 1.1))

#Add your own colors
ggplot(departing_cancellations, aes(x=FL_DATE, y=PctCancelled, fill=ORIGIN)) + 
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("black", "darkgrey", "white"))

#Get ugly
ggplot(departing_cancellations, aes(x=FL_DATE, y=PctCancelled, fill=ORIGIN)) + 
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("pink", "green", "blue"))


#Exercise stopped 2-5-19 #





#facet_grid

ggplot(departing_cancellations, aes(x=FL_DATE, y=PctCancelled, fill=ORIGIN)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x= element_text(angle=45, hjust = 1.3, vjust = 1.2)) +
  facet_grid(. ~ ORIGIN)

#You can also add fill=ORIGIN to aes(x=FL_DATE, y=PctCancelled) 
#if you’d like the graphs to have different colors.

departing_cancelled_raw <- filter(sandyCancelled, ORIGIN %in% c("JFK", "LGA", "EWR"))

ggplot(departing_cancelled_raw, aes(x=FL_DATE)) +
  geom_bar

#by airport
ggplot(departing_cancelled_raw, aes(x=FL_DATE)) +
  geom_bar() +
  theme(axis.text.x= element_text(angle=45, hjust = 1.3, vjust = 1.2)) +
  facet_grid(. ~ ORIGIN)

ggplot(departing_cancelled_raw, aes(x=FL_DATE)) +
  geom_bar() +
  theme(axis.text.x= element_text(angle=45, hjust = 1.3, vjust = 1.2)) +
  facet_grid(CARRIER ~ ORIGIN)


#housing prices
homeprices <- rio::import("data/state_statistics_for_download.xls", skip = 4)
str(homeprices)
head(homeprices)
summary(homeprices)
Hmisc::describe(homeprices$year_quarter)
unique(homeprices$year_quarter)
homeprices <- janitor::clean_names(homeprices)
head(homeprices)

homeprices <- filter(homeprices, !(is.na(state)))

#graph home prices by state
ggplot(homeprices, aes(x=year_quarter, y=median_price, group = state)) +
  geom_line() +
  facet_wrap(~ state, ncol = 8)

#clean up
options(scipen = 999)
ggplot(homeprices, aes(x=year_quarter, y=median_price, group = state)) +
  geom_line() +
  facet_wrap(~state, ncol = 12)  + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()
  )

#Map
pacman::p_load(geofacet)
get_grid_names()
grid_preview(us_state_grid2)
facet_geo(~ state, grid = "us_state_grid2")

ggplot(homeprices, aes(x=year_quarter, y=median_price, group = state)) +
  geom_line() +
  facet_geo(~ state, grid = "us_state_grid2") +
  theme(axis.text.x= element_blank(),
        axis.ticks.x=element_blank() ,
        axis.ticks.y=element_blank() ,
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()
  ) +
  ggtitle("Quarterly Median Home Prices 2000-2010") 


delayed_ny <- sandyFlights%>%
  filter(CANCELLED != 1, ORIGIN %in% c("JFK", "LGA", "EWR"))

ggplot(delayed_ny, aes(x = AIR_TIME, y = DEP_DELAY, color = ORIGIN)) +
  geom_point(position = "jitter")

filter(delayed_ny, is.na(AIR_TIME))

ggplot(departing_cancellations, aes(x=FL_DATE, y=PctCancelled, fill=ORIGIN)) + 
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("black", "darkgrey", "white"))

install.packages("ggiraph")
library(ggiraph)

lga$tooltip <- paste0(lga$CARRIER, " Flight ", lga$FL_NUM, " from ", lga$ORIGIN, " to ", 
                      lga$DEST, " on ", lga$FL_DATE)
lga$clickjs = paste0("alert(\"",lga$tooltip, "\")" )

myscatterplot <- ggplot(lga, aes(x = AIR_TIME, y = DEP_DELAY, 
                                 color = ORIGIN, tooltip = tooltip, onclick = clickjs)) +
  geom_point_interactive(position = "jitter") +
  scale_color_brewer(palette = "Dark2")

ggiraph(code = {print(myscatterplot)})


pacman::p_load(dplyr, lubridate, testthat)