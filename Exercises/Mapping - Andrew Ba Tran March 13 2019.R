#Mapping - Andrew Ba Tran Chapyter 5
# from http://learn.r-journalism.com/en/mapping/
# March 13, 2019

install.packages("usethis")

usethis::use_course("https://github.com/r-journalism/learn-chapter-5/archive/master.zip")

#Load 'er up, Bob!
install.packages("learnr")
install.packages("rmarkdown")
install.packages("tidyverse")
install.packages("sf")
install.packages("leaflet")
install.packages("viridis")

#Then
rmarkdown::run("chapter-5/index.Rmd")

# If you haven't installed ggplot2 or sf yet, uncomment and run the lines below
#install.packages("ggplot2")
#install.packages("sf")

library(ggplot2)
library(sf)

# If you're using a Mac, uncomment and run the lines below
options(device = "X11") 
X11.options(type = "cairo")

#Load 50 state data
fifty_location <- "static_maps/data/cb_2017_us_state_20m/cb_2017_us_state_20m.shp"
fifty_states <- st_read(fifty_location)

View(fifty_states)

st_read()
#Basic map
ggplot(fifty_states) + geom_sf()

#Load populations
# If you don't have readr installed yet, uncomment and run the line below
#install.packages("readr")

library(readr)
populations <- read_csv("static_maps/data/acs2016_1yr_B02001_04000US55.csv")

View(populations)

#Join data to blank shapefile and map

ncol(fifty_states)

library(dplyr)

fifty_states <- left_join(fifty_states, populations,
                          by=c("NAME"="name"))

ncol(fifty_states)

colnames(fifty_states)

forty_eight <- fifty_states %>% 
  filter(NAME!="Hawaii" & NAME!="Alaska" & NAME!="Puerto Rico")


ggplot(forty_eight) +
  geom_sf(aes(fill=B02001001)) +
  scale_fill_distiller(direction=1, name="Population") +
  labs(title="Population of 48 states", caption="Source: US Census")
