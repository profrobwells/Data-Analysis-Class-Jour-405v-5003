#Machlis - Maps in R Ch 11
#Updated March 24, 2019

#-------------------------------------
# Ch 11 Maps in R
#-------------------------------------  

library(devtools)
#Install mapping material
#install devtools if you haven't already done so
#install("devtools")

devtools::install_github('walkerke/tigris')
devtools::install_github('bhaskarvk/leaflet.extras', force = TRUE)

#Old school version
#library(devtools)
#install_github("walkerke/tigris")
#install_github('bhaskarvk/leaflet.extras', force = TRUE)

library(leaflet)
library(glue)
library(dplyr)
library(sf)
library(tmap)
library(tmaptools)
library(tidycensus)
library(ggmap)
library(htmltools)
library(htmlwidgets)

#Alternate install proceedure using pacman
#pacman::p_load(leaflet, glue, dplyr, sf, tmap, tmaptools, tidycensus, ggmap, htmltools, htmlwidgets) 
#pacman::p_load_gh(c("walkerke/tigris", "bhaskarvk/leaflet.extras"))

#load the map software
library(tigris)
options(tigris_use_cache = TRUE)

#create a cache directory on your machine
#Go to environment, New Folder, name is cache
#Go to Finder, select cache directory
#cntl+click on the cache folder. A menu appears
#cntl+option = copy cache directory path
#paste that path below

tigris_cache_dir("/Users/robwells/Dropbox/Classes/Data-Analysis-Class-Jour-405v-5003/cache")
readRenviron('~/.Renviron')
#Restart R


#-------------------------------------------------------------#
#               HERE WE GO!                                  #
#-------------------------------------------------------------#

#For many mapping analyses, you need 
#1) a file defining a geographic area such as towns, counties, or states; 
#2) a file with data about those units, such as which towns voted for what candidates
#3) a way to merge the two, and then display the results.


ca_counties <- counties("CA")
plot(ca_counties)

ca_schools <- school_districts("CA")

plot(ca_schools)

#plotting an simple features map
ca_counties_sf <- counties("CA", class = "sf")
plot(ca_counties_sf)

#Using "simple features" mapping standard 
#Simple features are conventional data frames 
#that have one special column of geography.
dplyr::glimpse(ca_counties_sf) 
#Easier to deal with than SpatialPolygonsDataFrame
dplyr::glimpse(ca_counties) 

#quick thematic mapping function qtm()
#tmap function similar to ggplot2’s qplot()
qtm(ca_counties_sf)

#Importing Map Data
#
#Load two folders to your data directory: acs2017_5yr and acs2015_5yr
 
ca_income <- read_shape(
  "data/acs2017_5yr_B19013_05000US06063/acs2017_5yr_B19013_05000US06063.shp",
  as.sf = TRUE)
#
str(ca_income)
head(ca_income)
#
#deleting first row in df that has statewide data
df = ca_income[-1,]
ca_income <- df

#Rename columns
names(ca_income)[2:4] <- c("County", "Median.Income", "MoE")
#
#scipen eliminates scientific notation on x axis for this plot below
options(scipen = 999)
hist(ca_income$Median.Income)
#
#Interactive maps with tmap
tmap_mode("view")
#
#Show median income by county
qtm(ca_income, fill = "Median.Income")
#
#Shortcut between views
ttm()
#Redraw last map
last_map()
#
#Interactive maps with tmap
tmap_mode("view")
#
#Interactive with name popups
tm_shape(ca_income) +
  tm_polygons(col = "Median.Income", id = "County")

#Interactive with rollovers and popups
ca_income_map <- tm_shape(ca_income) +
  tm_polygons(col = "Median.Income", id = "County")

#save as html object
tmap_save(ca_income_map, "CA_Counties_Map.html")
#
#Victory!
#

#-----------------------------------
#YOUR TURN
#Repeating steps for Arkansas
#-----------------------------------
ar_counties <- counties("AR")

#Get Median Income Shapefile from CensusReporter
https://censusreporter.org/
  
  #1: Tell the thing what you want
  #In the Explore box, type "Median income"
  #Select: Table B19013: Median Household Income
  #Type in "Arkansas" where it says “Start typing to pick a place”
  #Select Arkansas - state
  #Headline now says: Median Household Income in the Past 12 Months (In 2017 Inflation-adjusted Dollars)
  #See menu on left “Divide Arkansas into …”. Choose counties
  #See upper right, click to download data at the top right.
  #One of the choices is a shapefile including both geography and data. 
  #Download shapefile. Unzip it. 
#Move file to your data folder. Import it with:

ar_income <- read_shape("data/acs2017_5yr_B19013_05000US05055/acs2017_5yr_B19013_05000US05055.shp", 
                        as.sf = TRUE)