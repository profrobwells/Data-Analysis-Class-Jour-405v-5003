#Machlis - Maps in R Ch 10
#Updated March 12, 2019

#-------------------------------------
# Ch 11 Maps in R
#-------------------------------------  

#library(devtools)
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

ca_counties_sf <- counties("CA", class = "sf")
plot(ca_counties_sf)

#quick thematic mapping function qtm() 
qtm(ca_counties_sf)

#Load two folders to your data directory: acs2017_5yr and acs2015_5yr
 
ca_income <- read_shape("data/acs2017_5yr_B19013_05000US06063/acs2017_5yr_B19013_05000US06063.shp", as.sf = TRUE)

str(ca_income)
names(ca_income)[2:4] <- c("County", "Median.Income", "MoE")

options(scipen = 999)
hist(ca_income$Median.Income)

qtm(ca_income, fill = "Median.Income")

#-----------------------------------
#YOUR TURN
#Repeating steps for Arkansas
#-----------------------------------
ar_counties <- counties("AR")

