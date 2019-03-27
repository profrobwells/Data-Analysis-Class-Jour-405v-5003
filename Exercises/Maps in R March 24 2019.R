#Machlis - Maps in R Ch 11
#Updated March 27, 2019
#ADDED API SECTION AND CENSUS TRACT MAPS

#-------------------------------------
# Ch 11, MACHLIS, Maps in R
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

install.packages("tmap")
install.packages("tidycensus")
install.packages("ggmap")

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
library(ggplot2)
library(ggmap)
library(tigris)

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


#-----------------------------------
## Downloading Census data into R via API
#-----------------------------------

# First, sign up for a [census key](https://api.census.gov/data/key_signup.html).

# Add key to .Renviron
Sys.setenv(CENSUS_KEY="YOUR CENSUS KEY HERE")
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")


# If you don't have censusapi installed yet, uncomment the line below and run
install.packages("censusapi")

library(censusapi)

apis <- listCensusApis()
View(apis)

#This takes some time to load
acs_vars <- listCensusMetadata(name="acs/acs5", type="variables", vintage=2017)
#Be patient! It needs to load
#
View(acs_vars)
#
#To understand what is here, use the CensusReporter website as a guide
#https://censusreporter.org/
#See Topics section. See income
#
#Look up this table: B19013
#
#Another resource
https://api.census.gov/data/2017/acs/acs5/variables.html
#
#Build an Arkansas unemployment income table
#Find Arkansas state code
https://www.census.gov/geo/reference/ansi_statetables.html
#
#Find unemployment table in https://censusreporter.org/
#
#Find the variables you want to map. Search by table name
https://api.census.gov/data/2017/acs/acs5/variables.html
#
#Build your jobs table
#
ar_jobs <- getCensus(name = "acs/acs5", vintage = 2017, 
                     vars = c("NAME", "B23025_001E", "B23025_005E"), 
                     region = "county:*", regionin = "state:05")

#Calculate unemployment rate: Divide unemployed into population
ar_jobs$unem_rate <- (ar_jobs$B23025_005E/ar_jobs$B23025_001E)

#Alex Nichol decimal rounding:
ar_jobs$unem_rate<-round(ar_jobs$unem_rate, 2)

head(ar_jobs)

#Rename columns
names(ar_jobs)[3:5] <- c("Co_Name", "Population", "Unemployed")

#Load map data in Tigris - SF. set sf option
options(tigris_class = "sf")
#
#Load the Arkansas counties sf files
ar <- counties("AR", cb=T)
#
#Join on where there's already a consistent variable, even though the names don't line up
#
ar4ever <- left_join(ar, ar_jobs, by=c("COUNTYFP"="county"))
#
#Map it
ggplot(ar4ever) + 
  geom_sf(aes(fill=unem_rate), color="white") +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  scale_fill_distiller(palette="Oranges", direction=1, name="Median income") +
  labs(title="2017 Unemployment in Arkansas counties, age 16 and older", caption="Source: : U.S. Census Bureau (2017). Fantastic map by Rob Wells")

#-------------------------------------------------------------#
#             Census Tracts in Washington Co                  #
#-------------------------------------------------------------#
#
#Now get granular data from the census by census tracts
#region = tract:*
#regionin = "state:05+county:143"
#county number? Look at AR df, find Washington and the county number
#Genius.
#

fy_income <- getCensus(name = "acs/acs5", vintage = 2017, 
                       vars = c("NAME", "B19013_001E", "B19013_001M"), 
                       region = "tract:*", regionin = "state:05+county:143")

head(fy_income)


#Load map data - SF
# If you don't have tigris installed yet, uncomment the line below and run
#install.packages("tigris")
#library(tigris)
# set sf option
#options(tigris_class = "sf")
#ar <- counties("AR", cb=T)
tracts <- tracts(state = 'AR', county = 143, cb=TRUE)

# We could gsub out the string but we'll join on where there's already a consistent variable, even though the names don't line up
library(dplyr)
fy4ever <- left_join(tracts, fy_income, by=c("COUNTYFP"="county"))

library(ggplot2)

ggplot(fy4ever) + 
  geom_sf(aes(fill=B19013_001E), color="blue") +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  scale_fill_distiller(palette="Oranges", direction=1, name="Median income") +
  labs(title="2017 Median income in Washington County", caption="Source: US Census/ACS5 2017. Not so Fantastic map by Rob Wells")

#What is wrong with this picture?
#Look at what it is mapping
View(fy_income)
View(fy4ever)

#Duplicate tracts.
#Help ggplot do its job

#Try with a different join
#What kind of a join will give us just the 32 census tracts of Washington County?

colnames(tracts)
#[1] "STATEFP"  "COUNTYFP" "TRACTCE"  "AFFGEOID" "GEOID"    "NAME"     "LSAD"    
#[8] "ALAND"    "AWATER"   "geometry"

> colnames(fy_income)
#[1] "state"       "county"      "tract"       "NAME"        "B19013_001E"
#[6] "B19013_001M"


#Try this
fy4tract <- inner_join(tracts, fy_income, by=c("TRACTCE"="tract"))

ggplot(fy4tract) + 
  geom_sf(aes(fill=B19013_001E), color="blue") +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  scale_fill_distiller(palette="Oranges", direction=1, name="Median Income") +
  labs(title="YES!!! 2017 Median income in Washington County", 
       caption="Source: US Census/ACS5 2017. Fantastic map by Rob Wells")

#Victory!

#Now do this with Benton County