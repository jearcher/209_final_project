# Use data.table syntax, dplyrs library
library(data.table)
library(dplyr)
library(tidyverse)
# Ensure the directory is "./src/"
data <- fread("../data/raw/noaa_quakes.tsv")

# Remove NA first columnn and row
data = data[-1 ,-1]

# Drop NAs for Total Deaths
data = data[complete.cases(data[ , "Total Deaths"]),]
data = data[complete.cases(data[ , "Mag"]),]


# Create Log Total Deaths variable
data[ ,  "Log Total Deaths" := log(`Total Deaths`)]

#######################
# Bin Magnitudes by 1 #
#######################

# Note: `case_when()` is an alternative to if loops and 
# assigns values _in order_, so we don't have to use "Mag < 1 & Mag != NA" etc
# Go from most specific ----> most general when assigning
data[ , "Magnitude Bin" := case_when(
  Mag == NA ~ "NA",
  Mag < 1 ~ "0-0.9",
  Mag < 2 ~ "1-1.9",
  Mag < 3 ~ "2-2.9",
  Mag < 4 ~ "3-3.9",
  Mag < 5 ~ "4-4.9",
  Mag < 6 ~ "5-5.9",
  Mag < 7~ "6-6.9",
  Mag < 8 ~ "7-7.9",
  Mag < 9 ~ "8-8.9",
  Mag < 10 ~ "9-9.9",
  Mag == 10 ~ "10"
)]

##############
# Bin Deaths #
##############

data[ , "Total Deaths Bin" := case_when(
  `Total Deaths` == NA ~ "NA",
  `Total Deaths` < 100 ~ "0-99",
  `Total Deaths` < 1000 ~ "100-999",
  `Total Deaths` < 10000 ~ "1,000-9,999",
  `Total Deaths` < 100000 ~ "10,000-99,999",
  `Total Deaths` < 500000 ~ "100,000 - 499,999",
  `Total Deaths` >= 500000 ~ "500,000+"
)]
###########
# Bin Era #
###########

data[ , "Decade" := case_when(
  `Year` == NA ~ "NA",
  `Year` < 1900 ~ "1900 and Before",
  `Year` < 1910 ~ "1900-1909",
  `Year` < 1920 ~ "1910-1919",
  `Year` < 1930 ~ "1920-1929",
  `Year` < 1940 ~ "1930-1939",
  `Year` < 1950 ~ "1940-1949",
  `Year` < 1960 ~ "1950-1959",
  `Year` < 1970 ~ "1960-1969",
  `Year` < 1980 ~ "1970-1979",
  `Year` < 1990 ~ "1980-1989",
  `Year` < 2000 ~ "1990-1999",
  `Year` < 2010 ~ "2000-2009",
  `Year` >= 2010 ~ "2010-2021"
)]
########################
# Trim Location names #
########################

# Extract all words before ":"
location <- c()
for (i in data[ , "Location Name"]) {
  location <- c(location, sub(pattern = ":.*", replacement =  "", x = i))
}
data[ , "Location"] <- location
# Extract name before "-"
location <- c()
for (i in data[ , "Location"]) {
  location <- c(location, sub(pattern = "\\-.*", replacement =  "", x = i))
}
data[ , "Location"] <- location
# Finally extract name before ";" and ","
location <- c()
for (i in data[ , "Location"]) {
  location <- c(location, sub(pattern = "\\;.*", replacement =  "", x = i))
}
data[ , "Location"] <- location

location <- c()
for (i in data[ , "Location"]) {
  location <- c(location, sub(pattern = "\\,.*", replacement =  "", x = i))
}
data[ , "Location"] <- location

########################################
# Export to data/clean/noaa_quakes.csv #
########################################

fwrite(data, file = "../data/clean/noaa_quakes.csv")
