# Use data.table syntax, dplyrs library
library(data.table)
library(dplyr)

# Ensure the directory is "./src/"
data <- fread("../data/raw/noaa_quakes.tsv")

# Remove NA first columnn and row
data = data[-1 ,-1]

# Drop NAs for Total Deaths
data = data[complete.cases(data[ , "Total Deaths"]),]

# Create Log Total Deaths variable
data[ ,  "Log Total Deaths" := log(`Total Deaths`)]

# Bin Magnitudes by 1
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

# Bin Deaths

data[ , "Total Deaths Bin" := case_when(
  `Total Deaths` == NA ~ "NA",
  `Total Deaths` < 100 ~ "0-99",
  `Total Deaths` < 1000 ~ "100-999",
  `Total Deaths` < 10000 ~ "1,000-9,999",
  `Total Deaths` < 100000 ~ "10,000-99,999",
  `Total Deaths` < 500000 ~ "100,000 - 499,999",
  `Total Deaths` >= 500000 ~ "500,000+"
)]


# Export to data/clean/noaa_quakes.csv

fwrite(data, file = "../data/clean/noaa_quakes.csv")
