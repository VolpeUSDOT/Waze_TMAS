# Volume of Waze alerts by month vs. TMAS counts by month
# Statewide for OH

# Setup

source('utility/get_packages.R')

library(tidyverse)
library(lubridate)
library(rgdal)
library(rgeos)
library(sp)

# Load station csv
# Create station shapefile with latitude/longitude fields
# Load HMPS segments shapefile
# --> Note: Sample_ID (stations) = HMPS_Sample (segments)
# Create HMPS segment-constrained buffer region around each station
# Load Waze data
# Assign Waze alerts to station buffers by location
# Use Waze direction of flow to further assign to station lane/direction
# --> Note: can use stations' Travel_Dir field (See TMG Chapter 7 for code)
# Aggregate to get monthly Waze counts and traffic counts for each station
# Plot monthly count of Waze alerts versus monthly traffic counts
# --> Note: One plot per month. Points for individual station DIRECTION.