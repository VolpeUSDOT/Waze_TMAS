# Volume of Waze alerts by month vs. TMAS counts by month
# Statewide for OH

# Setup

source('utility/get_packages.R')

library(tidyverse)
library(lubridate)
library(rgdal)
library(rgeos)
library(sp)

input.loc = 'Data'

# Import and combine traffic count data
vol_files <- dir(file.path(input.loc, 'TMAS', 'Volume'))

vol <- vector()

for(i in 1:length(vol_files)){
  od <- read_csv(file.path(input.loc, 'TMAS', 'Volume', vol_files[i]))
  
  # Pivot longer by hour
  ox <- od %>%
    pivot_longer(cols = starts_with("Hour_"),
                 names_to = "Hour",
                 values_to = "Count",
    ) %>%
    mutate(Hour = as.numeric(sub("Hour_", "", Hour)))
  
  vol <- rbind(vol, ox)
}


# Get monthly counts
month_vol <- vol %>%
  group_by(Station_Id, Travel_Dir, Year_Record, Month_Record) %>%
  summarise(Month_Count = sum(Count))

# Pivot wider by month
month_vol <- month_vol %>%
  pivot_wider(names_from = c("Year_Record","Month_Record"),
              values_from = c("Month_Count"),
              names_prefix = "YM_")

# Get station information
sta <- read_csv(file.path(input.loc, 'TMAS', 'Station', 'OH 2019 (TMAS - STA).CSV'))

sta <- sta %>%
  mutate(Latitude = as.numeric(paste(substr(Latitude, 1, 2),
                                     substr(Latitude, 3, nchar(Latitude)), 
                                     sep = ".")),
         Longitude = -1*as.numeric(paste(substr(Longitude, 1, 2),
                                         substr(Longitude, 3, nchar(Longitude)), 
                                         sep = "."))
  )

plot(sta$Longitude, sta$Latitude)

# Collapse over lane data
sta <- sta %>%
  select(-Travel_Lane) %>% distinct()

# Add leading zeros to Station Id
lead_zeros <- strrep("0", 6-nchar(sta$Station_Id))
sta$Station_Id <- paste(lead_zeros, sta$Station_Id, sep = "")


# Join station and monthly count data
# TO-DO: Check which of the 260 stations do not have corresponding counts
# (only 217 station Ids in volume data)
sta_month <- sta %>%
  left_join(month_vol, by = c('Station_Id', 'Travel_Dir'))
            
sta_month %>% head(10) %>% View()

# Make spatial points df from stations
sta_s <- SpatialPointsDataFrame(coords = data.frame(sta$Longitude,
                                                    sta$Latitude),
                                sta,
                                proj4string = CRS("+proj=longlat +datum=WGS84"))


# Create circular buffer around each station
# Note: If station is two-way, each direction has its own entry

# Import HPMS segments

# Use station buffer to clip segments

# Join segments to stations according to HPMS ID

# Create a buffer region (w/ flat cap) around each segment


### Alternative Buffer Approach #1: ####
# Network-Constrained Service Area:
# --> Import HPMS segments
# --> Create an HPMS segment-constrained service area with "buffdist" radius around each station
# --> Use the buffer shapes (whether polygon or line output) to clip the HPMS segments
# --> Join clipped segments to station according to HPMS ID
# --> Create a buffer region (w/ flat cap) around each segment


### Alternative Buffer Approach #2: ####
# Create Polylines Using Traffic Direction:
# --> Calculate linear buffer's endpoints using flow of traffic (approximate) and "buffdist"
# --> Create a buffer region (w/ flat cap) around each line segment
# --> Maybe add some extra width to accommodate imprecision in flow direction





# NEXT STEPS
# ---> [Do SDC Query]
# ---> Import and join queried Waze data
# --> Plot monthly count of Waze alerts versus monthly traffic counts
# --> Note: One plot per month. Points for individual station DIRECTION.