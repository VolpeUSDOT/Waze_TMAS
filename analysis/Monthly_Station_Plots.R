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
  group_by(Station_Id, Travel_Dir, Travel_Lane, Year_Record, Month_Record) %>%
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

# Add leading zeros to Station Id
lead_zeros = strrep("0", 6-nchar(sta$Station_Id))
sta$Station_Id = paste(lead_zeros, sta$Station_Id, sep = "")


# Join station and monthly count data
# TO-DO: Check which of the 260 stations do not have corresponding counts
# (only 217 station Ids in volume data)
sta_month <- sta %>%
  left_join(month_vol, by = c('Station_Id', 'Travel_Dir','Travel_Lane'))

            
sta_month %>% head(10) %>% View()

# NEXT STEPS
# --> Aggregate over stations' travel lanes if same direction
# --> Create spatial points df using station coordinates
# --> Create rectangular buffer around stations. Can use HPMS segments to check accuracy?
# ---> [Do SDC Query]
# ---> Import and join queried Waze data
# --> Plot monthly count of Waze alerts versus monthly traffic counts
# --> Note: One plot per month. Points for individual station DIRECTION.