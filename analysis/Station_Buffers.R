# Volume of Waze alerts by month vs. TMAS counts by month
# Statewide for OH

# Setup ----

source('utility/get_packages.R')

library(tidyverse)
library(lubridate)
library(rgdal)
library(rgeos)
library(sp)
library(raster)
library(zip)

input.loc = 'Data'


# Import and combine traffic count data ----
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
# Sample_Id: Make sure not represented in scientific notation
sta <- read_csv(file.path(input.loc, 'TMAS', 'Station', 'OH 2019 (TMAS - STA).CSV'))

if(any(grepl('E+', sta$Sample_Id))){
  stop('Scientific notation in Sample_Id column, fix before proceeding')
}

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
  dplyr::select(-Travel_Lane) %>% distinct()


# Add leading zeros to Station ID and HPMS Sample ID

lead_zeros <- strrep("0", 6 - nchar(sta$Station_Id))
sta$Station_Id <- paste(lead_zeros, sta$Station_Id, sep = "")

lead_zeros <- strrep("0", 12-nchar(sta$Sample_Id))
sta$Sample_Id <- paste(lead_zeros, sta$Sample_Id, sep = "")

# Join station and monthly count data ----
# TO-DO: Check which of the 260 stations do not have corresponding counts
# (only 217 station Ids in volume data)
sta_month <- sta %>%
  left_join(month_vol, by = c('Station_Id', 'Travel_Dir'))
            
# sta_month %>% head(10) %>% View()

# Make spatial points df from stations with monthly counts
sta_s <- SpatialPointsDataFrame(coords = data.frame(sta$Longitude,
                                                    sta$Latitude),
                                sta_month,
                                proj4string = CRS("+proj=longlat +datum=WGS84"))

sta_s@data %>% head()


# Import HPMS segments
hpms.loc <- paste(input.loc, "/ODOT/HPMS_Segments", sep="")
hpms <- rgdal::readOGR(hpms.loc, layer = "WGIS_ROAD_INVENTORY_HPMSSegments")
summary(hpms)


# Projection to same system as station points
hpms <- spTransform(hpms, CRS("+proj=longlat +datum=WGS84"))
hpms@proj4string


plot(sta_s, axes=TRUE)
plot(hpms, add=TRUE, col='red', lwd=1.5)


# Keep only segments that match with a station

hpms_subset <- hpms[hpms$HPMS_SAMPL %in% sta_s$Sample_Id, ]

# How many station observations are we losing by not matching with an HPMS segment? Most of them...
nrow(sta_s[!sta_s$Sample_Id %in% hpms$HPMS_SAMPL,])

plot(sta_s, axes=TRUE)
plot(hpms_subset, add=TRUE, col='red', lwd = 3)

# Reproject station points and HPMS segments in meters ----

# USGS version of Albers Equal Area projection: 
proj.USGS <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

sta_s <- spTransform(sta_s, CRS(proj.USGS))

hpms_subset <- spTransform(hpms_subset, CRS(proj.USGS))

# Create station buffer, and clip segments to desired length
buffdist <- 1 * 1609.344 # units are meters in shapefiles, here get one mile in meters

circbuff <- gBuffer(sta_s, width=buffdist, byid=TRUE)
circbuff <- SpatialPolygonsDataFrame(circbuff, data=circbuff@data)

plot(sta_s, axes=TRUE)
plot(circbuff, add=TRUE, border='lightblue')

hpms_clipped <- intersect(hpms_subset, circbuff)

plot(hpms_subset)
plot(hpms_clipped)

# Create a buffer rectangle around each HPMS clip
rect_width = 0.5 * 1609.344 # half-mile buffer, in meters

# use byid = TRUE so that the result is not a single polygon (with multiple elements), but rather each individual polygon with the ID variable preserved. The output is now a spatial polygons data frame, with all data values preserved
segments <- gBuffer(hpms_clipped, width = rect_width, byid = TRUE)
plot(segments)

if(!dir.exists(file.path(input.loc, 'Buffers'))){
  dir.create(file.path(input.loc, 'Buffers'))
}

writeOGR(obj = segments, 
         dsn = file.path(input.loc,'Buffers'), 
         layer = 'segments',
         driver = 'ESRI Shapefile',
         overwrite_layer = T)

# And zip the segments file
# Skip any existing zip file
files_to_zip = dir(file.path(input.loc,'Buffers'))
files_to_zip = files_to_zip[!grepl('zip$', files_to_zip)]

zipr(zipfile = file.path(input.loc, 'Buffers', 'segments_1.zip'),
    files = file.path(input.loc,'Buffers', files_to_zip))

# Then manually upload to SDC for Waze query

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