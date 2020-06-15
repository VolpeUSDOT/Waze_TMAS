# Volume of Waze alerts by hour vs TMAS counts by hour,
# Statewide for OH, for quick plot to BTS


# setup ----

source('utility/get_packages.R')

library(tidyverse)
library(lubridate)
library(rgdal)
library(rgeos)
library(sp)

input.loc = 'Data'

load(file.path(input.loc, 'Compiled_county_counts_2020-06-11.RData'))

# Filter to OH and 2019, 2020. Use base R code, filter too slow, probably a group issue

# d <- compiled_counts %>%
#   filter(STATEFP == '39' &
#           yearday > '2018-12-31')
d <- compiled_counts[compiled_counts$STATEFP == '39',]
d <- d[d$yearday > '2018-12-31',]

d$fips <- paste0(d$STATEFP, d$COUNTYFP)

d <- d %>% ungroup() %>% dplyr::select(-STATEFP, -COUNTYFP)

# Join to CBSA for more rich filtering by metro areas
msa <- readxl::read_xls(file.path('Data', 'list1_2020.xls'),
                        skip = 2)
names(msa) <- make.names(names(msa))
na_row_cbsa <- which(is.na(msa$CBSA.Code))
msa <- msa[1:(na_row_cbsa-1),]
msa$fips <- paste0(msa$FIPS.State.Code, msa$FIPS.County.Code)
msa_to_join <- msa %>% 
  dplyr::select(-County.County.Equivalent,
                -State.Name,
                -FIPS.State.Code,
                -FIPS.County.Code)

d <- d %>%
  left_join(msa_to_join, by = 'fips')

# Hourly patterns by month of year and year

d <- d %>%
  mutate(year = as.factor(year(ymd(yearday))),
         month = month(ymd(yearday)),
         CBSA = !is.na(CBSA.Code),
         centralCSA = !is.na(CSA.Code) & Central.Outlying.County == 'Central',
         hour = as.numeric(hour)) %>%
  filter(yearday < Sys.Date())


d_a1 <- d %>%
  group_by(year, month, hour,
          centralCSA) %>%
  summarize(sumcount = sum(count, na.rm = F),
            n = n())
  
d_a2 <- d %>%
  group_by(year, month, hour,
           inCBSA) %>%
  summarize(sumcount = sum(count, na.rm = F),
            n = n())



ggplot(d_a2 %>% filter(inCBSA == TRUE), 
       aes(x = hour,
           y = sumcount,
           color = year)) +
  geom_line(size = 2) +
  theme_bw() +
  facet_wrap(~month) +
  ylab('Count of Waze Alerts') +
  xlab('Hour of Day') +
  ggtitle('Count of Waze alerts by hour \n Ohio, in a CBSA, Monthly, 2019-2020') 

ggsave(file = file.path('Example_Hourly_OH_In_CBSA.jpeg'),
       width = 8, height = 6)

ggplot(d_a1 %>% filter(centralCSA == TRUE & month <= 6), 
       aes(x = hour,
           y = sumcount,
           color = year)) +
  geom_line(size = 2) +
  theme_bw() +
  facet_wrap(~month) +
  ylab('Count of Waze Alerts') +
  xlab('Hour of Day') +
  ggtitle('Count of Waze alerts by hour \n Ohio, in a central CSA, Monthly, 2019-2020') 

ggsave(file = file.path('Example_Hourly_OH_In_Central_CSA.jpeg'),
       width = 8, height = 6)


ggplot(d_a2 %>% filter(inCBSA == FALSE), 
       aes(x = hour,
           y = sumcount,
           color = year)) +
  geom_line(size = 2) +
  theme_bw() +
  facet_wrap(~month) +
  ylab('Count of Waze Alerts') +
  xlab('Hour of Day') +
  ggtitle('Count of Waze alerts by hour \n Ohio, Not in a CBSA, Monthly, 2019-2020') 

ggsave(file = file.path('Example_Hourly_OH_Not_CBSA.jpeg'),
       width = 8, height = 6)


# Same for TMAS 

tmas_files <- dir(file.path(input.loc, 'TMAS', 'Volume'))

# Compile all 2019 and 2020 data 
tmas <- vector()

for(i in 1:length(tmas_files)){
  od <- read_csv(file.path(input.loc, 'TMAS', 'Volume',
                           tmas_files[i]))
  
  # Pivot longer by hour
  ox <- od %>%
    pivot_longer(cols = starts_with("Hour_"),
                 names_to = "Hour",
                 values_to = "Count",
    ) %>%
    mutate(Hour = as.numeric(sub("Hour_", "", Hour)))
  
  
  tmas <- rbind(tmas, ox)
}

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

# Make spatial, add county FIPS, then join to CBSA
plot(sta$Longitude,
     sta$Latitude)

sta_s <- SpatialPointsDataFrame(coords = data.frame(sta$Longitude,
                                           sta$Latitude),
                                sta,
                                proj4string = CRS("+proj=longlat +datum=WGS84"))

# County shapefile 
counties <- readOGR(file.path(input.loc, "Census"), layer = "cb_2018_us_county_500k")
counties <- spTransform(counties, CRS("+proj=longlat +datum=WGS84"))

oh_counties <- counties[counties$STATEFP == 39,]

plot(oh_counties)
points(sta_s)

i_pip <- over(sta_s, oh_counties[,c('GEOID', 'NAME')]) 

sta_s$fips <- i_pip$GEOID
sta_s$county <- i_pip$NAME

# Now join CBSA
sta_df <- sta_s@data %>%
  left_join(msa_to_join, by = 'fips')

# Which stations are in a CBSA in Ohio?

cbsa_sta <- sta_df %>%
  filter(!is.na(CBSA.Code))

cbsa_sta_id <- unique(cbsa_sta$Station_Id)

# Which stations are in a central CSA in Ohio?

csa_sta <- sta_df %>%
  filter(!is.na(CSA.Code) & Central.Outlying.County == 'Central')

csa_sta_id <- unique(csa_sta$Station_Id)


# Filter volume data to these stations
tmas_cbsa <- tmas %>%
  filter(Station_Id %in% cbsa_sta_id)

# Plot TMAS ----

# Summarize to year, month, hour of day
t_d <- tmas_cbsa %>%
  ungroup() %>%
  group_by(Year_Record, Month_Record, Hour) %>%
  summarize(Count = sum(Count, na.rm=T))

t_d <- t_d %>%
  ungroup() %>%
  mutate(Year = as.factor(Year_Record)) %>%
  rename(Month = Month_Record)

ggplot(t_d, 
       aes(x = Hour,
           y = Count,
           color = Year)) +
  geom_line(size = 2) +
  theme_bw() +
  facet_wrap(~Month) +
  ylab('Count of Vehicles by TMAS') +
  xlab('Hour of Day') +
  ggtitle('Count of vehicles from TMAS by hour \n Ohio, in a CBSA, Monthly, 2019-2020') 

ggsave(file = file.path('Example_TMAS_Hourly_OH_In_CBSA.jpeg'),
       width = 8, height = 6)


# Filter volume data to these stations
tmas_cbsa <- tmas %>%
  filter(Station_Id %in% cbsa_sta_id)

# Plot TMAS by csa ----

# Filter volume data to CSA stations
tmas_csa <- tmas %>%
  filter(Station_Id %in% csa_sta_id)

# Summarize to year, month, hour of day
t_d <- tmas_csa %>%
  ungroup() %>%
  group_by(Year_Record, Month_Record, Hour) %>%
  summarize(Count = sum(Count, na.rm=T))

t_d <- t_d %>%
  ungroup() %>%
  mutate(Year = as.factor(Year_Record)) %>%
  rename(Month = Month_Record)

ggplot(t_d, 
       aes(x = Hour,
           y = Count,
           color = Year)) +
  geom_line(size = 2) +
  theme_bw() +
  facet_wrap(~Month) +
  ylab('Count of Vehicles by TMAS') +
  xlab('Hour of Day') +
  ggtitle('Count of vehicles from TMAS by hour \n Ohio, in a central CSA, Monthly, 2019-2020') 

ggsave(file = file.path('Example_TMAS_Hourly_OH_In_Central_CSA.jpeg'),
       width = 8, height = 6)

