library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times

wildschwein_BE <- read_delim("wildschwein_BE_2056.csv",",")
wildschwein_BE <- st_as_sf(wildschwein_BE, coords = c("E", "N"), 
                           crs = 2056, remove = FALSE)
#calculating time diff betwen 2 and 1, then 3 and 2 etc...
timelag <- as.integer(difftime(wildschwein_BE$DatetimeUTC, 
                               lag(wildschwein_BE$DatetimeUTC),
                               units = "secs"))

#How many individuals were tracked?
wildschwein_BE$TierName %>% as.factor %>% summary (maxsum=60000) %>% 
  length