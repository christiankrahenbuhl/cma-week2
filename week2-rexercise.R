library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times

wildschwein_BE <- read_delim("wildschwein_BE_2056.csv",",")
wildschwein_BE <- st_as_sf(wildschwein_BE, coords = c("E", "N"), 
                           crs = 2056, remove = FALSE)
#mean of time differences
timelag <- mean(
  na.omit(
  as.integer(difftime(wildschwein_BE$DatetimeUTC, 
                               lag(wildschwein_BE$DatetimeUTC),
                               units = "secs"))
                                              ))

#How many individuals were tracked?
wildschwein_BE$TierName %>% as.factor %>% summary (maxsum=60000) %>% 
  length

#For how long were the individual tracked? Are there gaps?
wildschwein_BE <- as.in
wildschwein %>%
  group_by(TierID)
  summarise(
    mean_timelag = mean(timelag,na.rm = T)
  )
  
#Task 2
Ediff <- mean(na.omit(lead(wildschwein_BE$E) - wildschwein_BE$E))
Ndiff <- mean(na.omit(lead(wildschwein_BE$N) - wildschwein_BE$N))
steplength <- sqrt(Ediff^2+Ndiff^2)

speed <- steplength / timelag

#Task 3
caro <- read_delim("caro60.csv",",")
caro <- st_as_sf(caro, coords = c("E", "N"), crs = 2056, 
                 remove = FALSE)
#manually reducing the granularity
caro_3 <- slice(caro, seq(1, 200, 3))
caro_6 <- slice(caro, seq(1, 200, 6))
caro_9 <- slice(caro, seq(1, 200, 9))
