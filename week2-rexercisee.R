library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times

wildschwein_BE <- read_delim("wildschwein_BE_2056.csv",",")
wildschwein_BE <- st_as_sf(wildschwein_BE, coords = c("E", "N"), crs = 2056, remove = FALSE)
#calculating time diff betwen 2 and 1, then 3 and 2 etc...
timediff <- difftime(wildschwein_BE$DatetimeUTC[2:51246],  wildschwein_BE$DatetimeUTC[1:51245],units = "secs")
#transforming time diff into integer
timediff <- as.integer(timediff)
