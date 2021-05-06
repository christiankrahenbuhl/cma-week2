library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times

wildschwein_BE <- read_delim("wildschwein_BE_2056.csv",",")
wildschwein_BE <- st_as_sf(wildschwein_BE, coords = c("E", "N"), 
                           crs = 2056, remove = FALSE)
#timelag
wildschwein <- mutate(
  wildschwein_BE,timelag = as.integer(
    difftime(lead(DatetimeUTC),DatetimeUTC)))
timelag <- select(wildschwein, timelag)



#How many individuals were tracked? 3
wildschwein_BE$TierName %>% as.factor %>% summary (maxsum=60000) %>% 
  length

#For how long were the individuals tracked? Are there gaps?
wildschwein_BE %>%
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

#calculating speed, timelag and steplength for caro, 3, 6 and 9
#caro
timelag_1 <- mean(
  na.omit(
    as.integer(difftime(caro$DatetimeUTC, 
                        lag(caro$DatetimeUTC),
                        units = "secs"))
  ))

Ediff_1 <- mean(na.omit(lead(caro$E) - caro$E))
Ndiff_1 <- mean(na.omit(lead(caro$N) - caro$N))
steplength_1 <- sqrt(Ediff_1^2+Ndiff_1^2)

speed_1 <- steplength_1 / timelag_1
#caro_3
timelag_3 <- mean(
  na.omit(
    as.integer(difftime(caro_3$DatetimeUTC, 
                        lag(caro_3$DatetimeUTC),
                        units = "secs"))
  ))

Ediff_3 <- mean(na.omit(lead(caro_3$E) - caro_3$E))
Ndiff_3 <- mean(na.omit(lead(caro_3$N) - caro_3$N))
steplength_3 <- sqrt(Ediff_3^2+Ndiff_3^2)

speed_3 <- steplength_3 / timelag_3
#caro_6
timelag_6 <- mean(
  na.omit(
    as.integer(difftime(caro_6$DatetimeUTC, 
                        lag(caro_6$DatetimeUTC),
                        units = "secs"))
  ))

Ediff_6 <- mean(na.omit(lead(caro_6$E) - caro_6$E))
Ndiff_6 <- mean(na.omit(lead(caro_6$N) - caro_6$N))
steplength_6 <- sqrt(Ediff_6^2+Ndiff_6^2)

speed_6 <- steplength_6 / timelag_6

#caro_9
timelag_9 <- mean(
  na.omit(
    as.integer(difftime(caro_9$DatetimeUTC, 
                        lag(caro_9$DatetimeUTC),
                        units = "secs"))
  ))

Ediff_9 <- mean(na.omit(lead(caro_9$E) - caro_9$E))
Ndiff_9 <- mean(na.omit(lead(caro_9$N) - caro_9$N))
steplength_9 <- sqrt(Ediff_9^2+Ndiff_9^2)

speed_9 <- steplength_9 / timelag_9 

#Task4
library(zoo)
rollmean(speed_1, k = 3,fill = NA,align = "left")
