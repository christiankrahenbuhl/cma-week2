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
wildschwein_BE <- mutate(
  wildschwein_BE,timelag = as.integer(
    difftime(lead(DatetimeUTC),DatetimeUTC, units = "secs")))
timelag <- select(wildschwein_BE, timelag)
#getting rid of unexpected "geometry" column
timelag<-st_drop_geometry(timelag)


#timelag class is not integer???

#How many individuals were tracked? 3
unique(wildschwein_BE$TierName)  

#For how long were the individuals tracked? Are there gaps?
wildschwein_BE %>%st_drop_geometry()%>%
  group_by(TierID)%>%
  summarise(
    mean_timelag = mean(timelag,na.rm = T)
  )
  
#Were all individuals tracked concurrently or sequentially?
wildschwein_BE%>%st_drop_geometry()%>%
  group_by(TierID)%>%summarise(min(DatetimeUTC),max(DatetimeUTC))
#individuals 016A and 018A started at the same day and 002A and 018A ended at the same day
#What is the temporal sampling interval between the locations?
timelag

#Task 2
wildschwein_BE  <- wildschwein_BE %>%
  group_by(TierID) %>%
  mutate(
    Ediff =  lead(E) - E,
    Ndiff =  lead(N) - N,
    steplength =  sqrt(Ediff^2 + Ndiff^2),
    speed = steplength /timelag
  )

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
caro  <- caro %>%
  mutate(
    Ediff =  lead(E) - E,
    Ndiff =  lead(N) - N,
    steplength =  sqrt(Ediff^2 + Ndiff^2),
    timelag = as.integer(
      difftime(lead(DatetimeUTC),DatetimeUTC, units = "secs")),
    speed = steplength /timelag
  )

#caro_3
caro_3  <- caro_3 %>%
  mutate(
    Ediff =  lead(E) - E,
    Ndiff =  lead(N) - N,
    steplength =  sqrt(Ediff^2 + Ndiff^2),
    timelag = as.integer(
      difftime(lead(DatetimeUTC),DatetimeUTC, units = "secs")),
    speed = steplength /timelag
  )

#caro_6
caro_6  <- caro_6 %>%
  mutate(
    Ediff =  lead(E) - E,
    Ndiff =  lead(N) - N,
    steplength =  sqrt(Ediff^2 + Ndiff^2),
    timelag = as.integer(
      difftime(lead(DatetimeUTC),DatetimeUTC, units = "secs")),
    speed = steplength /timelag
  )

#caro_9
caro_9  <- caro_9 %>%
  mutate(
    Ediff =  lead(E) - E,
    Ndiff =  lead(N) - N,
    steplength =  sqrt(Ediff^2 + Ndiff^2),
    timelag = as.integer(
      difftime(lead(DatetimeUTC),DatetimeUTC, units = "secs")),
    speed = steplength /timelag
  )

#Plotting derived speed at different time intervals
ggplot(caro, aes(DatetimeUTC,speed_1))+
  geom_line(col='red') 
  geom_line(caro_3,col='blue')
#Task4
library(zoo)
#k=10
caro %>%
  mutate(rspeed= rollmean(speed_1, 10, align="left",fill=0)) %>%
  ggplot(aes(x=DatetimeUTC,y=speed_1)) +
  geom_col(fill="pink")+
  geom_line(aes(x=DatetimeUTC,y = rspeed), color = "red")
#k=5
caro %>%
  mutate(rspeed= rollmean(speed_1, 5, align="left",fill=0)) %>%
  ggplot(aes(x=DatetimeUTC,y=speed_1)) +
  geom_col(fill="pink")+
  geom_line(aes(x=DatetimeUTC,y = rspeed), color = "red")

#k=25
caro %>%
  mutate(rspeed= rollmean(speed_1, 25, align="left",fill=0)) %>%
  ggplot(aes(x=DatetimeUTC,y=speed_1)) +
  geom_col(fill="pink")+
  geom_line(aes(x=DatetimeUTC,y = rspeed), color = "red")
