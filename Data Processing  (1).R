##The following code processes csv files of depth and activity of yelloweye rockfish monitored in Hood Canal##

##############################
#### COMBINING DATAFRAMES ####
##############################


###create directories ###
dir.create("output")       
results.dir <- paste0("output/")



# Get the files names
files = list.files("Data", pattern="*.csv")

#  for loop, create dataframe, 
alldata <- data.frame()
for (i in 1:length(files)) {
  dat <- read.csv(paste0("Data/", files[i]))
  alldata <- rbind(alldata,dat)}


#filter for the depth transmitters

library(dplyr)
depth2 <- alldata %>%
  filter(Transmitter %in% c("A69-9004-3269", "A69-9004-3271", "A69-9004-3273", "A69-9004-3275", "A69-9004-3277", "A69-9004-3279", "A69-9004-3281", "A69-9004-3283", "A69-9004-3285", "A69-9004-3287", "A69-9004-3289", "A69-9004-3291", "A69-9004-3293", "A69-9004-3295", "A69-9004-3297" )) %>%
  mutate(Sensor.Value = Sensor.Value * 0.9097 - 3.6388,
         Sensor.Unit = "m") %>% 
  droplevels() %>%
  filter(Sensor.Value !=  228.3347) # look at specific max value (battery)
depth2



# filter for the acceleration transmitters 

# "bring in all the 2nd deployment data files"

accel2 <- alldata %>%
  filter(Transmitter %in% c("A69-9004-3270", "A69-9004-3272", "A69-9004-3274", "A69-9004-3276", "A69-9004-3278", "A69-9004-3280", "A69-9004-3282", "A69-9004-3284", "A69-9004-3286", "A69-9004-3288", "A69-9004-3290", "A69-9004-3292", "A69-9004-3294", "A69-9004-3296")) %>%
  mutate(Sensor.Value = Sensor.Value * 0.013588 + 0,
         Sensor.Unit = "m/s²") %>%
  droplevels()
accel2
library(tidyverse)
# change units from m/s2 to m/s^2
accel2$Sensor.Unit <- str_replace_all(accel2$Sensor.Unit, "m/s2", "m/s²")

# combine depth and acceleration into one table
combined.dat.2 <- rbind(depth2, accel2) #This should bring all three sets of files together


r <- as.POSIXct(YE.combined.dat$Date.and.Time..UTC., tz = "UTC")
r
library(tidyverse)
YE.combined.dat$Date.and.Time..UTC. <- r

# plotting depth for an individual 
ggplot(YE.combined.dat %>%
         dplyr::filter(Sensor.Unit == "m/s²"), aes(x = Date.and.Time..UTC., y = Sensor.Value)) +
  geom_point() +
  theme_bw() +
  facet_grid( ~ Transmitter) +
  # scale_x_datetime(labels = "%S:00") +
  labs(x = "Time", y = "Accel (m/s²)") +
  # scale_y_reverse() +
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 10),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 7)) +
  ggsave(device = "png", filename = paste0(results.dir, 'All accel pings.png'), width = 6.5, height = 8, units = "in") # save in output


library(lubridate)
###Calculate depth means and SD for each fish for each hour
sum.depths <- combined.dat.2 %>%
  filter(Sensor.Unit == "m") %>%
  mutate(hour.of.day = hour(Date.and.Time..UTC.)) %>%
  group_by(Transmitter, hour.of.day) %>%
  dplyr::summarise(mean = mean(Sensor.Value),
                   SD = sd(Sensor.Value))





# plotting depth/fish/hour (with error bars)
ggplot(sum.depths, aes(x = hour.of.day, y = mean)) +
  geom_point() +
  # geom_linerange(aes(ymin = mean-SD, ymax = mean+SD), width = .2) +
  theme_bw() +
  #  facet_wrap(~ Transmitter, scales = "free") +
  scale_y_reverse() +
  # scale_x_datetime(labels = "%S:00") +
  labs(x = "Time", y = "Depth (m)") +
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 10),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 7)) +
  ggsave(device = "png", filename = paste0(results.dir, 'Yelloweye depth per hour.png'), width = 6.5, height = 8, units = "in") # save in output


library(lubridate)
###Calculate accel means and SD for each fish for each hour
sum.accel <- combined.dat.2 %>%
  filter(Sensor.Unit == "m/s²") %>%
  mutate(hour.of.day = hour(Date.and.Time..UTC.)) %>%
  group_by(Transmitter, hour.of.day) %>%
  dplyr::summarise(mean = mean(Sensor.Value),
                   SD = sd(Sensor.Value))



# plotting accel/fish/hour (with error bars)
ggplot(sum.accel, aes(x = hour.of.day, y = mean)) +
  geom_point() +
  geom_linerange(aes(ymin = mean-SD, ymax = mean+SD), width = .2) +
  theme_bw() +
  facet_wrap(~ Transmitter, scales = "free") +
  # scale_x_datetime(labels = "%S:00") +
  labs(x = "Time", y = "Acceleration (m/s²)") +
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 10),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 7)) +
  ggsave(device = "png", filename = paste0(results.dir, 'Yelloweye accel per hour.png'), width = 6.5, height = 8, units = "in") # save in output



# putting first deployment data into r 
combined.dat.1 <- read.csv("alldata.1.csv")


class(combined.dat.1$Date.and.Time..UTC.)
x <- as.POSIXct(combined.dat.1$Date.and.Time..UTC., tz = "UTC")
x
library(tidyverse)
combined.dat.1$Date.and.Time..UTC. <- x
class(x)

#filter for the depth transmitters

library(dplyr)
depth1 <- combined.dat.1 %>%
  filter(Transmitter %in% c("A69-9004-3268", "A69-9004-3269", "A69-9004-3271", "A69-9004-3273", "A69-9004-3275", "A69-9004-3277", "A69-9004-3279", "A69-9004-3281", "A69-9004-3283", "A69-9004-3285", "A69-9004-3287", "A69-9004-3289", "A69-9004-3291", "A69-9004-3293", "A69-9004-3295", "A69-9004-3297" )) %>%
  filter(Sensor.Value !=  228.3347) # look at specific max value (battery)
depth1
class(depth1$Date.and.Time..UTC.)


# filter for the acceleration transmitters 

# "bring in all the 2nd deployment data files"

accel1 <- combined.dat.1 %>%
  filter(Transmitter %in% c("A69-9004-3268", "A69-9004-3270", "A69-9004-3272", "A69-9004-3274", "A69-9004-3276", "A69-9004-3278", "A69-9004-3280", "A69-9004-3282", "A69-9004-3284", "A69-9004-3286", "A69-9004-3288", "A69-9004-3290", "A69-9004-3292", "A69-9004-3294", "A69-9004-3296")) %>%
  droplevels()


# removing X column in depth1 and accel1 before rbind(
combined.dat.1 = dplyr::select(combined.dat.1, -X)
accel1 = dplyr::select(accel1, -X)
depth1 = dplyr::select(depth1, -X)


# combining depth and accel data
combined.depth<- rbind(depth1, depth2)

combined.accel <- rbind(accel1, accel2)

YE.combined.dat <- rbind(combined.depth, combined.accel)
class(YE.combined.dat$Date.and.Time..UTC.)


##############################################
### CHANGING TIME ZONE TO PDT ################
##############################################

# changing UTC time from factor to POSIX
r <- as.POSIXct(YE.combined.dat$Date.and.Time..UTC., tz = "UTC")
r
library(tidyverse)
YE.combined.dat$Date.and.Time..UTC. <- r

library(lubridate)
# convert time 
class(YE.combined.dat$Date.and.Time..UTC.)

YE.combined.dat$Date.and.Time..PST.PDT. <- as.POSIXct(YE.combined.dat$Date.and.Time..UTC.,
                                                      format = "%Y-%m-%dT%H:%M")
combined.depth$Date.and.Time..PST.PDT. <- as.POSIXct(combined.depth$Date.and.Time..UTC.,
                                                     format = "%Y-%m-%dT%H:%M")

combined.accel$Date.and.Time..PST.PDT. <- as.POSIXct(combined.accel$Date.and.Time..UTC.,
                                                     format = "%Y-%m-%dT%H:%M")

class(YE.combined.dat$Date.and.Time..PST.PDT.)

# change time zone 
YE.combined.dat$Date.and.Time..PST.PDT. <- with_tz(YE.combined.dat$Date.and.Time..PST.PDT.,
                                                   tz = "PST8PDT")

Depth$Date.and.Time..PST.PDT. <- with_tz(Depth$Date.and.Time..PST.PDT.,
                                         tz = "PST8PDT")

Accel$Date.and.Time..PST.PDT. <- with_tz(Accel$Date.and.Time..PST.PDT.,
                                         tz = "PST8PDT")

## the following code adds lat/long, dawn/dusk to combined depth/accel data file (YE.combined.dat) ##
# average depth per month 

YE.combined.dat$month_year <-format(YE.combined.dat$Date.and.Time..PST.PDT.,'%m-%Y')

#  Get months
YE.combined.dat$Month <- months(YE.combined.dat$Date.and.Time..PST.PDT.)

#  Get years
YE.combined.dat$Year <- format(YE.combined.dat$Date.and.Time..PST.PDT.,format="%y") # change to 2016
# add column that is just day or night 


#  Aggregate 'depth' on months and year and get mean %>%


depth.month <- YE.combined.dat %>%
  aggregate(Sensor.Value ~ Month + Year , YE.combined.dat , mean ) %>%
  filter(Sensor.Unit == "m")


# average depth per month plot

ggplot(depth.month, aes(x = hour.of.day, y = Sensor.Value)) +
  geom_point() +
  geom_linerange(aes(ymin = mean-SD, ymax = mean+SD), width = .2) +
  theme_bw() +
  facet_wrap(~ month, scales = "free") +
  scale_y_reverse() +
  # scale_x_datetime(labels = date_format_tz()) +
  labs(x = "Time", y = "Depth (m)") +
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 10),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 7)) +
  ggsave(device = "png", filename = paste0(results.dir, 'Total Yelloweye depth per month.png'), width = 6.5, height = 8, units = "in") # save in output






# fill lat column
YE.combined.dat$Latitude <- 47.41625 # Red Bluffs coordinates 
# fill long column
YE.combined.dat$Longitude <- -123.10438


# day/night, dawn/dusk
library(maptools)
library(suncalc)


#Create vector of locations in Spatial Points format: 
stcalc<-subset(YE.combined.dat, select=c("Longitude","Latitude"))
stcalc <- SpatialPoints(stcalc, proj4string=CRS("+proj=longlat +datum=WGS84"))


dt_local <- as.POSIXct(YE.combined.dat$Date.and.Time..PST.PDT., tz="PST8PDT",format="%Y-%m-%d %H:%M:%S")

#Calculate sunrise and sunset using local time
sr<-sunriset(stcalc, dt_local, direction = "sunrise", POSIXct.out = TRUE)
st<-sunriset(stcalc, dt_local, direction="sunset", POSIXct.out=TRUE)

# calculate dawn & dusk times with local date/time
dawn<-crepuscule(stcalc@coords, dt_local, solarDep=12, direction="dawn", POSIXct.out=TRUE) #nautical dawn = 12 degrees for angle of sun below horizon
dusk<-crepuscule(stcalc, dt_local, solarDep=12, direction="dusk", POSIXct.out=TRUE)

# Adding data to YE.combined.dat

YE.combined.dat$Sunrise<-sr$time
YE.combined.dat$Sunset<-st$time
YE.combined.dat$Dawn<-dawn$time 
YE.combined.dat$Dusk<-dusk$time

# sorting day/night dusk/dawn into intervals
# use sunrise/sunset/dawn/dusk to determine light levels
YE.combined.dat$Light = ifelse(YE.combined.dat$Date.and.Time..PST.PDT. < YE.combined.dat$Dawn, "Night", ifelse(YE.combined.dat$Date.and.Time..PST.PDT. < YE.combined.dat$Sunrise, "Dawn", ifelse(YE.combined.dat$Date.and.Time..PST.PDT. < YE.combined.dat$Sunset, "Day", ifelse(YE.combined.dat$Date.and.Time..PST.PDT. < YE.combined.dat$Dusk, "Dusk", "Night"))))

# day/night addition to dataframe
YE.combined.dat$dayNight <- ifelse(YE.combined.dat$Date.and.Time..PST.PDT. > YE.combined.dat$Sunrise & YE.combined.dat$Date.and.Time..PST.PDT. < YE.combined.dat$Sunset, 'day', 'night')

# change dataframe column to factor
YE.combined.dat$Light = factor(YE.combined.dat$Light)
YE.combined.dat$dayNight = factor(YE.combined.dat$dayNight)




############################
##### Physical covariates ##
############################


# reading data
phys.dat <- read.csv("MarineAmbientSummarizedProfileResults_2021Jul01_206.csv")
tz(phys.dat$Field_Collection_Start_Date_Time)

# filtering for DO data
DO.dat <- phys.dat %>%
  filter(Reslt_Parameter_Name == "Dissolved Oxygen (profile median)", "Dissolved Oxygen (profile minimum)","Dissolved Oxygen (profile maximum)" ) %>%
  filter(Location_ID == "HCB003") %>%
  filter(Result_Unit == "mg/L") 

# filtering for temp data
temp.dat <- phys.dat %>%
  filter(Result_Unit == "deg C")

# filtering for fluorescence data
flu.dat <- phys.dat %>%
  filter(Result_Unit == "mg/m3")


library(lubridate)
# changing to as.POSIXct
DO.dat$Field_Collection_Start_Date_Time <- as.POSIXct(x = DO.dat$Field_Collection_Start_Date_Time,
                                                      format = "%d/%m/%Y %H:%M", tz = "PST8PDT")

DO.dat$Field_Collection_End_Date_Time <- as.POSIXct(x = DO.dat$Field_Collection_End_Date_Time,
                                                    format = "%d/%m/%Y %H:%M", tz = "PST8PDT")


tz(DO.dat$Field_Collection_Start_Date_Time)

#############################################
### PAIR DETECTIONS WITH DO LEVELS ##########
#############################################

# equivalent of v lookup in R 
# Pair detections with DO levels 
phys.dat.profiles$Date.and.Time..PST.PDT. <- phys.dat.profiles$Field_Collection_Date_Time

# Year/month/day 
YE.combined.dat$YMD  <- format(YE.combined.dat$Date.and.Time..PST.PDT.,'%m-%d-%Y')

DO.profiles$YMD  <- format(DO.profiles$Field_Collection_Date_Time,'%m-%d-%Y')

# Filter DO
library(tidyverse)
library(dplyr)
DO.profiles <- phys.dat.profiles %>%
  filter(Result_Parameter_Name == "Dissolved Oxygen")
# select(Field_Collection_Date_Time, Location_ID)

DO.profiles <- DO.profiles %>%
  dplyr::select(Location_ID, Field_Collection_Date, Result_Value, Depth_Value) %>%
  group_by(Location_ID) %>%
  filter(Depth_Value > 50) %>% # think about this more before final run 
  group_by(Location_ID, Field_Collection_Date) %>%
  summarise(mean.DO = mean(Result_Value))

# joining DO profiles with combined data                                     
q <- inner_join(YE.combined.dat, DO.profiles, by = c("Location_ID", "Field_Collection_Date"))

YE.combined.dat$Field_Collection_Date <- format(YE.combined.dat$Date.and.Time..PST.PDT.,'%m/%d/%Y')

# Full join combines dataframes and puts NA all dataframes without DO data
q <- full_join(YE.combined.dat, DO.profiles, by = c("Location_ID", "Field_Collection_Date"))

# Designating sites
YE.combined.dat$Site <- ifelse(YE.combined.dat$Station.Name %in% c("Red Bluff 1", "Red Bluff 2", "Red Bluff 3", "Red Bluff 4", "Red Bluff 5"), "Red Bluff", ifelse(YE.combined.dat$Station.Name %in%  c("Dewatto 1", "Dewatto 2", "Dewatto 3", "Dewatto 4", "Dewatto 5"), "Dewatto", "Seal Rock"))


# matching profile results to site ID 
YE.combined.dat$Location_ID <- ifelse(YE.combined.dat$Station.Name %in% c("Red Bluff 1", "Red Bluff 2", "Red Bluff 3", "Red Bluff 4", "Red Bluff 5", "Dewatto 1", "Dewatto 2", "Dewatto 3", "Dewatto 4", "Dewatto 5"), 'HCB003', 'HCB010')



# "hypoxic" or "normoxic
q$O2 <- ifelse(q$site %in% c(q$mean.DO %in% c("01/12/2017","02/03/2017", "03/09/2017"),  "Hypoxic","Normoxic"))

DO.profiles$O2 <- ifelse(q$site %in% c(q$mean.DO %in% c("01/12/2017","02/03/2017", "03/09/2017"),  "Hypoxic","Normoxic"))


# Jan feb march - change to hypoxic

