#### Download data from Jackson weather station via Weather Underground

#### 1. Load required libraries #########
# Note: Do not forget to install them first!
library(weatherData)
library(ggplot2)
library(mgcv)
library(scales)
library(plyr)
library(reshape2)
library(gridExtra)
library(lubridate)
#library(weathermetrics) # Does not work

#### 2. Station and date details ############################

# Personal Weather Station (PWS) at Environmental Technology,
# School of Industrial Technology, Universiti Sains Malaysia.
# List of parameters
# * Wind speed (ms-1)
# * Wind direction (deg)
# * Temperature (deg C)
# * Relative humidity (%)

station.id="KJAN"
start_date='2007-08-24' #strftime(as.POSIXct(Sys.Date()),format='%Y-%m-%d')
end_date='2008-02-16'

#### 3. Download detailed weather station data ##############################

# 'wd' stands for 'weather data'
wd<-getWeatherForDate(station.id,start_date,end_date, opt_all_columns=T,
                      opt_detailed = TRUE)
wd$Wind_SpeedKm_h[wd$Wind_SpeedKm_h=="Calm"] <- 0

wd$Wind_SpeedKm_h<-as.numeric(wd$Wind_SpeedKm_h)
