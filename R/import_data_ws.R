##### Import and organize Weather Station data ####

#### 0. Preliminaries ####
# To group the data according to month, hour.
library(dplyr)
library(Hmisc)

#### 1. Import data ####
# Import secondary data from weather station and creating the data frame
# Data must be changed to csv format
data_weat_sta1 <- read.csv('data/2007_8.csv',skip=16,strip.white=TRUE)
data_weat_sta2 <- read.csv('data/2007_09.csv',skip=16,strip.white=TRUE)
data_weat_sta3 <- read.csv('data/2007_10.csv',skip=16,strip.white=TRUE)
data_weat_sta4 <- read.csv('data/2007_11.csv',skip=16,strip.white=TRUE)
data_weat_sta5 <- read.csv('data/2007_12.csv',skip=16,strip.white=TRUE)
data_weat_sta6 <- read.csv('data/2008_01.csv',skip=16,strip.white=TRUE)
data_weat_sta7 <- read.csv('data/2008_02.csv',skip=16,strip.white=TRUE)

# Ignore the warnings
df <- rbind(data_weat_sta1,data_weat_sta2,data_weat_sta3,data_weat_sta4,
            data_weat_sta5,data_weat_sta6,data_weat_sta7)
# Clean up the workspace
rm(data_weat_sta1,data_weat_sta2,data_weat_sta3,data_weat_sta4,
   data_weat_sta5,data_weat_sta6,data_weat_sta7)

date <- as.character(df$Date)
time <- as.character(df$Time)
date <- as.character(date)
# To add zeros in front of time of less than 4 digits
time <- as.character(sprintf("%04s",time))
time_stamp <- paste(date,time)

ws <- as.character(df$WindSpeed)
wd <- as.character(df$WindDirection)

rm(df)

# Creating the dataframe
df_weat <- cbind(time_stamp,ws,wd)
df_weat <- as.data.frame(df_weat)

# Changing to characters to prevent values from changing
df_weat$time_stamp <- as.character(df_weat$time_stamp)
df_weat$ws <- as.character(df_weat$ws)
df_weat$wd <- as.character(df_weat$wd)

# Removing all non-numeric values from wind speed and direction
df_weat$ws[which(df_weat$ws=='')] <- NA
df_weat$ws[which(df_weat$ws=='M')] <- NA
df_weat$wd[which(df_weat$wd=='')] <- NA
df_weat$wd[which(df_weat$wd=='M')] <- NA
df_weat$wd[which(df_weat$wd=='VR')] <- NA
df_weat$time_stamp[which(df_weat$time_stamp=='NA 00NA')] <- NA

# Changing time_stamp in characters to POSIX
df_weat$time_stamp <- strptime(df_weat$time_stamp,"%Y%m%d %H%M")

# Changing to numeric and adding to dataframe
df_weat$ws <- as.numeric(df_weat$ws)
df_weat$wd <- as.numeric(df_weat$wd)

# Clearning up the workspace
rm(date,time,time_stamp,ws,wd)

# Removing non-relevant data from dataframe
x <- seq(1,603)
y <- seq(5700,nrow(df_weat))
df_weat <- df_weat[-x,]
df_weat <- df_weat[-y,]
row.names(df_weat) <- NULL
rm(x,y)
##### 2. Categorization ####
# Grouping the data according to year, month, and hour
df_group <- df_weat %>% 
  mutate(time_stamp=as.POSIXct(time_stamp)) %>%
  group_by(year=format(time_stamp, '%Y'),
           month=format(time_stamp, '%m'),
           hour=format(as.POSIXlt(cut(time_stamp,breaks='hour')),'%H')) %>%
  summarise(ws=mean(ws,na.rm=TRUE), wd = mean(wd,na.rm=TRUE))

##### 3. Plot #####
# Plot
## A. Wind direction
plot(df_group$hour[df_group$month == '08'],
     df_group$wd[df_group$month == '08'],
     ylab='Wind direction',xlab='Local time',type='l',ylim=c(0,360),lwd=2,
     main='08')
plot(df_group$hour[df_group$month == '09'],
     df_group$wd[df_group$month == '09'],
     ylab='Wind direction',xlab='Local time',type='l',ylim=c(0,360),lwd=2,
     main='09')
plot(df_group$hour[df_group$month == '10'],
     df_group$wd[df_group$month == '10'],
     ylab='Wind direction',xlab='Local time',type='l',ylim=c(0,360),lwd=2,
     main='10')
plot(df_group$hour[df_group$month == '11'],
     df_group$wd[df_group$month == '11'],
     ylab='Wind direction',xlab='Local time',type='l',ylim=c(0,360),lwd=2,
     main='11')
plot(df_group$hour[df_group$month == '12'],
     df_group$wd[df_group$month == '12'],
     ylab='Wind direction',xlab='Local time',type='l',ylim=c(0,360),lwd=2,
     main='12')
plot(df_group$hour[df_group$month == '01'],
     df_group$wd[df_group$month == '01'],
     ylab='Wind direction',xlab='Local time',type='l',ylim=c(0,360),lwd=2,
     main='01')
plot(df_group$hour[df_group$month == '02'],
     df_group$wd[df_group$month == '02'],
     ylab='Wind direction',xlab='Local time',type='l',ylim=c(0,360),lwd=2,
     main='02')

# B. Wind speed
plot(df_group$hour[df_group$month == '08'],
     df_group$ws[df_group$month == '08'],
     ylab='Wind speed',xlab='Local time',type='l',ylim=c(0,20),lwd=2,
     main='08')
plot(df_group$hour[df_group$month == '09'],
     df_group$ws[df_group$month == '09'],
     ylab='Wind speed',xlab='Local time',type='l',ylim=c(0,20),lwd=2,
     main='09')
plot(df_group$hour[df_group$month == '10'],
     df_group$ws[df_group$month == '10'],
     ylab='Wind speed',xlab='Local time',type='l',ylim=c(0,20),lwd=2,
     main='10')
plot(df_group$hour[df_group$month == '11'],
     df_group$ws[df_group$month == '11'],
     ylab='Wind speed',xlab='Local time',type='l',ylim=c(0,20),lwd=2,
     main='11')
plot(df_group$hour[df_group$month == '12'],
     df_group$ws[df_group$month == '12'],
     ylab='Wind speed',xlab='Local time',type='l',ylim=c(0,20),lwd=2,
     main='12')
plot(df_group$hour[df_group$month == '01'],
     df_group$ws[df_group$month == '01'],
     ylab='Wind speed',xlab='Local time',type='l',ylim=c(0,20),lwd=2,
     main='01')
plot(df_group$hour[df_group$month == '02'],
     df_group$ws[df_group$month == '02'],
     ylab='Wind speed',xlab='Local time',type='l',ylim=c(0,20),lwd=2,
     main='02')

rm(df_weat,df_group)

