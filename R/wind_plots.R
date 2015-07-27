##### ABOUT: FLUX AND WIND #################################################
# Title: Surface energy exchange and evaporation under different wind speeds
#
# Note: The script: 'lake_analysis.R' needs to be sourced first.
#
# Author: Yusri Yusup, PhD
# Affiliation:  Environmental Technology, School of Industrial Technology, 
#               Universiti Sains Malaysia (USM)
# Date created: 2014-10-01
# Date modified: 2014-10-02
# Version: 1.1

##### 0. Preliminaries ############################################################

# Installing packages or sourcing custom functions
#install.packages('Hmisc')
#install.packages('ggplot2')
# To data convert factor to numeric

library(Hmisc) # To use cut2 and others
library(ggplot2) # For plots 

source('R/convert_magic.R')
source('R/convert_magic_num.R')
source('R/vap_pres_Buck.R') # To calculate vapor pressure using Buck's (1981) equation
source('R/water_vap_mix_ratio.R') # To calculate water vapor mixing ratio
source('R/bulk_moist_coef.R') # To calculate C_E.L, bulk transfer coeffficient of moisture
source('R/ideal_gas_eq_rho.R') # To calculate air density
source('R/unst_stab_category.R') # To categorize atmospheric stability
source('R/unst_stab_category2.R') # To categorize atmospheric stability to either unstable or stable
source('R/multiplot.R')

##### 1. Data classification ####################

# To obtain the 25th and 75th percentile values from summary.
quartile1 <- as.numeric(summary(data$WS_Spd_WVT)[2])
quartile2 <- as.numeric(summary(data$WS_Spd_WVT)[5])


##### 2. Plots ##################################

old.par <- par()
par(mar=(c(4.1,4.5,2.1,2.1)))
# Plot general LE versus U
plot(data$WS_Spd_WVT,
     data$LE,pch=19,col='blue', cex=0.5,
     xlab='U at 4 m (m/s)',ylab=expression(paste('LE (W/m'^2,')')),main='All winds',ylim=c(-300,500))
minor.tick(nx=10,ny=10)

# LE versus U category 1: lower than 25th percentile
plot(data$WS_Spd_WVT[which(data$WS_Spd_WVT < quartile1)],
     data$LE[which(data$WS_Spd_WVT < quartile1)],pch=19,col='blue', cex=0.5,
     xlab='U at 4 m (m/s)',ylab=expression(paste('LE (W/m'^2,')')),main='U < 25th',ylim=c(-300,500))
minor.tick(nx=10,ny=10)

# LE versus U category 2: between 25th and 75th percentile
plot(data$WS_Spd_WVT[which(data$WS_Spd_WVT > quartile1 & data$WS_Spd_WVT < quartile2)],
     data$LE[which(data$WS_Spd_WVT > quartile1 & data$WS_Spd_WVT < quartile2)],pch=19,col='blue', cex=0.5,
     xlab='U at 4 m (m/s)',ylab=expression(paste('LE (W/m'^2,')')),main='25th < U < 75th',ylim=c(-300,500))
minor.tick(nx=10,ny=10)

# LE versus U category 3: more than 75th percentile
plot(data$WS_Spd_WVT[which(data$WS_Spd_WVT > quartile2)],
     data$LE[which(data$WS_Spd_WVT > quartile2)],pch=19,col='blue', cex=0.5,
     xlab='U at 4 m (m/s)',ylab=expression(paste('LE (W/m'^2,')')),main='U > 75th',ylim=c(-300,500))
minor.tick(nx=10,ny=10)

# Plot general H versus U
plot(data$WS_Spd_WVT,
     data$H,pch=19,col='blue', cex=0.5,
     xlab='U at 4 m (m/s)',ylab=expression(paste('H (W/m'^2,')')),main='All winds',ylim=c(-300,500))
minor.tick(nx=10,ny=10)

# H versus U category 1: lower than 25th percentile
plot(data$WS_Spd_WVT[which(data$WS_Spd_WVT < quartile1)],
     data$H[which(data$WS_Spd_WVT < quartile1)],pch=19,col='blue', cex=0.5,
     xlab='U at 4 m (m/s)',ylab=expression(paste('H (W/m'^2,')')),main='U < 25th',ylim=c(-300,500))
minor.tick(nx=10,ny=10)

# H versus U category 2: between 25th and 75th percentile
plot(data$WS_Spd_WVT[which(data$WS_Spd_WVT > quartile1 & data$WS_Spd_WVT < quartile2)],
     data$H[which(data$WS_Spd_WVT > quartile1 & data$WS_Spd_WVT < quartile2)],pch=19,col='blue', cex=0.5,
     xlab='U at 4 m (m/s)',ylab=expression(paste('H (W/m'^2,')')),main='25th < U < 75th',ylim=c(-300,500))
minor.tick(nx=10,ny=10)

# H versus U category 3: more than 75th percentile
plot(data$WS_Spd_WVT[which(data$WS_Spd_WVT > quartile2)],
     data$H[which(data$WS_Spd_WVT > quartile2)],pch=19,col='blue', cex=0.5,
     xlab='U at 4 m (m/s)',ylab=expression(paste('H (W/m'^2,')')),main='U > 75th',ylim=c(-300,500))
minor.tick(nx=10,ny=10)

# Times series plot of z/L
plot(data$time_stamp,data$Z.L,type='l',xlab='Date',ylab='z/L')
minor.tick(nx=10,ny=10)

# Time series plot of wind speed at 4.0 m
plot(data$time_stamp,data$WS_Spd_WVT,type='l',xlab='Date',ylab='U at 4 m (m/s)')
minor.tick(nx=10,ny=10)

# Identifying high and low wind speed days
high_U_dates_index <- which(data$WS_Spd_WVT >= quartile2)
low_U_dates_index <- which(data$WS_Spd_WVT <= quartile1)
temp_wind <- data$WS_Spd_WVT
temp_wind[high_U_dates_index] <- 20
temp_wind[low_U_dates_index] <- 40

# Each month time stamps
# Identification of high wind days (> 3rd quartile winds = 5.125 m/s)
# August
i_aug <- which(data$time_stamp > as.POSIXct("2007-08-01 00:00:00") & data$time_stamp < as.POSIXct("2007-09-01 00:00:00"))
plot(data$time_stamp[min(i_aug):max(i_aug)],temp_wind[min(i_aug):max(i_aug)],ylim=c(15,60))
# September
i_sept <- which(data$time_stamp > as.POSIXct("2007-09-01 00:00:00") & data$time_stamp < as.POSIXct("2007-10-01 00:00:00"))
plot(data$time_stamp[min(i_sept):max(i_sept)],temp_wind[min(i_sept):max(i_sept)],ylim=c(15,60))
# 2007-09-14
plot(data$time_stamp[974:1022],temp_wind[974:1022],ylim=c(15,25))
# October
i_oct <- which(data$time_stamp > as.POSIXct("2007-10-01 00:00:00") & data$time_stamp < as.POSIXct("2007-11-01 00:00:00"))
plot(data$time_stamp[min(i_oct):max(i_oct)],temp_wind[min(i_oct):max(i_oct)],ylim=c(15,60))
# 2007-10-18
plot(data$time_stamp[2606:2654],temp_wind[2606:2654],ylim=c(15,25))
# 2007-10-23
plot(data$time_stamp[2846:2894],temp_wind[2846:2894],ylim=c(15,25))
# 2007-10-24
plot(data$time_stamp[2895:2943],temp_wind[2895:2943],ylim=c(15,25))
# 2007-10-28
plot(data$time_stamp[3086:3134],temp_wind[3086:3134],ylim=c(15,25))
# November
i_nov <- which(data$time_stamp > as.POSIXct("2007-11-01 00:00:00") & data$time_stamp < as.POSIXct("2007-12-01 00:00:00"))
plot(data$time_stamp[min(i_nov):max(i_nov)],temp_wind[min(i_nov):max(i_nov)],ylim=c(15,60))
# 2007-11-06
plot(data$time_stamp[3518:3566],temp_wind[3518:3566],ylim=c(15,25))
# 2007-11-22
plot(data$time_stamp[4286:4334],temp_wind[4286:4334],ylim=c(15,25))
# 2007-11-23
plot(data$time_stamp[4334:4382],temp_wind[4334:4382],ylim=c(15,25))
# 2007-11-26
plot(data$time_stamp[4478:4526],temp_wind[4478:4526],ylim=c(15,25))
# December
i_dec <- which(data$time_stamp > as.POSIXct("2007-12-01 00:00:00") & data$time_stamp < as.POSIXct("2008-01-01 00:00:00"))
plot(data$time_stamp[min(i_dec):max(i_dec)],temp_wind[min(i_dec):max(i_dec)],ylim=c(15,60))
# 2007-12-03
plot(data$time_stamp[4814:(4814+48)],temp_wind[4814:(4814+48)],ylim=c(15,25))
# 2007-12-16
plot(data$time_stamp[5438:(5438+48)],temp_wind[5438:(5438+48)],ylim=c(15,25))
# 2007-12-22
plot(data$time_stamp[5726:(5726+48)],temp_wind[5726:(5726+48)],ylim=c(15,25))
# January 2008
i_jan <- which(data$time_stamp > as.POSIXct("2008-01-01 00:00:00") & data$time_stamp < as.POSIXct("2008-02-01 00:00:00"))
plot(data$time_stamp[min(i_jan):max(i_jan)],temp_wind[min(i_jan):max(i_jan)],ylim=c(15,60))
# 2008-01-01
plot(data$time_stamp[6209:(6209+48)],temp_wind[6209:(6209+48)],ylim=c(15,25))
# 2008-01-02
plot(data$time_stamp[6257:(6257+48)],temp_wind[6257:(6257+48)],ylim=c(15,25))
# 2008-01-07
plot(data$time_stamp[6494:(6494+48)],temp_wind[6494:(6494+48)],ylim=c(15,25))
# 2008-01-08
plot(data$time_stamp[6542:(6542+48)],temp_wind[6542:(6542+48)],ylim=c(15,25))
# 2008-01-19
plot(data$time_stamp[7070:(7070+48)],temp_wind[7070:(7070+48)],ylim=c(15,25))
# 2008-01-24
plot(data$time_stamp[7310:(7310+48)],temp_wind[7310:(7310+48)],ylim=c(15,25))
# February 2008
i_feb <- which(data$time_stamp > as.POSIXct("2008-02-01 00:00:00") & data$time_stamp < as.POSIXct("2008-02-28 00:00:00"))
plot(data$time_stamp[min(i_feb):max(i_feb)],temp_wind[min(i_feb):max(i_feb)],ylim=c(15,60))
# 2008-02-04
plot(data$time_stamp[7838:(7838+48)],temp_wind[7838:(7838+48)],ylim=c(15,25))
# 2008-02-05
plot(data$time_stamp[7886:(7886+48)],temp_wind[7886:(7886+48)],ylim=c(15,25))

# Low wind days (< 1st quartile of wind speed = 2.316 m/s)
# August
# 2007-08-26
plot(data$time_stamp[62:110],temp_wind[62:110],ylim=c(30,50))
# September
# 2007-09-04
plot(data$time_stamp[494:542],temp_wind[494:542],ylim=c(30,50))
# 2007-09-09
plot(data$time_stamp[734:782],temp_wind[734:782],ylim=c(30,50))
# 2007-09-26
plot(data$time_stamp[1550:1598],temp_wind[1550:1598],ylim=c(30,50))
# October
# 2007-10-03
plot(data$time_stamp[1886:1934],temp_wind[1886:1934],ylim=c(30,50))
# 2007-10-08
plot(data$time_stamp[2126:2174],temp_wind[2126:2174],ylim=c(30,50))
# 2007-10-13
plot(data$time_stamp[2366:2414],temp_wind[2366:2414],ylim=c(30,50))
# 2007-10-31
plot(data$time_stamp[3230:3278],temp_wind[3230:3278],ylim=c(30,50))
# November
# 2007-11-08
plot(data$time_stamp[3614:3662],temp_wind[3614:3662],ylim=c(30,50))
# 2007-11-28
plot(data$time_stamp[4574:4622],temp_wind[4574:4622],ylim=c(30,50))
# December
# 2007-12-24
plot(data$time_stamp[5822:5870],temp_wind[5822:5870],ylim=c(30,50))
# 2007-11-30
plot(data$time_stamp[6110:6158],temp_wind[6110:6158],ylim=c(30,50))
# January
# 2008-01-12
plot(data$time_stamp[6734:6782],temp_wind[6734:6782],ylim=c(30,50))
# February
# 2008-02-10
plot(data$time_stamp[8126:8174],temp_wind[8126:8174],ylim=c(30,50))

#### 3. Cleaning up ####

# Resetting plots

par <- old.par
# Deleting original margin settings for plots
rm(quartile1,quartile2,old.par,par)
rm(high_U_dates_index,low_U_dates_index,i_aug,i_dec,i_feb,i_jan,i_nov,i_oct,i_sept)
