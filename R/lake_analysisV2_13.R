##### ABOUT: LAKE FLUX ANALYSIS ################################################
# Title: Latent and sensible heat flux study over a lake/reservoir
# 
# Data provided by Heping Liu, PhD, from Washington State University (WSU) in MS Excel
# and converted to csv format within Excel.
# 
# Author: Yusri Yusup, PhD
# Affiliation:  Environmental Technology, School of Industrial Technology, 
#               Universiti Sains Malaysia (USM)
# Date created: 2014-02-xx
# Date modified: 2015-07-24
# Version: 2.13

##### 0. Preliminaries ############################################################

# Installing packages or sourcing custom functions
#install.packages('Hmisc')
#install.packages('ggplot2')
#install.packages('dplyr')

# To data convert factor to numeric

library(Hmisc) # To use cut2 and others
library(ggplot2) # For plots
library(dplyr) # To group data into hours, months, years, seasons
library(gridBase) # To combine ggplot2 and base graphics
library(grid)
library(openair) # To plot windrose

source('R/convert_magic.R')
source('R/convert_magic_num.R')
source('R/vap_pres_Buck.R') # To calculate vapor pressure using Buck's (1981) equation
source('R/water_vap_mix_ratio.R') # To calculate water vapor mixing ratio
source('R/bulk_moist_coef.R') # To calculate C_E.L, bulk transfer coeffficient of moisture
source('R/ideal_gas_eq_rho.R') # To calculate air density
source('R/unst_stab_category.R') # To categorize atmospheric stability
source('R/unst_stab_category2.R') # To categorize atmospheric stability to either unstable or stable
source('R/C_E_calc.R') # To calculate bulk transfer coefficient uisng M-O
source('R/multiplot.R')
source("R/multiplot2.R")
source("R/WindRose.R")
source("R/midpoint.R")
source("R/footprint_hsieh1.R") # Calculate footprint using Hsieh et al. (2000)
source("R/footprint_hsieh_peak.R") # Calculate peak footprint
source("R/footprint_schmid.R") # Calculate the peak distance using Schmid (1994)

##### 1. Data import and clean up #################################################

# Read data from original csv file
data <- read.csv('data/lake_data_new.csv')

# Read from EC processed data using Foken (2004) chapter
# Skip to line 12 to only include data from 2007-08-24 17:45 (or 17:30)
#temp <-read.csv('data/Reservoir_data_08242007-02162008.csv')
#header <- names(temp)
#dataEC <- read.csv('data/Reservoir_data_08242007-02162008.csv',skip=36)
#dataEC <- dataEC[-c(8449:nrow(dataEC)),]
#names(dataEC) <- header
#rm(temp,header)

# Remove the first four rows because they are empty rows or non-data rows
data <- data[c(-1,-2,-3,-4),]
# Remove row.names
row.names(data) <- NULL
# Rename column to "date" and "time"
colnames(data)[1] <- 'time_stamp'
# Convert from factor to numeric using the function 'convert_magic.R'
# Convert data to character first
data <- convert_magic(data,c(rep('character',times = 30)))
# Then, convert to numeric
data <- convert_magic_num(data,c('character',rep('numeric',times = 29)))
# Convert "date" from character to date
data$time_stamp<-strptime(data$time_stamp,"%m/%d/%y %H:%M")
# Changing all the '-999' (missing data) to NA
for (i in 1:length(data)){
  data[i][data[i] == -999] <- NA
}
rm(i)

# Remove Rn_Q71_Avg to be able to be combined with the 2008 data
data <- data[,-19]

# Combine data from 2008, dataframe d_2008 will be created
source('R/lake_analysis2008.R')

data <- rbind(data,d_2008)

##### 1A. Bad data removal, cold front classification ##############################
# This section is dataset-dependent. Rows with bad data obtained from visual
# inspection of data. Removal of data depends on plotting multiple box plots 
# and looking for outliers.

# Remove rows from 2008-02-16 17:30:00 (from [8449,) because anomalous T and RH trends
# observed when visually inspecting the time series.
data <- data[c(-8449:-9315),]
# Removing obs with unrealistic value of Wd_Spd_014A_Avg.1, 2, and 3 and Press_CS115_Avg from data
# An arbitrary value of > 100 m/s and < 20 hPa are chosen to depict "unrealistic" values
data$Wd_Spd_014A_Avg.1.[which(data$Wd_Spd_014A_Avg.1.>100)] <- NA
data$Wd_Spd_014A_Avg.2.[which(data$Wd_Spd_014A_Avg.2.>100)] <- NA
data$Wd_Spd_014A_Avg.3.[which(data$Wd_Spd_014A_Avg.3.>100)] <- NA
data$Press_CS115_Avg[which(data$Press_CS115_Avg<20)] <- NA
# Remove to unstable Z.L values less than z/L = -10 and more than z/L = 10
data$Z.L[which(data$Z.L < -10)] <- NA
data$Z.L[which(data$Z.L > 10)] <- NA
# Wd_Spd_014A_Avg.1. and Wd_Spd_014A_Avg.2. sometimes get stuck on value 0.447 and removed
data$Wd_Spd_014A_Avg.1.[which(data$Wd_Spd_014A_Avg.1.==0.447)] <- NA
data$Wd_Spd_014A_Avg.2.[which(data$Wd_Spd_014A_Avg.2.==0.447)] <- NA
# Pyra_Avg has extreme values of more than 1500, needs to be removed
data$Pyra_Avg[which(data$Pyra_Avg > 1500)] <- NA
# Rn_Q71_Avg and Rn_Lite_Avg get stuck at 184.7701 and 143.1251 respectively
# and removed data index no. from 3647 to 3884
# data$Rn_Q71_Avg[3647:3884] <- NA # Temporarily delete due to it being NA for 2008
data$Rn_Lite_Avg[3647:3884] <- NA
# rh_hmp_1_Avg and the other 2 (2 and 3) have values lower than zero
# t_hmp_1_Avg and the other 2 (2 and 3) have values lower than -20C
data$rh_hmp_1_Avg[which(data$rh_hmp_1_Avg < 0.1)] <- NA
data$rh_hmp_2_Avg[which(data$rh_hmp_2_Avg < 0.1)] <- NA
data$rh_hmp_3_Avg[which(data$rh_hmp_3_Avg < 0.1)] <- NA
data$t_hmp_1_Avg[which(data$t_hmp_1_Avg < -20)] <- NA
data$t_hmp_2_Avg[which(data$t_hmp_2_Avg < -20)] <- NA
data$t_hmp_3_Avg[which(data$t_hmp_3_Avg < -20)] <- NA

# Removing all improbable values of sonic_WS and sonic_WD
data$sonic_WS[which(data$sonic_WS < -100)] <- NA
data$sonic_WD[which(data$sonic_WD < -100)] <- NA

# Removing other spikes from visual inspection
# caused by rain
no_rain_index <- which(data$Rain_mm_Tot > 0)
data[no_rain_index,-1] <- NA
# z/L spikes from visual inspection
no_zl_spike <- c(66,814,1088,4225,5490,5808,6158,6623,6624,6625,7507,
                 8115,8116,8151,8160,8161)
data$Z.L[no_zl_spike] <- NA
# ustar spikes from visual inspection
# row numbers are given in despike.R
# Remove ustar and z/L spike values from original data row numbers
no_ustar_spike <- as.numeric(c(2503,4132:4138,5424,5771,2833:2880,4806,5665:5712,
                               7282,7283,7284))
data$U.[no_ustar_spike] <- NA

# Cleaning up
rm(no_rain_index,no_ustar_spike,no_zl_spike)
# Classifying data into two sets, 1 and 2
# index no. 3499 is when the data starts to become missing
# until 3886; missing data is categorized as set_no = 2
set_no = 0
k = nrow(data)
for (k in 1:k) {
  if (k < 3499) set_no[k] <- 1
  else if (k >= 3499 & k < 3886) set_no[k] <- 2
  else set_no[k] <- 3
}

# Adding set_no into the data frame
data<-cbind(data,set_no)
rm(set_no) # Deleting temporary set_no variable

# Classifying some days as "cold front" days (TRUE or FALSE).
# In this case, 18 days were determined to be cold front days.
cold_front <- (data$time_stamp >= as.POSIXct("2007-09-14 23:00:00") &
                 data$time_stamp < as.POSIXct("2007-09-17 11:00:00")) |
  (data$time_stamp >= as.POSIXct("2007-09-27 21:00:00") &
     data$time_stamp < as.POSIXct("2007-09-29 13:00:00")) |
  (data$time_stamp >= as.POSIXct("2007-11-06 03:00:00") &
     data$time_stamp < as.POSIXct("2007-11-07 17:00:00")) |
  (data$time_stamp >= as.POSIXct("2007-11-14 22:00:00") &
     data$time_stamp < as.POSIXct("2007-11-16 11:00:00")) |
  (data$time_stamp >= as.POSIXct("2007-11-21 23:00:00") &
     data$time_stamp < as.POSIXct("2007-11-25 15:00:00")) |
  (data$time_stamp >= as.POSIXct("2007-10-09 07:00:00") &
     data$time_stamp < as.POSIXct("2007-10-12 14:00:00")) |
  (data$time_stamp >= as.POSIXct("2007-10-18 20:00:00") &
     data$time_stamp < as.POSIXct("2007-10-20 09:00:00")) |
  (data$time_stamp >= as.POSIXct("2007-10-23 07:00:00") &
     data$time_stamp < as.POSIXct("2007-10-24 17:00:00")) |
  (data$time_stamp >= as.POSIXct("2007-12-02 23:00:00") &
     data$time_stamp < as.POSIXct("2007-12-04 09:00:00")) |
  (data$time_stamp >= as.POSIXct("2007-12-15 17:00:00") &
     data$time_stamp < as.POSIXct("2007-12-17 07:00:00")) |
  (data$time_stamp >= as.POSIXct("2007-12-22 23:00:00") &
     data$time_stamp < as.POSIXct("2007-12-23 15:00:00")) |
  (data$time_stamp >= as.POSIXct("2007-12-31 21:00:00") &
     data$time_stamp < as.POSIXct("2008-01-03 14:00:00")) |
  (data$time_stamp >= as.POSIXct("2008-01-08 23:30:00") &
     data$time_stamp < as.POSIXct("2008-01-09 15:00:00")) |
  (data$time_stamp >= as.POSIXct("2008-01-10 21:00:00") &
     data$time_stamp < as.POSIXct("2008-01-14 13:00:00")) |
  (data$time_stamp >= as.POSIXct("2008-01-24 01:00:00") &
     data$time_stamp < as.POSIXct("2008-01-25 17:00:00")) |
  (data$time_stamp >= as.POSIXct("2008-01-29 23:00:00") &
     data$time_stamp < as.POSIXct("2008-02-01 17:00:00")) |
  (data$time_stamp >= as.POSIXct("2008-02-05 22:00:00") &
     data$time_stamp < as.POSIXct("2008-02-08 17:00:00")) |
  (data$time_stamp >= as.POSIXct("2008-02-12 22:00:00") &
     data$time_stamp < as.POSIXct("2008-02-14 16:00:00"))

# To add non-windy 13 days and windy 12 days classification into 'data'
windy <- (data$time_stamp >= as.POSIXct("2007-09-14 00:00:00") &
            data$time_stamp < as.POSIXct("2007-09-15 00:00:00")) |
  (data$time_stamp >= as.POSIXct("2007-10-18 00:00:00") &
     data$time_stamp < as.POSIXct("2007-10-19 00:00:00")) |
  (data$time_stamp >= as.POSIXct("2007-10-23 00:00:00") &
     data$time_stamp < as.POSIXct("2007-10-24 00:00:00")) |
  (data$time_stamp >= as.POSIXct("2007-10-24 00:00:00") &
     data$time_stamp < as.POSIXct("2007-10-25 00:00:00")) |
  (data$time_stamp >= as.POSIXct("2007-10-28 00:00:00") &
     data$time_stamp < as.POSIXct("2007-10-29 00:00:00")) |
  (data$time_stamp >= as.POSIXct("2007-11-06 00:00:00") &
     data$time_stamp < as.POSIXct("2007-11-07 00:00:00")) |
  (data$time_stamp >= as.POSIXct("2007-11-22 00:00:00") &
     data$time_stamp < as.POSIXct("2007-11-23 00:00:00")) |
  (data$time_stamp >= as.POSIXct("2007-11-23 00:00:00") &
     data$time_stamp < as.POSIXct("2007-11-24 00:00:00")) |
  (data$time_stamp >= as.POSIXct("2007-11-26 00:00:00") &
     data$time_stamp < as.POSIXct("2007-11-27 00:00:00")) |
  (data$time_stamp >= as.POSIXct("2007-12-03 00:00:00") &
     data$time_stamp < as.POSIXct("2007-12-04 00:00:00")) |
  (data$time_stamp >= as.POSIXct("2007-12-16 00:00:00") &
     data$time_stamp < as.POSIXct("2007-12-17 00:00:00")) |
  (data$time_stamp >= as.POSIXct("2007-12-22 00:00:00") &
     data$time_stamp < as.POSIXct("2007-12-23 00:00:00"))

n_windy <- (data$time_stamp >= as.POSIXct("2007-08-26 00:00:00") &
            data$time_stamp < as.POSIXct("2007-08-27 00:00:00")) |
  (data$time_stamp >= as.POSIXct("2007-09-04 00:00:00") &
     data$time_stamp < as.POSIXct("2007-09-05 00:00:00")) |
  (data$time_stamp >= as.POSIXct("2007-09-09 00:00:00") &
     data$time_stamp < as.POSIXct("2007-09-10 00:00:00")) |
  (data$time_stamp >= as.POSIXct("2007-09-26 00:00:00") &
     data$time_stamp < as.POSIXct("2007-09-27 00:00:00")) |
  (data$time_stamp >= as.POSIXct("2007-10-03 00:00:00") &
     data$time_stamp < as.POSIXct("2007-10-04 00:00:00")) |
  (data$time_stamp >= as.POSIXct("2007-10-08 00:00:00") &
     data$time_stamp < as.POSIXct("2007-10-09 00:00:00")) |
  (data$time_stamp >= as.POSIXct("2007-10-13 00:00:00") &
     data$time_stamp < as.POSIXct("2007-10-14 00:00:00")) |
  (data$time_stamp >= as.POSIXct("2007-10-31 00:00:00") &
     data$time_stamp < as.POSIXct("2007-11-01 00:00:00")) |
  (data$time_stamp >= as.POSIXct("2007-11-08 00:00:00") &
     data$time_stamp < as.POSIXct("2007-11-09 00:00:00")) |
  (data$time_stamp >= as.POSIXct("2007-11-28 00:00:00") &
     data$time_stamp < as.POSIXct("2007-11-29 00:00:00")) |
  (data$time_stamp >= as.POSIXct("2007-12-24 00:00:00") &
     data$time_stamp < as.POSIXct("2007-12-25 00:00:00")) |
  (data$time_stamp >= as.POSIXct("2008-01-12 00:00:00") &
     data$time_stamp < as.POSIXct("2008-01-13 00:00:00")) |
  (data$time_stamp >= as.POSIXct("2008-02-10 00:00:00") &
     data$time_stamp < as.POSIXct("2008-02-11 00:00:00"))

# Classifying the data into days, wind category, and max wind category
# A total of 177 days

# Classifying into days
start <- 1187884800 # 2007-08-24 00:00:00
difference <- 86400 # 24 * 60 * 60 seconds
end <- 1203177600 # Might not need to use 
index = 1 # The number the days
day = numeric(length=nrow(data)) # Initialize day variable

for (i in 1:nrow(data)){
  # A failsafe if time_stamp is NA
  if(is.na(as.numeric(data$time_stamp[i]))){
    # Assigned the NA value as the day before
    day[i] <- index
  } else {
    if(as.numeric(data$time_stamp[i]) >= start & 
         as.numeric(data$time_stamp[i]) < (difference + start)){
      day[i] <- index
    } else {
      day[i] <- index
      index <- index + 1
      start <- difference + start
    }
  }
}

rm(start,difference,end,index)

##### 2. Parameter calculations #################################################
#
# A.  Temperature difference: 
#       deltaT = Ts - Ta
#         Ts = Water surface temperature, measured 0.5 m under the water
#         Ta = Atmospheric temperature, measured 5.4 m above the water
#     Calculate deltaT = Ts - Ta, Ts = Water.surface.temperature, Ta = t_hmp_3_Avg [10] for stable 
#     conditions and include this into data frame

deltaT <- data$Water.surface.temperature - data$t_hmp_3_Avg
data <- cbind(data,deltaT)
rm(deltaT) # Deleting temporary variable deltaT

# B.  Wind speed and temperature difference: U_deltaT = U (Ts - Ta)
#     U is from WS_Spd_WVT at 4.00 m.
#     Calculate U (Ts - Ta) and add to data frame,

u_deltaT <- data$WS_Spd_WVT * data$deltaT
data <- cbind(data,u_deltaT)
rm(u_deltaT) # Deleting temporary variable u_deltaT

# C.  Calculating vapor pressures of Water.surface.temperature and t_hmp_3_Avg (Ta) at 5.46 m
#     using Buck (1981) vapor pressure equation using the custom function 'vap_pres_Buck.R'.
#     Visit http://faculty.eas.ualberta.ca/jdwilson/EAS372_13/Vomel_CIRES_satvpformulae.html for details
#     Buck's equation:  e = 6.1121 exp (17.502 T)/(240.97 + T)
#                     e = vapor pressure (hPa = hectoPascal) 
#                     T = temperature at a given height (deg C)
#     Note: t_hmp_1_Avg and rh_hmp_1_Avg is at 1.9 m above water surface
#         t_hmp_2_Avg and rh_hmp_2_Avg is at 3.0 m above water surface
#         t_hmp_3_Avg and rh_hmp_3_Avg is at 5.46 m above water surface

e_s1 <- vap_pres_Buck(data$Water.surface.temperature, 1.00)  # RH = 1.00 because saturated
e_s2 <- vap_pres_Buck(data$t_hmp_1_Avg, data$rh_hmp_1_Avg)
e_s3 <- vap_pres_Buck(data$t_hmp_2_Avg, data$rh_hmp_2_Avg)
e_a <- vap_pres_Buck(data$t_hmp_3_Avg, data$rh_hmp_3_Avg) 
# deltaE must be in kPa for bulk aerodynamic transfer equation below
deltaE <- (e_s1 - e_a) * 0.1 # because to convert to kPa

# D.  Wind and water vapor pressure difference
#     U_deltaE = U (es - ea) (kPa.m/s)
#       U = wind speed (m/s)
#       e = vapor pressure (kPa)
 
u_deltaE <- data$WS_Spd_WVT * deltaE

# E.  Calculating water vapor mixing ratio (kg/kg) using formula:
#       q = 0.622 (e / [Patm - e])
#         e = vap = water vapor pressure at a height (Pa)
#         Patm = atmospheric pressure (Pa)

qs <- water_vap_mix_ratio(e_s1 * 100, data$Press_CS115_Avg * 1000) # x100 and x1000 because change to Pa
qa <- water_vap_mix_ratio(e_a * 100, data$Press_CS115_Avg * 1000)

# Difference between qs and qa
deltaQ <- qs - qa

# F.  Calculate C_E (bulk transfer coefficient) [dimensionless] using the bulk aerodynamic mass transfer
#     equation
#       LE = L C_E U (es - ea) (W/m2)
#         L = 2,540,000 J/kg = latent heat of vaporization
#         U = wind speed (m/s) 
#         e = vapor pressure (Pa) 
#         C_E = bulk transfer coefficient [(s/m)2]


C_E = bulk_moist_coef(data$LE,data$WS_Spd_WVT,deltaQ)/2540000


# Adding all data from sections C, D, E, and F into data frame
data<-cbind(data,qs,qa,deltaQ,e_s1,e_s2,e_s3,e_a,deltaE,u_deltaE,C_E)
rm(e_s1,e_s2,e_s3,e_a,qs,qa,deltaQ,deltaE,C_E,u_deltaE) # Deleting all temporary variables

# G. Calculating atmospheric air density (kg/m3) using the ideal gas law density 
#     rho = P/RT, 
#     R = universal gas constant = 286.9 J/kg K 
#     T = absolute temperature (K) at 5.46 m
#     P = pressure (Pa, not kPA)
density <- ideal_gas_eq_rho(data$Press_CS115_Avg * 1000, data$t_hmp_3_Avg + 273.15)

#     Calculate C_H (bulk transfer coefficient) [dimensionless] using the bulk aerodynamic mass transfer
#     equation
#       H = rho_a C_H C_P U (Ts - Ta) (W/m2)
#       rho_a = air density (kg/m3)
#       C_P = 1005 J/kg K = heat capacity of air at 20 C
#       U = wind speed (m/s) 
#       T = Temperature (K) 
#       C_H = bulk transfer coefficient [dimensionless]

C_P <- 1005
C_H <- data$H / (density * C_P * data$WS_Spd_WVT * data$deltaT)
rm(C_P)


# Adding data to data frame
data <- cbind(data,density,C_H)
rm(density,C_H) # Deleting temporary variable density

# H. Calculating using Charnock's (1955) equation over water
# z0 = 0.016 (ustar^2/g) unit m
# Constant is 0.016 +- 20%
roughness_length <- 0.0192*data$U.*data$U./9.80
data <-cbind(data,roughness_length)
rm(roughness_length)

# I. Calculate C_E using similarity theory (M-O) and Businger et al. (1971)
# profiles
C_E_estim <- 0
height <- 5.46

for(i in 1:nrow(data)){
  C_E_estim[i] <- C_E_calc(height,data$roughness_length[i],data$Z.L[i])
}

diff <- data$C_E - C_E_estim
data <- cbind(data,C_E_estim,diff)
rm(C_E_estim,height,diff)

# J. Calculate LE/U and LE/deltaE to remove the affect of U or deltaE on LE
LE_U <- data$LE/data$WS_Spd_WVT
LE_deltaE <- data$LE/data$deltaE
LE_udeltaE <- data$LE/data$u_deltaE

H_U <- data$H/data$WS_Spd_WVT
H_deltaT <- data$H/data$deltaT
H_udeltaT <- data$H/data$u_deltaT
data <- cbind(data,LE_U,LE_deltaE,LE_udeltaE,H_U,H_deltaT,H_udeltaT)
rm(LE_U,LE_deltaE,LE_udeltaE,H_U,H_deltaT,H_udeltaT)

# Lastly since some of these calculations would results in NaN because division by zero,
# we need to remove these NaN's from the data.
# Remove all all NaN to become NA from 'data',
# There will be warnings because some columns do not have NaNs
k = length(data)
for (k in 2:k){ # Starts from 2 because the first column is the time stamp
  data[which(is.nan(data[,k])),] <- NA
}
rm(k) # Deleting the temporary k variable

# Classifying into wind categories
# cat1 = < 2.322; cat2 = 2.322 < U < 3.708; cat3 = 3.708 < U < 5.142
# cat4 = U > 5.142
wind_category <- numeric(length=nrow(data))

# To obtain the 25th and 75th percentile values from summary.
quartile1 <- as.numeric(summary(data$WS_Spd_WVT)[2])
quartile2 <- as.numeric(summary(data$WS_Spd_WVT)[3])
quartile3 <- as.numeric(summary(data$WS_Spd_WVT)[5])

for (i in 1:nrow(data)){
  # A failsafe if time_stamp is NA
  if(is.na(data$WS_Spd_WVT[i])){
    # Assigned NA if WS_Spd_WVT is NA
    wind_category[i] <- NA
  } else {
    if(data$WS_Spd_WVT[i] <= quartile1){
      wind_category[i] <- 1
    } else if (data$WS_Spd_WVT[i] > quartile1 & data$WS_Spd_WVT[i] <= quartile2){
      wind_category[i] <- 2
    } else if (data$WS_Spd_WVT[i] > quartile2 & data$WS_Spd_WVT[i] <= quartile3){
      wind_category[i] <- 3
    } else if (data$WS_Spd_WVT[i] > quartile3){
      wind_category[i] <- 4
    }
  }
}

day_wind <- numeric(length=177)
for (i in 1:177){ # 177 total days in dataset
  cat1 <- sum(wind_category==1 & day==i,na.rm=TRUE)
  cat2 <- sum(wind_category==2 & day==i,na.rm=TRUE)
  cat3 <- sum(wind_category==3 & day==i,na.rm=TRUE)
  cat4 <- sum(wind_category==4 & day==i,na.rm=TRUE)
  day_wind[i] <- which.max(c(cat1,cat2,cat3,cat4))
}

rm(cat1,cat2,cat3,cat4,i)

wind_category_day <- numeric(length=nrow(data))

for (i in 1:length(day_wind)) {
  for (k in 1:nrow(data)) {
    if (is.na(day[k])) {
      wind_category_day[k] <- NA
    } else if (day[k]==i){
      wind_category_day[k] <- day_wind[i]
    }
  }
}

data <- cbind(data,wind_category_day)
rm(day_wind,k,wind_category_day)

# Adding to data frame
data <- cbind(data,day,cold_front,windy,n_windy,wind_category)
rm(cold_front,windy,n_windy,wind_category) # Deleting temporary variable
rm(quartile1,quartile2,quartile3,i,day)

# Drag coefficient, C_D
# Formulae: u*^2 = C_D * U^2
# Where, C_D = drag coefficient [dimensionless]
# U = mean wind speed

C_D = data$U.^2 / data$WS_Spd_WVT^2
data <- cbind(data,C_D)
rm(C_D)

##### 3. Classifying the data ############################################
# Classifying the data into unstable and stable categories

stability_no <- sapply(data$Z.L,unst_stab_category)
stability <- sapply(data$Z.L,unst_stab_category2)

# If season is summer-fall then 1 if winter then 0
season <- as.numeric((data$time_stamp >= as.POSIXct("2007-08-24 17:30:00") &
                 data$time_stamp < as.POSIXct("2007-12-01 00:00:00"))) 

# Adding to the data frame
data <- cbind(data,stability_no,stability,season)
rm(stability_no,stability,season) # Deleting temporary variables

# Grouping the data according month Sept 2007
data_group <- data %>% 
  mutate(time_stamp=as.POSIXct(time_stamp)) %>%
  group_by(month=format(time_stamp, '%m'),
           hour=format(as.POSIXlt(cut(time_stamp,breaks='hour')),'%H')) %>%
  summarise(LE=mean(LE,na.rm=TRUE), H=mean(H,na.rm=TRUE), Z.L = mean(Z.L,na.rm=TRUE),
            WS = mean(WS_Spd_WVT,na.rm=TRUE),water_temp=mean(Water.surface.temperature,na.rm=TRUE),
            es=mean(e_s1,na.rm=TRUE) * 0.1 ,ea=mean(e_a,na.rm=TRUE) * 0.1,deltaE=mean(deltaE,na.rm=TRUE),
            Ta=mean(t_hmp_3_Avg,na.rm=TRUE))# need to multiply e_s1 and e_a with 0.1 because units in hPa not kPa

data_group_sd <- data %>% 
  mutate(time_stamp=as.POSIXct(time_stamp)) %>%
  group_by(month=format(time_stamp, '%m'),
           hour=format(as.POSIXlt(cut(time_stamp,breaks='hour')),'%H')) %>%
  summarise(sdLE = sd(LE,na.rm=TRUE), sdH=sd(H,na.rm=TRUE),sdZ.L = sd(Z.L,na.rm=TRUE),
            sdWS = sd(WS_Spd_WVT,na.rm=TRUE),sdwater_temp=sd(Water.surface.temperature,na.rm=TRUE),
            sdes=sd(e_s1,na.rm=TRUE),sdea=sd(e_a,na.rm=TRUE),sddeltaE=sd(deltaE,na.rm=TRUE),
            sdTa=sd(t_hmp_3_Avg,na.rm=TRUE))
data_group <- cbind(data_group,data_group_sd)
rm(data_group_sd)

## Using the openair package, to correct for missing or erroneous time_stamp
# Need to change the header of time_stamp to date first and GMT time
names(data)[1] <- 'date'
data$date <- as.POSIXct(data$date,tz='GMT')
data <- timeAverage(data, avg.time = '30 min', statistic = 'mean')
# Changing it back
names(data)[1] <- 'time_stamp'
data$time_stamp <- as.POSIXlt(data$time_stamp)

## Combine QC data for LE and H into data, note decided not to include QC values
# qc_H <- as.factor(dataEC$QCFlag_h)
# qc_LE <- as.factor(dataEC$QCFlag_e)
# qc_Tau <- as.factor(dataEC$QCFlag_t)

# data <- cbind(data,qc_H,qc_LE,qc_Tau)
# rm(qc_H,qc_LE,qc_Tau)

# 80% footprint (m) calculations using Hsieh et al. (2000) model
Distance <- numeric()
zm <- rep(4,nrow(data))
for (i in 1:nrow(data)) {
  Distance[i] <- footprint_hsieh1(data$U.[i],data$H[i],data$t_hmp_3_Avg[i],
                                  zm[i],zo=0.001,perc=0.7) * 4.00 
  # 4 [m] is height of EC system 
}
data <- cbind(data,Distance)
rm(zm,i,Distance)

# Peak footprint (m) calculations using Hsieh et al. (2000) model
Peak <- numeric()
zm <- rep(4,nrow(data))
for (i in 1:nrow(data)) {
  Peak[i] <- footprint_hsieh_peak(data$U.[i],data$H[i],data$t_hmp_3_Avg[i],
                              zm[i],zo=0.001,perc=0.7) * 4.00 
  # 4 [m] is height of EC system 
}
data <- cbind(data,Peak)
rm(zm,i,Peak)

## Decided to not remove according to Foken 'quality flags' procedures
## due to maintain generality of results

# Remove all qc = 9 for LE and H (LE_U, LE_deltaE, H_U, H_deltaT, C_E, C_H)
#data$LE[which(data$qc_LE == '9' | data$qc_LE == '-9999' | data$qc_LE == '7' |
#                data$qc_LE == '8')] <- NA
#data$LE[which(data$stability_no==10)] <- NA
#data$H[which(data$qc_H == '9' | data$qc_H == '-9999' | data$qc_LE == '7' |
#               data$qc_LE == '8')] <- NA
#data$U.[which(data$qc_Tau == '9' | data$qc_Tau == '9999' | data$qc_LE == '7' |
#                data$qc_LE == '8')] <- NA

#data$LE_U[which(data$qc_LE == '9' | data$qc_LE == '-9999' | data$qc_LE == '7' |
#                data$qc_LE == '8')] <- NA 
#data$LE_U[which(data$stability_no==10)] <- NA #Remove one stability_no=10
#data$H_U[which(data$qc_H == '9' | data$qc_H == '-9999' | data$qc_LE == '7' |
#               data$qc_LE == '8')] <- NA
#data$LE_deltaE[which(data$qc_LE == '9' | data$qc_LE == '-9999' | data$qc_LE == '7' |
#                  data$qc_LE == '8')] <- NA

#data$LE_deltaE[which(data$stability_no==10)] <- NA #Remove one stability_no=10

#data$H_deltaT[which(data$qc_H == '9' | data$qc_H == '-9999' | data$qc_LE == '7' |
#                 data$qc_LE == '8')] <- NA
#data$C_E[which(data$qc_LE == '9' | data$qc_LE == '-9999' | data$qc_LE == '7' |
#                 data$qc_LE == '8')] <- NA

#data$C_E[which(data$stability_no == 10)] <- NA

#data$C_H[which(data$qc_H == '9' | data$qc_H == '-9999' | data$qc_LE == '7' |
#                 data$qc_LE == '8')] <- NA

# Remove all NaN created by timeAverage function and others

#data[sapply(data,is.na)] <- NA

#d <- sapply(d, function(x){ x[is.nan(x)] <- NA})

##### 4. Some statistical analysis ###########################

## 1. To find the different between LE in unstable and stable conditions
# Wilcoxon rank sum test unpaired between LE in stable and unstable conditions
wilcox.test(data$LE[which(data$Z.L>=0)],data$LE[which(data$Z.L<0)])
# Median and number of data of LE in stable conditions
median(data$LE[which(data$Z.L>=0)],na.rm=TRUE)
sum(data$Z.L>=0,na.rm=TRUE)
# Median and number of data of LE in unstable conditions
median(data$LE[which(data$Z.L<0)],na.rm=TRUE)
sum(data$Z.L<0,na.rm=TRUE)

### 2. To find the difference between deltaE in unstable and stable conditions
#Wilcoxon rank sum test unpaired between deltaE in stable and unstable conditions
wilcox.test(data$deltaE[which(data$Z.L>=0)],data$deltaE[which(data$Z.L<0)])
#Median and number of data of deltaE in stable conditions
median(data$deltaE[which(data$Z.L>=0)],na.rm=TRUE)
#Median and number of data of deltaE in unstable conditions
median(data$deltaE[which(data$Z.L<0)],na.rm=TRUE)

### 3. To find the difference between U in unstable and stable conditions
#Wilcoxon rank sum test unpaired between U in stable and unstable conditions
wilcox.test(data$WS_Spd_WVT[which(data$Z.L>=0)],data$WS_Spd_WVT[which(data$Z.L<0)])
#Median and number of data of U in stable conditions
median(data$WS_Spd_WVT[which(data$Z.L>=0)],na.rm=TRUE)
#Median and number of data of U in unstable conditions
summary(data$WS_Spd_WVT[which(data$Z.L<0)],na.rm=TRUE)

### 4. To find the difference between u_deltaE in unstable and stable conditions
#Wilcoxon rank sum test unpaired between deltaP_q in stable and unstable conditions
wilcox.test(data$u_deltaE[which(data$Z.L>=0)],data$u_deltaE[which(data$Z.L<0)])
#Median and number of data of deltaP_q in stable conditions
median(data$u_deltaE[which(data$Z.L>=0)],na.rm=TRUE)
#Median and number of data of deltaP_q in unstable conditions
median(data$u_deltaE[which(data$Z.L<0)],na.rm=TRUE)

# Subsequent plots require 'data_rsq'
source('R/regression_results.R')

##### 5. Plots ###############################################

### Final plots for publication: Evaporation and stability

# Names of various figures and box plots
names_boxplot = c('\u221210\u2264\u03B6<\u22121','\u22121\u2264\u03B6<\u22120.5','\u22120.5\u2264\u03B6<\u22120.1','\u22120.1\u2264\u03B6<\u22120.05',
                  '\u22120.05\u2264\u03B6<0','0\u2264\u03B6<0.05','0.05\u2264\u03B6<0.1','0.1\u2264\u03B6<0.5','0.5\u2264\u03B6<1',
                  '1\u2264\u03B6<10')

### Keeping original margin settings for plots
old.par <- par()

##### Fig. 2 r and LE (with regressions) ####

## Separating the data into negative deltaE and positve deltaE
neg <- subset(data,deltaE<0)
pos <- subset(data,deltaE>=0)

## Regression lines for plot
lm1<-lm(neg$LE[which(neg$Z.L<0)]~neg$deltaE[which(neg$Z.L<0)])
lm2<-lm(neg$LE[which(neg$Z.L>=0)]~neg$deltaE[which(neg$Z.L>=0)])

lm3<-lm(pos$LE[which(pos$Z.L<0)]~pos$deltaE[which(pos$Z.L<0)])
lm4<-lm(pos$LE[which(pos$Z.L>=0)]~pos$deltaE[which(pos$Z.L>=0)])

lm5<-lm(neg$LE[which(neg$Z.L<0)]~neg$WS_Spd_WVT[which(neg$Z.L<0)])
lm6<-lm(neg$LE[which(neg$Z.L>=0)]~neg$WS_Spd_WVT[which(neg$Z.L>=0)])

lm7<-lm(pos$LE[which(pos$Z.L<0)]~pos$WS_Spd_WVT[which(pos$Z.L<0)])
lm8<-lm(pos$LE[which(pos$Z.L>=0)]~pos$WS_Spd_WVT[which(pos$Z.L>=0)])

lm9<-lm(neg$LE[which(neg$Z.L<0)]~neg$u_deltaE[which(neg$Z.L<0)])
lm10<-lm(neg$LE[which(neg$Z.L>=0)]~neg$u_deltaE[which(neg$Z.L>=0)])

lm11<-lm(pos$LE[which(pos$Z.L<0)]~pos$u_deltaE[which(pos$Z.L<0)])
lm12<-lm(pos$LE[which(pos$Z.L>=0)]~pos$u_deltaE[which(pos$Z.L>=0)])


# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/fig_2.jpg')
jpeg(file=path_fig,width=3060, height=3600,res=360)
## Creating 6 panels of plots
plot.new()
par(mfrow=c(3,2),mar=c(4.1,4.5,2.1,1.1),family='Times')

## Plotting LE against deltaE for negative deltaE values
plot(neg$deltaE[which(neg$Z.L<0)],neg$LE[which(neg$Z.L<0)],pch=17,col='red',
     xlab=expression(paste(Delta,'e')),ylab='LE',xlim=c(-0.8,0),ylim=c(-90,40),cex.lab=2.2,cex.axis=2.2,xaxt='n',yaxt='n')
axis(at=seq(-0.8,-0.2,0.2),side=1,labels=paste("\U2212",seq(0.8,0.2,-0.2),sep=""),cex.axis=2.2)
axis(at=0,side=1,labels=0,cex.axis=2.2)
axis(at=c(-80,-40),side=2,labels=paste("\U2212",c(80,40),sep=""),cex.axis=2.2)
axis(at=c(0,40),side=2,labels=c(0,40),cex.axis=2.2)
text(-0.80,38,'a)',cex=2.2)
minor.tick(ny=2,nx=2,tick.ratio=0.5)
points(neg$deltaE[which(neg$Z.L>0)],neg$LE[which(neg$Z.L>0)],pch=19, col='blue')
abline(lm1,col='red',lwd=3)
abline(lm2,col='blue',lty=2,lwd=3)

## Plotting LE against deltaE for positive deltaE values
plot(pos$deltaE[which(pos$Z.L<0)],pos$LE[which(pos$Z.L<0)],pch=17,col='red',
     xlab=expression(paste(Delta,'e')),ylab='',xlim=c(0,3),ylim=c(-50,500),cex.lab=2.2,cex.axis=2.2)
text(0,492,'d)',cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
points(pos$deltaE[which(pos$Z.L>0)],pos$LE[which(pos$Z.L>0)],pch=19, col='blue')
abline(lm3,col='red',lwd=3)
abline(lm4,col='blue',lty=2,lwd=3)

## Plotting LE against U for negative deltaE values
plot(neg$WS_Spd_WVT[which(neg$Z.L<0)],neg$LE[which(neg$Z.L<0)],pch=17,col='red',
     xlab='U',ylab='LE',xlim=c(0,14),ylim=c(-90,40),cex.lab=2.2,cex.axis=2.2,yaxt='n')
axis(at=c(-80,-40),side=2,labels=paste("\U2212",c(80,40),sep=""),cex.axis=2.2)
axis(at=c(0,39),side=2,labels=c(0,40),cex.axis=2.2)
text(0,38,'b)',cex=2.2)
minor.tick(ny=2,nx=2,tick.ratio=0.5)
points(neg$WS_Spd_WVT[which(neg$Z.L>=0)],neg$LE[which(neg$Z.L>=0)],pch=19, col='blue')
abline(lm5,col='red',lwd=3)
abline(lm6,col='blue',lty=2,lwd=3)

## Plotting LE against U for positive deltaE values
plot(pos$WS_Spd_WVT[which(pos$Z.L<0)],pos$LE[which(pos$Z.L<0)],pch=17,col='red',
     xlab='U',ylab='',ylim=c(-50,500),xlim=c(0,14),cex.lab=2.2,cex.axis=2.2)
text(0,492,'e)',cex=2.2)
minor.tick(ny=2,nx=2,tick.ratio=0.5)
points(pos$WS_Spd_WVT[which(pos$Z.L>=0)],pos$LE[which(pos$Z.L>=0)],pch=19,col='blue')
abline(lm7,col='red',lwd=3)
abline(lm8,col='blue',lty=2,lwd=3)

## Plotting LE against u_deltaE for negative deltaE values
plot(neg$u_deltaE[which(neg$Z.L<0)],neg$LE[which(neg$Z.L<0)],pch=17,col='red',
     xlab=expression(paste('U',Delta,'e')),ylab='LE',xlim=c(-5,0),ylim=c(-90,40),cex.lab=2.2,cex.axis=2.2,yaxt='n',xaxt='n')
axis(at=c(-80,-40),side=2,labels=paste("\U2212",c(80,40),sep=""),cex.axis=2.2)
axis(at=c(0,38),side=2,labels=c(0,40),cex.axis=2.2)
axis(at=seq(-5,-1,1),side=1,labels=paste("\U2212",seq(5,1,-1),sep=""),cex.axis=2.2)
axis(at=0,side=1,labels=0,cex.axis=2.2)
text(-5,38,'c)',cex=2.2)
minor.tick(ny=2,nx=2,tick.ratio=0.5)
points(neg$u_deltaE[which(neg$Z.L>=0)],neg$LE[which(neg$Z.L>=0)],pch=19, col='blue')
abline(lm9,col='red',lwd=3)
abline(lm10,col='blue',lty=2,lwd=3)

## Plotting LE against u_deltaE for positive deltaE values
plot(pos$u_deltaE[which(pos$Z.L<0)],pos$LE[which(pos$Z.L<0)],pch=17,col='red',
     xlab=expression(paste('U',Delta,'e')),ylab='',ylim=c(-50,500),xlim=c(0,15),cex.lab=2.2,cex.axis=2.2)
text(0,492,'f)',cex=2.2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
points(pos$u_deltaE[which(pos$Z.L>=0)],pos$LE[which(pos$Z.L>=0)],pch=19, col='blue')
abline(lm11,col='red',lwd=3)
abline(lm12,col='blue',lty=2,lwd=3)

# To turn off plotting in a different window
dev.off()
## Cleaning up
rm(neg,pos,lm1,lm2,lm3,lm4,lm5,lm6,lm7,lm8,lm9,lm10,lm11,lm12)

##### Fig. 3 r and H (with regressions) ####

## Separating the data into negative deltaT and positve deltaT
neg <- subset(data,deltaT<0)
pos <- subset(data,deltaT>=0)

## Regression lines for plot
lm1<-lm(neg$H[which(neg$Z.L<0)]~neg$deltaT[which(neg$Z.L<0)])
lm2<-lm(neg$H[which(neg$Z.L>=0)]~neg$deltaT[which(neg$Z.L>=0)])

lm3<-lm(pos$H[which(pos$Z.L<0)]~pos$deltaT[which(pos$Z.L<0)])
lm4<-lm(pos$H[which(pos$Z.L>=0)]~pos$deltaT[which(pos$Z.L>=0)])

lm5<-lm(neg$H[which(neg$Z.L<0)]~neg$WS_Spd_WVT[which(neg$Z.L<0)])
lm6<-lm(neg$H[which(neg$Z.L>=0)]~neg$WS_Spd_WVT[which(neg$Z.L>=0)])

lm7<-lm(pos$H[which(pos$Z.L<0)]~pos$WS_Spd_WVT[which(pos$Z.L<0)])
lm8<-lm(pos$H[which(pos$Z.L>=0)]~pos$WS_Spd_WVT[which(pos$Z.L>=0)])

lm9<-lm(neg$H[which(neg$Z.L<0)]~neg$u_deltaT[which(neg$Z.L<0)])
lm10<-lm(neg$H[which(neg$Z.L>=0)]~neg$u_deltaT[which(neg$Z.L>=0)])

lm11<-lm(pos$H[which(pos$Z.L<0)]~pos$u_deltaT[which(pos$Z.L<0)])
lm12<-lm(pos$H[which(pos$Z.L>=0)]~pos$u_deltaT[which(pos$Z.L>=0)])


# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/fig_3.jpg')
jpeg(file=path_fig,width=3060, height=3600,res=360)
## Creating 6 panels of plots
plot.new()
par(mfrow=c(3,2),mar=c(4.1,4.5,2.1,1.1),family='Times')

## Plotting H against deltaT for negative deltaT values
plot(neg$deltaT[which(neg$Z.L<0)],neg$H[which(neg$Z.L<0)],pch=17,col='red',
     xlab=expression(paste(Delta,'T')),ylab='H',xlim=c(-12,0),ylim=c(-150,55),cex.lab=2.1,cex.axis=2.1,xaxt='n',yaxt='n')
axis(at=seq(-12,-2,2),side=1,labels=paste("\U2212",seq(12,2,-2),sep=""),cex.axis=2.1)
axis(at=0,side=1,labels=0,cex.axis=2.1)
axis(at=c(-150,-50),side=2,labels=paste("\U2212",c(150,50),sep=""),cex.axis=2.1)
axis(at=c(0,50),side=2,labels=c(0,50),cex.axis=2.1)
text(-12,52,'a)',cex=2.1)
minor.tick(ny=2,nx=2,tick.ratio=0.5)
points(neg$deltaT[which(neg$Z.L>0)],neg$H[which(neg$Z.L>0)],pch=19, col='blue')
abline(lm1,col='red',lwd=3)
abline(lm2,col='blue',lty=2,lwd=3)
axis(side=2,at=c(-100),labels=paste("\U2212",100,sep=""),cex.axis=2.1)

## Plotting H against deltaT for positive deltaT values
plot(pos$deltaT[which(pos$Z.L<0)],pos$H[which(pos$Z.L<0)],pch=17,col='red',
     xlab=expression(paste(Delta,'T')),ylab='',xlim=c(0,12),ylim=c(-80,250),cex.lab=2.1,cex.axis=2.1,yaxt='n')
axis(side=2,at=c(-50),labels=paste("\U2212",50,sep=""),cex.axis=2.1)
axis(side=2,at=c(0,50,250),labels=c(0,50,250),cex.axis=2.1)
axis(side=2,at=150,labels=150,cex.axis=2.1)
axis(side=2,at=50,labels=50,cex.axis=2.1)
text(0,245,'d)',cex=2.1)
minor.tick(ny=2,nx=2,tick.ratio=0.5)
points(pos$deltaT[which(pos$Z.L>0)],pos$H[which(pos$Z.L>0)],pch=19, col='blue')
abline(lm3,col='red',lwd=3)
abline(lm4,col='blue',lty=2,lwd=3)
axis(side=2,at=c(0),cex.axis=2.1)

## Plotting H against U for negative deltaT values
plot(neg$WS_Spd_WVT[which(neg$Z.L<0)],neg$H[which(neg$Z.L<0)],pch=17,col='red',
     xlab='U',ylab='H',xlim=c(0,14),ylim=c(-150,55),cex.lab=2.1,cex.axis=2.1,yaxt='n')
axis(at=c(-150,-50),side=2,labels=paste("\U2212",c(150,50),sep=""),cex.axis=2.1)
axis(at=c(0,50),side=2,labels=c(0,50),cex.axis=2.1)
text(0,52,'b)',cex=2.1)
minor.tick(ny=2,nx=2,tick.ratio=0.5)
points(neg$WS_Spd_WVT[which(neg$Z.L>=0)],neg$H[which(neg$Z.L>=0)],pch=19, col='blue')
abline(lm5,col='red',lwd=3)
abline(lm6,col='blue',lty=2,lwd=3)
axis(side=2,at=c(-100),labels=paste("\U2212",100,sep=""),cex.axis=2.1)

## Plotting H against U for positive deltaT values
plot(pos$WS_Spd_WVT[which(pos$Z.L<0)],pos$H[which(pos$Z.L<0)],pch=17,col='red',
     xlab='U',ylab='',ylim=c(-80,250),xlim=c(0,14),cex.lab=2.1,cex.axis=2.1,yaxt='n')
axis(side=2,at=c(-50),labels=paste("\U2212",50,sep=""),cex.axis=2.1)
axis(side=2,at=c(0,50,150,250),labels=c(0,50,150,250),cex.axis=2.1)
axis(side=2,at=150,labels=150,cex.axis=2.1)
axis(side=2,at=50,labels=50,cex.axis=2.1)
text(0,245,'e)',cex=2.1)
minor.tick(ny=2,nx=2,tick.ratio=0.5)
points(pos$WS_Spd_WVT[which(pos$Z.L>=0)],pos$H[which(pos$Z.L>=0)],pch=19,col='blue')
abline(lm7,col='red',lwd=3)
abline(lm8,col='blue',lty=2,lwd=3)
axis(side=2,at=c(0),cex.axis=2.1)

## Plotting H against u_deltaT for negative deltaT values
plot(neg$u_deltaT[which(neg$Z.L<0)],neg$H[which(neg$Z.L<0)],pch=17,col='red',
     xlab=expression(paste('U',Delta,'T')),ylab='H',xlim=c(-100,0),ylim=c(-150,55),cex.lab=2.1,cex.axis=2.1,
     xaxt='n',yaxt='n')
axis(at=seq(-100,-20,20),side=1,labels=paste("\U2212",seq(100,20,-20),sep=""),cex.axis=2.1)
axis(at=0,side=1,labels=0,cex.axis=2.1)
axis(at=c(-150,-50),side=2,labels=paste("\U2212",c(150,50),sep=""),cex.axis=2.1)
axis(at=c(0,50),side=2,labels=c(0,50),cex.axis=2.1)
text(-100,52,'c)',cex=2.1)
minor.tick(ny=2,nx=2,tick.ratio=0.5)
points(neg$u_deltaT[which(neg$Z.L>=0)],neg$H[which(neg$Z.L>=0)],pch=19, col='blue')
abline(lm9,col='red',lwd=3)
abline(lm10,col='blue',lty=2,lwd=3)
axis(side=2,at=c(-100),labels=paste("\U2212",100,sep=""),cex.axis=2.1)

## Plotting H against u_deltaT for positive deltaT values
plot(pos$u_deltaT[which(pos$Z.L<0)],pos$H[which(pos$Z.L<0)],pch=17,col='red',
     xlab=expression(paste('U',Delta,'T')),ylab='',ylim=c(-80,250),xlim=c(0,125),cex.lab=2.1,cex.axis=2.1,yaxt='n')
axis(side=2,at=c(-50),labels=paste("\U2212",50,sep=""),cex.axis=2.1)
axis(side=2,at=c(0,50,150,250),labels=c(0,50,150,250),cex.axis=2.1)
axis(side=2,at=150,labels=150,cex.axis=2.1)
axis(side=2,at=50,labels=50,cex.axis=2.1)
text(0,245,'f)',cex=2.1)
minor.tick(ny=2,nx=2,tick.ratio=0.5)
points(pos$u_deltaT[which(pos$Z.L>=0)],pos$H[which(pos$Z.L>=0)],pch=19, col='blue')
abline(lm11,col='red',lwd=3)
abline(lm12,col='blue',lty=2,lwd=3)
axis(side=2,at=c(0),cex.axis=2.1)
axis(side=1,at=c(120),cex.axis=2.1)

# To turn off plotting in a different window
dev.off()
## Cleaning up
rm(neg,pos,lm1,lm2,lm3,lm4,lm5,lm6,lm7,lm8,lm9,lm10,lm11,lm12)

##### Fig. 4: LE, deltaE, u, u_deltaE vs. atmospheric stability category ####

# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/fig_4.jpg')
jpeg(file=path_fig,width=5, height=10,res=360,units='in')
## Creating a new plot
plot.new()
# a) LE

LE <- data$LE
stability_no <- data$stability_no
d <- data.frame(LE,stability_no)
d$LE[which(d$stability_no==10)] <- -1000 # Create fake data to include category 10
rm(LE,stability_no)

plot1 <- ggplot(na.omit(data[,c('LE','stability_no')]), aes(factor(stability_no),LE)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/32),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='black',geom='text',label='---',size=7) + 
  labs(x="",y="LE") + theme_bw() + annotate("text",x=0.8,y=410,label="a)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(1,1,4,-2.5),"mm")) + 
  scale_y_continuous(breaks=seq(-100,400,by=100),labels=c(paste("\u2212",100,sep=""),0,100,200,300,400)) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) +
  coord_cartesian(ylim = c(-100, 450))
# b) delta E
plot2 <- ggplot(na.omit(data[,c('deltaE','stability_no')]), aes(factor(stability_no),deltaE)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/32),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='black',geom='text',label='---',size=7) + 
  labs(x='',y=expression(paste(Delta,'e'))) + theme_bw() + annotate("text",x=0.8,y=2.9,label="b)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times'),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-9,1,12,6),"mm")) + 
  scale_y_continuous(breaks=seq(-1,3,by=1)) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))
# c) U
plot3 <- ggplot(na.omit(data[,c('WS_Spd_WVT','stability_no')]), aes(factor(stability_no),WS_Spd_WVT)) + geom_boxplot(outlier.size=0,fill='white') + 
  geom_jitter(alpha=I(1/32),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='black',geom='text',label='---',size=7) + 
  labs(x='',y='U') + theme_bw() + annotate("text",x=0.8,y=13,label="c)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times'),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-17,1,27,3.2),"mm")) + 
  coord_cartesian(ylim = c(0, 14)) +
  scale_y_continuous(breaks=seq(0,14,by=5)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))  
# d) U delta E
plot4 <- ggplot(na.omit(data[,c('u_deltaE','stability_no')]), aes(factor(stability_no),u_deltaE)) + geom_boxplot(outlier.size=0,fill='white') + 
  geom_jitter(alpha=I(1/32),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='black',geom='text',label='---',size=7) + 
  labs(x='',y=expression(paste('U',Delta,'e'))) + theme_bw() + annotate("text",x=0.8,y=13.8,label="d)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.x=element_text(size=16,family='Times'),axis.title.y=element_text(size=16,family='Times'),
        axis.text.x=element_text(angle=90,size=16,hjust=1,family='Times',vjust=0.5),axis.text.y=element_text(size=16,family='Times'),
        plot.margin=unit(c(-31,1,7,2.7),"mm")) + 
  scale_y_continuous(breaks=seq(-5,15,by=5),labels=c(paste("\u2212",5,sep=""),0,5,10,15)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) +
  scale_x_discrete(labels=names_boxplot)
multiplot2(plot1,plot2,plot3,plot4,
           cols=1,labs=list("             ASL stability ranges",""))
dev.off()
rm(d)

#### Fig. 5 Arrows of LE with deltaE, U, UdeltaE###########
# Path where the plots will be saved
source("R/para_grouped_mean.R")
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/fig_5.jpg')
jpeg(file=path_fig,width=1346, height=3600,res=360)
## Creating 6 panels of plots
plot.new()
par(mfrow=c(3,1),mar=c(4.1,4.6,1,1.1),family='Times')

# a) LE and deltaE
plot(d_mean$deltaE_mean,d_mean$LE_mean,type='l',xlim=c(0,1),ylim=c(0,120),
     lwd=2,xlab=expression(paste(Delta,'e')),ylab='LE',cex.axis=2.2,cex.lab=2.2,
     yaxt='n')
mid1 <- midpoint(d_mean$deltaE_mean[5],d_mean$LE_mean[5],d_mean$deltaE_mean[6],
                 d_mean$LE_mean[6],ratio1=2,ratio2=3)
mid2 <- midpoint(d_mean$deltaE_mean[6],d_mean$LE_mean[6],d_mean$deltaE_mean[5],
                 d_mean$LE_mean[5],ratio1=2,ratio2=3)
arrows(x0=d_mean$deltaE_mean[5],y0=d_mean$LE_mean[5],x1=mid1[1],y1=mid1[2],
       lwd=3,length=0.15)
arrows(x0=d_mean$deltaE_mean[6],y0=d_mean$LE_mean[6],x1=mid2[1],y1=mid2[2],
       lwd=3,length=0.15)
points(d_mean$deltaE_mean,d_mean$LE_mean,pch=19,cex=2)
axis(side=2,at=c(0,50,100),cex.axis=2.2)
text(0.011,118,'a)',cex=2.2)
text(0.3,30,'stable',cex=2)
text(0.48,60,'near-neutral',cex=2)
text(0.82,110,'unstable',cex=2)

# b) LE and U
plot(d_mean$WS_mean,d_mean$LE_mean,type='l',xlim=c(0,6),ylim=c(0,120),
     lwd=2,xlab='U',ylab='LE',cex.axis=2.2,cex.lab=2.2,
     yaxt='n')
mid1 <- midpoint(d_mean$WS_mean[5],d_mean$LE_mean[5],d_mean$WS_mean[6],
                 d_mean$LE_mean[6],ratio1=2,ratio2=3)
mid2 <- midpoint(d_mean$WS_mean[6],d_mean$LE_mean[6],d_mean$WS_mean[5],
                 d_mean$LE_mean[5],ratio1=2,ratio2=3)
arrows(x0=d_mean$WS_mean[5],y0=d_mean$LE_mean[5],x1=mid1[1],y1=mid1[2],
       lwd=3,length=0.15)
arrows(x0=d_mean$WS_mean[6],y0=d_mean$LE_mean[6],x1=mid2[1],y1=mid2[2],
       lwd=3,length=0.15)
points(d_mean$WS_mean,d_mean$LE_mean,pch=19,cex=2)
axis(side=2,at=c(0,50,100),cex.axis=2.2)
text(0.1,118,'b)',cex=2.2)
text(3.8,10,'stable',cex=2)
text(3.4,60,'near-neutral',cex=2)
text(4.5,105,'unstable',cex=2)

# c) LE and UdeltaE
plot(d_mean$udeltaE_mean,d_mean$LE_mean,type='l',xlim=c(0,5),ylim=c(0,120),
     lwd=2,xlab=expression(paste('U',Delta,'e')),ylab='LE',cex.axis=2.2,cex.lab=2.2,
     yaxt='n')
mid1 <- midpoint(d_mean$udeltaE_mean[5],d_mean$LE_mean[5],d_mean$udeltaE_mean[6],
                 d_mean$LE_mean[6],ratio1=2,ratio2=3)
mid2 <- midpoint(d_mean$udeltaE_mean[6],d_mean$LE_mean[6],d_mean$udeltaE_mean[5],
                 d_mean$LE_mean[5],ratio1=2,ratio2=3)
arrows(x0=d_mean$udeltaE_mean[5],y0=d_mean$LE_mean[5],x1=mid1[1],y1=mid1[2],
       lwd=3,length=0.15)
arrows(x0=d_mean$udeltaE_mean[6],y0=d_mean$LE_mean[6],x1=mid2[1],y1=mid2[2],
       lwd=3,length=0.15)
points(d_mean$udeltaE_mean,d_mean$LE_mean,pch=19,cex=2)
axis(side=2,at=c(0,50,100),cex.axis=2.2)
text(0.1,118,'c)',cex=2.2)
text(2,10,'stable',cex=2)
text(4,55,'near-neutral',cex=2)
text(2.5,100,'unstable',cex=2)

# To turn off plotting in a different window
dev.off()
## Cleaning up
rm(mid1,mid2)

##### Fig. 6: Arrows U and u* and u* for different ASL stability ranges ####

# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/fig_6.jpg')
jpeg(file=path_fig,width=5,height=10,res=360,units='in')

#### First base graphic plot

par(mfrow=c(2,1),mar=c(4.1,4.6,1,1.1),family='Times')

#### a) First ggplot plot
plot.new()
# To combine ggplot and base graphics
vps <- baseViewports()
pushViewport(vps$figure)
vp1 <- plotViewport(margins=c(-0.5,0.5,-0.5,0))

plot5 <- ggplot(na.omit(data[,c('U.','stability_no')]), aes(factor(stability_no),U.)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/8),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='black',geom='text',label='---',size=9) + 
  labs(x="  ASL stability ranges",y=expression("u"["*"])) + theme_bw() +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=18,family='Times'),axis.title.x=element_text(size=18,family='Times'),
        axis.text.y=element_text(size=18,family='Times'),axis.text.x=element_text(angle=90,size=18,hjust=1,family='Times',vjust=0.5)) + 
  scale_x_discrete(labels=names_boxplot) +
  scale_y_continuous(breaks=seq(0,0.7,by=0.1)) + annotate("text",x=0.8,y=0.7,label="a)",family='Times',size=7) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) 
print(plot5,vp = vp1)

#### b) Second base graphic plot

plot(d_mean$WS_mean,d_mean$ustar_mean,type='l',xlim=c(0,6),ylim=c(0,0.25),
     lwd=2,xlab='U',ylab=expression('u'['*']),cex.axis=1.5,cex.lab=1.5,
     yaxt='n',las=1,tck=-0.015)
mid1 <- midpoint(d_mean$WS_mean[3],d_mean$ustar_mean[3],d_mean$WS_mean[4],
                 d_mean$ustar_mean[4],ratio1=2,ratio2=3)
mid2 <- midpoint(d_mean$WS_mean[7],d_mean$ustar_mean[7],d_mean$WS_mean[6],
                 d_mean$ustar_mean[6],ratio1=2,ratio2=3)
arrows(x0=d_mean$WS_mean[3],y0=d_mean$ustar_mean[3],x1=mid1[1],y1=mid1[2],
       lwd=3,length=0.15)
arrows(x0=d_mean$WS_mean[7],y0=d_mean$ustar_mean[7],x1=mid2[1],y1=mid2[2],
       lwd=3,length=0.15)
points(d_mean$WS_mean,d_mean$ustar_mean,pch=19,cex=2)
axis(side=2,tck=-0.015,labels=NA)
axis(side=2,at=c(0,0.05,0.1,0.15,0.2,0.25),cex.axis=1.5,las=1,lwd=0,line=-0.5)
text(0,0.25,'b)',cex=1.5)
text(3.8,0.1,'stable',cex=1.2)
text(5,0.24,'near-neutral',cex=1.2)
text(2.5,0.15,'unstable',cex=1.2)

dev.off()
rm(mid1,mid2,plot5,vp1,vps)

##### Fig. 7: r2 for LE ####

# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/fig_7.jpg')
jpeg(file=path_fig,width=5, height=10,res=360,units='in')
## Creating a new plot
plot.new()
#regression R^2 results plot between LE and U
plot1 <- ggplot(data=data_rsq,aes(x=cat_no,y=r_LE_U)) +
  geom_bar(stat='identity',fill='white',color='black',position='identity') +
  xlab('') + ylab(expression(paste('R'^'2'))) + ylim(-0.05,1.0) +
  theme(axis.title.y=element_text(size=16,family='Times'),
        axis.text.x=element_blank(), axis.text.y=element_text(color='black',size=16,family='Times'),
        panel.background=element_rect(fill='white',color='black'),
        plot.margin=unit(c(1,2,8,0),"mm")) +
  annotate('text', x = 0.8, y = 1, label='a)',family='Times',size=7) # LE and U')

#regression R^2 results plot between LE and deltaE
plot2 <- ggplot(data=data_rsq,aes(x=cat_no,y=r_LE_dE)) +
  geom_bar(stat='identity',fill='white',color='black',position='identity') +
  xlab('') + ylab(expression(paste('R'^'2'))) + ylim(-0.05,1.0) +
  theme(axis.title.y=element_text(size=16,family='Times'),
        axis.text.x=element_blank(), axis.text.y=element_text(size=16,color='black',family='Times'),
        panel.background=element_rect(fill='white',color='black'),
        plot.margin=unit(c(-14,2,22,0),"mm")) +
  annotate('text', x = 0.8, y = 1, label= 'b)',family='Times',size=7) #expression(paste('b) LE and ',Delta,'e')))

#regression R^2 results plot between LE and deltaE
plot3 <- ggplot(data=data_rsq,aes(x=cat_no,y=r_LE_UdE)) +
  geom_bar(stat='identity',fill='white',color='black',position='identity') +
  xlab('') + ylab(expression(paste('R'^'2'))) + ylim(-0.05,1.0) +
  theme(axis.title.y=element_text(size=16,family='Times'),
        axis.text.x=element_text(angle=90,size=16,hjust=1,color ='black',family='Times',vjust=0.5), axis.text.y=element_text(size=16,color='black',family='Times'),
        panel.background=element_rect(fill='white',color='black'),
        plot.margin=unit(c(-28,2,10,0),"mm")) +
  scale_x_discrete(labels = names_boxplot) +
  annotate('text',x=0.8,y=1,label= 'c)',family='Times',size=7) #expression(paste('c) LE and ','U',Delta,'e')))

#Plotting all three above together in one plot
multiplot2(plot1,plot2,plot3,cols=1,labs=list('             ASL stability ranges',''))
dev.off()

##### Fig. 4 (prev. 8): H, deltaT, u, u_deltaT versus atmospheric stability category ####

# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/fig_8.jpg')
jpeg(file=path_fig,width=5, height=10,res=360,units='in')

## Creating a new plot
plot.new()
# a) H
plot1 <- ggplot(na.omit(data[,c('H','stability_no')]), aes(factor(stability_no),H)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='black',geom='text',label='---',size=9) + 
  labs(x="",y="H") + theme_bw() + annotate("text",x=0.8,y=74,label="e)",family='Times',size=7) +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times'),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(1,1,4,2),"mm")) + 
  scale_y_continuous(breaks=seq(-50,80,by=50),labels=c(paste("\u2212",50,sep=""),0,50)) + 
  coord_cartesian(ylim = c(-50, 80)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

# b) delta T
plot2 <- ggplot(na.omit(data[,c('deltaT','stability_no')]), aes(factor(stability_no),deltaT)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/32),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='black',geom='text',label='---',size=7) + 
  labs(x='',y=expression(paste(Delta,'T'))) + theme_bw() + annotate("text",x=0.8,y=11.3,label="f)",family='Times',size=7) +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times'),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-9,1,12,1.5),"mm")) + 
  scale_y_continuous(breaks=seq(-10,10,by=5),labels=c(paste("\u2212",10,sep=""),paste("\u2212",5,sep=""),0,5,10)) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

# c) U
plot3 <- ggplot(na.omit(data[,c('WS_Spd_WVT','stability_no')]), aes(factor(stability_no),WS_Spd_WVT)) + geom_boxplot(outlier.size=0,fill='white') + 
  geom_jitter(alpha=I(1/32),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='black',geom='text',label='---',size=7) + 
  labs(x='',y='U') + theme_bw() + annotate("text",x=0.8,y=13,label="g)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times'),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-17,1,26,4.7),"mm")) +
  coord_cartesian(ylim = c(0, 14)) +
  scale_y_continuous(breaks=seq(0,14,by=5)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))  

# d) U delta T
plot4 <- ggplot(na.omit(data[,c('u_deltaT','stability_no')]), aes(factor(stability_no),u_deltaT)) + geom_boxplot(outlier.size=0,fill='white') + 
  geom_jitter(alpha=I(1/32),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='black',geom='text',label='---',size=7) + 
  labs(x='',y=expression(paste('U',Delta,'T'))) + theme_bw() + annotate("text",x=0.8,y=70,label="h)",family='Times',size=7) +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.x=element_text(size=16,family='Times'),axis.title.y=element_text(size=16,family='Times'),
        axis.text.x=element_text(angle=90,size=16,hjust=1,family='Times',vjust=0.5),axis.text.y=element_text(size=16,family='Times'),
        plot.margin=unit(c(-31,1,7,1.5),"mm")) + 
  coord_cartesian(ylim = c(-50, 80)) +
  scale_y_continuous(breaks=seq(-50,80,by=50),labels=c(paste("\u2212",50,sep=""),0,50)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) +
  scale_x_discrete(labels=names_boxplot)

multiplot2(plot1,plot2,plot3,plot4,
           cols=1,labs=list("            ASL stability ranges",""))
dev.off()

##### Fig. 5 (prev. 9): Arrows for H #################
# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/fig_9.jpg')
jpeg(file=path_fig,width=1346, height=3600,res=360)
## Creating 6 panels of plots
plot.new()
par(mfrow=c(3,1),mar=c(4.1,4.6,1,1.1),family='Times')

# H and deltaT
plot(d_mean$deltaT_mean,d_mean$H_mean,type='l',xlim=c(-4,4),ylim=c(-20,50),
     lwd=2,xlab=expression(paste(Delta,'T')),ylab='H',cex.axis=2.2,cex.lab=2.2,
     yaxt='n',xaxt='n')
mid1 <- midpoint(d_mean$deltaT_mean[5],d_mean$H_mean[5],d_mean$deltaT_mean[6],
                 d_mean$H_mean[6],ratio1=2,ratio2=3)
mid2 <- midpoint(d_mean$deltaT_mean[6],d_mean$H_mean[6],d_mean$deltaT_mean[5],
                 d_mean$H_mean[5],ratio1=2,ratio2=3)
arrows(x0=d_mean$deltaT_mean[5],y0=d_mean$H_mean[5],x1=mid1[1],y1=mid1[2],
       lwd=3,length=0.15)
arrows(x0=d_mean$deltaT_mean[6],y0=d_mean$H_mean[6],x1=mid2[1],y1=mid2[2],
       lwd=3,length=0.15)
points(d_mean$deltaT_mean,d_mean$H_mean,pch=19,cex=2)
axis(side=2,at=c(-20,0,20,40),cex.axis=2.2,labels=c(paste("\u2212",20,sep=""),0,20,40))
axis(side=1,at=c(-4,-2,0,2,4),cex.axis=2.2,labels=c(paste("\u2212",4,sep=""),paste("\u2212",2,sep=""),0,2,4))
text(-3.9,49,'d)',cex=2.2)
text(-3,-20,'stable',cex=2)
text(1,0,'near-neutral',cex=2)
text(3,35,'unstable',cex=2)

# H and U
plot(d_mean$WS_mean,d_mean$H_mean,type='l',xlim=c(0,6),ylim=c(-20,50),
     lwd=2,xlab='U',ylab='H',cex.axis=2.2,cex.lab=2.2,
     yaxt='n')
mid1 <- midpoint(d_mean$WS_mean[5],d_mean$H_mean[5],d_mean$WS_mean[6],
                 d_mean$H_mean[6],ratio1=2,ratio2=3)
mid2 <- midpoint(d_mean$WS_mean[6],d_mean$H_mean[6],d_mean$WS_mean[5],
                 d_mean$H_mean[5],ratio1=2,ratio2=3)
arrows(x0=d_mean$WS_mean[5],y0=d_mean$H_mean[5],x1=mid1[1],y1=mid1[2],
       lwd=3,length=0.15)
arrows(x0=d_mean$WS_mean[6],y0=d_mean$H_mean[6],x1=mid2[1],y1=mid2[2],
       lwd=3,length=0.15)
points(d_mean$WS_mean,d_mean$H_mean,pch=19,cex=2)
axis(side=2,at=c(-20,0,20,40),cex.axis=2.2,labels=c(paste("\u2212",20,sep=""),0,20,40))
text(0.05,49,'e)',cex=2.2)
text(5,-15,'stable',cex=2)
text(3.4,5,'near-neutral',cex=2)
text(4,35,'unstable',cex=2)

# H and UdeltaT
plot(d_mean$udeltaT_mean,d_mean$H_mean,type='l',xlim=c(-20,20),ylim=c(-20,50),
     lwd=2,xlab=expression(paste('U',Delta,'T')),ylab='H',cex.axis=2.2,cex.lab=2.2,
     yaxt='n',xaxt='n')
lines(c(8.5,8.5),c(12,33),lty=2,lwd=2)
text(6.5,30,'A',cex=2)
text(10.5,15,'B',cex=2)
mid1 <- midpoint(d_mean$udeltaT_mean[5],d_mean$H_mean[5],d_mean$udeltaT_mean[6],
                 d_mean$H_mean[6],ratio1=2,ratio2=3)
mid2 <- midpoint(d_mean$udeltaT_mean[6],d_mean$H_mean[6],d_mean$udeltaT_mean[5],
                 d_mean$H_mean[5],ratio1=2,ratio2=3)
arrows(x0=d_mean$udeltaT_mean[5],y0=d_mean$H_mean[5],x1=mid1[1],y1=mid1[2],
       lwd=3,length=0.15)
arrows(x0=d_mean$udeltaT_mean[6],y0=d_mean$H_mean[6],x1=mid2[1],y1=mid2[2],
       lwd=3,length=0.15)
points(d_mean$udeltaT_mean,d_mean$H_mean,pch=19,cex=2)
axis(side=2,at=c(-20,0,20,40),cex.axis=2.2,labels=c(paste("\u2212",20,sep=""),0,20,40))
axis(side=1,at=c(-20,-10,0,10,20),cex.axis=2.2,labels=c(paste("\u2212",20,sep=""),paste("\u2212",10,sep=""),0,10,20))
text(-20,49,'f)',cex=2.2)
text(-5,-15,'stable',cex=2)
text(6,0,'near-neutral',cex=2)
text(15,35,'unstable',cex=2)

# To turn off plotting in a different window
dev.off()
## Cleaning up
rm(mid1,mid2)

##### Fig. 7 (prev. 10): r2 for H ############
source("R/r_sq_H.R")
# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/fig_10.jpg')
jpeg(file=path_fig,width=5, height=10,res=360,units='in')
## Creating a new plot
plot.new()
#regression R^2 results plot between LE and U
plot1 <- ggplot(data=data_rsq2,aes(x=cat_no,y=r_H_U)) +
  geom_bar(stat='identity',fill='white',color='black',position='identity') +
  xlab('') + ylab('') + ylim(-0.05,1.0) +
  theme(axis.title.y=element_text(size=16,family='Times'),
        axis.text.x=element_blank(), axis.text.y=element_text(color='black',size=16,family='Times'),
        panel.background=element_rect(fill='white',color='black'),
        plot.margin=unit(c(1,2,8,0),"mm")) +
  annotate('text', x = 0.8, y = 1, label='d)',family='Times',size=7) # LE and U')

#regression R^2 results plot between LE and deltaE
plot2 <- ggplot(data=data_rsq2,aes(x=cat_no,y=r_H_dT)) +
  geom_bar(stat='identity',fill='white',color='black',position='identity') +
  xlab('') + ylab('') + ylim(-0.05,1.0) +
  theme(axis.title.y=element_text(size=16,family='Times'),
        axis.text.x=element_blank(), axis.text.y=element_text(size=16,color='black',family='Times'),
        panel.background=element_rect(fill='white',color='black'),
        plot.margin=unit(c(-14,2,22,0),"mm")) +
  annotate('text', x = 0.8, y = 1, label= 'e)',family='Times',size=7) #expression(paste('b) LE and ',Delta,'e')))

#regression R^2 results plot between LE and deltaE
plot3 <- ggplot(data=data_rsq2,aes(x=cat_no,y=r_H_UdT)) +
  geom_bar(stat='identity',fill='white',color='black',position='identity') +
  xlab('') + ylab('') + ylim(-0.05,1.0) +
  theme(axis.title.y=element_text(size=16,family='Times'),
        axis.text.x=element_text(angle=90,size=16,hjust=1,color ='black',family='Times',vjust=0.5), axis.text.y=element_text(size=16,color='black',family='Times'),
        panel.background=element_rect(fill='white',color='black'),
        plot.margin=unit(c(-28,2,10,0),"mm")) +
  scale_x_discrete(labels = names_boxplot) +
  annotate('text',x=0.8,y=1,label= 'f)',family='Times',size=7) #expression(paste('c) LE and ','U',Delta,'e')))

#Plotting all three above together in one plot
multiplot2(plot1,plot2,plot3,cols=1,labs=list('              ASL stability ranges',''))
dev.off()

##### Fig. 8 (prev. 11): Time series Sept 2007 ####

path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/fig_11.jpg')
jpeg(file=path_fig,width=5,height=10,res=360,units='in')
## Creating 5 panels of plots
plot.new()
par(family='Times',mfrow=c(5,1),oma=c(5.1,0.1,0.1,0.1))

# a) LE and H
par(mai=c(0,0.6,0.1,0.1))
plot(data_group$hour[data_group$month == '09'],
     data_group$LE[data_group$month == '09'],
     ylab='LE and H',xlab='',type='l',ylim=c(0,200),lwd=2,xaxt='n',yaxt='n',cex.lab=2.2)
lines(data_group$hour[data_group$month == '09'],
      data_group$H[data_group$month == '09'],lty=2,lwd=2)
text(0,196,'a)',cex=2.2)
legend(17.5,220,y.intersp=0.8,bty='n',lty=c(1,2),lwd=c(2,2),c('LE','H'),cex=2)
axis(side=2,at=c(0,100,200),cex.axis=2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

# b) Vapor pressure (kPa)
par(mai=c(0,0.6,0.3,0.1))
plot(data_group$hour[data_group$month == '09'],
     data_group$es[data_group$month == '09'],
     ylab='e',xlab='',type='l',lwd=2,ylim=c(0,6),xaxt='n',yaxt='n',cex.lab=2.2,lty=2)
axis(side=2,at=c(0,2,4,6),cex.axis=2)
lines(data_group$hour[data_group$month == '09'],
      data_group$ea[data_group$month == '09'],
      ylab=expression('e'['a']),type='l',lty=1,lwd=2)
lines(data_group$hour[data_group$month == '09'],
      data_group$deltaE[data_group$month == '09'],
      ylab=expression(paste(Delta,'e')),type='l',lty=3,lwd=2)
text(0,5.8,'b)',cex=2)
legend(17.5,6.5,y.intersp=0.6,bty='n',lty=c(2,1,3),lwd=c(2,2,2),
       c(expression('e'['s']),expression('e'['a']),expression(paste(Delta,'e'))),cex=2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

# c) zeta
par(mai=c(0.1,0.6,0.25,0.1))
plot(data_group$hour[data_group$month == '09'],
     data_group$Z.L[data_group$month == '09'],
     ylab=expression(zeta),xlab='',type='l',lwd=2,ylim=c(-1,0.5),xaxt ='n',yaxt='n',cex.lab=2,cex.axis=2)
axis(side=2,at=c(-1,0,0.5),labels=c(paste("\u2212",1,sep=""),0,0.5),cex.axis=2)
axis(side=2,at=c(-0.5),labels=c(paste("\u2212",0.5,sep="")),cex.axis=2)
text(0,0.45,'c)',cex=2)
minor.tick(ny=5,nx=5,tick.ratio=0.5)

# d) Temperature
par(mai=c(0,0.6,0.2,0.1))
plot(data_group$hour[data_group$month == '09'],
     data_group$Ta[data_group$month == '09'],
     ylab='T',xlab='',type='l',ylim=c(20,40),lwd=2,xaxt='n',yaxt='n',cex.lab=2.2)
axis(side=2,at=c(20,25,30,35,40),cex.axis=2)
lines(data_group$hour[data_group$month == '09'],
      data_group$water_temp[data_group$month == '09'],ylab='T',type='l',lty=2,lwd=2)
legend(17.5,42,y.intersp=1,bty='n',lty=c(1,2),lwd=c(2,2),c(expression('T'['a']),expression('T'['s'])),cex=2)
minor.tick(ny=5,nx=5,tick.ratio=0.5)
text(0,39,'d)',cex=2)

# e) Wind speed
par(mai=c(0,0.6,0.3,0.1))
plot(data_group$hour[data_group$month == '09'],
     data_group$WS[data_group$month == '09'],
     ylab='U',xlab='',type='l',lwd=2,ylim=c(2,5),
     xaxt='n',cex.lab=2,cex.axis=2)
text(0,4.9,'e)',cex=2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=2)

title(xlab='Hour (local time)',ylab='',outer=TRUE,cex.lab=2)

rm(path_fig)

dev.off()

##### Fig. 9 (prev. 12): LE/U and LE/deltaE for different ASL stability ranges ####

# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/fig_12.jpg')
jpeg(file=path_fig,width=5,height=10,res=360,units='in')
## Creating a new plot
plot.new()
plot1 <- ggplot(na.omit(data[,c('LE_U','stability_no')]), aes(factor(stability_no),LE_U)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/8),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='black',geom='text',label='---',size=9) + 
  labs(x="",y='LE/U') + theme_bw() +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times'),axis.title.x=element_text(size=16,family='Times'),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_text(size=16,family='Times'),
        plot.margin=unit(c(1,1,12,2.5),"mm")) + 
  scale_x_discrete(labels='') +
  coord_cartesian(ylim = c(-20, 100)) +
  scale_y_continuous(breaks=seq(-20,100,by=20),labels=c(paste("\u2212",20,sep=""),0,20,40,60,80,100)) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) +
  annotate("text",x=0.8,y=100,label="a)",family='Times',size=7)

plot2 <- ggplot(na.omit(data[,c('LE_deltaE','stability_no')]), aes(factor(stability_no),LE_deltaE)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/8),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='black',geom='text',label='---',size=9) + 
  labs(x="",y=expression(paste('LE/',Delta,'e'))) + theme_bw() +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times'),axis.title.x=element_text(size=16,family='Times'),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_text(angle=90,size=16,hjust=1,family='Times',vjust=0.5),
        plot.margin=unit(c(-20,1,10,-0.5),"mm")) + 
  scale_x_discrete(labels=names_boxplot) +
  coord_cartesian(ylim = c(-100, 300)) +
  scale_y_continuous(breaks=seq(-100,300,by=100),labels=c(paste("\u2212",100,sep=""),0,100,200,300)) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) +
  annotate("text",x=0.8,y=300,label="b)",family='Times',size=7)

multiplot2(plot1,plot2,cols=1,labs=list('              ASL stability ranges',''))
dev.off()

##### Fig. 9 (prev. 13): H/U and H/deltaE for different ASL stability ranges ####

# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/fig_13.jpg')
jpeg(file=path_fig,width=5,height=10,res=360,units='in')
## Creating a new plot
plot.new()
plot1 <- ggplot(na.omit(data[,c('H_U','stability_no')]), aes(factor(stability_no),H_U)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='black',geom='text',label='---',size=9) + 
  labs(x="",y='H/U') + theme_bw() +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times'),axis.title.x=element_text(size=16,family='Times'),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_text(size=16,family='Times'),
        plot.margin=unit(c(1,1,12,0.5),"mm")) + 
  scale_x_discrete(labels='') +
  coord_cartesian(ylim = c(-10, 40)) +
  scale_y_continuous(breaks=seq(-10,40,by=10),labels=c(paste("\u2212",10,sep=""),0,10,20,30,40)) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) +
  annotate("text",x=0.80,y=40,label="c)",family='Times',size=7)

plot2 <- ggplot(na.omit(data[,c('H_deltaT','stability_no')]), aes(factor(stability_no),H_deltaT)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='black',geom='text',label='---',size=9) + 
  labs(x="",y=expression(paste('H/',Delta,'T'))) + theme_bw() +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times'),axis.title.x=element_text(size=16,family='Times'),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_text(angle=90,size=16,hjust=1,family='Times',vjust=0.5),
        plot.margin=unit(c(-20,1,10,0),"mm")) + 
  scale_x_discrete(labels=names_boxplot) +
  coord_cartesian(ylim = c(-20, 30)) +
  scale_y_continuous(breaks=seq(-20,30,by=10),labels=c(paste("\u2212",20,sep=""),paste("\u2212",10,sep=""),0,10,20,30)) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) +
  annotate("text",x=0.80,y=30,label="d)",family='Times',size=7)

multiplot2(plot1,plot2,cols=1,labs=list('              ASL stability ranges',''))
dev.off()

##### Fig. 10 (prev. 14): CE and CH under different ASL stability ranges ####

# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/fig_14.jpg')
jpeg(file=path_fig,width=5,height=10,res=360,units='in')
## Creating a new plot
plot.new()

plot1 <- ggplot(na.omit(data[,c('C_E','stability_no')]), aes(factor(stability_no),C_E)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/8),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='black',geom='text',label='---',size=8) + 
  labs(x="",y=expression("C"["E"])) + theme_bw() +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times'),axis.title.x=element_text(size=16,family='Times'),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_text(angle=90,size=16,hjust=1,family='Times',vjust=0.5),
        plot.margin=unit(c(1,1,5,1),"mm")) + 
  scale_x_discrete(labels=names_boxplot) +
  coord_cartesian(ylim = c(-0.001, 0.005)) +
  scale_y_continuous(breaks=seq(-0.001,0.005,by=0.0010),labels=c(paste("\u2212",0.001,sep=""),0.000,0.001,0.002,0.003,0.004,0.005)) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) +
  annotate("text",x=0.80,y=0.005,label="a)",family='Times',size=7)

plot2 <- ggplot(na.omit(data[,c('C_H','stability_no')]), aes(factor(stability_no),C_H)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/8),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='black',geom='text',label='---',size=8) + 
  labs(x="",y=expression("C"["H"])) + theme_bw() +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times'),axis.title.x=element_text(size=16,family='Times'),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_text(angle=90,size=16,hjust=1,family='Times',vjust=0.5),
        plot.margin=unit(c(-8,1,10,0.5),"mm")) + 
  scale_x_discrete(labels=names_boxplot) +
  coord_cartesian(ylim = c(-0.003, 0.005)) +
  scale_y_continuous(breaks=seq(-0.003,0.005,by=0.002),labels=c(paste("\u2212",0.003,sep=""),paste("\u2212",0.001,sep=""),0.001,0.003,0.005)) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) +
  annotate("text",x=0.80,y=0.005,label="b)",family='Times',size=7)

multiplot2(plot1,plot2,cols=1,labs=list('                 ASL stability ranges',''))
dev.off()

####  Fig. 11 (prev. 15): CE and CH against U under unstable, near-neutral, and stable conditions  ####
# Categorize data into near neutral conditions (-0.05 < z/L < 0.05)
near_neutral <- which(data$Z.L > -0.05 & data$Z.L < 0.05)
# Categorize data into unstable conditions (-0.5 < z/L < -0.1)
unstable <- which(data$Z.L > -0.5 & data$Z.L < -0.1)
# Categorize data into near stable conditions (0.1 < z/L < 0.5)
stable <- which(data$Z.L > 0.1 & data$Z.L < 0.5)
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/fig_15.jpg')
jpeg(file=path_fig,width=3060, height=3600,res=360)

plot.new()
# Plots of near neutral,stable,unstable conditions
par(mfrow=c(3,2),mar=c(4.1,4.8,2.1,1.1),family='Times')
# Unstable (-0.5 < z/L < -0.1)
plot(data$WS_Spd_WVT[unstable],data$C_E[unstable],
     xlab='',ylab=expression('C'['E']),pch=19,col='red',xlim=c(-1,15),cex.lab=2.2,ylim=c(-0.005,0.01),cex.axis=2.2,xaxt='n',yaxt='n')
axis(at=c(-0.005,0.000,0.005,0.010),side=2,labels=c(paste("\u2212",0.005,sep=""),0.000,0.005,0.010),cex.axis=2.2)
lines(c(-5:20),rep(0.0011,26),lty=2,lwd=2)
lines(rep(4,26),seq(-0.010,0.015,by=0.001),lwd=2)
text(-0.9,0.0097,'a)',cex=2.2)
minor.tick(ny=5,nx=5,tick.ratio=0.5)

plot(data$WS_Spd_WVT[unstable],data$C_H[unstable],
     xlab='',ylab=expression('C'['H']),pch=19,col='red',xlim=c(-1,15),cex.lab=2.2,ylim=c(-0.005,0.01),cex.axis=2.2,xaxt='n',yaxt='n')
axis(at=c(-0.005,0.000,0.005,0.010),side=2,labels=c(paste("\u2212",0.005,sep=""),0.000,0.005,0.010),cex.axis=2.2)
lines(c(-5:20),rep(0.0011,26),lty=2,lwd=2)
lines(rep(4,26),seq(-0.010,0.015,by=0.001),lwd=2)
text(-0.9,0.0097,'d)',cex=2.2)
minor.tick(ny=5,nx=5,tick.ratio=0.5)

# Near-neutral (-0.05 < z/L < 0.05)
plot(data$WS_Spd_WVT[near_neutral],data$C_E[near_neutral],
     xlab='',ylab=expression('C'['E']),pch=19,xlim=c(-1,15),cex.lab=2.2,ylim=c(-0.005,0.01),cex.axis=2.2,xaxt='n',yaxt='n')
axis(at=c(-0.005,0.000,0.005,0.010),side=2,labels=c(paste("\u2212",0.005,sep=""),0.000,0.005,0.010),cex.axis=2.2)
lines(c(-5:20),rep(0.0011,26),lty=2,lwd=2)
lines(rep(4,26),seq(-0.010,0.015,by=0.001),lwd=2)
text(-0.9,0.0097,'b)',cex=2.2)
minor.tick(ny=5,nx=5,tick.ratio=0.5)

plot(data$WS_Spd_WVT[near_neutral],data$C_H[near_neutral],
     xlab='',ylab=expression('C'['H']),pch=19,xlim=c(-1,15),cex.lab=2.2,ylim=c(-0.005,0.01),cex.axis=2.2,xaxt='n',yaxt='n')
axis(at=c(-0.005,0.000,0.005,0.010),side=2,labels=c(paste("\u2212",0.005,sep=""),0.000,0.005,0.010),cex.axis=2.2)
lines(c(-5:20),rep(0.0011,26),lty=2,lwd=2)
lines(rep(4,26),seq(-0.010,0.015,by=0.001),lwd=2)
text(-0.9,0.0097,'e)',cex=2.2)
minor.tick(ny=5,nx=5,tick.ratio=0.5)

# Stable (0.1 < z/L < 0.5)
plot(data$WS_Spd_WVT[stable],data$C_E[stable],
     xlab='U',ylab=expression('C'['E']),pch=19,col='blue',xlim=c(-1,15),cex.lab=2.2,ylim=c(-0.005,0.01),cex.axis=2.2,yaxt='n')
axis(at=c(-0.005,0.000,0.005,0.010),side=2,labels=c(paste("\u2212",0.005,sep=""),0.000,0.005,0.010),cex.axis=2.2)
lines(c(-5:20),rep(0.0011,26),lty=2,lwd=2)
lines(rep(4,26),seq(-0.010,0.015,by=0.001),lwd=2)
text(-0.9,0.0097,'c)',cex=2.2)
minor.tick(ny=5,nx=5,tick.ratio=0.5)

plot(data$WS_Spd_WVT[stable],data$C_H[stable],
     xlab='U',ylab=expression('C'['H']),pch=19,col='blue',xlim=c(-1,15),cex.lab=2.2,ylim=c(-0.005,0.01),cex.axis=2.2,yaxt='n')
axis(at=c(-0.005,0.000,0.005,0.010),side=2,labels=c(paste("\u2212",0.005,sep=""),0.000,0.005,0.010),cex.axis=2.2)
lines(c(-5:20),rep(0.0011,26),lty=2,lwd=2)
lines(rep(4,26),seq(-0.010,0.015,by=0.001),lwd=2)
text(-0.9,0.0097,'f)',cex=2.2)
minor.tick(ny=5,nx=5,tick.ratio=0.5)

dev.off()

#rm(stable,unstable,near_neutral)

#### 6. Cleaning up ####

# Deleting original margin settings for plots
rm(path_fig,plot1,plot2,plot3,plot4,plot5,plot6,names_boxplot,i)
rm(old.par)
rm(d_mean,d_mean_cor,data_group,data_rsq,data_rsq2,cat_no,day,near_neutral,stable,unstable)

