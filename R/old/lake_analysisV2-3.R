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
# Date modified: 2014-11-09
# Version: 2.3

##### 0. Preliminaries ############################################################

# Installing packages or sourcing custom functions
#install.packages('Hmisc')
#install.packages('ggplot2')
#install.packages('dplyr')

# To data convert factor to numeric

library(Hmisc) # To use cut2 and others
library(ggplot2) # For plots
library(dplyr) # To group data into hours, months, years, seasons

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

##### 1. Data import and clean up #################################################

# Read data from original csv file
data <- read.csv('data/lake_data.csv')
# Remove the first four columns because they are empty rows or non-data rows
data <- data[c(-1,-2,-3,-4),]
# Remove row.names
row.names(data) <- NULL
# Rename column to "date" and "time"
colnames(data)[1] <- 'time_stamp'
# Convert from factor to numeric using the function 'convert_magic.R'
# Convert data to character first
data <- convert_magic(data,c(rep('character',times = 28)))
# Then, convert to numeric
data <- convert_magic_num(data,c('character',rep('numeric',times = 27)))
# Convert "date" from character to date
data$time_stamp<-strptime(data$time_stamp,"%m/%d/%y %H:%M")
# Changing all the '-999' (missing data) to NA
for (i in 1:length(data)){
  data[i][data[i] == -999] <- NA
}
rm(i)

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
data$Rn_Q71_Avg[3647:3884] <- NA
data$Rn_Lite_Avg[3647:3884] <- NA
# rh_hmp_1_Avg and the other 2 (2 and 3) have values lower than zero
# t_hmp_1_Avg and the other 2 (2 and 3) have values lower than -20C
data$rh_hmp_1_Avg[which(data$rh_hmp_1_Avg < 0.1)] <- NA
data$rh_hmp_2_Avg[which(data$rh_hmp_2_Avg < 0.1)] <- NA
data$rh_hmp_3_Avg[which(data$rh_hmp_3_Avg < 0.1)] <- NA
data$t_hmp_1_Avg[which(data$t_hmp_1_Avg < -20)] <- NA
data$t_hmp_2_Avg[which(data$t_hmp_2_Avg < -20)] <- NA
data$t_hmp_3_Avg[which(data$t_hmp_3_Avg < -20)] <- NA
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

# Adding to data frame
data <- cbind(data,cold_front)
rm(cold_front) # Deleting temporary variable

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

# F.  Calculate C_E (bulk transfer coefficient) [(s/m)2] using the bulk aerodynamic mass transfer
#     equation
#       LE = L C_E U (es - ea) (W/m2)
#         L = 2,540,000 J/kg = latent heat of vaporization
#         U = wind speed (m/s) 
#         e = vapor pressure (Pa) 
#         C_E = bulk transfer coefficient [(s/m)2]


C_E = bulk_moist_coef(data$LE,data$WS_Spd_WVT,deltaQ)/2540000

# Adding all data from sections C, D, E, and F into data frame
data<-cbind(data,qs,qa,deltaQ,e_s1,e_a,deltaE,u_deltaE,C_E)
rm(e_s1,e_s2,e_s3,e_a,qs,qa,deltaQ,deltaE,C_E,u_deltaE) # Deleting all temporary variables

# G. Calculating atmospheric air density (kg/m3) using the ideal gas law density 
#     rho = P/RT, 
#     R = universal gas constant = 286.9 J/kg K 
#     T = absolute temperature (K) at 5.46 m
#     P = pressure (Pa, not kPA)
density <- ideal_gas_eq_rho(data$Press_CS115_Avg * 1000, data$t_hmp_3_Avg + 273.15)

# Adding data to data frame
data <- cbind(data,density)
rm(density) # Deleting temporary variable density

# H. Calculating using Charnock's (1955) equation over water
# z0 = 0.015 (ustar/g) unit m
roughness_length <- 0.015*data$U.*data$U./9.80
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
data <- cbind(data,LE_U,LE_deltaE)
rm(LE_U,LE_deltaE)

# Lastly since some of these calculations would results in NaN because division by zero,
# we need to remove these NaN's from the data.
# Remove all all NaN to become NA from 'data',
# There will be warnings because some columns do not have NaNs
k = length(data)
for (k in 2:k){ # Starts from 2 because the first column is the time stamp
  data[which(is.nan(data[,k])),] <- NA
}
rm(k) # Deleting the temporary k variable

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

# Grouping the data according to year, month, and hour, season for time series analysis
data_group <- data %>% 
  mutate(time_stamp=as.POSIXct(time_stamp)) %>%
  group_by(hour=format(as.POSIXlt(cut(time_stamp,breaks='hour')),'%H'),season) %>%
  summarise(LE=mean(LE,na.rm=TRUE), H = mean(H,na.rm=TRUE), Z.L = mean(Z.L,na.rm=TRUE),
            U. = mean(U.,na.rm=TRUE),WS = mean(WS_Spd_WVT,na.rm=TRUE),water_temp=mean(Water.surface.temperature,na.rm=TRUE),
            es=mean(e_s1,na.rm=TRUE),ea=mean(e_a,na.rm=TRUE),deltaE=mean(deltaE,na.rm=TRUE),P = mean(Press_CS115_Avg,na.rm=TRUE),
            Ta=mean(t_hmp_3_Avg,na.rm=TRUE))# %>%

data_group_sd <- data %>% 
  mutate(time_stamp=as.POSIXct(time_stamp)) %>%
  group_by(hour=format(as.POSIXlt(cut(time_stamp,breaks='hour')),'%H'),season) %>%
  summarise(sdLE = sd(LE,na.rm=TRUE), sdH = sd(H,na.rm=TRUE), sdZ.L = sd(Z.L,na.rm=TRUE),
            sdU. = sd(U.,na.rm=TRUE),sdWS = sd(WS_Spd_WVT,na.rm=TRUE),sdwater_temp=sd(Water.surface.temperature,na.rm=TRUE),
            sdes=sd(e_s1,na.rm=TRUE),sdea=sd(e_a,na.rm=TRUE),sddeltaE=sd(deltaE,na.rm=TRUE),sdP = sd(Press_CS115_Avg,na.rm=TRUE),
            sdTa=sd(t_hmp_3_Avg,na.rm=TRUE))
data_group <- cbind(data_group,data_group_sd)
rm(data_group_sd)

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
names_boxplot = c('-10\u2264\u03B6<-1','-1\u2264\u03B6<-0.5','-0.5\u2264\u03B6<-0.1','-0.1\u2264\u03B6<-0.05',
                  '-0.05\u2264\u03B6<0','0\u2264\u03B6<0.05','0.05\u2264\u03B6<0.1','0.1\u2264\u03B6<0.5','0.5\u2264\u03B6<1',
                  '1\u2264\u03B6<10')

### Keeping original margin settings for plots
old.par <- par()

##### 5A. Fig. 2 (with regressions): In "Latent heat flux and stability" paper ####

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
par(mfrow=c(3,2),mar=c(4.1,4.1,2.1,1.1))

## Plotting LE against deltaE for negative deltaE values
plot(neg$deltaE[which(neg$Z.L<0)],neg$LE[which(neg$Z.L<0)],pch=17,col='red',
     xlab=expression(paste(Delta,'e')),ylab='LE',xlim=c(-0.8,0),ylim=c(-90,40),cex.lab=1.3,cex.axis=1.2)
text(-0.78,35,'a)',cex=1.5)
minor.tick(ny=10,nx=10,tick.ratio=0.5)
points(neg$deltaE[which(neg$Z.L>0)],neg$LE[which(neg$Z.L>0)],pch=19, col='blue')
abline(lm1,col='red',lwd=3)
abline(lm2,col='blue',lty=2,lwd=3)

## Plotting LE against deltaE for positive deltaE values
plot(pos$deltaE[which(pos$Z.L<0)],pos$LE[which(pos$Z.L<0)],pch=17,col='red',
     xlab=expression(paste(Delta,'e')),ylab='',xlim=c(0,3),ylim=c(-50,450),cex.lab=1.3,cex.axis=1.2)
text(0.07,420,'d)',cex=1.5)
minor.tick(ny=10,nx=10,tick.ratio=0.5)
points(pos$deltaE[which(pos$Z.L>0)],pos$LE[which(pos$Z.L>0)],pch=19, col='blue')
abline(lm3,col='red',lwd=3)
abline(lm4,col='blue',lty=2,lwd=3)

## Plotting LE against U for negative deltaE values
plot(neg$WS_Spd_WVT[which(neg$Z.L<0)],neg$LE[which(neg$Z.L<0)],pch=17,col='red',
     xlab='U',ylab='LE',xlim=c(0,14),ylim=c(-90,40),cex.lab=1.3,cex.axis=1.2)
text(0.4,32,'b)',cex=1.5)
minor.tick(ny=10,nx=10,tick.ratio=0.5)
points(neg$WS_Spd_WVT[which(neg$Z.L>=0)],neg$LE[which(neg$Z.L>=0)],pch=19, col='blue')
abline(lm5,col='red',lwd=3)
abline(lm6,col='blue',lty=2,lwd=3)

## Plotting LE against U for positive deltaE values
plot(pos$WS_Spd_WVT[which(pos$Z.L<0)],pos$LE[which(pos$Z.L<0)],pch=17,col='red',
     xlab='U',ylab='',ylim=c(-50,450),xlim=c(0,14),cex.lab=1.3,cex.axis=1.2)
text(0.4,420,'e)',cex=1.5)
minor.tick(ny=10,nx=10,tick.ratio=0.5)
points(pos$WS_Spd_WVT[which(pos$Z.L>=0)],pos$LE[which(pos$Z.L>=0)],pch=19,col='blue')
abline(lm7,col='red',lwd=3)
abline(lm8,col='blue',lty=2,lwd=3)

## Plotting LE against u_deltaE for negative deltaE values
plot(neg$u_deltaE[which(neg$Z.L<0)],neg$LE[which(neg$Z.L<0)],pch=17,col='red',
     xlab=expression(paste('U',Delta,'e')),ylab='LE',xlim=c(-5,0),ylim=c(-90,40),cex.lab=1.3,cex.axis=1.2)
text(-4.9,32,'c)',cex=1.5)
minor.tick(ny=10,nx=10,tick.ratio=0.5)
points(neg$u_deltaE[which(neg$Z.L>=0)],neg$LE[which(neg$Z.L>=0)],pch=19, col='blue')
abline(lm9,col='red',lwd=3)
abline(lm10,col='blue',lty=2,lwd=3)

## Plotting LE against u_deltaE for positive deltaE values
plot(pos$u_deltaE[which(pos$Z.L<0)],pos$LE[which(pos$Z.L<0)],pch=17,col='red',
     xlab=expression(paste('U',Delta,'e')),ylab='',ylim=c(-50,450),xlim=c(0,15),cex.lab=1.3,cex.axis=1.2)
text(0.4,418,'f)',cex=1.5)
minor.tick(ny=10,nx=10,tick.ratio=0.5)
points(pos$u_deltaE[which(pos$Z.L>=0)],pos$LE[which(pos$Z.L>=0)],pch=19, col='blue')
abline(lm11,col='red',lwd=3)
abline(lm12,col='blue',lty=2,lwd=3)

# To turn off plotting in a different window
dev.off()
## Cleaning up
rm(neg,pos,lm1,lm2,lm3,lm4,lm5,lm6,lm7,lm8,lm9,lm10,lm11,lm12)

##### 5B. Fig. 3: LE, deltaE, u, u_deltaE versus atmospheric stability category ####

# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/fig_3.jpg')
jpeg(file=path_fig,width=1346, height=3600,res=360)
## Creating a new plot
plot.new()
# a) LE
plot1 <- ggplot(na.omit(data[,c('LE','stability_no')]), aes(factor(stability_no),LE)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/32),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='black',geom='text',label='---',size=7) + 
  labs(x="",y="LE") + theme_bw() + annotate("text",x=0.75,y=450,label="a)") +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=12),
        axis.text.y=element_text(size=11),axis.text.x=element_blank(),
        plot.margin=unit(c(1,1,-2,-1.2),"mm")) + 
  scale_y_continuous(breaks=seq(-100,500,by=100),limits=c(-100,450)) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))
# b) delta E
plot2 <- ggplot(na.omit(data[,c('deltaE','stability_no')]), aes(factor(stability_no),deltaE)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/32),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='black',geom='text',label='---',size=7) + 
  labs(x='',y=expression(paste(Delta,'e'))) + theme_bw() + annotate("text",x=0.75,y=3,label="b)") +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=12),
        axis.text.y=element_text(size=11),axis.text.x=element_blank(),
        plot.margin=unit(c(-4,1,5,0),"mm")) + 
  scale_y_continuous(breaks=seq(-3,3,by=0.5)) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))
# c) U
plot3 <- ggplot(na.omit(data[,c('WS_Spd_WVT','stability_no')]), aes(factor(stability_no),WS_Spd_WVT)) + geom_boxplot(outlier.size=0,fill='white') + 
  geom_jitter(alpha=I(1/32),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='black',geom='text',label='---',size=7) + 
  labs(x='',y='U') + theme_bw() + annotate("text",x=0.75,y=14,label="c)") +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=12),
        axis.text.y=element_text(size=11),axis.text.x=element_blank(),
        plot.margin=unit(c(-11,1,14,2),"mm")) + 
  scale_y_continuous(breaks=seq(0,14,by=2),limits=c(0,14)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))  
# d) U delta E
plot4 <- ggplot(na.omit(data[,c('u_deltaE','stability_no')]), aes(factor(stability_no),u_deltaE)) + geom_boxplot(outlier.size=0,fill='white') + 
  geom_jitter(alpha=I(1/32),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='black',geom='text',label='---',size=7) + 
  labs(x='',y=expression(paste('U',Delta,'e'))) + theme_bw() + annotate("text",x=0.75,y=14.1,label="d)") +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.x=element_text(size=11),axis.title.y=element_text(size=12),
        axis.text.x=element_text(angle=90,size=10,vjust=1),axis.text.y=element_text(size=11),
        plot.margin=unit(c(-20,1,7,-1.2),"mm")) + 
  scale_y_continuous(breaks=seq(-5,15,by=2.5)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) +
  scale_x_discrete(labels=names_boxplot)

multiplot2(plot1,plot2,plot3,plot4,cols=1,labs=list("ASL stability ranges",""))
dev.off()

##### 5C. Fig. 4: u* for different ASL stability ranges ####
names_boxplot = c('-10\u2264\u03B6<-1','-1\u2264\u03B6<-0.5','-0.5\u2264\u03B6<-0.1','-0.1\u2264\u03B6<-0.05',
                  '-0.05\u2264\u03B6<0','0\u2264\u03B6<0.05','0.05\u2264\u03B6<0.1','0.1\u2264\u03B6<0.5','0.5\u2264\u03B6<1',
                  '1\u2264\u03B6<10')
# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/fig_4.jpg')
jpeg(file=path_fig,width=3600,height=3060,res=360)
## Creating a new plot
plot.new()
plot5 <- ggplot(na.omit(data[,c('U.','stability_no')]), aes(factor(stability_no),U.)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/8),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='black',geom='text',label='-----',size=10) + 
  labs(x="ASL stability ranges",y=expression("u"["*"])) + theme_bw() +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=12),axis.title.x=element_text(size=12),
        axis.text.y=element_text(size=12),axis.text.x=element_text(size=12)) + 
  scale_x_discrete(labels=names_boxplot) +
  scale_y_continuous(breaks=seq(0,0.7,by=0.05)) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))
plot5
dev.off()

##### 5D. Fig. 5: LE/U and LE/deltaE for different ASL stability ranges ####
names_boxplot = c('-10\u2264\u03B6<-1','-1\u2264\u03B6<-0.5','-0.5\u2264\u03B6<-0.1','-0.1\u2264\u03B6<-0.05',
                  '-0.05\u2264\u03B6<0','0\u2264\u03B6<0.05','0.05\u2264\u03B6<0.1','0.1\u2264\u03B6<0.5','0.5\u2264\u03B6<1',
                  '1\u2264\u03B6<10')
# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/fig_5.jpg')
jpeg(file=path_fig,width=3600,height=3060,res=360)
## Creating a new plot
plot.new()
plot6 <- ggplot(na.omit(data[,c('LE_U','stability_no')]), aes(factor(stability_no),LE_U)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/8),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='black',geom='text',label='-----',size=10) + 
  labs(x="",y='LE/U') + theme_bw() +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=12),axis.title.x=element_text(size=12),
        axis.text.y=element_text(size=12),axis.text.x=element_text(size=12),
        plot.margin=unit(c(1,1,0,0),"mm")) + 
  scale_x_discrete(labels='') +
  scale_y_continuous(breaks=seq(-20,100,by=20),limits=c(-20,100)) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) +
  annotate("text",x=0.70,y=100,label="a)")
plot6

plot6a <- ggplot(na.omit(data[,c('LE_deltaE','stability_no')]), aes(factor(stability_no),LE_deltaE)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/8),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='black',geom='text',label='-----',size=10) + 
  labs(x="",y=expression(paste('LE/',Delta,'e'))) + theme_bw() +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=12),axis.title.x=element_text(size=12),
        axis.text.y=element_text(size=12),axis.text.x=element_text(size=12),
        plot.margin=unit(c(-10,1,10,0),"mm")) + 
  scale_x_discrete(labels=names_boxplot) +
  scale_y_continuous(breaks=seq(-60,300,by=50),limits=c(-60,300)) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) +
  annotate("text",x=0.70,y=300,label="b)")
plot6a

multiplot2(plot6,plot6a,cols=1,labs=list('ASL stability ranges',''))
dev.off()

##### 5D. Fig. 6: Bar plot of correlation coefficient, r, for differet atmospheric stability category ####

# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/fig_6.jpg')
jpeg(file=path_fig,width=1346, height=3600,res=360)
## Creating a new plot
plot.new()
#regression R^2 results plot between LE and U
plot7 <- ggplot(data=data_rsq,aes(x=cat_no,y=r_LE_U)) +
  geom_bar(stat='identity',fill='white',color='black',position='identity') +
  xlab('') + ylab('r') + ylim(-0.05,1.0) +
  theme(axis.title.y=element_text(size=14),
        axis.text.x=element_blank(), axis.text.y=element_text(color='black',size=12),
        panel.background=element_rect(fill='white',color='black'),
        plot.margin=unit(c(1,2,5,0),"mm")) +
  annotate('text', x = 0.8, y = 1, label='a)') # LE and U')

#regression R^2 results plot between LE and deltaE
plot8 <- ggplot(data=data_rsq,aes(x=cat_no,y=r_LE_deltaE)) +
  geom_bar(stat='identity',fill='white',color='black',position='identity') +
  xlab('') + ylab('r') + ylim(-0.05,1.0) +
  theme(axis.title.y=element_text(size=14),
        axis.text.x=element_blank(), axis.text.y=element_text(size=12,color='black'),
        panel.background=element_rect(fill='white',color='black'),
        plot.margin=unit(c(-10,2,16,0),"mm")) +
  annotate('text', x = 0.8, y = 1, label= 'b)') #expression(paste('b) LE and ',Delta,'e')))

#regression R^2 results plot between LE and deltaE
plot9 <- ggplot(data=data_rsq,aes(x=cat_no,y=r_LE_udeltaE)) +
  geom_bar(stat='identity',fill='white',color='black',position='identity') +
  xlab('') + ylab('r') + ylim(-0.05,1.0) +
  theme(axis.title.y=element_text(size=14),
        axis.text.x=element_text(angle=90,size=12,vjust=1,color ='black'), axis.text.y=element_text(size=12,color='black'),
        panel.background=element_rect(fill='white',color='black'),
        plot.margin=unit(c(-22,2,10,0),"mm")) +
  scale_x_discrete(labels = names_boxplot) +
  annotate('text',x=0.8,y=1,label= 'c)') #expression(paste('c) LE and ','U',Delta,'e')))

#Plotting all three above together in one plot
multiplot2(plot7,plot8,plot9,cols=1,labs=list('ASL stability ranges',''))
dev.off()

##### 5F. Fig. 7: LE against U, deltaE, and u_deltaE under unstable, near-neutral, and stable conditions ####
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/fig_7.jpg')
jpeg(file=path_fig,width=3600,height=3600,res=360)

plot.new()
# Categorize data into near neutral conditions (-0.05 < z/L < 0.05)
near_neutral <- which(data$Z.L > -0.05 & data$Z.L < 0.05)
# Categorize data into unstable conditions (-0.5 < z/L < -0.1)
unstable <- which(data$Z.L > -0.5 & data$Z.L < -0.1)
# Categorize data into near stable conditions (0.1 < z/L < 0.5)
stable <- which(data$Z.L > 0.1 & data$Z.L < 0.5)

# Plots of near neutral,stable,unstable conditions
par(mfrow=c(3,3),mar=c(4.1,4.1,1.1,1.1))

# Unstable (-0.5 < z/L < -0.1)
plot(data$WS_Spd_WVT[unstable],data$LE[unstable],xlab='',ylab='LE',pch=19,col='red',ylim=c(-100,450),xlim=c(0,15),cex.lab=1.5)
text(0.5,400,'a)',cex=2)
lm_U_unstable <- lm(LE[unstable]~WS_Spd_WVT[unstable],data=data)
abline(lm_U_unstable,col='red',lwd=2)
minor.tick(ny=10,nx=10,tick.ratio=0.5)

plot(data$deltaE[unstable],data$LE[unstable],xlab='',
     ylab='',pch=19,ylim=c(-100,450),xlim=c(-1,3),col='red',cex.lab=1.5)
text(-0.8,400,'d)',cex=2)
lm_deltaE_unstable <- lm(LE[unstable]~deltaE[unstable],data=data)
abline(lm_deltaE_unstable,col='red',lwd=2)
minor.tick(ny=10,nx=10,tick.ratio=0.5)

plot(data$u_deltaE[unstable],data$LE[unstable],xlab='',
     ylab='',pch=19,ylim=c(-100,450),xlim=c(-5,15),col='red',cex.lab=1.5)
text(-4.5,400,'g)',cex=2)
lm_u_deltaE_unstable <- lm(LE[unstable]~u_deltaE[unstable],data=data)
abline(lm_u_deltaE_unstable,col='red',lwd=2)
minor.tick(ny=10,nx=10,tick.ratio=0.5)

# Near-neutral (-0.05 < z/L < 0.05)
plot(data$WS_Spd_WVT[near_neutral],data$LE[near_neutral],xlab='',ylab='LE',pch=19,col='black',ylim=c(-100,450),xlim=c(0,15),cex.lab=1.5)
text(0.5,400,'b)',cex=2)
lm_U_near_neutral <- lm(LE[near_neutral]~WS_Spd_WVT[near_neutral],data=data)
abline(lm_U_near_neutral,lwd=2)
minor.tick(ny=10,nx=10,tick.ratio=0.5)

plot(data$deltaE[near_neutral],data$LE[near_neutral],xlab='',
     ylab='',pch=19,ylim=c(-100,450),xlim=c(-1,3),col='black',cex.lab=1.5)
text(-0.8,400,'e)',cex=2)
lm_deltaE_near_neutral <- lm(LE[near_neutral]~deltaE[near_neutral],data=data)
abline(lm_deltaE_unstable,col='black',lwd=2)
minor.tick(ny=10,nx=10,tick.ratio=0.5)

plot(data$u_deltaE[near_neutral],data$LE[near_neutral],xlab='',
     ylab='',pch=19,ylim=c(-100,450),xlim=c(-5,15),col='black',cex.lab=1.5)
text(-4.5,400,'h)',cex=2)
lm_u_deltaE_near_neutral <- lm(LE[near_neutral]~u_deltaE[near_neutral],data=data)
abline(lm_u_deltaE_near_neutral,col='black',lwd=2)
minor.tick(ny=10,nx=10,tick.ratio=0.5)

# Stable (0.1 < z/L < 0.5)
plot(data$WS_Spd_WVT[stable],data$LE[stable],xlab='U',ylab='LE',pch=19,col='blue',ylim=c(-100,450),xlim=c(0,15),cex.lab=1.5)
text(0.5,400,'c)',cex=2)
lm_U_stable <- lm(LE[stable]~WS_Spd_WVT[stable],data=data)
abline(lm_U_stable,col='blue',lwd=2)
minor.tick(ny=10,nx=10,tick.ratio=0.5)

plot(data$deltaE[stable],data$LE[stable],xlab=expression(paste(Delta,'e')),
     ylab='',pch=19,ylim=c(-100,450),xlim=c(-1,3),col='blue',cex.lab=1.5)
text(-0.8,400,'f)',cex=2)
lm_deltaE_stable <- lm(LE[stable]~deltaE[stable],data=data)
abline(lm_deltaE_stable,col='blue',lwd=2)
minor.tick(ny=10,nx=10,tick.ratio=0.5)

plot(data$u_deltaE[stable],data$LE[stable],xlab=expression(paste('U',Delta,'e')),
     ylab='',pch=19,ylim=c(-100,450),xlim=c(-5,15),col='blue',cex.lab=1.5)
text(-4.5,400,'i)',cex=2)
lm_u_deltaE_stable <- lm(LE[stable]~u_deltaE[stable],data=data)
abline(lm_u_deltaE_stable,col='blue',lwd=2)
minor.tick(ny=10,nx=10,tick.ratio=0.5)

dev.off()
rm(lm_deltaE_near_neutral,lm_deltaE_stable,lm_deltaE_unstable)
rm(lm_u_deltaE_near_neutral,lm_u_deltaE_stable,lm_u_deltaE_unstable)
rm(lm_U_near_neutral,lm_U_stable,lm_U_unstable)

##### 5E. Fig. 8: CE under different ASL stability ranges ####
names_boxplot = c('-10\u2264\u03B6<-1','-1\u2264\u03B6<-0.5','-0.5\u2264\u03B6<-0.1','-0.1\u2264\u03B6<-0.05',
                  '-0.05\u2264\u03B6<0','0\u2264\u03B6<0.05','0.05\u2264\u03B6<0.1','0.1\u2264\u03B6<0.5','0.5\u2264\u03B6<1',
                  '1\u2264\u03B6<10')
# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/fig_8.jpg')
jpeg(file=path_fig,width=3600,height=3060,res=360)
## Creating a new plot
plot.new()

plot10 <- ggplot(na.omit(data[,c('C_E','stability_no')]), aes(factor(stability_no),C_E)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/8),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='black',geom='text',label='-----',size=10) + 
  labs(x="ASL stability ranges",y=expression("C"["E"])) + theme_bw() +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=12),axis.title.x=element_text(size=12),
        axis.text.y=element_text(size=12),axis.text.x=element_text(size=12)) + 
  scale_x_discrete(labels=names_boxplot) +
  scale_y_continuous(breaks=seq(0,0.005,by=0.0005),limits=c(0,0.005)) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))
plot10
dev.off()

#### 5H. Fig. 9: C_E against U under unstable, near-neutral, and stable conditions  ####
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/fig_9.jpg')
jpeg(file=path_fig,width=1346, height=3600,res=360)

plot.new()
# Plots of near neutral,stable,unstable conditions
par(mfrow=c(3,1),mar=c(4.1,4.4,1.1,1.1))
# Unstable (-0.5 < z/L < -0.1)
plot(data$WS_Spd_WVT[unstable],data$C_E[unstable],
     xlab='',ylab=expression('C'['E']),pch=19,col='red',xlim=c(-1,15),cex.lab=1.5,ylim=c(-0.005,0.01))
lines(c(-5:20),rep(0.0011,26),lty=2,lwd=2)
lines(rep(4,26),seq(-0.010,0.015,by=0.001),lwd=2)
text(-0.5,0.009,'a)',cex=2)
minor.tick(ny=10,nx=10,tick.ratio=0.5)

# Near-neutral (-0.05 < z/L < 0.05)
plot(data$WS_Spd_WVT[near_neutral],data$C_E[near_neutral],
     xlab='',ylab=expression('C'['E']),pch=19,xlim=c(-1,15),cex.lab=1.5,ylim=c(-0.005,0.01))
lines(c(-5:20),rep(0.0011,26),lty=2,lwd=2)
lines(rep(4,26),seq(-0.010,0.015,by=0.001),lwd=2)
text(-0.5,0.009,'b)',cex=2)
minor.tick(ny=10,nx=10,tick.ratio=0.5)

# Stable (0.1 < z/L < 0.5)
plot(data$WS_Spd_WVT[stable],data$C_E[stable],
     xlab='U',ylab=expression('C'['E']),pch=19,col='blue',xlim=c(-1,15),cex.lab=1.5,ylim=c(-0.005,0.01))
lines(c(-5:20),rep(0.0011,26),lty=2,lwd=2)
lines(rep(4,26),seq(-0.010,0.015,by=0.001),lwd=2)
text(-0.5,0.009,'c)',cex=2)
minor.tick(ny=10,nx=10,tick.ratio=0.5)

dev.off()

#rm(stable,unstable,near_neutral)

#### 5I: Fig. 10: Seasonal hourly time series plot ####

path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/fig_10.jpg')
jpeg(file=path_fig,width=2900,height=3600,res=360)
## Creating 6 panels of plots
plot.new()
par(mfrow=c(6,2),oma=c(5.1,0.1,0.1,0.1))

# a) Fall: Latent and sensible heat fluxes
par(mai=c(0,0.6,0.1,0))
plot(data_group$hour[data_group$season == 1],
     data_group$LE[data_group$season == 1],
     ylab='LE and H',xlab='',type='l',ylim=c(-10,150),lwd=2,xaxt='n',cex.lab=2)
lines(data_group$hour[data_group$season == 1],
      data_group$H[data_group$season == 1],ylab='H',type='l',lty=2,lwd=2)
text(0,148,'a)',cex=1)
legend(20,160,y.intersp=1,bty='n',lty=c(1,2),lwd=c(2,2),c('LE','H'),cex=1)
minor.tick(ny=5,nx=5,tick.ratio=0.5)

# g) Winter: Latent and sensible heat fluxes
par(mai=c(0,0.3,0.1,0.1))
plot(data_group$hour[data_group$season == 0],
     data_group$LE[data_group$season == 0],
     ylab='',xlab='',type='l',ylim=c(-10,150),lwd=2,xaxt='n',yaxt='n')
lines(data_group$hour[data_group$season == 0],
      data_group$H[data_group$season == 0],ylab='H',type='l',lty=2,lwd=2)
text(0,148,'g)',cex=1)
legend(20,160,y.intersp=1,bty='n',lty=c(1,2),lwd=c(2,2),c('LE','H'),cex=1)
minor.tick(ny=5,nx=5,tick.ratio=0.5)

# b) Fall: zeta
par(mai=c(0,0.6,0.1,0))
plot(data_group$hour[data_group$season == 1],
     data_group$Z.L[data_group$season == 1],
     ylab=expression(zeta),xlab='',type='l',lwd=2,ylim=c(-1,1),xaxt ='n',cex.lab=2)
text(0,0.95,'b)',cex=1)
minor.tick(ny=5,nx=5,tick.ratio=0.5)

# h) Winter: zeta
par(mai=c(0,0.3,0.1,0.1))
plot(data_group$hour[data_group$season == 0],
     data_group$Z.L[data_group$season == 0],
     ylab='',xlab='',type='l',lwd=2,ylim=c(-1,1),xaxt ='n',yaxt='n')
text(0,0.95,'h)',cex=1)
minor.tick(ny=5,nx=5,tick.ratio=0.5)

# c) Fall: Wind speed
par(mai=c(0,0.6,0.1,0))
plot(data_group$hour[data_group$season == 1],
     data_group$WS[data_group$season == 1],
     ylab='U',xlab='',type='l',lwd=2,ylim=c(0,5),xaxt='n',cex.lab=2)
text(0,4.9,'c)',cex=1)
minor.tick(ny=5,nx=5,tick.ratio=0.5)

# i) Fall: Wind speed
par(mai=c(0,0.3,0.1,0.1))
plot(data_group$hour[data_group$season == 0],
     data_group$WS[data_group$season == 0],
     ylab='',xlab='',type='l',lwd=2,ylim=c(0,5),xaxt='n',yaxt='n')
text(0,4.9,'i)',cex=1)
minor.tick(ny=5,nx=5,tick.ratio=0.5)

# d) Fall: Temperature
par(mai=c(0,0.6,0.1,0))
plot(data_group$hour[data_group$season == 1],
     data_group$Ta[data_group$season == 1],
     ylab='T',xlab='',type='l',ylim=c(5,30),lwd=2,xaxt='n',cex.lab=2)
lines(data_group$hour[data_group$season == 1],
      data_group$water_temp[data_group$season == 1],ylab='T',type='l',lty=2,lwd=2)
legend(20,15,y.intersp=1,bty='n',lty=c(1,2),lwd=c(2,2),c(expression('T'['a']),expression('T'['s'])),cex=1)
minor.tick(ny=5,nx=5,tick.ratio=0.5)
text(0,29,'d)',cex=1)

# j) Winter: Temperature
par(mai=c(0,0.3,0.1,0.1))
plot(data_group$hour[data_group$season == 0],
     data_group$Ta[data_group$season == 0],
     ylab='',xlab='',type='l',ylim=c(5,30),lwd=2,xaxt='n',yaxt='n',cex.lab=1)
lines(data_group$hour[data_group$season == 0],
      data_group$water_temp[data_group$season == 0],ylab='T',type='l',lty=2,lwd=2)
legend(20,28,y.intersp=1,bty='n',lty=c(1,2),lwd=c(2,2),c(expression('T'['a']),expression('T'['s'])),cex=1)
minor.tick(ny=5,nx=5,tick.ratio=0.5)
text(0,29,'j)',cex=1)

# e) Fall: Pressure
par(mai=c(0,0.6,0.1,0))
plot(data_group$hour[data_group$season == 1],
     data_group$P[data_group$season == 1],
     ylab='P',xlab='',type='l',ylim=c(100.5,101.5),lwd=2,xaxt='n',cex.lab=2)
minor.tick(ny=5,nx=5,tick.ratio=0.5)
text(0,101.4,'e)',cex=1)

# h) Winter: Pressure
par(mai=c(0,0.3,0.1,0.1))
plot(data_group$hour[data_group$season == 0],
     data_group$P[data_group$season == 0],
     ylab='',xlab='',type='l',ylim=c(100.5,101.5),lwd=2,xaxt='n',yaxt='n',cex.lab=1)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
text(0,101.4,'h)',cex=1)


# f) Fall: Water vapor pressure (kPa)
par(mai=c(0,0.6,0.1,0))
plot(data_group$hour[data_group$season == 1],
     data_group$es[data_group$season == 1]/10,
     ylab='e',xlab='',type='l',lwd=2,ylim=c(0,4),cex.lab=2)
lines(data_group$hour[data_group$season == 1],
      data_group$ea[data_group$season == 1]/10,
      ylab=expression('e'['a']),type='l',lty=2,lwd=2)
lines(data_group$hour[data_group$season == 1],
      data_group$deltaE[data_group$season == 1],
      ylab=expression(paste(Delta,'e')),type='l',lty=3,lwd=2)
text(0,3.9,'f)',cex=1)
legend(20,4.2,y.intersp=1,bty='n',lty=c(1,2,3),lwd=c(2,2,2),
       c(expression('e'['s']),expression('e'['a']),expression(paste(Delta,'e'))),cex=1)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

# l) Winter: Water vapor pressure (kPa)
par(mai=c(0,0.3,0.1,0.1))
plot(data_group$hour[data_group$season == 0],
     data_group$es[data_group$season == 0]/10, main='',
     ylab='',xlab='',type='l',lwd=2,ylim=c(0,4),yaxt='n',cex.lab=2)
lines(data_group$hour[data_group$season == 0],
      data_group$ea[data_group$season == 0]/10,
      ylab=expression('e'['a']),type='l',lty=2,lwd=2)
lines(data_group$hour[data_group$season == 0],
      data_group$deltaE[data_group$season == 0],
      ylab=expression(paste(Delta,'q')),type='l',lty=3,lwd=2)
text(0,3.9,'l)',cex=1)
legend(20,4,y.intersp=1,bty='n',lty=c(1,2,3),lwd=c(2,2,2),
       c(expression('e'['s']),expression('e'['a']),expression(paste(Delta,'e'))),cex=1)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

title(xlab='Hour (local time)',ylab='',outer=TRUE,cex.lab=2)

dev.off()

#### 6. Cleaning up ####

# Deleting original margin settings for plots
rm(path_fig,plot1,plot2,plot3,plot4,plot5,plot6,plot6a,plot7,
   plot8,plot9,plot10,names_boxplot,i)
rm(old.par)

