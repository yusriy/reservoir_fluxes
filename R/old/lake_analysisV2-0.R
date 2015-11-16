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
# Date modified: 2014-09-26
# Version: 2.0

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

e_s1 <- vap_pres_Buck(data$Water.surface.temperature, 1.00) # RH = 1.00 because saturated
e_s2 <- vap_pres_Buck(data$t_hmp_1_Avg, data$rh_hmp_1_Avg)
e_s3 <- vap_pres_Buck(data$t_hmp_2_Avg, data$rh_hmp_2_Avg)
e_a <- vap_pres_Buck(data$t_hmp_3_Avg, data$rh_hmp_3_Avg)
# deltaE must be in kPa for bulk aerodynamic transfer equation below
deltaE <- (e_s1 - e_a)  * 0.10

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

# F.  Calculate C_E (bulk transfer coefficient) [(s/m)2] using the bulk aerodynamic mass transfer
#     equation
#       LE = L C_E U (es - ea) (W/m2)
#         L = 2,540,000 J/kg = latent heat of vaporization
#         U = wind speed (m/s) 
#         e = vapor pressure (Pa) 
#         C_E = bulk transfer coefficient [(s/m)2]

# Note: C_E here is product of C_E and L = 2,540,000 J/kg, 'C_E.L' and value is factor of
# 1/1000 since vapor pressure is in kPa not Pa
C_E = bulk_moist_coef(data$LE,data$WS_Spd_WVT,deltaE)

# Adding all data from sections C, D, E, and F into data frame
data<-cbind(data,qs,qa,deltaE,u_deltaE,C_E)
rm(e_s1,e_s2,e_s3,e_a,qs,qa,deltaE,C_E,u_deltaE) # Deleting all temporary variables

# G. Calculating atmospheric air density (kg/m3) using the ideal gas law density 
#     rho = P/RT, 
#     R = universal gas constant = 286.9 J/kg K 
#     T = absolute temperature (K) at 5.46 m
#     P = pressure (Pa, not kPA)
density <- ideal_gas_eq_rho(data$Press_CS115_Avg * 1000, data$t_hmp_3_Avg + 273.15)

# Adding data to data frame
data <- cbind(data,density)
rm(density) # Deleting temporary variable density

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

# Adding to the data frame
data <- cbind(data,stability_no,stability)
rm(stability_no,stability) # Deleting temporary variables

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
names_boxplot = c('-10\u2264z/L<-1','-1\u2264z/L<-0.5','-0.5\u2264z/L<-0.1','-0.1\u2264z/L<-0.05',
                  '-0.05\u2264z/L<0','0\u2264z/L<0.05','0.05\u2264z/L<0.1','0.1\u2264z/L<0.5','0.5\u2264z/L<1',
                  '1\u2264z/L<10')

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

## Starting fresh
par(old.par)
# Path where the plots will be saved
path_fig2 <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/fig_2.jpg')
jpeg(file=path_fig2,width=800, height=1000,res=80)
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
     xlab=expression(paste(Delta,'e')),ylab='LE',xlim=c(0,3),ylim=c(-50,450),cex.lab=1.3,cex.axis=1.2)
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
     xlab='U',ylab='LE',ylim=c(-50,450),xlim=c(0,14),cex.lab=1.3,cex.axis=1.2)
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
     xlab=expression(paste('U',Delta,'e')),ylab='LE',ylim=c(-50,450),xlim=c(0,15),cex.lab=1.3,cex.axis=1.2)
text(0.4,418,'f)',cex=1.5)
minor.tick(ny=10,nx=10,tick.ratio=0.5)
points(pos$u_deltaE[which(pos$Z.L>=0)],pos$LE[which(pos$Z.L>=0)],pch=19, col='blue')
abline(lm11,col='red',lwd=3)
abline(lm12,col='blue',lty=2,lwd=3)

# To turn off plotting in a different window
dev.off()
## Cleaning up
rm(neg,pos,lm1,lm2,lm3,lm4,lm5,lm6,lm7,lm8,lm9,lm10,lm11,lm12)

##### 5B. Fig. 3: LE versus atmospheric stability category ####

## Starting fresh
par(old.par)
# Path where the plots will be saved
path_fig3 <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/fig_3.jpg')
jpeg(file=path_fig3,res=100)
## Creating a new plot
plot.new()
plot1 <- ggplot(na.omit(data[,c('LE','stability_no')]), aes(factor(stability_no),LE)) + geom_boxplot(outlier.size=0,aes(fill=factor(stability_no))) + geom_jitter(alpha=I(1/16),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='white',geom='text',label='---',size=9,color='white') + 
  scale_x_discrete(labels=names_boxplot) + labs(x='Atmospheric stability category') + 
  theme_bw() + theme(legend.position='none',panel.grid.major=element_line(size=0.75),panel.grid.minor=element_line(size=0.50,color='gray'),
                     axis.title.x=element_text(size=14),axis.title.y=element_text(size=14),
                     axis.text.x=element_text(angle=90,size=12,vjust=1),axis.text.y=element_text(size=12)) + 
  scale_y_continuous(breaks=seq(-100,500,by=50)) +
  scale_fill_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))
plot1
dev.off()

##### 5C. Fig. 4: (es - ea) versus atmospheric stability category ####
## Starting fresh
par(old.par)
# Path where the plots will be saved
path_fig4 <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/fig_4.jpg')
jpeg(file=path_fig4,res=100)
## Creating a new plot
plot.new()
plot2 <- ggplot(na.omit(data[,c('deltaE','stability_no')]), aes(factor(stability_no),deltaE)) + geom_boxplot(outlier.size=0,aes(fill=factor(stability_no))) + geom_jitter(alpha=I(1/16),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='white',geom='text',label='---',size=9,color='white') + 
  scale_x_discrete(labels=names_boxplot) + labs(x='Atmospheric stability category',y=expression(paste(Delta,'e'))) + 
  theme_bw() + theme(legend.position='none',panel.grid.major=element_line(size=0.75),panel.grid.minor=element_line(size=0.50,color='gray'),
                     axis.title.x=element_text(size=14),axis.title.y=element_text(size=14),
                     axis.text.x=element_text(size=12,angle=90,vjust=1),axis.text.y=element_text(size=12)) + 
  scale_y_continuous(breaks=seq(-3,3,by=0.5)) +
  scale_fill_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))  
plot2
dev.off()

##### 5D. Fig. 5: U versus atmospheric stability category ####
## Starting fresh
par(old.par)
# Path where the plots will be saved
path_fig5 <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/fig_5.jpg')
jpeg(file=path_fig5,res=100)
## Creating a new plot
plot.new()
plot3 <- ggplot(na.omit(data[,c('WS_Spd_WVT','stability_no')]), aes(factor(stability_no),WS_Spd_WVT)) + geom_boxplot(outlier.size=0,aes(fill=factor(stability_no))) + geom_jitter(alpha=I(1/16),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='white',geom='text',label='---',size=9,color='white') + 
  scale_x_discrete(labels=names_boxplot) + labs(x='Atmospheric stability category',y='U') + 
  theme_bw() + theme(legend.position='none',panel.grid.major=element_line(size=0.75),panel.grid.minor=element_line(size=0.50,color='gray'),
                     axis.title.x=element_text(size=14),axis.title.y=element_text(size=14),
                     axis.text.x=element_text(size=12,vjust=1,angle=90),axis.text.y=element_text(size=12)) + 
  scale_y_continuous(breaks=seq(0,15,by=1)) +
  scale_fill_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))  
plot3
dev.off()

##### 5E. Fig. 6: u_deltaE versus atmospheric stability category ####
## Starting fresh
par(old.par)
# Path where the plots will be saved
path_fig6 <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/fig_6.jpg')
jpeg(file=path_fig6,res=100)
## Creating a new plot
plot.new()
plot4 <- ggplot(na.omit(data[,c('u_deltaE','stability_no')]), aes(factor(stability_no),u_deltaE)) + geom_boxplot(outlier.size=0,aes(fill=factor(stability_no))) + geom_jitter(alpha=I(1/16),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='white',geom='text',label='---',size=9,color='white') + 
  scale_x_discrete(labels=names_boxplot) + labs(x='Atmospheric stability category',y=expression(paste('U',Delta,'e'))) + 
  theme_bw() + theme(legend.position='none',panel.grid.major=element_line(size=0.75),panel.grid.minor=element_line(size=0.50,color='gray'),
                     axis.title.x=element_text(size=14),axis.title.y=element_text(size=14),
                     axis.text.x=element_text(size=12,vjust=1,angle=90),axis.text.y=element_text(size=12)) + 
  scale_y_continuous(breaks=seq(-5,15,by=2.5)) +
  scale_fill_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))  
plot4
dev.off()

##### 5F. Fig. 7: Bar plot of correlation coefficient, r, for differet atmospheric stability category ####
## Starting fresh
par(old.par)
# Path where the plots will be saved
path_fig7 <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/fig_7.jpg')
jpeg(file=path_fig7,width=800, height=1500,res=100)
## Creating a new plot
plot.new()
#regression R^2 results plot between LE and U
plot5 <- ggplot(data=data_rsq,aes(x=cat_no,y=r_LE_U)) +
  geom_bar(stat='identity',fill='skyblue',color='black',position='identity') +
  xlab('Atmospheric stability category') + ylab('r') + ylim(-0.05,1.0) +
  scale_x_discrete(labels=names_boxplot) + 
  theme(axis.title.x=element_text(size=20),axis.title.y=element_text(size=20),
        axis.text.x=element_text(color='black',size=20,vjust=1,angle=90), axis.text.y=element_text(color='black',size=20),
        panel.background=element_rect(fill='white',color='black'),title=element_text(size=20)) +
  ggtitle('(a) LE and U')

#regression R^2 results plot between LE and deltaE
plot6 <- ggplot(data=data_rsq,aes(x=cat_no,y=r_LE_deltaE)) +
  geom_bar(stat='identity',fill='skyblue',color='black',position='identity') +
  xlab('Atmospheric stability category') + ylab('r') + ylim(-0.05,1.0) +
  scale_x_discrete(labels=names_boxplot) +
  theme(axis.title.x=element_text(size=20),axis.title.y=element_text(size=20),
        axis.text.x=element_text(size=20,angle=90,vjust=1,color='black'), axis.text.y=element_text(size=20,color='black'),
        panel.background=element_rect(fill='white',color='black'),title=element_text(size=20)) +
  ggtitle(expression(paste('(b) LE and ',Delta,'e')))

#regression R^2 results plot between LE and deltaE
plot7 <- ggplot(data=data_rsq,aes(x=cat_no,y=r_LE_udeltaE)) +
  geom_bar(stat='identity',fill='skyblue',color='black',position='identity') +
  xlab('Atmospheric stability category') + ylab('r') + ylim(-0.05,1.0) +
  scale_x_discrete(labels=names_boxplot) +
  theme(axis.title.x=element_text(size=20),axis.title.y=element_text(size=20),
        axis.text.x=element_text(size=20,color='black',vjust=1,angle=90), axis.text.y=element_text(size=20,color='black'),
        panel.background=element_rect(fill='white',color='black'),title=element_text(size=20)) +
  ggtitle(expression(paste('(c) LE and ','U',Delta,'e')))

#Plotting all three above together in one plot
multiplot(plot5,plot6,plot7,cols=1)
dev.off()

##### 5G. Fig. 8: In "Latent heat flux and stability" paper ####
## Starting fresh
par(old.par)
# Path where the plots will be saved
path_fig8 <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/fig_8.jpg')
jpeg(file=path_fig8,res=100)
## Creating a new plot
plot.new()
## Extract data and assign them to categories
# Classifying data into stability categories for the purpose of box plotting
# z/L = -10 to -1 = cat1
# z/L = -1 to -0.5 = cat2
# z/L = -0.5 to -0.1 = cat3
# z/L = -0.1 to -0.05 = cat4
# z/L = -0.05 to 0 = cat5
# z/L =  0 to 0.05 = cat6
# z/L =  0.05 to 0.10 = cat7
# z/L =  0.10 to 0.50 = cat8
# z/L =  0.50 to 1.00 = cat9
# z/L =  1 to 10 = cat10
cat1 <- data$C_E[which(data$stability_no==1)]
cat2 <- data$C_E[which(data$stability_no==2)]
cat3 <- data$C_E[which(data$stability_no==3)]
cat4 <- data$C_E[which(data$stability_no==4)]
cat5 <- data$C_E[which(data$stability_no==5)]
cat6 <- data$C_E[which(data$stability_no==6)]
cat7 <- data$C_E[which(data$stability_no==7)]
cat8 <- data$C_E[which(data$stability_no==8)]
cat9 <- data$C_E[which(data$stability_no==9)]
cat10 <- data$C_E[which(data$stability_no==10)]

## Creating a new dataframe
# The warnings can be ignored because each 'cat' has different lengths
df <- cbind(cat1,cat2,cat3,cat4,cat5,cat6,cat7,cat8,cat9,cat10)
df <- as.data.frame(df)
# Getting the quantiles for the different categories using the function "quantile"
quantile_data <- as.data.frame(lapply(na.omit(df),FUN=quantile,probs=c(0.05,0.50,0.95)))
# Only shade from 25% to 75% quantile
new_data5<-quantile_data[1,]
new_data50<-quantile_data[2,]
new_data95<-quantile_data[3,]

## Creating the plot
# Setting the margin
par(mar=c(9,4.1,1,2) + 0.1)
# Plotting. Note: Removing ylab =""
plot(x=1:10,y=new_data50,type='l', xlab="",
     ylab=expression(paste('C'[E],'.L')),
     ylim=c(-10,100),xaxt='n',las=1,cex.lab=1.2,cex.axis=1.2)
minor.tick(ny=10,tick.ratio=0.5)
title(xlab='Atmospheric stability category',line=8,cex.lab=1.2)
axis(side=1,at=1:10,labels = names_boxplot,cex.axis=1.2,las=3)
polygon(c(1:10,10:1),c(new_data5,rev(new_data95)),col='skyblue',border=NA)
lines(1:10,new_data50,lwd=2,lty=1)

dev.off()
## Cleaning up
rm(cat1,cat2,cat3,cat4,cat5,cat6,cat7,cat8,cat9,cat10,
   df,quantile_data,new_data5,new_data50,new_data95)

##### 5H. Fig. 9: C_EL against LE, U, deltaE, and u_deltaE ####
## Starting fresh
par(old.par)
# Path where the plots will be saved
path_fig9 <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/fig_9.jpg')
jpeg(file=path_fig9,width=800, height=1400,res=100)
## Creating a new plot
plot.new()
# (a) Plotting C_E against LE
plot8 <- ggplot(data=data,aes(x=LE,y=C_E,group=stability,color=stability,shape=stability)) + geom_point(size=3,na.rm=TRUE) + scale_color_manual(values=c('blue','red')) +
  scale_y_continuous(breaks=seq(-100,450,by=50),limits=c(-100,450)) + scale_x_continuous(breaks=seq(-200,450,by=50)) +
  theme(panel.background=element_rect(fill='white',color='black'), panel.grid.major=element_line(size=0.30,color='gray'),
        panel.grid.minor=element_line(size=0.10,color='gray'),axis.title.x=element_text(size=20),axis.title.y=element_text(size=20),
        axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),legend.position='none') + 
  ylab(expression(paste('C'[E],'.L'))) + xlab('LE') + ggtitle('(a)')

# (b) Plotting C_E against U
plot9 <- ggplot(data=data,aes(x=WS_Spd_WVT,y=C_E,group=stability,color=stability,shape=stability)) + geom_point(size=3, na.rm=TRUE) + scale_color_manual(values=c('blue','red')) +
  scale_y_continuous(breaks=seq(-100,450,by=50),limits=c(-100,450)) + scale_x_continuous(breaks=seq(0,14,by=1)) +
  theme(panel.background = element_rect(fill='white',color='black'), panel.grid.major=element_line(size=0.30,color='gray'), 
        panel.grid.minor=element_line(size=0.10,color='gray'),axis.title.x=element_text(size=20),axis.title.y=element_text(size=20),
        axis.text.x=element_text(size=15), axis.text.y=element_text(size=15),legend.position='none') +
  ylab(expression(paste('C'[E],'.L'))) + xlab('U') + ggtitle('(b)')

# (c) Plotting C_E against deltaE
plot10 <- ggplot(data=data,aes(x=deltaE,y=C_E,group=stability,color=stability,shape=stability)) + geom_point(size=3,na.rm=TRUE) + scale_color_manual(values=c('blue','red')) +
  scale_y_continuous(breaks=seq(-100,450,by=50),limits=c(-100,450)) + scale_x_continuous(breaks=seq(-1,4,by=0.5)) +
  theme(panel.background=element_rect(fill='white',color='black'), panel.grid.major=element_line(size=0.30,color='gray'),
        panel.grid.minor=element_line(size=0.10,color='gray'),axis.title.x=element_text(size=20),axis.title.y=element_text(size=20),
        axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),legend.position='none') + 
  ylab(expression(paste('C'[E],'.L'))) + xlab(expression(paste(Delta,'e'))) + ggtitle('(c)')

# (d) Plotting C_E against u_deltaE
plot11 <- ggplot(data=data,aes(x=u_deltaE,y=C_E,group=stability,color=stability,shape=stability)) + geom_point(size=3,na.rm=TRUE) + scale_color_manual(values=c('blue','red')) +
  scale_y_continuous(breaks=seq(-100,450,by=50),limits=c(-100,450)) + scale_x_continuous(breaks=seq(-10,18,by=1)) +
  theme(panel.background=element_rect(fill='white',color='black'), panel.grid.major=element_line(size=0.30,color='gray'),
        panel.grid.minor=element_line(size=0.10,color='gray'),axis.title.x=element_text(size=20),axis.title.y=element_text(size=20),
        axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),legend.position='none') + 
  ylab(expression(paste('C'[E],'.L'))) + xlab(expression(paste('U',Delta,'e'))) + ggtitle('(d)')

multiplot(plot8,plot9,plot10,plot11)
dev.off()

#### 6. Cleaning up ####

# Deleting original margin settings for plots
rm(path_fig2,path_fig3,path_fig4,path_fig5,path_fig6,path_fig7,
   path_fig8,path_fig9,plot1,plot2,plot3,plot4,plot5,plot6,plot7,
   plot8,plot9,plot10,plot11,names_boxplot)
rm(old.par)
