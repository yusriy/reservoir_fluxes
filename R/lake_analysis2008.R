##### ABOUT: LAKE FLUX (2008) ANALYSIS ################################################
# Title: 2008 Latent and sensible heat flux study over a lake/reservoir
# 
# Data provided by Heping Liu, PhD, from Washington State University (WSU) in MS Excel
# and converted to csv format within Excel.
# 
# Author: Yusri Yusup, PhD
# Affiliation:  Environmental Technology, School of Industrial Technology, 
#               Universiti Sains Malaysia (USM)
# Date created: 2015-09-23
# 
# Version: 1.00

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
source('R/footprint_schmid.R') # Calculate the peak distance using Schmid (1994)

##### 1. Data import and clean up #################################################

# Read data from original csv file
d_2008 <- read.csv('data/lake_2008.csv')

# Remove the first two rows because they are empty rows or 
# non-data rows
d_2008 <- d_2008[c(-1,-2),]

# Remove row.names
row.names(d_2008) <- NULL

# Rename column to "date" and "time"
colnames(d_2008)[1] <- 'time_stamp'

# Convert from factor to numeric using the function 'convert_magic.R'
# Convert data to character first
d_2008 <- convert_magic(d_2008,c(rep('character',times = 49)))

# Then, convert to numeric
d_2008 <- convert_magic_num(d_2008,c('character',rep('numeric',times = 48)))

# Convert "date" from character to date
d_2008$time_stamp <- strptime(d_2008$time_stamp,"%m/%d/%y %H:%M")

# Rename all the headers so that it is the same as original data
names(d_2008)[32] <- 'U.'
names(d_2008)[33] <- 'Z.L'
names(d_2008)[36] <- 'sonic_WS'
names(d_2008)[37] <- 'sonic_WD'
names(d_2008)[17] <- 'Water.surface.temperature'

# Remove 2nd, 3rd, 4th, and 5th columns not available in the original data
d_2008 <- d_2008[,c(-2,-3,-4,-5,-13,-23,-34,-35,-38:-49)]
# Changing all the '-999' (missing data) to NA
for (i in 1:length(d_2008)){
  d_2008[i][d_2008[i] < -990] <- NA
}
rm(i)

# Rearrange the columns according to data

d_2008 <- d_2008[c('time_stamp','H','LE','Z.L','U.','t_hmp_1_Avg',
                   'rh_hmp_1_Avg','t_hmp_2_Avg','rh_hmp_2_Avg',
                   't_hmp_3_Avg','rh_hmp_3_Avg','sonic_WS','sonic_WD',
                   'WS_Spd_WVT','Wd_Spd_014A_Avg.1.','Wd_Spd_014A_Avg.2.',
                   'Wd_Spd_014A_Avg.3.','Water.surface.temperature','Rn_Lite_Avg',
                   'Quant_Avg','Pyra_Avg','Press_CS115_Avg','Rain_mm_Tot',
                   'Water_temp_Avg.3.','Water_temp_Avg.4.','Water_temp_Avg.5.',
                   'Water_temp_Avg.6.','Water_temp_Avg.7.','Water_temp_Avg.8.')]

