##### TIME SERIES ANALYSIS #############################
## Note: run this analysis once lake_analysis.R has run
## Automatically save the plots of time series

# Initializing "day" to store number of days
# The day starts at 00:00 on Dec 1, 2007; i is number of days in the dataset
# One day is 84600 sec in POSIXct units
# A 30 min unit is 1800 sec when using as.numeric on a POSIXct variable
# Initialize i as number of days and time_start as starting time 

time_start = as.numeric(data$time_stamp[4718])
time_step = 86400
i = 30 # Only 30 days for the month of Sept 2007

# Names of plots
names = as.character(as.Date(data$time_stamp[350:1789]))
# Index of names of plots
j = 1

# Plot deltaE and z/L for 24 hours
# Before for loop starts, initialize time_2 as time_start
time_2 = time_start

for (i in 1:i) {
  # File path of where the plots will be saved
  path <- file.path("/Users/Yusri/Documents/Work/Data analysis/lake/figs/time_series_122007",
                    paste("ts_",names[j],".jpg",sep="")) 
  # For the next index for day
  j = j + 48 # to only include names for i after 1 day (48 slots if 1 hour has 2 obs (30 min))
  
  time_1 = time_2 
  time_2 = time_1 + time_step
  
  # day is a logical variable
  day <- as.numeric(data$time_stamp) >= time_1 & as.numeric(data$time_stamp) < time_2
  
  # Plot
  jpeg(file=path,width=500,height=250)
  mytitle = paste("Date:",names[i])
  
  # Create a two-panel daily time series plots
  plot.new()
  # Plot specs
  par(mfrow=c(1,2),mar=c(4.1,4.1,1.1,1.1))
  
  plot(subset(data$time_stamp,day),subset(data$deltaE,day),xlab='Hour (local time)',ylab=expression(paste(Delta,'e')),type='l',lwd=2,ylim=c(-1,3))
  plot(subset(data$time_stamp,day),subset(data$Z.L,day),xlab='Hour (local time)',ylab=expression(zeta),type='l',lwd=2,ylim=c(-10,10))
  dev.off() # to turn on plotting to R window dev.set(2), 2 is RStudio
}
dev.set(2)

rm(time_1,time_2,time_start,time_step,path,names,mytitle,i,j,day)

