##### TIME SERIES ANALYSIS #############################
## Note: run this analysis once lake_analysis.R has run
## Automatically save the plots of time series

# Initializing "day" to store number of days
# The day starts at 17:30 on August 24, 2007; in numeric is 1187947800, i is number of days in the dataset
# One day is 84600 sec in POSIXct units
# A 30 min unit is 1800 sec when using as.numeric on a POSIXct variable
# Initialize i as number of days and time_start as starting time 

time_start = as.numeric(data$time_stamp[1])
# time_end = as.numeric(data$time_stamp[nrow(data)])
time_step = 86400
i = as.integer(nrow(data)/(24 * 2))
# Names of plots
names = as.character(as.Date(data$time_stamp))
j = 1
# Plot LE, H, z/L and u* for 24 hours
# Before for loop starts, initialize time_2 as time_start
time_2 = time_start
for (i in 1:i) {
  # File path of where the plots will be saved
  # "A", "B", "C", etc are added to the end of the file name for a new set of time series
  path <- file.path("/Users/Yusri/Documents/Work/Data analysis/lake/figs/time_series_plots",
                    paste("ts_",names[j],"A.jpg",sep="")) 
  j = j + 48 # to only include names for i after 1 day (48 slots if 1 hour has 2 obs (30 min))
  time_1 = time_2 
  time_2 = time_1 + time_step
  # day is a logical variable
  day <- as.numeric(data$time_stamp) >= time_1 & as.numeric(data$time_stamp) < time_2
  # Plot
  jpeg(file=path)
    mytitle = paste("Date:",names[i])
    # Create a four-panel daily time series plots
    plot.new()
    par(mfrow=c(2,2))
    # Margin of plots
    par(mar=c(4.1,4.1,1.1,1.1))
    plot(subset(data$time_stamp,day),subset(data$deltaT,day),xlab='Time LST',ylab='Ts - Ta',col='blue',type='l',ylim=c(-12,12))
    plot(subset(data$time_stamp,day),subset(data$deltaP_q,day),xlab='Time LST',ylab='qs - qa',col='red',type='l',ylim=c(-0.005,0.02))
    plot(subset(data$time_stamp,day),subset(data$u_deltaT,day),xlab='Time LST',ylab='u (Ts - Ta)',type='l',ylim=c(-125,100))
    plot(subset(data$time_stamp,day),subset(data$u_deltaP_q,day),xlab='Time LST',ylab='u (es - ea)',col='purple',type='l',ylim=c(-0.05,0.1))
  dev.off() # to turn on plotting to R window dev.set(2), 2 is RStudio
  }
dev.set(2)