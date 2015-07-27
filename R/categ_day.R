# Already included in Lake Analysis V2-8

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

