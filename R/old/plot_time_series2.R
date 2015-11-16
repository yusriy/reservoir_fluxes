##### PLOTTING TIME SERIES ######################
## Plotting in the console
# 3-in-1 plots
par(mfrow=c(3,1))
plot(data$time_stamp[which(data$cold_front==FALSE)],data$WS_Spd_WVT[which(data$cold_front==FALSE)],ylab='U',xlab='Time',main='No Cold Fronts',cex=0.25)
plot(data$time_stamp[which(data$cold_front==FALSE)],data$qs[which(data$cold_front==FALSE)],ylab='q',xlab='Time',cex=0.25)
points(data$time_stamp[which(data$cold_front==FALSE)],data$qa[which(data$cold_front==FALSE)],col='blue',cex=0.25)
plot(data$time_stamp[which(data$cold_front==FALSE)],data$LE[which(data$cold_front==FALSE)],xlab='Time',ylab='LE',cex=0.25)