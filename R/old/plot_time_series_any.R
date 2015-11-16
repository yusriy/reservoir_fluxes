
peak_CE <- which(data$C_E > 0.015 & data$cold_front == FALSE)# & data$Z.L < 0) # & data$deltaE > 0)

for (i in 1:21){
  plot(data$time_stamp[(peak_CE[i]-10):(peak_CE[i]+10)],data$WS_Spd_WVT[(peak_CE[i]-10):(peak_CE[i]+10)],type='l',
       xlab='Time',ylab='U',ylim=c(0,10))
  points(data$time_stamp[peak_CE[i]],data$WS_Spd_WVT[peak_CE[i]],col='red',pch=19)
}