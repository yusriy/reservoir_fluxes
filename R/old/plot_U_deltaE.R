barplot(data_rsq$p_U_deltaE,ylab='p-value',main='U versus deltaE')

# Plot deltaE vs U
for (i in 1:10){
  plot(data$WS_Spd_WVT[data$stability_no==i],data$deltaE[data$stability_no==i],
       xlim=c(0,15),ylim=c(-1,3),xlab='U',ylab=expression(paste(Delta,'e')),main=i)
}

# Plot deltaE vs ustar
for (i in 1:10){
  plot(data$U.[data$stability_no==i],data$deltaE[data$stability_no==i],
       xlim=c(0,1),ylim=c(-1,3),xlab='u*',ylab=expression(paste(Delta,'e')),main=i)
}

