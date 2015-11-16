## Other finalized plots (exploratory)
# LE vs U
plot(data$WS_Spd_WVT[which(data$stability_no==5)],data$LE[which(data$stability_no==5)],col='red',pch=19,xlab='U',ylab='CE')
points(data$WS_Spd_WVT[which(data$stability_no==6)],data$LE[which(data$stability_no==6)],col='blue',pch=19)
# Regression for LE vs U (unstable and stable)
lmLE_U_unst <- lm(data$LE[which(data$stability_no==1)] ~ data$WS_Spd_WVT[which(data$stability_no==1)])
summary(lmLE_U_unst)
lmLE_U_sta <- lm(data$LE[which(data$stability_no==10)] ~ data$WS_Spd_WVT[which(data$stability_no==10)])
summary(lmLE_U_sta)

# LE vs u*
plot(data$U.[which(data$stability_no==5)],data$LE[which(data$stability_no==5)],col='red',pch=19,xlab='u*',ylab='LE')
points(data$U.[which(data$stability_no==6)],data$LE[which(data$stability_no==6)],col='blue',pch=19)
# Regression for LE vs U (unstable and stable)
lmLE_ustar_unst <- lm(data$LE[which(data$stability_no==1)] ~ data$U.[which(data$stability_no==1)])
summary(lmLE_ustar_unst)
lmLE_ustar_sta <- lm(data$LE[which(data$stability_no==10)] ~ data$U.[which(data$stability_no==10)])
summary(lmLE_ustar_sta)

# LE vs deltaE
plot(data$deltaE[which(data$stability_no==5)],data$LE[which(data$stability_no==5)],col='red',pch=19,xlab=expression(paste(Delta,'e')),ylab='LE')
points(data$deltaE[which(data$stability_no==6)],data$LE[which(data$stability_no==6)],col='blue',pch=19)
# Regression for LE vs deltaE (unstable and stable)
lmLE_deltaE_unst <- lm(data$LE[which(data$stability_no==1)] ~ data$deltaE[which(data$stability_no==1)])
summary(lmLE_deltaE_unst)
lmLE_deltaE_sta <- lm(data$LE[which(data$stability_no==10)] ~ data$deltaE[which(data$stability_no==10)])
summary(lmLE_deltaE_sta)

# LE vs u_deltaE
plot(data$u_deltaE[which(data$stability_no==5)],data$LE[which(data$stability_no==5)],col='red',pch=19,xlab=expression(paste(U,Delta,'e')),ylab='LE')
points(data$u_deltaE[which(data$stability_no==6)],data$LE[which(data$stability_no==6)],col='blue',pch=19)
# Regression for LE vs deltaE (unstable and stable)
lmLE_UdeltaE_unst <- lm(data$LE[which(data$stability_no==1)] ~ data$u_deltaE[which(data$stability_no==1)])
summary(lmLE_UdeltaE_unst)
lmLE_UdeltaE_sta <- lm(data$LE[which(data$stability_no==10)] ~ data$u_deltaE[which(data$stability_no==10)])
summary(lmLE_UdeltaE_sta)