##### 5G. Fig. 8: LE against U, deltaE, and u_deltaE in different ranges ####
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/fig_8.jpg')
jpeg(file=path_fig,width=1000,height=800)

plot.new()
# Categorize data into near neutral conditions (-0.05 < z/L < 0.05)
near_neutral <- which(data$Z.L > -0.05 & data$Z.L < 0.05)
# Categorize data into unstable conditions (-0.5 < z/L < -0.1)
unstable <- which(data$Z.L > -0.5 & data$Z.L < -0.1)
# Categorize data into near stable conditions (0.1 < z/L < 0.5)
stable <- which(data$Z.L > 0.1 & data$Z.L < 0.5)

# Plots of near neutral,stable,unstable conditions
par(mfrow=c(3,4),mar=c(4.1,4.1,1.1,1.1))

# Unstable (-0.5 < z/L < -0.1)
plot(data$WS_Spd_WVT[unstable],data$LE[unstable],xlab='U',ylab='LE',pch=19,col='red',ylim=c(-100,450),xlim=c(0,15),cex.lab=1.5)
text(0.5,400,'a)',cex=2)
lm_U_unstable <- lm(LE[unstable]~WS_Spd_WVT[unstable],data=data)
abline(lm_U_unstable,col='red',lwd=2)

plot(data$deltaE[unstable],data$LE[unstable],xlab=expression(paste(Delta,'e')),
     ylab='',pch=19,ylim=c(-100,450),xlim=c(-1,3),col='red',cex.lab=1.5)
text(-0.8,400,'d)',cex=2)
lm_deltaE_unstable <- lm(LE[unstable]~deltaE[unstable],data=data)
abline(lm_deltaE_unstable,col='red',lwd=2)

plot(data$u_deltaE[unstable],data$LE[unstable],xlab=expression(paste('U',Delta,'e')),
     ylab='',pch=19,ylim=c(-100,450),xlim=c(-5,15),col='red',cex.lab=1.5)
text(-4.5,400,'g)',cex=2)
lm_u_deltaE_unstable <- lm(LE[unstable]~u_deltaE[unstable],data=data)
abline(lm_u_deltaE_unstable,col='red',lwd=2)

plot(data$U.[unstable],data$LE[unstable],xlab=expression('u'['*']),
     ylab='',pch=19,ylim=c(-100,450),xlim=c(0,0.6),col='red',cex.lab=1.5)
text(0.02,400,'j)',cex=2)
lm_ustar_unstable <- lm(LE[unstable]~U.[unstable],data=data)
abline(lm_ustar_unstable,col='red',lwd=2)

# Near-neutral (-0.05 < z/L < 0.05)
plot(data$WS_Spd_WVT[near_neutral],data$LE[near_neutral],xlab='U',ylab='LE',pch=19,col='black',ylim=c(-100,450),xlim=c(0,15),cex.lab=1.5)
text(0.5,400,'b)',cex=2)
lm_U_near_neutral <- lm(LE[near_neutral]~WS_Spd_WVT[near_neutral],data=data)
abline(lm_U_near_neutral,lwd=2)

plot(data$deltaE[near_neutral],data$LE[near_neutral],xlab=expression(paste(Delta,'e')),
     ylab='',pch=19,ylim=c(-100,450),xlim=c(-1,3),col='black',cex.lab=1.5)
text(-0.8,400,'e)',cex=2)
lm_deltaE_near_neutral <- lm(LE[near_neutral]~deltaE[near_neutral],data=data)
abline(lm_deltaE_unstable,col='black',lwd=2)

plot(data$u_deltaE[near_neutral],data$LE[near_neutral],xlab=expression(paste('U',Delta,'e')),
     ylab='',pch=19,ylim=c(-100,450),xlim=c(-5,15),col='black',cex.lab=1.5)
text(-4.5,400,'h)',cex=2)
lm_u_deltaE_near_neutral <- lm(LE[near_neutral]~u_deltaE[near_neutral],data=data)
abline(lm_u_deltaE_near_neutral,col='black',lwd=2)

plot(data$U.[near_neutral],data$LE[near_neutral],xlab=expression('u'['*']),
     ylab='',pch=19,ylim=c(-100,450),xlim=c(0,0.6),col='black',cex.lab=1.5)
text(0.02,400,'k)',cex=2)
lm_ustar_near_neutral <- lm(LE[near_neutral]~U.[near_neutral],data=data)
abline(lm_ustar_near_neutral,col='black',lwd=2)

# Stable (0.1 < z/L < 0.5)
plot(data$WS_Spd_WVT[stable],data$LE[stable],xlab='U',ylab='LE',pch=19,col='blue',ylim=c(-100,450),xlim=c(0,15),cex.lab=1.5)
text(0.5,400,'c)',cex=2)
lm_U_stable <- lm(LE[stable]~WS_Spd_WVT[stable],data=data)
abline(lm_U_stable,col='blue',lwd=2)

plot(data$deltaE[stable],data$LE[stable],xlab=expression(paste(Delta,'e')),
     ylab='',pch=19,ylim=c(-100,450),xlim=c(-1,3),col='blue',cex.lab=1.5)
text(-0.8,400,'f)',cex=2)
lm_deltaE_stable <- lm(LE[stable]~deltaE[stable],data=data)
abline(lm_deltaE_stable,col='blue',lwd=2)

plot(data$u_deltaE[stable],data$LE[stable],xlab=expression(paste('U',Delta,'e')),
     ylab='',pch=19,ylim=c(-100,450),xlim=c(-5,15),col='blue',cex.lab=1.5)
text(-4.5,400,'i)',cex=2)
lm_u_deltaE_stable <- lm(LE[stable]~u_deltaE[stable],data=data)
abline(lm_u_deltaE_stable,col='blue',lwd=2)

plot(data$U.[stable],data$LE[stable],xlab=expression('u'['*']),
     ylab='',pch=19,ylim=c(-100,450),xlim=c(0,0.6),col='blue',cex.lab=1.5)
text(0.02,400,'l)',cex=2)
lm_ustar_stable <- lm(LE[stable]~U.[stable],data=data)
abline(lm_ustar_stable,col='blue',lwd=2)

dev.off()
rm(lm_deltaE_near_neutral,lm_deltaE_stable,lm_deltaE_unstable)
rm(lm_u_deltaE_near_neutral,lm_u_deltaE_stable,lm_u_deltaE_unstable)
rm(lm_U_near_neutral,lm_U_stable,lm_U_unstable)
rm(lm_ustar_near_neutral,lm_ustar_stable,lm_ustar_unstable)

#### 5H. Fig. 9: C_E against U and u* ####
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/fig_9.jpg')
jpeg(file=path_fig,width=800,height=1000)

plot.new()
# Plots of near neutral,stable,unstable conditions
par(mfrow=c(3,2),mar=c(4.1,5.1,1.1,1.1))
# Unstable (-0.5 < z/L < -0.1)
plot(data$U.[unstable],data$C_E[unstable],
     xlab=expression('u'['*']),ylab=expression('C'['E']),pch=19,xlim=c(0,0.8),col='red',cex.lab=1.5,ylim=c(-0.005,0.01))
text(0,0.010,'a)',cex=2)
plot(data$WS_Spd_WVT[unstable],data$C_E[unstable],
     xlab='U',ylab='',pch=19,col='red',xlim=c(0,14),cex.lab=1.5,ylim=c(-0.005,0.01))
text(0,0.010,'d)',cex=2)

# Near-neutral (-0.05 < z/L < 0.05)
plot(data$U.[near_neutral],data$C_E[near_neutral],
     xlab=expression('u'['*']),ylab=expression('C'['E']),pch=19,xlim=c(0,0.8),col='black',cex.lab=1.5,ylim=c(-0.005,0.01))
text(0,0.010,'b)',cex=2)
plot(data$WS_Spd_WVT[near_neutral],data$C_E[near_neutral],
     xlab='U',ylab='',pch=19,xlim=c(0,14),cex.lab=1.5,ylim=c(-0.005,0.01))
text(0,0.010,'e)',cex=2)

# Stable (0.1 < z/L < 0.5)
plot(data$U.[stable],data$C_E[stable],
     xlab=expression('u'['*']),ylab=expression('C'['E']),pch=19,xlim=c(0,0.8),col='blue',cex.lab=1.5,ylim=c(-0.005,0.01))
text(0,0.010,'c)',cex=2)
plot(data$WS_Spd_WVT[stable],data$C_E[stable],
     xlab='U',ylab='',pch=19,col='blue',xlim=c(0,14),cex.lab=1.5,ylim=c(-0.005,0.01))
text(0,0.010,'f)',cex=2)

dev.off()

rm(stable,unstable,near_neutral)

#### 5I: Fig. 10: Roughness length with U ####

# Plotting
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/fig_10.jpg')
jpeg(file=path_fig,width=500,height=500)
plot.new()
plot(data$WS_Spd_WVT[near_neutral],roughness_length,
     xlab='U',ylab=expression('z'[0]),pch=19)
dev.off()

