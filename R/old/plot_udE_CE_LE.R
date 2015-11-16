# unstable
par(mfrow=c(2,2))#oma=c(1,1,0.1,0.1))
par(mai=c(0.6,0.9,0.1,0))
plot(data$u_deltaE[unstable],data$LE[unstable],pch=19,col='red',
     xlim=c(-5,15),ylim=c(0,500),xlab='',ylab='LE',xaxt='n')
minor.tick(ny=2,nx=2,tick.ratio=0.5)
par(mai=c(0.6,0.5,0.1,0.1))
plot(data$LE[unstable],data$LE[unstable],pch=19,col='red',xlab='',ylab='',ylim=c(0,500),xlim=c(0,500),
     xaxt='n',yaxt='n')
minor.tick(ny=2,nx=2,tick.ratio=0.5)
par(mai=c(0.9,0.9,0,0))
plot(data$u_deltaE[unstable],data$C_E[unstable],pch=19,col='red',
     xlim=c(-5,15),ylim=c(0,0.005),xlab=expression(paste('U',Delta,'e')),ylab=expression('C'['E']))
minor.tick(ny=2,nx=2,tick.ratio=0.5)
par(mai=c(0.9,0.5,0,0.1))
plot(data$LE[unstable],data$C_E[unstable],pch=19,col='red',
     xlim=c(0,500),ylim=c(0,0.005),xlab='LE',ylab='',yaxt='n')
minor.tick(ny=2,nx=2,tick.ratio=0.5)


# near neutral
par(mfrow=c(2,2))#oma=c(1,1,0.1,0.1))
par(mai=c(0.6,0.9,0.1,0))
plot(data$u_deltaE[near_neutral],data$LE[near_neutral],pch=19,col='black',
     xlim=c(-5,15),ylim=c(0,500),xlab='',ylab='LE',xaxt='n')
minor.tick(ny=2,nx=2,tick.ratio=0.5)
par(mai=c(0.6,0.5,0.1,0.1))
plot(data$LE[near_neutral],data$LE[near_neutral],pch=19,col='black',xlab='',ylab='',ylim=c(0,500),xlim=c(0,500),
     xaxt='n',yaxt='n')
minor.tick(ny=2,nx=2,tick.ratio=0.5)
par(mai=c(0.9,0.9,0,0))
plot(data$u_deltaE[near_neutral],data$C_E[near_neutral],pch=19,col='black',
     xlim=c(-5,15),ylim=c(0,0.005),xlab=expression(paste('U',Delta,'e')),ylab=expression('C'['E']))
minor.tick(ny=2,nx=2,tick.ratio=0.5)
par(mai=c(0.9,0.5,0,0.1))
plot(data$LE[near_neutral],data$C_E[near_neutral],pch=19,col='black',
     xlim=c(0,500),ylim=c(0,0.005),xlab='LE',ylab='',yaxt='n')
minor.tick(ny=2,nx=2,tick.ratio=0.5)


# stable
par(mfrow=c(2,2))#oma=c(1,1,0.1,0.1))
par(mai=c(0.6,0.9,0.1,0))
plot(data$u_deltaE[stable],data$LE[stable],pch=19,col='blue',
     xlim=c(-5,15),ylim=c(0,500),xlab='',ylab='LE',xaxt='n')
minor.tick(ny=2,nx=2,tick.ratio=0.5)
par(mai=c(0.6,0.5,0.1,0.1))
plot(data$LE[stable],data$LE[stable],pch=19,col='blue',xlab='',ylab='',ylim=c(0,500),xlim=c(0,500),
     xaxt='n',yaxt='n')
minor.tick(ny=2,nx=2,tick.ratio=0.5)
par(mai=c(0.9,0.9,0,0))
plot(data$u_deltaE[stable],data$C_E[stable],pch=19,col='blue',
     xlim=c(-5,15),ylim=c(0,0.005),xlab=expression(paste('U',Delta,'e')),ylab=expression('C'['E']))
minor.tick(ny=2,nx=2,tick.ratio=0.5)
par(mai=c(0.9,0.5,0,0.1))
plot(data$LE[stable],data$C_E[stable],pch=19,col='blue',
     xlim=c(0,500),ylim=c(0,0.005),xlab='LE',ylab='',yaxt='n')
minor.tick(ny=2,nx=2,tick.ratio=0.5)

