# Script to plot Businger-Dyer profiles
zL <- seq(-1,1,by=0.1)
# Unstable
phi_H_unstable <- 0.74 * (1 - 9 *zL)^-0.5
phi_M_unstable <- (1 - 15 * zL)^-0.25

# Stable
phi_H_stable <- 0.74 + 4.7 * zL
phi_M_stable <- 1 + 4.7 * zL

plot(zL,phi_H_unstable,xlab='z/L',ylab=expression(phi),type='l',lwd=2,col='red')
lines(zL,phi_M_unstable,lwd=2,col='red',lty=2)

lines(zL,phi_H_stable,lwd=2)
lines(zL,phi_M_stable,lwd=2,lty=2)

rm(zL,phi_H_unstable,phi_M_unstable,phi_H_stable,phi_M_stable)
