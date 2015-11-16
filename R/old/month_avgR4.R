#install.packages('dplyr')
library(dplyr)
library(Hmisc)

# Grouping the data according to year, month, and hour, season
data_group <- data %>% 
  mutate(time_stamp=as.POSIXct(time_stamp)) %>%
  group_by(hour=format(as.POSIXlt(cut(time_stamp,breaks='hour')),'%H'),season) %>%
  summarise(LE=mean(LE,na.rm=TRUE), H = mean(H,na.rm=TRUE), Z.L = mean(Z.L,na.rm=TRUE),
            U. = mean(U.,na.rm=TRUE),WS = mean(WS_Spd_WVT,na.rm=TRUE),water_temp=mean(Water.surface.temperature,na.rm=TRUE),
            es=mean(e_s1,na.rm=TRUE) * 0.1 ,ea=mean(e_a,na.rm=TRUE) * 0.1,deltaE=mean(deltaE,na.rm=TRUE),P = mean(Press_CS115_Avg,na.rm=TRUE),
            Ta=mean(t_hmp_3_Avg,na.rm=TRUE))# need to multiply e_s1 and e_a with 0.1 because units in hPa not kPa

data_group_sd <- data %>% 
  mutate(time_stamp=as.POSIXct(time_stamp)) %>%
  group_by(hour=format(as.POSIXlt(cut(time_stamp,breaks='hour')),'%H'),season) %>%
  summarise(sdLE = sd(LE,na.rm=TRUE), sdH = sd(H,na.rm=TRUE), sdZ.L = sd(Z.L,na.rm=TRUE),
            sdU. = sd(U.,na.rm=TRUE),sdWS = sd(WS_Spd_WVT,na.rm=TRUE),sdwater_temp=sd(Water.surface.temperature,na.rm=TRUE),
            sdes=sd(e_s1,na.rm=TRUE),sdea=sd(e_a,na.rm=TRUE),sddeltaE=sd(deltaE,na.rm=TRUE),sdP = sd(Press_CS115_Avg,na.rm=TRUE),
            sdTa=sd(t_hmp_3_Avg,na.rm=TRUE))
data_group <- cbind(data_group,data_group_sd)
rm(data_group_sd)
            
#### Seasonal hourly time series plot ####

path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/time_series_plots/final_time_series.jpg')
jpeg(file=path_fig,width=2900,height=3600,res=360)
## Creating 6 panels of plots
plot.new()
par(mfrow=c(6,2),oma=c(5.1,0.1,0.1,0.1))

# a) Fall: Latent and sensible heat fluxes
par(mai=c(0,0.6,0.1,0))
plot(data_group$hour[data_group$season == 1],
     data_group$LE[data_group$season == 1],
     ylab='LE and H',xlab='',type='l',ylim=c(-10,150),lwd=2,xaxt='n',cex.lab=2)
lines(data_group$hour[data_group$season == 1],
     data_group$H[data_group$season == 1],ylab='H',type='l',lty=2,lwd=2)
text(0,148,'a)',cex=1)
legend(20,160,y.intersp=1,bty='n',lty=c(1,2),lwd=c(2,2),c('LE','H'),cex=1)
minor.tick(ny=5,nx=5,tick.ratio=0.5)

# g) Winter: Latent and sensible heat fluxes
par(mai=c(0,0.3,0.1,0.1))
plot(data_group$hour[data_group$season == 0],
     data_group$LE[data_group$season == 0],
     ylab='',xlab='',type='l',ylim=c(-10,150),lwd=2,xaxt='n',yaxt='n')
lines(data_group$hour[data_group$season == 0],
      data_group$H[data_group$season == 0],ylab='H',type='l',lty=2,lwd=2)
text(0,148,'g)',cex=1)
legend(20,160,y.intersp=1,bty='n',lty=c(1,2),lwd=c(2,2),c('LE','H'),cex=1)
minor.tick(ny=5,nx=5,tick.ratio=0.5)

# b) Fall: zeta
par(mai=c(0,0.6,0.1,0))
plot(data_group$hour[data_group$season == 1],
     data_group$Z.L[data_group$season == 1],
     ylab=expression(zeta),xlab='',type='l',lwd=2,ylim=c(-1,1),xaxt ='n',cex.lab=2)
text(0,0.95,'b)',cex=1)
minor.tick(ny=5,nx=5,tick.ratio=0.5)

# h) Winter: zeta
par(mai=c(0,0.3,0.1,0.1))
plot(data_group$hour[data_group$season == 0],
     data_group$Z.L[data_group$season == 0],
     ylab='',xlab='',type='l',lwd=2,ylim=c(-1,1),xaxt ='n',yaxt='n')
text(0,0.95,'h)',cex=1)
minor.tick(ny=5,nx=5,tick.ratio=0.5)

# c) Fall: Wind speed
par(mai=c(0,0.6,0.1,0))
plot(data_group$hour[data_group$season == 1],
     data_group$WS[data_group$season == 1],
     ylab='U',xlab='',type='l',lwd=2,ylim=c(0,5),xaxt='n',cex.lab=2)
text(0,4.9,'c)',cex=1)
minor.tick(ny=5,nx=5,tick.ratio=0.5)

# i) Fall: Wind speed
par(mai=c(0,0.3,0.1,0.1))
plot(data_group$hour[data_group$season == 0],
     data_group$WS[data_group$season == 0],
     ylab='',xlab='',type='l',lwd=2,ylim=c(0,5),xaxt='n',yaxt='n')
text(0,4.9,'i)',cex=1)
minor.tick(ny=5,nx=5,tick.ratio=0.5)

# d) Fall: Temperature
par(mai=c(0,0.6,0.1,0))
plot(data_group$hour[data_group$season == 1],
     data_group$Ta[data_group$season == 1],
     ylab='T',xlab='',type='l',ylim=c(5,30),lwd=2,xaxt='n',cex.lab=2)
lines(data_group$hour[data_group$season == 1],
      data_group$water_temp[data_group$season == 1],ylab='T',type='l',lty=2,lwd=2)
legend(20,15,y.intersp=1,bty='n',lty=c(1,2),lwd=c(2,2),c(expression('T'['a']),expression('T'['s'])),cex=1)
minor.tick(ny=5,nx=5,tick.ratio=0.5)
text(0,29,'d)',cex=1)

# j) Winter: Temperature
par(mai=c(0,0.3,0.1,0.1))
plot(data_group$hour[data_group$season == 0],
     data_group$Ta[data_group$season == 0],
     ylab='',xlab='',type='l',ylim=c(5,30),lwd=2,xaxt='n',yaxt='n',cex.lab=1)
lines(data_group$hour[data_group$season == 0],
      data_group$water_temp[data_group$season == 0],ylab='T',type='l',lty=2,lwd=2)
legend(20,28,y.intersp=1,bty='n',lty=c(1,2),lwd=c(2,2),c(expression('T'['a']),expression('T'['s'])),cex=1)
minor.tick(ny=5,nx=5,tick.ratio=0.5)
text(0,29,'j)',cex=1)

# e) Fall: Pressure
par(mai=c(0,0.6,0.1,0))
plot(data_group$hour[data_group$season == 1],
     data_group$P[data_group$season == 1],
     ylab='P',xlab='',type='l',ylim=c(100.5,101.5),lwd=2,xaxt='n',cex.lab=2)
minor.tick(ny=5,nx=5,tick.ratio=0.5)
text(0,101.4,'e)',cex=1)

# h) Winter: Pressure
par(mai=c(0,0.3,0.1,0.1))
plot(data_group$hour[data_group$season == 0],
     data_group$P[data_group$season == 0],
     ylab='',xlab='',type='l',ylim=c(100.5,101.5),lwd=2,xaxt='n',yaxt='n',cex.lab=1)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
text(0,101.4,'h)',cex=1)


# f) Fall: Vapor pressure (kPa)
par(mai=c(0,0.6,0.1,0))
plot(data_group$hour[data_group$season == 1],
     data_group$es[data_group$season == 1],
     ylab='e',xlab='',type='l',lwd=2,ylim=c(0,3.5),cex.lab=2,lty=2)
lines(data_group$hour[data_group$season == 1],
     data_group$ea[data_group$season == 1],
     ylab=expression('e'['a']),type='l',lty=1,lwd=2)
lines(data_group$hour[data_group$season == 1],
     data_group$deltaE[data_group$season == 1],
     ylab=expression(paste(Delta,'e')),type='l',lty=3,lwd=2)
text(0,3.4,'f)',cex=1)
legend(20,1,y.intersp=0.7,bty='n',lty=c(2,1,3),lwd=c(2,2,2),
       c(expression('e'['s']),expression('e'['a']),expression(paste(Delta,'e'))),cex=1)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

# l) Winter: Vapor pressure (kPa)
par(mai=c(0,0.3,0.1,0.1))
plot(data_group$hour[data_group$season == 0],
     data_group$es[data_group$season == 0], main='',
     ylab='',xlab='',type='l',lwd=2,ylim=c(0,3.5),yaxt='n',cex.lab=2,lty=2)
lines(data_group$hour[data_group$season == 0],
      data_group$ea[data_group$season == 0],
      ylab=expression('e'['a']),type='l',lty=1,lwd=2)
lines(data_group$hour[data_group$season == 0],
      data_group$deltaE[data_group$season == 0],
      ylab=expression(paste(Delta,'e')),type='l',lty=3,lwd=2)
text(0,3.4,'l)',cex=1)
legend(20,3.4,y.intersp=1,bty='n',lty=c(2,1,3),lwd=c(2,2,2),
       c(expression('e'['s']),expression('e'['a']),expression(paste(Delta,'e'))),cex=1)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

title(xlab='Hour (local time)',ylab='',outer=TRUE,cex.lab=2)

rm(path_fig)

dev.off()