# To separate data into windy days and non-windy days
#install.packages('dplyr')
library(dplyr)
library(Hmisc)

# Grouping the data according month Sept 2007
data_group_wind <- data %>% 
  mutate(time_stamp=as.POSIXct(time_stamp)) %>%
  group_by(windy,
           hour=format(as.POSIXlt(cut(time_stamp,breaks='hour')),'%H')) %>%
  summarise(LE=mean(LE,na.rm=TRUE), H= mean(H,na.rm=TRUE),Z.L = mean(Z.L,na.rm=TRUE),
            WS = mean(WS_Spd_WVT,na.rm=TRUE),water_temp=mean(Water.surface.temperature,na.rm=TRUE),
            es=mean(e_s1,na.rm=TRUE) * 0.1 ,ea=mean(e_a,na.rm=TRUE) * 0.1,deltaE=mean(deltaE,na.rm=TRUE),
            Ta=mean(t_hmp_3_Avg,na.rm=TRUE))# need to multiply e_s1 and e_a with 0.1 because units in hPa not kPa
data_group_wind <- data_group_wind[-49,]

data_group_n_wind <- data %>% 
  mutate(time_stamp=as.POSIXct(time_stamp)) %>%
  group_by(n_windy,
           hour=format(as.POSIXlt(cut(time_stamp,breaks='hour')),'%H')) %>%
  summarise(LE = mean(LE,na.rm=TRUE), H= mean(H,na.rm=TRUE),Z.L = mean(Z.L,na.rm=TRUE),
            WS = mean(WS_Spd_WVT,na.rm=TRUE),water_temp=mean(Water.surface.temperature,na.rm=TRUE),
            es=mean(e_s1,na.rm=TRUE) * 0.1,ea=mean(e_a,na.rm=TRUE) * 0.1,deltaE=mean(deltaE,na.rm=TRUE),
            Ta=mean(t_hmp_3_Avg,na.rm=TRUE))
data_group_n_wind <- data_group_n_wind[-49,]
#data_group_wind <- cbind(data_group_wind,data_group_n_wind)
#rm(data_group_n_wind)
            
#### For windy days ####
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/time_series_plots/windy_time_series.jpg')
jpeg(file=path_fig,width=2900,height=3600,res=360)
## Creating 5 panels of plots
plot.new()
par(family='Times',mfrow=c(5,1),oma=c(5.1,0.1,0.1,0.1))

# a) LE
par(mai=c(0,0.6,0.1,0.1))
plot(data_group_wind$hour[data_group_wind$windy == TRUE],data_group_wind$LE[data_group_wind$windy == TRUE],
     ylab='LE and H',xlab='',type='l',ylim=c(0,200),lwd=2,xaxt='n',yaxt='n',cex.lab=2,cex.axis=2)
lines(data_group_wind$hour[data_group_wind$windy == TRUE],data_group_wind$H[data_group_wind$windy == TRUE],lty=2,lwd=2)
axis(side=2,at=c(0,100,200),cex.axis=2)
text(-0.5,195,'a)',cex=2)
legend(20,80,y.intersp=1,bty='n',lty=c(1,2),lwd=c(2,2,2),
       c(expression('LE'),expression('H')),cex=2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

# b) Vapor pressure (kPa)
par(mai=c(0,0.6,0.1,0.1))
plot(data_group_wind$hour[data_group_wind$windy == TRUE],data_group_wind$es[data_group_wind$windy == TRUE],
     ylab='e',xlab='',type='l',lwd=2,ylim=c(-0.5,3.5),xaxt='n',yaxt='n',cex.lab=2,lty=2,cex.axis=2)
lines(data_group_wind$hour[data_group_wind$windy == TRUE],data_group_wind$ea[data_group_wind$windy == TRUE],
      ylab=expression('e'['a']),type='l',lty=1,lwd=2)
lines(data_group_wind$hour[data_group_wind$windy == TRUE],data_group_wind$deltaE[data_group_wind$windy == TRUE],
      ylab=expression(paste(Delta,'e')),type='l',lty=3,lwd=2)
text(-0.5,3.40,'b)',cex=2)
legend(19.8,0.9,y.intersp=0.5,bty='n',lty=c(2,1,3),lwd=c(2,2,2),
       c(expression('e'['s']),expression('e'['a']),expression(paste(Delta,'e'))),cex=2.2)
axis(side=2,at=c(0,1,2,3),cex.axis=2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

# c) zeta
par(mai=c(0,0.6,0.1,0.1))
plot(data_group_wind$hour[data_group_wind$windy == TRUE],data_group_wind$Z.L[data_group_wind$windy == TRUE],
     ylab=expression(zeta),xlab='',type='l',lwd=2,ylim=c(-1.5,0.5),xaxt ='n',yaxt='n',cex.lab=2,cex.axis=2)
text(-0.5,0.45,'c)',cex=2)
axis(side=2,at=c(-1,0),cex.axis=2)
minor.tick(ny=5,nx=5,tick.ratio=0.5)


# d) Temperature
par(mai=c(0,0.6,0.1,0.1))
plot(data_group_wind$hour[data_group_wind$windy == TRUE],data_group_wind$Ta[data_group_wind$windy == TRUE],
     ylab='T',xlab='',type='l',ylim=c(5,30),lwd=2,xaxt='n',cex.lab=2,cex.axis=2)
lines(data_group_wind$hour[data_group_wind$windy == TRUE],data_group_wind$water_temp[data_group_wind$windy == TRUE],ylab='T',type='l',lty=2,lwd=2)
legend(20,32,y.intersp=1,bty='n',lty=c(1,2),lwd=c(2,2),c(expression('T'['a']),expression('T'['s'])),cex=2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
text(-0.5,29,'d)',cex=2)

# e) Wind speed
par(mai=c(0,0.6,0.1,0.1))
plot(data_group_wind$hour[data_group_wind$windy == TRUE],data_group_wind$WS[data_group_wind$windy == TRUE],
     ylab='U',xlab='',type='l',lwd=2,ylim=c(0,10),cex.lab=2,xaxt='n',cex.axis=2)
text(-0.5,9.5,'e)',cex=2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=2)
title(xlab='Hour (local time)',ylab='',outer=TRUE,cex.lab=2)

rm(path_fig)

dev.off()

#### For non-windy days ####

path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/time_series_plots/non_windy_time_series.jpg')
jpeg(file=path_fig,width=2900,height=3600,res=360)
## Creating 5 panels of plots
plot.new()
par(family='Times',mfrow=c(5,1),oma=c(5.1,0.1,0.1,0.1))

# a) LE
par(mai=c(0,0.6,0.1,0.1))
plot(data_group_n_wind$hour[data_group_n_wind$n_windy == TRUE],data_group_n_wind$LE[data_group_n_wind$n_windy == TRUE],
     ylab='LE and H',xlab='',type='l',ylim=c(0,200),lwd=2,xaxt='n',yaxt='n',cex.lab=2,cex.axis=2)
lines(data_group_n_wind$hour[data_group_n_wind$n_windy == TRUE],data_group_n_wind$H[data_group_n_wind$n_windy == TRUE],lty=2,lwd=2)
text(-0.5,195,'a)',cex=2)
axis(side=2,at=c(0,100,200),cex.axis=2)
legend(20,210,y.intersp=1,bty='n',lty=c(1,2),lwd=c(2,2,2),
       c(expression('LE'),expression('H')),cex=2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

# b) Vapor pressure (kPa)
par(mai=c(0,0.6,0.1,0.1))
plot(data_group_n_wind$hour[data_group_n_wind$n_windy == TRUE],data_group_n_wind$es[data_group_n_wind$n_windy == TRUE],
     ylab='e',xlab='',type='l',lwd=2,ylim=c(-0.5,3.5),xaxt='n',yaxt='n',cex.lab=2,lty=2,cex.axis=2)
lines(data_group_n_wind$hour[data_group_n_wind$n_windy == TRUE],data_group_n_wind$ea[data_group_n_wind$n_windy == TRUE],
      ylab=expression('e'['a']),type='l',lty=1,lwd=2)
lines(data_group_n_wind$hour[data_group_n_wind$n_windy == TRUE],data_group_n_wind$deltaE[data_group_n_wind$n_windy == TRUE],
      ylab=expression(paste(Delta,'e')),type='l',lty=3,lwd=2)
text(-0.5,3.40,'b)',cex=2)
legend(19.8,0.9,y.intersp=0.5,bty='n',lty=c(2,1,3),lwd=c(2,2,2),
       c(expression('e'['s']),expression('e'['a']),expression(paste(Delta,'e'))),cex=2.2)
axis(side=2,at=c(0,1,2,3),cex.axis=2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

# c) zeta
par(mai=c(0,0.6,0.1,0.1))
plot(data_group_n_wind$hour[data_group_n_wind$n_windy == TRUE],data_group_n_wind$Z.L[data_group_n_wind$n_windy == TRUE],
     ylab=expression(zeta),xlab='',type='l',lwd=2,ylim=c(-1.5,0.5),xaxt ='n',yaxt='n',cex.lab=2,cex.axis=2)
text(-0.5,0.45,'c)',cex=2)
axis(side=2,at=c(-1,0),cex.axis=2)
minor.tick(ny=5,nx=5,tick.ratio=0.5)


# d) Temperature
par(mai=c(0,0.6,0.1,0.1))
plot(data_group_n_wind$hour[data_group_n_wind$n_windy == TRUE],data_group_n_wind$Ta[data_group_n_wind$n_windy == TRUE],
     ylab='T',xlab='',type='l',ylim=c(5,30),lwd=2,xaxt='n',cex.lab=2,cex.axis=2)
lines(data_group_n_wind$hour[data_group_n_wind$n_windy == TRUE],data_group_n_wind$water_temp[data_group_n_wind$n_windy == TRUE],ylab='T',type='l',lty=2,lwd=2)
legend(20,32,y.intersp=1,bty='n',lty=c(1,2),lwd=c(2,2),c(expression('T'['a']),expression('T'['s'])),cex=2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
text(-0.5,29,'d)',cex=2)

# e) Wind speed
par(mai=c(0,0.6,0.1,0.1))
plot(data_group_n_wind$hour[data_group_n_wind$n_windy == TRUE],data_group_n_wind$WS[data_group_n_wind$n_windy == TRUE],
     ylab='U',xlab='',type='l',lwd=2,ylim=c(0,10),cex.lab=2,xaxt='n',cex.axis=2)
text(-0.5,9.5,'e)',cex=2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=2)

title(xlab='Hour (local time)',ylab='',outer=TRUE,cex.lab=2)

rm(path_fig)

dev.off()

