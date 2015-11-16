#install.packages('dplyr')
library(dplyr)
library(Hmisc)

# Grouping the data according month Sept 2007
data_group <- data %>% 
  mutate(time_stamp=as.POSIXct(time_stamp)) %>%
  group_by(month=format(time_stamp, '%m'),
           hour=format(as.POSIXlt(cut(time_stamp,breaks='hour')),'%H')) %>%
  summarise(LE=mean(LE,na.rm=TRUE), Z.L = mean(Z.L,na.rm=TRUE),
            WS = mean(WS_Spd_WVT,na.rm=TRUE),water_temp=mean(Water.surface.temperature,na.rm=TRUE),
            es=mean(e_s1,na.rm=TRUE) * 0.1 ,ea=mean(e_a,na.rm=TRUE) * 0.1,deltaE=mean(deltaE,na.rm=TRUE),
            Ta=mean(t_hmp_3_Avg,na.rm=TRUE))# need to multiply e_s1 and e_a with 0.1 because units in hPa not kPa

data_group_sd <- data %>% 
  mutate(time_stamp=as.POSIXct(time_stamp)) %>%
  group_by(month=format(time_stamp, '%m'),
           hour=format(as.POSIXlt(cut(time_stamp,breaks='hour')),'%H')) %>%
  summarise(sdLE = sd(LE,na.rm=TRUE), sdZ.L = sd(Z.L,na.rm=TRUE),
            sdWS = sd(WS_Spd_WVT,na.rm=TRUE),sdwater_temp=sd(Water.surface.temperature,na.rm=TRUE),
            sdes=sd(e_s1,na.rm=TRUE),sdea=sd(e_a,na.rm=TRUE),sddeltaE=sd(deltaE,na.rm=TRUE),
            sdTa=sd(t_hmp_3_Avg,na.rm=TRUE))
data_group <- cbind(data_group,data_group_sd)
rm(data_group_sd)
            
#### Seasonal hourly time series plot ####

path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/time_series_plots/final_time_series.jpg')
jpeg(file=path_fig,width=2900,height=3600,res=360)
## Creating 5 panels of plots
plot.new()
par(mfrow=c(5,1),oma=c(5.1,0.1,0.1,0.1))

# a) LE
par(mai=c(0,0.6,0.1,0.1))
plot(data_group$hour[data_group$month == '09'],
     data_group$LE[data_group$month == '09'],
     ylab='LE',xlab='',type='l',ylim=c(80,150),lwd=2,xaxt='n',cex.lab=2)
text(0,148,'a)',cex=2)
minor.tick(ny=5,nx=5,tick.ratio=0.5)

# b) Vapor pressure (kPa)
par(mai=c(0,0.6,0.1,0.1))
plot(data_group$hour[data_group$month == '09'],
     data_group$es[data_group$month == '09'],
     ylab='e',xlab='',type='l',lwd=2,ylim=c(0,5),xaxt='n',cex.lab=2,lty=2)
lines(data_group$hour[data_group$month == '09'],
      data_group$ea[data_group$month == '09'],
      ylab=expression('e'['a']),type='l',lty=1,lwd=2)
lines(data_group$hour[data_group$month == '09'],
      data_group$deltaE[data_group$month == '09'],
      ylab=expression(paste(Delta,'e')),type='l',lty=3,lwd=2)
text(0,4.8,'b)',cex=2)
legend(12,1.8,y.intersp=0.5,bty='n',lty=c(2,1,3),lwd=c(2,2,2),
       c(expression('e'['s']),expression('e'['a']),expression(paste(Delta,'e'))),cex=2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

# c) zeta
par(mai=c(0,0.6,0.1,0.1))
plot(data_group$hour[data_group$month == '09'],
     data_group$Z.L[data_group$month == '09'],
     ylab=expression(zeta),xlab='',type='l',lwd=2,ylim=c(-1,0.5),xaxt ='n',cex.lab=2)
text(0,0.45,'c)',cex=2)
minor.tick(ny=5,nx=5,tick.ratio=0.5)


# d) Temperature
par(mai=c(0,0.6,0.1,0.1))
plot(data_group$hour[data_group$month == '09'],
     data_group$Ta[data_group$month == '09'],
     ylab='T',xlab='',type='l',ylim=c(15,35),lwd=2,xaxt='n',cex.lab=2)
lines(data_group$hour[data_group$month == '09'],
      data_group$water_temp[data_group$month == '09'],ylab='T',type='l',lty=2,lwd=2)
legend(20,22.5,y.intersp=1,bty='n',lty=c(1,2),lwd=c(2,2),c(expression('T'['a']),expression('T'['s'])),cex=2)
minor.tick(ny=5,nx=5,tick.ratio=0.5)
text(0,34,'d)',cex=2)

# e) Wind speed
par(mai=c(0,0.6,0.1,0.1))
plot(data_group$hour[data_group$month == '09'],
     data_group$WS[data_group$month == '09'],
     ylab='U',xlab='',type='l',lwd=2,ylim=c(2,5),cex.lab=2)
text(0,4.9,'e)',cex=2)
minor.tick(ny=5,nx=5,tick.ratio=0.5)

title(xlab='Hour (local time)',ylab='',outer=TRUE,cex.lab=2)

rm(path_fig)

dev.off()