#install.packages('dplyr')
library(dplyr)

# Grouping the data according to year, month, and hour, season
data_group <- data %>% 
  mutate(time_stamp=as.POSIXct(time_stamp)) %>%
  group_by(year=format(time_stamp, '%Y'),
           month=format(time_stamp, '%m'),
           hour=format(as.POSIXlt(cut(time_stamp,breaks='hour')),'%H'),
           season) %>%
  summarise(LE=mean(LE,na.rm=TRUE), H = mean(H,na.rm=TRUE), Z.L = mean(Z.L,na.rm=TRUE),
            U. = mean(U.,na.rm=TRUE),WS = mean(WS_Spd_WVT,na.rm=TRUE),water_temp=mean(Water.surface.temperature,na.rm=TRUE),
            P = mean(Press_CS115_Avg,na.rm=TRUE),qs=mean(qs,na.rm=TRUE),qa=mean(qa,na.rm=TRUE),deltaQ=mean(deltaQ,na.rm=TRUE))# %>%

data_group_sd <- data %>% 
  mutate(time_stamp=as.POSIXct(time_stamp)) %>%
  group_by(year=format(time_stamp, '%Y'),
           month=format(time_stamp, '%m'),
           hour=format(as.POSIXlt(cut(time_stamp,breaks='hour')),'%H')) %>%
  summarise(sdLE = sd(LE,na.rm=TRUE), sdH = sd(H,na.rm=TRUE), sdZ.L = sd(Z.L,na.rm=TRUE),
            sdU. = sd(U.,na.rm=TRUE),sdWS = sd(WS_Spd_WVT,na.rm=TRUE),sdwater_temp=sd(Water.surface.temperature,na.rm=TRUE),
            sdqs=sd(qs,na.rm=TRUE),sdqa=sd(qa,na.rm=TRUE),sddeltaQ=sd(deltaQ,na.rm=TRUE),sdP = sd(Press_CS115_Avg,na.rm=TRUE))
data_group <- cbind(data_group,data_group_sd)
rm(data_group_sd)
            
#### Testing out plots ####

path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/time_series_plots/2008_02.jpg')
jpeg(file=path_fig,width=3060,height=3060,res=360)
## Creating 6 panels of plots
plot.new()
par(mfrow=c(5,2),mar=c(4.1,4.1,2.1,1.1))

# Latent and sensible heat fluxes
plot(data_group$hour[data_group$month=='02' & data_group$year == '2008'],
     data_group$LE[data_group$month=='02' & data_group$year == '2008'],
     main='2008-02',xlab='Hour (local time)',ylab='Energy flux',type='l',ylim=c(-10,200))
lines(data_group$hour[data_group$month=='02' & data_group$year == '2008'],
     data_group$H[data_group$month=='02' & data_group$year == '2008'],
     main='2008-02',xlab='Hour (local time)',ylab='H',type='l',lty=2)

# zeta
plot(data_group$hour[data_group$month=='02' & data_group$year == '2008'],
     data_group$Z.L[data_group$month=='02' & data_group$year == '2008'],
     main='2008-02',xlab='Hour (local time)',ylab=expression(zeta),type='l',ylim=c(-2,1))

# Friction velocity
plot(data_group$hour[data_group$month=='02' & data_group$year == '2008'],
     data_group$U.[data_group$month=='02' & data_group$year == '2008'],
     main='2008-02',xlab='Hour (local time)',ylab=expression('u'['*']),type='l',ylim=c(0,0.25))

# Wind speed
plot(data_group$hour[data_group$month=='02' & data_group$year == '2008'],
     data_group$WS[data_group$month=='02' & data_group$year == '2008'],
     main='2008-02',xlab='Hour (local time)',ylab='U',type='l',ylim=c(0,5))

# Water surface temperature
plot(data_group$hour[data_group$month=='02' & data_group$year == '2008'],
     data_group$water_temp[data_group$month=='02' & data_group$year == '2008'],
     main='2008-02',xlab='Hour (local time)',ylab=expression('T'['S']),type='l',ylim=c(0,35))

# Specific humidity
plot(data_group$hour[data_group$month=='02' & data_group$year == '2008'],
     data_group$qs[data_group$month=='02' & data_group$year == '2008'],
     main='2008-02',xlab='Hour (local time)',ylab='q',type='l',ylim=c(0,0.035))
lines(data_group$hour[data_group$month=='02' & data_group$year == '2008'],
     data_group$qa[data_group$month=='02' & data_group$year == '2008'],
     main='2008-02',xlab='Hour (local time)',ylab=expression('q'['a']),type='l',ylim=c(0,0.020),lty=2)
lines(data_group$hour[data_group$month=='02' & data_group$year == '2008'],
     data_group$deltaQ[data_group$month=='02' & data_group$year == '2008'],
     main='2008-02',xlab='Hour (local time)',ylab=expression(paste(Delta,'q')),type='l',ylim=c(0,0.015),lty=3)

plot(data_group$hour[data_group$month=='02' & data_group$year == '2008'],
     data_group$P[data_group$month=='02' & data_group$year == '2008'],
     main='2008-02',xlab='Hour (local time)',ylab='Pressure',type='l',ylim=c(100,102))



rm(path_fig)

dev.off()