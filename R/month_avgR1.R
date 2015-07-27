#install.packages('dplyr')
library(Hmisc)
library(dplyr)

# Grouping the data according to year, month, and hour
data_group <- data %>% 
  mutate(time_stamp=as.POSIXct(time_stamp)) %>%
  group_by(year=format(time_stamp, '%Y'),
           month=format(time_stamp, '%m'),
           hour=format(as.POSIXlt(cut(time_stamp,breaks='hour')),'%H')) %>%
  summarise(LE=mean(LE,na.rm=TRUE), H = mean(H,na.rm=TRUE), Z.L = mean(Z.L,na.rm=TRUE),
            U. = mean(U.,na.rm=TRUE),WS = mean(WS_Spd_WVT,na.rm=TRUE),water_temp=mean(Water.surface.temperature,na.rm=TRUE),
            qs=mean(qs,na.rm=TRUE),qa=mean(qa,na.rm=TRUE),deltaQ=mean(deltaQ,na.rm=TRUE))# %>%

data_group_sd <- data %>% 
  mutate(time_stamp=as.POSIXct(time_stamp)) %>%
  group_by(year=format(time_stamp, '%Y'),
           month=format(time_stamp, '%m'),
           hour=format(as.POSIXlt(cut(time_stamp,breaks='hour')),'%H')) %>%
  summarise(sdLE = sd(LE,na.rm=TRUE), sdH = sd(H,na.rm=TRUE), sdZ.L = sd(Z.L,na.rm=TRUE),
            sdU. = sd(U.,na.rm=TRUE),sdWS = sd(WS_Spd_WVT,na.rm=TRUE),sdwater_temp=sd(Water.surface.temperature,na.rm=TRUE),
            sdqs=sd(qs,na.rm=TRUE),sdqa=sd(qa,na.rm=TRUE),sddeltaQ=sd(deltaQ,na.rm=TRUE))
data_group <- cbind(data_group,data_group_sd)
rm(data_group_sd)
            
#### Testing out plots ####

path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/time_series_plots/2008_02.jpg')
jpeg(file=path_fig,width=3060,height=3060,res=360)
## Creating 6 panels of plots
plot.new()
par(mfrow=c(3,2),mar=c(4.1,4.1,2.1,1.1))

# Latent (LE) fluxes
errbar(as.numeric(data_group$hour[data_group$month=='02' & data_group$year == '2008']),
       data_group$LE[data_group$month=='02' & data_group$year == '2008'],
       data_group$LE[data_group$month=='02' & data_group$year == '2008'] + data_group$sdLE[data_group$month=='02' & data_group$year == '2008'],
       data_group$LE[data_group$month=='02' & data_group$year == '2008'] - data_group$sdLE[data_group$month=='02' & data_group$year == '2008'], 
       main='2008-02',xlab='Hour (local time)',ylab='LE',type='l',ylim=c(-100,200))
minor.tick(ny=10,nx=10,tick.ratio=0.5)

# Sensible (H) fluxes
errbar(as.numeric(data_group$hour[data_group$month=='02' & data_group$year == '2008']),
       data_group$H[data_group$month=='02' & data_group$year == '2008'],
       data_group$H[data_group$month=='02' & data_group$year == '2008'] + data_group$sdH[data_group$month=='02' & data_group$year == '2008'],
       data_group$H[data_group$month=='02' & data_group$year == '2008'] - data_group$sdH[data_group$month=='02' & data_group$year == '2008'], 
       main='2008-02',xlab='Hour (local time)',ylab='H',type='l',ylim=c(-100,200))
minor.tick(ny=10,nx=10,tick.ratio=0.5)

# Atmospheric stability (zeta)
errbar(as.numeric(data_group$hour[data_group$month=='02' & data_group$year == '2008']),
       data_group$Z.L[data_group$month=='02' & data_group$year == '2008'],
       data_group$Z.L[data_group$month=='02' & data_group$year == '2008'] + data_group$sdZ.L[data_group$month=='02' & data_group$year == '2008'],
       data_group$Z.L[data_group$month=='02' & data_group$year == '2008'] - data_group$sdZ.L[data_group$month=='02' & data_group$year == '2008'], 
       main='2008-02',xlab='Hour (local time)',ylab=expression(zeta),type='l',ylim=c(-2,1))
minor.tick(ny=10,nx=10,tick.ratio=0.5)

# Friction velocity
#plot(data_group$hour[data_group$month=='02' & data_group$year == '2008'],
#     data_group$U.[data_group$month=='02' & data_group$year == '2008'],
#     main='2008-02',xlab='Hour (local time)',ylab=expression('u'['*']),type='l',ylim=c(0,0.25))

# Wind speed
errbar(as.numeric(data_group$hour[data_group$month=='02' & data_group$year == '2008']),
       data_group$WS[data_group$month=='02' & data_group$year == '2008'],
       data_group$WS[data_group$month=='02' & data_group$year == '2008'] + data_group$sdWS[data_group$month=='02' & data_group$year == '2008'],
       data_group$WS[data_group$month=='02' & data_group$year == '2008'] - data_group$sdWS[data_group$month=='02' & data_group$year == '2008'], 
       main='2008-02',xlab='Hour (local time)',ylab='U',type='l',ylim=c(0,10))
minor.tick(ny=10,nx=10,tick.ratio=0.5)

# Water surface temperature
errbar(as.numeric(data_group$hour[data_group$month=='02' & data_group$year == '2008']),
       data_group$water_temp[data_group$month=='02' & data_group$year == '2008'],
       data_group$water_temp[data_group$month=='02' & data_group$year == '2008'] + data_group$sdwater_temp[data_group$month=='02' & data_group$year == '2008'],
       data_group$water_temp[data_group$month=='02' & data_group$year == '2008'] - data_group$sdwater_temp[data_group$month=='02' & data_group$year == '2008'], 
       main='2008-02',xlab='Hour (local time)',ylab=expression('T'['S']),type='l',ylim=c(0,35))
minor.tick(ny=10,nx=10,tick.ratio=0.5)

# Specific humidity
plot(data_group$hour[data_group$month=='02' & data_group$year == '2008'],
     data_group$qs[data_group$month=='02' & data_group$year == '2008'],
     main='2008-02',xlab='Hour (local time)',ylab='Specific humidity',type='l',ylim=c(0,0.035))
lines(data_group$hour[data_group$month=='02' & data_group$year == '2008'],
     data_group$qa[data_group$month=='02' & data_group$year == '2008'],
     main='2008-02',xlab='Hour (local time)',ylab=expression('q'['a']),type='l',ylim=c(0,0.020),lty=2)
lines(data_group$hour[data_group$month=='02' & data_group$year == '2008'],
     data_group$deltaQ[data_group$month=='02' & data_group$year == '2008'],
     main='2008-02',xlab='Hour (local time)',ylab=expression(paste(Delta,'q')),type='l',ylim=c(0,0.015),lty=3)

rm(path_fig)

dev.off()