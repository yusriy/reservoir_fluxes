# To separate data into the 4 categories of winds
#install.packages('dplyr')
library(dplyr)
library(Hmisc)

# Grouping the data according the 4 categories of winds
data_group_4wind <- data %>% 
  mutate(time_stamp=as.POSIXct(time_stamp)) %>%
  group_by(wind_category_day,
           hour=format(as.POSIXlt(cut(time_stamp,breaks='hour')),'%H')) %>%
  summarise(LE=mean(LE,na.rm=TRUE), H= mean(H,na.rm=TRUE),Z.L = mean(Z.L,na.rm=TRUE),
            WS = mean(WS_Spd_WVT,na.rm=TRUE),water_temp=mean(Water.surface.temperature,na.rm=TRUE),
            es=mean(e_s1,na.rm=TRUE) * 0.1 ,ea=mean(e_a,na.rm=TRUE) * 0.1,deltaE=mean(deltaE,na.rm=TRUE),
            Ta=mean(t_hmp_3_Avg,na.rm=TRUE))# need to multiply e_s1 and e_a with 0.1 because units in hPa not kPa
data_group_4wind <- data_group_4wind[-98,]

#### For wind category day 1 ####
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/wind_figs/wind_cat_1.jpg')
jpeg(file=path_fig,width=1450,height=1800,res=320)
## Creating 5 panels of plots
plot.new()
par(family='Times',mfrow=c(5,1),oma=c(0.4,0.1,1.3,0.1))

# a) LE
par(mai=c(0,0.6,0.1,0.1))
plot(data_group_4wind$hour[data_group_4wind$wind_category_day == 1],data_group_4wind$LE[data_group_4wind$wind_category_day == 1],
     ylab='LE and H',xlab='',type='l',ylim=c(0,200),lwd=2,xaxt='n',yaxt='n',cex.lab=2,cex.axis=2)
lines(data_group_4wind$hour[data_group_4wind$wind_category_day == 1],data_group_4wind$H[data_group_4wind$wind_category_day == 1],lty=2,lwd=2)
axis(side=2,at=c(0,100,200),cex.axis=2)
#text(4,180,'a) Wind-class A',cex=2)
legend(-1,220,bty='n',lty=c(1,2),lwd=c(2,2,2),
       c(expression('LE'),expression('H')),cex=1.5)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
title(main='\t\t\t\t\t\t\t\t\ta) Wind-class Day A',outer=TRUE,cex.main=2,font.main=1,adj=0)

# b) Vapor pressure (kPa)
par(mai=c(0,0.6,0.1,0.1))
plot(data_group_4wind$hour[data_group_4wind$wind_category_day == 1],data_group_4wind$es[data_group_4wind$wind_category_day == 1],
     ylab='e',xlab='',type='l',lwd=2,ylim=c(-0.5,5.0),xaxt='n',yaxt='n',cex.lab=2,lty=2,cex.axis=2)
lines(data_group_4wind$hour[data_group_4wind$wind_category_day == 1],data_group_4wind$ea[data_group_4wind$wind_category_day == 1],
      ylab=expression('e'['a']),type='l',lty=1,lwd=2)
lines(data_group_4wind$hour[data_group_4wind$wind_category_day == 1],data_group_4wind$deltaE[data_group_4wind$wind_category_day == 1],
      ylab=expression(paste(Delta,'e')),type='l',lty=3,lwd=2)
#text(0,3.1,'b)',cex=2)
legend(-1,5.6,bty='n',lty=c(2,1,3),lwd=c(2,2,2),y.intersp=0.5,
       c(expression('e'['s']),expression('e'['a']),expression(paste(Delta,'e'))),cex=1.8)
axis(side=2,at=c(0,1,2,3,4,5),cex.axis=2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

# c) zeta
par(mai=c(0,0.6,0.1,0.1))
plot(data_group_4wind$hour[data_group_4wind$wind_category_day == 1],data_group_4wind$Z.L[data_group_4wind$wind_category_day == 1],
     ylab=expression(zeta),xlab='',type='l',lwd=2,ylim=c(-1.5,0.5),xaxt ='n',yaxt='n',cex.lab=2,cex.axis=2)
#text(-0.5,0.45,'c)',cex=2)
axis(side=2,at=c(-1,0),cex.axis=2)
minor.tick(ny=5,nx=5,tick.ratio=0.5)


# d) Temperature
par(mai=c(0,0.6,0.1,0.1))
plot(data_group_4wind$hour[data_group_4wind$wind_category_day == 1],data_group_4wind$Ta[data_group_4wind$wind_category_day == 1],
     ylab='T',xlab='',type='l',ylim=c(5,30),lwd=2,xaxt='n',yaxt='n',cex.lab=2,cex.axis=2)
lines(data_group_4wind$hour[data_group_4wind$wind_category_day == 1],data_group_4wind$water_temp[data_group_4wind$wind_category_day == 1],ylab='T',type='l',lty=2,lwd=2)
legend(-1,34,y.intersp=0.8,bty='n',lty=c(1,2),lwd=c(2,2),c(expression('T'['a']),expression('T'['s'])),cex=2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=2,at=c(10,30),cex.axis=2)
axis(side=2,at=20,cex.axis=2)
#text(-0.5,29,'d)',cex=2)

# e) Wind speed
par(mai=c(0,0.6,0.1,0.1))
plot(data_group_4wind$hour[data_group_4wind$wind_category_day == 1],data_group_4wind$WS[data_group_4wind$wind_category_day == 1],
     ylab='U',xlab='',type='l',lwd=2,ylim=c(0,10),cex.lab=2,xaxt='n',cex.axis=2)
#text(-0.5,9.5,'e)',cex=2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=2)
#title(xlab='Hour (local time)',ylab='',outer=TRUE,cex.lab=2)

rm(path_fig)

dev.off()

#### For wind category day 2 ####

path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/wind_figs/wind_cat_2.jpg')
jpeg(file=path_fig,width=1450,height=1800,res=320)
## Creating 5 panels of plots
plot.new()
par(family='Times',mfrow=c(5,1),oma=c(0.4,0.1,1.3,0.1))

# a) LE
par(mai=c(0,0.6,0.1,0.1))
plot(data_group_4wind$hour[data_group_4wind$wind_category_day == 2],data_group_4wind$LE[data_group_4wind$wind_category_day == 2],
     ylab='',xlab='',type='l',ylim=c(0,200),lwd=2,xaxt='n',yaxt='n',cex.lab=2,cex.axis=2)
lines(data_group_4wind$hour[data_group_4wind$wind_category_day == 2],data_group_4wind$H[data_group_4wind$wind_category_day == 2],lty=2,lwd=2)
#axis(side=2,at=c(0,100,200),cex.axis=2)
#text(-0.5,195,'a)',cex=2)
#legend(20,80,y.intersp=1,bty='n',lty=c(1,2),lwd=c(2,2,2),
#       c(expression('LE'),expression('H')),cex=2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
title(main='\t\t\t\t\t\t\t\t\tb) Wind-class Day B',outer=TRUE,cex.main=2,font.main=1,adj=0)

# b) Vapor pressure (kPa)
par(mai=c(0,0.6,0.1,0.1))
plot(data_group_4wind$hour[data_group_4wind$wind_category_day == 2],data_group_4wind$es[data_group_4wind$wind_category_day == 2],
     ylab='',xlab='',type='l',lwd=2,ylim=c(-0.5,5),xaxt='n',yaxt='n',cex.lab=2,lty=2,cex.axis=2)
lines(data_group_4wind$hour[data_group_4wind$wind_category_day == 2],data_group_4wind$ea[data_group_4wind$wind_category_day == 2],
      ylab=expression('e'['a']),type='l',lty=1,lwd=2)
lines(data_group_4wind$hour[data_group_4wind$wind_category_day == 2],data_group_4wind$deltaE[data_group_4wind$wind_category_day == 2],
      ylab=expression(paste(Delta,'e')),type='l',lty=3,lwd=2)
#text(-0.5,3.40,'b)',cex=2)
#legend(19.8,0.9,y.intersp=0.5,bty='n',lty=c(2,1,3),lwd=c(2,2,2),
#       c(expression('e'['s']),expression('e'['a']),expression(paste(Delta,'e'))),cex=2.2)
#axis(side=2,at=c(0,1,2,3,4),cex.axis=2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

# c) zeta
par(mai=c(0,0.6,0.1,0.1))
plot(data_group_4wind$hour[data_group_4wind$wind_category_day == 2],data_group_4wind$Z.L[data_group_4wind$wind_category_day == 2],
     ylab='',xlab='',type='l',lwd=2,ylim=c(-1.5,0.5),xaxt ='n',yaxt='n',cex.lab=2,cex.axis=2)
#text(-0.5,0.45,'c)',cex=2)
#axis(side=2,at=c(-1,0),cex.axis=2)
minor.tick(ny=5,nx=5,tick.ratio=0.5)


# d) Temperature
par(mai=c(0,0.6,0.1,0.1))
plot(data_group_4wind$hour[data_group_4wind$wind_category_day == 2],data_group_4wind$Ta[data_group_4wind$wind_category_day == 2],
     ylab='',xlab='',type='l',ylim=c(5,30),lwd=2,xaxt='n',yaxt='n',cex.lab=2,cex.axis=2)
lines(data_group_4wind$hour[data_group_4wind$wind_category_day == 2],data_group_4wind$water_temp[data_group_4wind$wind_category_day == 2],ylab='T',type='l',lty=2,lwd=2)
#legend(20,32,y.intersp=1,bty='n',lty=c(1,2),lwd=c(2,2),c(expression('T'['a']),expression('T'['s'])),cex=2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
#axis(side=2,at=c(10,30),cex.axis=2)
#axis(side=2,at=c(20),cex.axis=2)
#text(-0.5,29,'d)',cex=2)

# e) Wind speed
par(mai=c(0,0.6,0.1,0.1))
plot(data_group_4wind$hour[data_group_4wind$wind_category_day == 2],data_group_4wind$WS[data_group_4wind$wind_category_day == 2],
     ylab='',xlab='',type='l',lwd=2,ylim=c(0,10),cex.lab=2,xaxt='n',yaxt='n',cex.axis=2)
#text(-0.5,9.5,'e)',cex=2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
#axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=2)
#title(xlab='Hour (local time)',ylab='',outer=TRUE,cex.lab=2)

rm(path_fig)

dev.off()

#### For wind category day 3 ####
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/wind_figs/wind_cat_3.jpg')
jpeg(file=path_fig,width=1450,height=1800,res=320)
## Creating 5 panels of plots
plot.new()
par(family='Times',mfrow=c(5,1),oma=c(5.1,0.1,1.3,0.1))

# a) LE
par(mai=c(0,0.6,0.1,0.1))
plot(data_group_4wind$hour[data_group_4wind$wind_category_day == 3],data_group_4wind$LE[data_group_4wind$wind_category_day == 3],
     ylab='LE and H',xlab='',type='l',ylim=c(0,200),lwd=2,xaxt='n',yaxt='n',cex.lab=2,cex.axis=2)
lines(data_group_4wind$hour[data_group_4wind$wind_category_day == 3],data_group_4wind$H[data_group_4wind$wind_category_day == 3],lty=2,lwd=2)
axis(side=2,at=c(0,100,200),cex.axis=2)
#text(-0.5,195,'a)',cex=2)
legend(20,80,y.intersp=1,bty='n',lty=c(1,2),lwd=c(2,2,2),
       c(expression('LE'),expression('H')),cex=2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
title(main='\t\t\t\t\t\t\t\t\tc) Wind-class Day C',outer=TRUE,cex.main=2,font.main=1,adj=0)


# b) Vapor pressure (kPa)
par(mai=c(0,0.6,0.1,0.1))
plot(data_group_4wind$hour[data_group_4wind$wind_category_day == 3],data_group_4wind$es[data_group_4wind$wind_category_day == 3],
     ylab='e',xlab='',type='l',lwd=2,ylim=c(-0.5,5),xaxt='n',yaxt='n',cex.lab=2,lty=2,cex.axis=2)
lines(data_group_4wind$hour[data_group_4wind$wind_category_day == 3],data_group_4wind$ea[data_group_4wind$wind_category_day == 3],
      ylab=expression('e'['a']),type='l',lty=1,lwd=2)
lines(data_group_4wind$hour[data_group_4wind$wind_category_day == 3],data_group_4wind$deltaE[data_group_4wind$wind_category_day == 3],
      ylab=expression(paste(Delta,'e')),type='l',lty=3,lwd=2)
#text(-0.5,3.40,'b)',cex=2)
#legend(19.8,0.9,y.intersp=0.5,bty='n',lty=c(2,1,3),lwd=c(2,2,2),
#       c(expression('e'['s']),expression('e'['a']),expression(paste(Delta,'e'))),cex=2.2)
axis(side=2,at=c(0,1,2,3,4),cex.axis=2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

# c) zeta
par(mai=c(0,0.6,0.1,0.1))
plot(data_group_4wind$hour[data_group_4wind$wind_category_day == 3],data_group_4wind$Z.L[data_group_4wind$wind_category_day == 3],
     ylab=expression(zeta),xlab='',type='l',lwd=2,ylim=c(-1.5,0.5),xaxt ='n',yaxt='n',cex.lab=2,cex.axis=2)
#text(-0.5,0.45,'c)',cex=2)
axis(side=2,at=c(-1,0),cex.axis=2)
minor.tick(ny=5,nx=5,tick.ratio=0.5)


# d) Temperature
par(mai=c(0,0.6,0.1,0.1))
plot(data_group_4wind$hour[data_group_4wind$wind_category_day == 3],data_group_4wind$Ta[data_group_4wind$wind_category_day == 3],
     ylab='T',xlab='',type='l',ylim=c(5,30),lwd=2,xaxt='n',yaxt='n',cex.lab=2,cex.axis=2)
lines(data_group_4wind$hour[data_group_4wind$wind_category_day == 3],data_group_4wind$water_temp[data_group_4wind$wind_category_day == 3],ylab='T',type='l',lty=2,lwd=2)
#legend(20,32,y.intersp=1,bty='n',lty=c(1,2),lwd=c(2,2),c(expression('T'['a']),expression('T'['s'])),cex=2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=2,at=c(10,30),cex.axis=2)
axis(side=2,at=c(20),cex.axis=2)
#text(-0.5,29,'d)',cex=2)

# e) Wind speed
par(mai=c(0,0.6,0.1,0.1))
plot(data_group_4wind$hour[data_group_4wind$wind_category_day == 3],data_group_4wind$WS[data_group_4wind$wind_category_day == 3],
     ylab='U',xlab='',type='l',lwd=2,ylim=c(0,10),cex.lab=2,xaxt='n',cex.axis=2)
#text(-0.5,9.5,'e)',cex=2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=2)
title(xlab='Hour (local time)',ylab='',outer=TRUE,cex.lab=2)

rm(path_fig)

dev.off()

#### For wind category day 4 ####

path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/wind_figs/wind_cat_4.jpg')
jpeg(file=path_fig,width=1450,height=1800,res=320)
## Creating 5 panels of plots
plot.new()
par(family='Times',mfrow=c(5,1),oma=c(5.1,0.1,1.3,0.1))

# a) LE
par(mai=c(0,0.6,0.1,0.1))
plot(data_group_4wind$hour[data_group_4wind$wind_category_day == 4],data_group_4wind$LE[data_group_4wind$wind_category_day == 4],
     ylab='',xlab='',type='l',ylim=c(0,200),lwd=2,xaxt='n',yaxt='n',cex.lab=2,cex.axis=2)
lines(data_group_4wind$hour[data_group_4wind$wind_category_day == 4],data_group_4wind$H[data_group_4wind$wind_category_day == 4],lty=2,lwd=2)
#axis(side=2,at=c(0,100,200),cex.axis=2)
#text(-0.5,195,'a)',cex=2)
#legend(20,80,y.intersp=1,bty='n',lty=c(1,2),lwd=c(2,2,2),
#       c(expression('LE'),expression('H')),cex=2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
title(main='\t\t\t\t\t\t\t\t\td) Wind-class Day D',outer=TRUE,cex.main=2,font.main=1,adj=0)


# b) Vapor pressure (kPa)
par(mai=c(0,0.6,0.1,0.1))
plot(data_group_4wind$hour[data_group_4wind$wind_category_day == 4],data_group_4wind$es[data_group_4wind$wind_category_day == 4],
     ylab='',xlab='',type='l',lwd=2,ylim=c(-0.5,5),xaxt='n',yaxt='n',cex.lab=2,lty=2,cex.axis=2)
lines(data_group_4wind$hour[data_group_4wind$wind_category_day == 4],data_group_4wind$ea[data_group_4wind$wind_category_day == 4],
      ylab=expression('e'['a']),type='l',lty=1,lwd=2)
lines(data_group_4wind$hour[data_group_4wind$wind_category_day == 4],data_group_4wind$deltaE[data_group_4wind$wind_category_day == 4],
      ylab=expression(paste(Delta,'e')),type='l',lty=3,lwd=2)
#text(-0.5,3.40,'b)',cex=2)
#legend(19.8,0.9,y.intersp=0.5,bty='n',lty=c(2,1,3),lwd=c(2,2,2),
#       c(expression('e'['s']),expression('e'['a']),expression(paste(Delta,'e'))),cex=2.2)
#axis(side=2,at=c(0,1,2,3,4),cex.axis=2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

# c) zeta
par(mai=c(0,0.6,0.1,0.1))
plot(data_group_4wind$hour[data_group_4wind$wind_category_day == 4],data_group_4wind$Z.L[data_group_4wind$wind_category_day == 4],
     ylab='',xlab='',type='l',lwd=2,ylim=c(-1.5,0.5),xaxt ='n',yaxt='n',cex.lab=2,cex.axis=2)
#text(-0.5,0.45,'c)',cex=2)
#axis(side=2,at=c(-1,0),cex.axis=2)
minor.tick(ny=5,nx=5,tick.ratio=0.5)


# d) Temperature
par(mai=c(0,0.6,0.1,0.1))
plot(data_group_4wind$hour[data_group_4wind$wind_category_day == 4],data_group_4wind$Ta[data_group_4wind$wind_category_day == 4],
     ylab='',xlab='',type='l',ylim=c(5,30),lwd=2,xaxt='n',yaxt='n',cex.lab=2,cex.axis=2)
lines(data_group_4wind$hour[data_group_4wind$wind_category_day == 4],data_group_4wind$water_temp[data_group_4wind$wind_category_day == 4],ylab='T',type='l',lty=2,lwd=2)
#legend(20,32,y.intersp=1,bty='n',lty=c(1,2),lwd=c(2,2),c(expression('T'['a']),expression('T'['s'])),cex=2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
#text(-0.5,29,'d)',cex=2)
#axis(side=2,at=c(10,30),cex.axis=2)
#axis(side=2,at=c(20),cex.axis=2)

# e) Wind speed
par(mai=c(0,0.6,0.1,0.1))
plot(data_group_4wind$hour[data_group_4wind$wind_category_day == 4],data_group_4wind$WS[data_group_4wind$wind_category_day == 4],
     ylab='',xlab='',type='l',lwd=2,ylim=c(0,10),cex.lab=2,xaxt='n',yaxt='n',cex.axis=2)
#text(-0.5,9.5,'e)',cex=2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
axis(side=1,at=c(0,3,6,9,12,15,18,21,24),cex.axis=2)
title(xlab='Hour (local time)',ylab='',outer=TRUE,cex.lab=2)

rm(path_fig)

dev.off()