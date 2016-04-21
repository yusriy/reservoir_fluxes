########################################################################
# Title:  Wind speeds and energy fluxes in the above-water surface layer
#         in windy and non-windy days in different ASL ranges
# 
# Data provided by Heping Liu, PhD, from Washington State University (WSU) in 
# MS Excel and converted to csv format within Excel.
# This script is to plot results for second manuscript.
# Need to run "lake_analysisV2_13.R" first to get data
# 
# Data duration: 2007-08-24 to 2008-03-05
#
# Author: Yusri Yusup, PhD
# Affiliation:  Environmental Technology, School of Industrial Technology, 
#               Universiti Sains Malaysia (USM)
# Date created: 2015-11-16
# 
# Version: 1.00
# 

##### Preliminaries #########
library(ggplot2)
library(Hmisc)
library(dplyr)
library(MASS)
library(scales)
source("R/tools/multiplot2.R")

# Load and processed the data if needed
#source('R/lake_analysisV2_13.R')


##### Data preparation to sperate into the 4 wind class days ######

# Grouping the data according the 4 categories of winds to plot time series
data_group_4wind <- data %>% 
  mutate(time_stamp=as.POSIXct(time_stamp)) %>%
  group_by(wind_category_day,
           hour=format(as.POSIXlt(cut(time_stamp,breaks='hour')),'%H')) %>%
  summarise(LE=mean(LE,na.rm=TRUE), H= mean(H,na.rm=TRUE),Z.L = mean(Z.L,na.rm=TRUE),
            WS = mean(WS_Spd_WVT,na.rm=TRUE),water_temp=mean(Water.surface.temperature,na.rm=TRUE),
            es=mean(e_s1,na.rm=TRUE) * 0.1 ,ea=mean(e_a,na.rm=TRUE) * 0.1,deltaE=mean(deltaE,na.rm=TRUE),
            Ta=mean(t_hmp_3_Avg,na.rm=TRUE),Rn=mean(Rn_Q71_Avg,na.rm=TRUE))
            # need to multiply e_s1 and e_a with 0.1 because units in hPa not kPa
data_group_4wind <- data_group_4wind[-c(97:120),]

no_stability1 <- data$stability_no[which(data$wind_category_day==1)]
LE1 <- data$LE[which(data$wind_category_day==1)]
H1 <- data$H[which(data$wind_category_day==1)]
U1 <- data$WS_Spd_WVT[which(data$wind_category_day==1)]
ea1 <- data$e_a[which(data$wind_category_day==1)]
es31 <- data$e_s3[which(data$wind_category_day==1)]
es21 <- data$e_s2[which(data$wind_category_day==1)]
es1 <- data$e_s1[which(data$wind_category_day==1)]
deltaE1 <- data$deltaE[which(data$wind_category_day==1)]
deltaT1 <- data$deltaT[which(data$wind_category_day==1)]
udeltaT1 <- data$u_deltaT[which(data$wind_category_day==1)]
udeltaE1 <- data$u_deltaE[which(data$wind_category_day==1)]
C_D1 <- data$C_D[which(data$wind_category_day==1)]
C_E1 <- data$C_E[which(data$wind_category_day==1)]
C_H1 <- data$C_H[which(data$wind_category_day==1)]
zl1 <- data$Z.L[which(data$wind_category_day==1)]
ustar1 <- data$U.[which(data$wind_category_day==1)]


no_stability2 <- data$stability_no[which(data$wind_category_day==2)]
LE2 <- data$LE[which(data$wind_category_day==2)]
H2 <- data$H[which(data$wind_category_day==2)]
U2 <- data$WS_Spd_WVT[which(data$wind_category_day==2)]
ea2 <- data$e_a[which(data$wind_category_day==2)]
es32 <- data$e_s3[which(data$wind_category_day==2)]
es22 <- data$e_s2[which(data$wind_category_day==2)]
es2 <- data$e_s1[which(data$wind_category_day==2)]
deltaE2 <- data$deltaE[which(data$wind_category_day==2)]
deltaT2 <- data$deltaT[which(data$wind_category_day==2)]
udeltaT2 <- data$u_deltaT[which(data$wind_category_day==2)]
udeltaE2 <- data$u_deltaE[which(data$wind_category_day==2)]
C_D2 <- data$C_D[which(data$wind_category_day==2)]
C_E2 <- data$C_E[which(data$wind_category_day==2)]
C_H2 <- data$C_H[which(data$wind_category_day==2)]
zl2 <- data$Z.L[which(data$wind_category_day==2)]
ustar2 <- data$U.[which(data$wind_category_day==2)]

no_stability3 <- data$stability_no[which(data$wind_category_day==3)]
LE3 <- data$LE[which(data$wind_category_day==3)]
H3 <- data$H[which(data$wind_category_day==3)]
U3 <- data$WS_Spd_WVT[which(data$wind_category_day==3)]
ea3 <- data$e_a[which(data$wind_category_day==3)]
es33 <- data$e_s3[which(data$wind_category_day==3)]
es23 <- data$e_s2[which(data$wind_category_day==3)]
es3 <- data$e_s1[which(data$wind_category_day==3)]
deltaE3 <- data$deltaE[which(data$wind_category_day==3)]
deltaT3 <- data$deltaT[which(data$wind_category_day==3)]
udeltaT3 <- data$u_deltaT[which(data$wind_category_day==3)]
udeltaE3 <- data$u_deltaE[which(data$wind_category_day==3)]
C_D3 <- data$C_D[which(data$wind_category_day==3)]
C_E3 <- data$C_E[which(data$wind_category_day==3)]
C_H3 <- data$C_H[which(data$wind_category_day==3)]
zl3 <- data$Z.L[which(data$wind_category_day==3)]
ustar3 <- data$U.[which(data$wind_category_day==3)]

no_stability4 <- data$stability_no[which(data$wind_category_day==4)]
LE4 <- data$LE[which(data$wind_category_day==4)]
H4 <- data$H[which(data$wind_category_day==4)]
U4 <- data$WS_Spd_WVT[which(data$wind_category_day==4)]
ea4 <- data$e_a[which(data$wind_category_day==4)]
es34 <- data$e_s3[which(data$wind_category_day==4)]
es24 <- data$e_s2[which(data$wind_category_day==4)]
es4 <- data$e_s1[which(data$wind_category_day==4)]
deltaE4 <- data$deltaE[which(data$wind_category_day==4)]
deltaT4 <- data$deltaT[which(data$wind_category_day==4)]
udeltaT4 <- data$u_deltaT[which(data$wind_category_day==4)]
udeltaE4 <- data$u_deltaE[which(data$wind_category_day==4)]
C_D4 <- data$C_D[which(data$wind_category_day==4)]
C_E4 <- data$C_E[which(data$wind_category_day==4)]
C_H4 <- data$C_H[which(data$wind_category_day==4)]
zl4 <- data$Z.L[which(data$wind_category_day==4)]
ustar4 <- data$U.[which(data$wind_category_day==4)]


df1 <- data.frame(no_stability1,LE1,H1,U1,deltaE1,ea1,es31,es21,es1,deltaT1,udeltaT1,udeltaE1,C_D1,
                  C_E1,C_H1,zl1,ustar1)
df2 <- data.frame(no_stability2,LE2,H2,U2,deltaE2,ea2,es32,es22,es2,deltaT2,udeltaT2,udeltaE2,C_D2,
                  C_E2,C_H2,zl2,ustar2)
df3 <- data.frame(no_stability3,LE3,H3,U3,deltaE3,ea3,es33,es23,es3,deltaT3,udeltaT3,udeltaE3,C_D3,
                  C_E3,C_H3,zl3,ustar3)
df4 <- data.frame(no_stability4,LE4,H4,U4,deltaE4,ea4,es34,es24,es4,deltaT4,udeltaT4,udeltaE4,C_D4,
                  C_E4,C_H4,zl4,ustar4)
rm(no_stability1,no_stability2,no_stability3,no_stability4,LE1,LE2,LE3,LE4,H1,H2,H3,H4,
   U1,U2,U3,U4,deltaE1,deltaE2,deltaE3,deltaE4,ea1,es1,ea2,es2,ea3,es3,ea4,es4,
   es31,es21,es32,es22,es33,es23,es34,es24,deltaT1,deltaT2,deltaT3,deltaT4,
   udeltaT1,udeltaE1,udeltaT2,udeltaE2,udeltaT3,udeltaE3,udeltaT4,udeltaE4,
   C_D1,C_D2,C_D3,C_D4,C_E1,C_E2,C_E3,C_E4,C_H1,C_H2,C_H3,C_H4,zl1,zl2,zl3,zl4,ustar1,ustar2,ustar3,ustar4)

##### Names of various figures and box plots ####
names_boxplot = c('\u221210\u2264\u03B6<\u22121','\u22121\u2264\u03B6<\u22120.5','\u22120.5\u2264\u03B6<\u22120.1','\u22120.1\u2264\u03B6<\u22120.05',
                  '\u22120.05\u2264\u03B6<0','0\u2264\u03B6<0.05','0.05\u2264\u03B6<0.1','0.1\u2264\u03B6<0.5','0.5\u2264\u03B6<1',
                  '1\u2264\u03B6<10')

#### Fig. 1 Time series of parameters ####
## For wind category day 1 
path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/lake/figs/wind_figs/fig_1_wind_cat_1.jpg')
jpeg(file=path_fig,width=1450,height=1800,res=320)
## Creating 5 panels of plots
plot.new()
par(family='Times',mfrow=c(5,1),oma=c(0.4,0.1,1.3,0.1))

# a) LE, H, Rn and G?
par(mai=c(0,0.6,0.1,0.1))
plot(data_group_4wind$hour[data_group_4wind$wind_category_day == 1],data_group_4wind$LE[data_group_4wind$wind_category_day == 1],
     ylab='LE and H',xlab='',type='l',ylim=c(0,200),lwd=2,xaxt='n',yaxt='n',cex.lab=2,cex.axis=2)
lines(data_group_4wind$hour[data_group_4wind$wind_category_day == 1],data_group_4wind$H[data_group_4wind$wind_category_day == 1],lty=2,lwd=2)
#lines(data_group_4wind$hour[data_group_4wind$wind_category_day == 1],data_group_4wind$Rn[data_group_4wind$wind_category_day == 1],lty=3,lwd=2)
axis(side=2,at=c(0,100,200),cex.axis=2)
#text(4,180,'a) Wind-class I',cex=2)
legend(-1,220,bty='n',lty=c(1,2),lwd=c(2,2),
       legend=c('LE','H'),cex=1.5)
minor.tick(ny=2,nx=5,tick.ratio=0.5)
title(main='\t\t\t\t\t\t\t\t\ta) Wind-class Day I',outer=TRUE,cex.main=2,font.main=1,adj=0)

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
axis(side=2,at=c(-1,0),labels=c(paste("\u2212",1,sep=""),0),cex.axis=2)

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

## For wind category day 2 

path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/lake/figs/wind_figs/fig_1_wind_cat_2.jpg')
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
title(main='\t\t\t\t\t\t\t\t\tb) Wind-class Day II',outer=TRUE,cex.main=2,font.main=1,adj=0)

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

## For wind category day 3 
path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/lake/figs/wind_figs/fig_1_wind_cat_3.jpg')
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
title(main='\t\t\t\t\t\t\t\t\tc) Wind-class Day III',outer=TRUE,cex.main=2,font.main=1,adj=0)


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
axis(side=2,at=c(-1,0),labels=c(paste("\u2212",1,sep=""),0),cex.axis=2)
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

## For wind category day 4 

path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/lake/figs/wind_figs/fig_1_wind_cat_4.jpg')
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
title(main='\t\t\t\t\t\t\t\t\td) Wind-class Day IV',outer=TRUE,cex.main=2,font.main=1,adj=0)


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
#### Fig. 2: Prob. density plots ####

path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/lake/figs/wind_figs/fig_2_density_plot.jpg')
jpeg(file=path_fig,width=1450,height=1800,res=320)

## Creating 6 panels of plots
plot.new()
par(family='Times',mfrow=c(3,2),oma=c(0.4,0.1,1.3,0.1))

dens_U1 <- density(df1$U1,na.rm=T)
dens_U2 <- density(df2$U2,na.rm=T)
dens_U3 <- density(df3$U3,na.rm=T)
dens_U4 <- density(df4$U4,na.rm=T)

dens_de1 <- density(df1$deltaE1,na.rm=T)
dens_de2 <- density(df2$deltaE2,na.rm=T)
dens_de3 <- density(df3$deltaE3,na.rm=T)
dens_de4 <- density(df4$deltaE4,na.rm=T)

dens_dT1 <- density(df1$deltaT1,na.rm=T)
dens_dT2 <- density(df2$deltaT2,na.rm=T)
dens_dT3 <- density(df3$deltaT3,na.rm=T)
dens_dT4 <- density(df4$deltaT4,na.rm=T)

dens_LE1 <- density(df1$LE1,na.rm=T)
dens_LE2 <- density(df2$LE2,na.rm=T)
dens_LE3 <- density(df3$LE3,na.rm=T)
dens_LE4 <- density(df4$LE4,na.rm=T)

dens_H1 <- density(df1$H1,na.rm=T)
dens_H2 <- density(df2$H2,na.rm=T)
dens_H3 <- density(df3$H3,na.rm=T)
dens_H4 <- density(df4$H4,na.rm=T)

dens_zl1 <- density(df1$zl1,na.rm=T)
dens_zl2 <- density(df2$zl2,na.rm=T)
dens_zl3 <- density(df3$zl3,na.rm=T)
dens_zl4 <- density(df4$zl4,na.rm=T)

# To plot density of U
par(mai=c(0.5,0.6,0,0))
plot(dens_U1,xlim=c(0,12),ylim=c(0,0.5),main='',xlab='',ylab='',cex.axis=1.2)
mtext(side = 1,'U',lty=1,line=2.5)
mtext(side=2,'Density',line=2.5)
text(0.3,0.47,'a)',cex = 1.5)
lines(dens_U2,lty=2,lwd=2)
lines(dens_U3,lty=3,lwd=2)
lines(dens_U4,lty=4,lwd=2)

# To plot density of z/L
par(mai=c(0.5,0.5,0,0.1))
plot(dens_zl1,xlim=c(-1,1),ylim=c(0,3),main='',xlab='',ylab='',cex.axis=1.2)
mtext(side = 1,expression(zeta),lty=1,line=2.5)
text(-0.95,2.85,'d)',cex = 1.5)
#mtext(side=2,'Density',line=2)
lines(dens_zl2,lty=2,lwd=2)
lines(dens_zl3,lty=3,lwd=2)
lines(dens_zl4,lty=4,lwd=2)

# To plot density of deltaE
par(mai=c(0.5,0.6,0,0.1))
plot(dens_de1,xlim=c(-1,2.5),ylim=c(0,1.5),main='',xlab='',ylab='',cex.axis=1.2)
mtext(side = 1,expression(paste(Delta,'e')),line=2.5,lty=1)
mtext(side=2,'Density',line=2.5)
text(-0.9,1.4,'b)',cex = 1.5)
lines(dens_de2,lty=2,lwd=2)
lines(dens_de3,lty=3,lwd=2)
lines(dens_de4,lty=4,lwd=2)

# To plot density of deltaT
par(mai=c(0.5,0.5,0,0.1))
plot(dens_dT1,xlim=c(-10,12),ylim=c(0,0.2),main='',xlab='',ylab='',cex.axis=1.2)
mtext(side = 1,expression(paste(Delta,'T')),line=2.5,lty=1)
text(-9.7,0.19,'e)',cex = 1.5)
#mtext(side=2,'Density',line=2)
lines(dens_dT2,lty=2,lwd=2)
lines(dens_dT3,lty=3,lwd=2)
lines(dens_dT4,lty=4,lwd=2)

# To plot density of LE
par(mai=c(0.5,0.6,0,0.1))
plot(dens_LE1,xlim=c(-100,300),ylim=c(0,0.012),main='',xlab='',ylab='',cex.axis=1.2)
mtext(side = 1,'LE',lty=1,line=2.5)
mtext(side=2,'Density',line=2.5)
text(-93,0.0114,'c)',cex = 1.5)
lines(dens_LE2,lty=2,lwd=2)
lines(dens_LE3,lty=3,lwd=2)
lines(dens_LE4,lty=4,lwd=2)

# To plot density of H
par(mai=c(0.5,0.5,0,0.1))
plot(dens_H1,xlim=c(-100,200),ylim=c(0,0.04),main='',xlab='',ylab='',cex.axis=1.2)
mtext(side = 1,'H',line=2.5,lty=1)
text(-95,0.038,'f)',cex = 1.5)
#mtext(side=2,'Density',line=2)
lines(dens_H2,lty=2,lwd=2)
lines(dens_H3,lty=3,lwd=2)
lines(dens_H4,lty=4,lwd=2)


# Cleanup
rm(dens_U1,dens_U2,dens_U3,dens_U4,
   dens_de1,dens_de2,dens_de3,dens_de4,
   dens_dT1,dens_dT2,dens_dT3,dens_dT4,
   dens_LE1,dens_LE2,dens_LE3,dens_LE4,
   dens_H1,dens_H2,dens_H3,dens_H4,
   dens_zl1,dens_zl2,dens_zl3,dens_zl4)

dev.off()

#### Fig. 2: U vs ASL wind categories #######
# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/lake/figs/wind_figs/fig_2.jpg')
jpeg(file=path_fig,width=5, height=10,res=360,units='in')
plot.new()

# a) For wind category 1 U
plot1 <- ggplot(na.omit(df1),aes(x=factor(no_stability1),y=U1)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability1))) + 
  stat_summary(fun.y="mean",geom='line',aes(group=1),size=1.5) + 
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=14.8,label="a)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(1,1,4,-2.5),"mm")) + 
  scale_y_continuous(breaks=seq(0,15,by=5),limits=c(0,15)) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

# b) For wind category 2 U
plot2 <- ggplot(na.omit(df2),aes(x=factor(no_stability2),y=U2)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability2))) + 
  stat_summary(fun.y="mean",geom='line',aes(group=1),size=1) + 
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=14.8,label="b)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-10,1,15,-2.5),"mm")) + 
  scale_y_continuous(breaks=seq(0,15,by=5),limits=c(0,15)) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))


# c) For wind category 3 U
plot3 <- ggplot(na.omit(df3),aes(x=factor(no_stability3),y=U3)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability3))) + 
  stat_summary(fun.y="mean",geom='line',aes(group=1),size=1) + 
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=14.8,label="c)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-20,1,25,-2.5),"mm")) + 
  scale_y_continuous(breaks=seq(0,15,by=5),limits=c(0,15)) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

# d) For wind category 4 U
plot4 <- ggplot(na.omit(df4),aes(x=factor(no_stability4),y=U4)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability4))) + 
  stat_summary(fun.y="mean",geom='line',aes(group=1),size=1) + 
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=14.8,label="d)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),axis.text.x=element_text(angle=90,size=16,hjust=1,family='Times',vjust=0.5),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-30,1,4,-2.5),"mm")) + 
  scale_y_continuous(breaks=seq(0,15,by=5),limits=c(0,15)) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) +
  scale_x_discrete(labels=names_boxplot)

multiplot2(plot1,plot2,plot3,plot4,
           cols=1,labs=list("\nASL stability ranges","U"))

dev.off()

#### Fig. 3 deltaE vs ASL wind categories #######
# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/lake/figs/wind_figs/fig_3.jpg')
jpeg(file=path_fig,width=5, height=10,res=360,units='in')
plot.new()

# a) For wind category 1 deltaE
plot1 <- ggplot(na.omit(df1),aes(x=factor(no_stability1),y=deltaE1)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability1))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=2.8,label="a)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(1,1,4,-2.5),"mm")) + 
  scale_y_continuous(breaks=seq(-1,3,by=1),limits=c(-1,3),
                     labels=c(paste("\u2212",1,sep=""),0,1,2,3)) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

# b) For wind category 2 deltaE
plot2 <- ggplot(na.omit(df2),aes(x=factor(no_stability2),y=deltaE2)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability2))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y=expression(paste(Delta,'e'))) + theme_bw() + annotate("text",x=0.8,y=2.8,label="b)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01,hjust=0.05),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-10,1,15,-2.5),"mm")) + 
  scale_y_continuous(breaks=seq(-1,3,by=1),limits=c(-1,3),
                     labels=c(paste("\u2212",1,sep=""),0,1,2,3)) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

# c) For wind category 3 deltaE
plot3 <- ggplot(na.omit(df3),aes(x=factor(no_stability3),y=deltaE3)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability3))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=2.8,label="c)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-20,1,25,-2.5),"mm")) + 
  scale_y_continuous(breaks=seq(-1,3,by=1),limits=c(-1,3),
                     labels=c(paste("\u2212",1,sep=""),0,1,2,3)) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

# d) For wind category 4 deltaE
plot4 <- ggplot(na.omit(df4),aes(x=factor(no_stability4),y=deltaE4)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability4))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=2.8,label="d)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),axis.text.x=element_text(angle=90,size=16,hjust=1,family='Times',vjust=0.5),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-30,1,4,-2.5),"mm")) + 
  scale_y_continuous(breaks=seq(-1,3,by=1),limits=c(-1,3),
                     labels=c(paste("\u2212",1,sep=""),0,1,2,3)) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) +
  scale_x_discrete(labels=names_boxplot)

multiplot2(plot1,plot2,plot3,plot4,
           cols=1,labs=list("ASL stability ranges",""))

dev.off()

#### Fig. 3a deltaT vs ASL wind categories ####
# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/lake/figs/wind_figs/fig_3a.jpg')
jpeg(file=path_fig,width=5, height=10,res=360,units='in')
plot.new()

# a) For wind category 1 deltaE
plot1 <- ggplot(na.omit(df1),aes(x=factor(no_stability1),y=deltaT1)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability1))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=9.8,label="e)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(1,1,4,-2.5),"mm")) + 
  scale_y_continuous(breaks=seq(-10,10,by=5),labels=c(paste("\u2212",10,sep=""),paste("\u2212",5,sep=""),0,5,10),limits=c(-10,10)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

# b) For wind category 2 deltaE
plot2 <- ggplot(na.omit(df2),aes(x=factor(no_stability2),y=deltaT2)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability2))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y=expression(paste(Delta,'T'))) + theme_bw() + annotate("text",x=0.8,y=9.7,label="f)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01,hjust=0.07),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-10,1,15,-2.5),"mm")) +
  scale_y_continuous(breaks=seq(-10,10,by=5),labels=c(paste("\u2212",10,sep=""),paste("\u2212",5,sep=""),0,5,10),limits=c(-10,10)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

# c) For wind category 3 deltaE
plot3 <- ggplot(na.omit(df3),aes(x=factor(no_stability3),y=deltaT3)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability3))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=9.7,label="g)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-20,1,25,-2.5),"mm")) + 
  scale_y_continuous(breaks=seq(-10,10,by=5),labels=c(paste("\u2212",10,sep=""),paste("\u2212",5,sep=""),0,5,10),limits=c(-10,10)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

# d) For wind category 4 deltaE
plot4 <- ggplot(na.omit(df4),aes(x=factor(no_stability4),y=deltaT4)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability4))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=9.7,label="h)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),axis.text.x=element_text(angle=90,size=16,hjust=1,family='Times',vjust=0.5),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-30,1,4,-2.5),"mm")) + 
  scale_y_continuous(breaks=seq(-10,10,by=5),labels=c(paste("\u2212",10,sep=""),paste("\u2212",5,sep=""),0,5,10),limits=c(-10,10)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) +
  scale_x_discrete(labels=names_boxplot)

multiplot2(plot1,plot2,plot3,plot4,
           cols=1,labs=list("ASL stability ranges",""))

dev.off()

#### Fig. 4 LE vs ASL stability ranges for 4 ws categories ####

# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/lake/figs/wind_figs/fig_4.jpg')
jpeg(file=path_fig,width=5, height=10,res=360,units='in')
plot.new()
# a) For wind category 1 LE
plot1 <- ggplot(na.omit(df1),aes(x=factor(no_stability1),y=LE1)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability1))) + 
  #stat_summary(fun.y="median",colour='black',geom='text',label='---',size=7) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) +
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=440,label="a)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(1,1,5,-2.5),"mm")) + 
  scale_y_continuous(breaks=seq(-100,400,by=100),limits=c(-100,450),labels=c(paste("\u2212",100,sep=""),0,100,200,300,400)) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

# b) For wind category 2 LE
plot2 <- ggplot(na.omit(df2),aes(x=factor(no_stability2),y=LE2)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability2))) + 
  #stat_summary(fun.y="median",colour='black',geom='text',label='---',size=7) +
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) +
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=440,label="b)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-9,1,14,-2.5),"mm")) + 
  scale_y_continuous(breaks=seq(-100,400,by=100),limits=c(-100,450),labels=c(paste("\u2212",100,sep=""),0,100,200,300,400)) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

# c) For wind category 3 LE
plot3 <- ggplot(na.omit(df3),aes(x=factor(no_stability3),y=LE3)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability3))) + 
  #stat_summary(fun.y="median",colour='black',geom='text',label='---',size=7) +
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) +
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=440,label="c)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-19,1,23,-2.5),"mm")) + 
  scale_y_continuous(breaks=seq(-100,400,by=100),limits=c(-100,450),labels=c(paste("\u2212",100,sep=""),0,100,200,300,400)) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

# d) For wind category 4 LE
plot4 <- ggplot(na.omit(df4),aes(x=factor(no_stability4),y=LE4)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/8),aes(color=factor(no_stability4))) + 
  #stat_summary(fun.y="median",colour='black',geom='text',label='---',size=7) +
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) +
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=440,label="d)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.x=element_text(size=16,family='Times'),axis.title.y=element_text(size=16,family='Times',vjust=0.01),
        axis.text.x=element_text(angle=90,size=16,hjust=1,family='Times',vjust=0.5),axis.text.y=element_text(size=16,family='Times'),
        plot.margin=unit(c(-28,1,4,-2.5),"mm")) + 
  scale_y_continuous(breaks=seq(-100,400,by=100),limits=c(-100,450),labels=c(paste("\u2212",100,sep=""),0,100,200,300,400)) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) +
  scale_x_discrete(labels=names_boxplot)

multiplot2(plot1,plot2,plot3,plot4,
           cols=1,labs=list("ASL stability ranges","LE"))

dev.off()

#### Fig. 4a H vs ASL for 4 ws categories ####
# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/lake/figs/wind_figs/fig_4a.jpg')
jpeg(file=path_fig,width=5, height=10,res=360,units='in')
plot.new()

# a) H for wind cat 1
plot1 <- ggplot(na.omit(df1),aes(x=factor(no_stability1),y=H1)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/32),aes(color=factor(no_stability1))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=245,label="e)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(1,1,5,-2.5),"mm")) + 
  scale_y_continuous(breaks=seq(-100,300,by=100),limits=c(-100,250),labels=c(paste("\u2212",100,sep=""),0,100,200,300)) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

# b) For wind category 2 H
plot2 <- ggplot(na.omit(df2),aes(x=factor(no_stability2),y=H2)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/32),aes(color=factor(no_stability2))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) +
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=245,label="f)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-10,1,15,-2.5),"mm")) + 
  scale_y_continuous(breaks=seq(-100,300,by=100),limits=c(-100,250),labels=c(paste("\u2212",100,sep=""),0,100,200,300)) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

# c) For wind category 3 H
plot3 <- ggplot(na.omit(df3),aes(x=factor(no_stability3),y=H3)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/32),aes(color=factor(no_stability3))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) +
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=245,label="g)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-20,1,25,-2.5),"mm")) + 
  scale_y_continuous(breaks=seq(-100,300,by=100),limits=c(-100,250),labels=c(paste("\u2212",100,sep=""),0,100,200,300)) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

# d) For wind category 4 H
plot4 <- ggplot(na.omit(df4),aes(x=factor(no_stability4),y=H4)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability4))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) +
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=245,label="h)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.x=element_text(size=16,family='Times'),axis.title.y=element_text(size=16,family='Times',vjust=0.01),
        axis.text.x=element_text(angle=90,size=16,hjust=1,family='Times',vjust=0.5),axis.text.y=element_text(size=16,family='Times'),
        plot.margin=unit(c(-30,1,4,-2.5),"mm")) +
  scale_y_continuous(breaks=seq(-100,300,by=100),limits=c(-100,250),labels=c(paste("\u2212",100,sep=""),0,100,200,300)) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) +
  scale_x_discrete(labels=names_boxplot)

multiplot2(plot1,plot2,plot3,plot4,
           cols=1,labs=list("ASL stability ranges","H"))

dev.off()

#### Fig. 5 LE vs deltaE for 4 ws categories ####
# Path where the plots will be saved
path_fig <- 
  file.path('/Users/Yusri/Documents/Work/Data_analysis/lake/figs/wind_figs/fig_5.jpg')
jpeg(file=path_fig,width=8, height=16,res=360,units='cm')
plot.new()

par(family='Times',mfrow=c(2,1),oma=c(0.4,0.1,1.3,0.1))

# a) For positive deltaE
# Linear regression lines
lm1 <- lm(df1$LE1[which(df1$deltaE1>0)] ~ df1$deltaE1[which(df1$deltaE1>0)])
lm2 <- lm(df2$LE2[which(df2$deltaE2>0)] ~ df2$deltaE2[which(df2$deltaE2>0)])
lm3 <- lm(df3$LE3[which(df3$deltaE3>0)] ~ df3$deltaE3[which(df3$deltaE3>0)])
lm4 <- lm(df4$LE4[which(df4$deltaE4>0)] ~ df4$deltaE4[which(df4$deltaE4>0)])

par(mai=c(0.5,0.6,0,0.1))
library(RColorBrewer)
cols <- brewer.pal(4,"Set1")
plot(df1$deltaE1[which(df1$deltaE1>0)],df1$LE1[which(df1$deltaE1>0)],
     pch=19,col=cols[1],
     xlab='',ylab='',cex=0.2,cex.axis=1,
     cex.lab=1)
text(x=0.1,y=348,labels='a)',cex=1.2)
mtext(side=2,'LE',line=2,cex=1)
points(df2$deltaE2[which(df1$deltaE1>0)],df2$LE2[which(df1$deltaE1>0)],
       pch=19,col = cols[2],cex=0.2)
points(df3$deltaE3[which(df1$deltaE1>0)],df3$LE3[which(df1$deltaE1>0)],
       pch=19,col = cols[3],cex=0.2)
points(df4$deltaE4[which(df1$deltaE1>0)],df4$LE4[which(df1$deltaE1>0)],
       pch=19,col = cols[4],cex=0.2)
minor.tick(nx=2,ny=2)

abline(lm1,lwd=4,lty=1,col=cols[1])
abline(lm2,lwd=4,lty=2,col=cols[2])
abline(lm3,lwd=4,lty=5,col=cols[3])
abline(lm4,lwd=4,lty=4,col=cols[4])

# b) For negative deltaE
# Linear regression lines
lm1 <- lm(df1$LE1[which(df1$deltaE1<0)] ~ df1$deltaE1[which(df1$deltaE1<0)])
lm2 <- lm(df2$LE2[which(df2$deltaE2<0)] ~ df2$deltaE2[which(df2$deltaE2<0)])
lm3 <- lm(df3$LE3[which(df3$deltaE3<0)] ~ df3$deltaE3[which(df3$deltaE3<0)])
lm4 <- lm(df4$LE4[which(df4$deltaE4<0)] ~ df4$deltaE4[which(df4$deltaE4<0)])
par(mai=c(0.6,0.6,0,0.1))
plot(df1$deltaE1[which(df1$deltaE1<0)],df1$LE1[which(df1$deltaE1<0)],
     pch=19,col=cols[1],
     xlab='',ylab='',cex=0.2,cex.axis=1,xaxt='n',yaxt='n',
     cex.lab=1,xlim=c(-0.5,0))
axis(side=2,at=c(-30,-20,-10,0,10),
     labels=c(paste("\u2212",30,sep=""),paste("\u2212",20,sep=""),
              paste("\u2212",10,sep=""),0,10),cex.axis=1)
axis(side=1,at=c(-0.5,-0.4,-0.3,-0.2,-0.1,0),
    labels=c(paste("\u2212",0.5,sep=""),paste("\u2212",0.4,sep=""),
             paste("\u2212",0.3,sep=""),paste("\u2212",0.2,sep=""),
             paste("\u2212",0.1,sep=""),0),cex.axis=1)
text(x=-0.48,y=9,labels='b)',cex=1.2)
mtext(side=2,'LE',line=2,cex=1)
mtext(side=1,expression(paste(Delta,'e')),line=2.2,cex=1)
points(df2$deltaE2[which(df1$deltaE1<0)],df2$LE2[which(df1$deltaE1<0)],
       pch=19,col=cols[2],cex=0.2)
points(df3$deltaE3[which(df1$deltaE1<0)],df3$LE3[which(df1$deltaE1<0)],
       pch=19,col=cols[3],cex=0.2)
points(df4$deltaE4[which(df1$deltaE1<0)],df4$LE4[which(df1$deltaE1<0)],
       pch=19,col=cols[4],cex=0.2)
minor.tick(nx=2,ny=2)
abline(lm1,lwd=4,lty=1,col=cols[1])
abline(lm2,lwd=4,lty=2,col=cols[2])
abline(lm3,lwd=4,lty=5,col=cols[3])
abline(lm4,lwd=4,lty=4,col=cols[4])

rm(lm1,lm2,lm3,lm4)
dev.off()

#### Fig. 5 H vs deltaT for 4 ws categories ####
# Path where the plots will be saved
# Path where the plots will be saved
path_fig <- 
  file.path('/Users/Yusri/Documents/Work/Data_analysis/lake/figs/wind_figs/fig_5a.jpg')
library(RColorBrewer)
cols <- brewer.pal(4,"Set1")
jpeg(file=path_fig,width=8, height=16,res=360,units='cm')
plot.new()

par(family='Times',mfrow=c(2,1),oma=c(0.4,0.1,1.3,0.1))

# c) For positive deltaT
# Linear regression lines
lm1 <- lm(df1$H1[which(df1$deltaT1>0)] ~ df1$deltaT1[which(df1$deltaT1>0)])
lm2 <- lm(df2$H2[which(df2$deltaT2>0)] ~ df2$deltaT2[which(df2$deltaT2>0)])
lm3 <- lm(df3$H3[which(df3$deltaT3>0)] ~ df3$deltaT3[which(df3$deltaT3>0)])
lm4 <- lm(df4$H4[which(df4$deltaT4>0)] ~ df4$deltaT4[which(df4$deltaT4>0)])

par(mai=c(0.5,0.6,0,0.1))
plot(df1$deltaT1[which(df1$deltaT1>0)],df1$H1[which(df1$deltaT1>0)],
     pch=19,col=cols[1],
     xlab='',ylab='',cex=0.2,cex.axis=1,
     cex.lab=1)
text(x=0.2,y=105,labels='c)',cex=1.2)
mtext(side=2,'H',line=2,cex=1)
points(df2$deltaT2[which(df1$deltaT1>0)],df2$H2[which(df1$deltaT1>0)],
       pch=19,col=cols[2],cex=0.2)
points(df3$deltaT3[which(df1$deltaT1>0)],df3$H3[which(df1$deltaT1>0)],
       pch=19,col=cols[3],cex=0.2)
points(df4$deltaT4[which(df1$deltaT1>0)],df4$H4[which(df1$deltaT1>0)],
       pch=19,col=cols[4],cex=0.2)
minor.tick(nx=2,ny=2)
abline(lm1,lwd=4,lty=1,col=cols[1])
abline(lm2,lwd=4,lty=2,col=cols[2])
abline(lm3,lwd=4,lty=5,col=cols[3])
abline(lm4,lwd=4,lty=4,col=cols[4])

# d) For negative deltaT
# Linear regression lines
lm1 <- lm(df1$H1[which(df1$deltaT1<0)] ~ df1$deltaT1[which(df1$deltaT1<0)])
lm2 <- lm(df2$H2[which(df2$deltaT2<0)] ~ df2$deltaT2[which(df2$deltaT2<0)])
lm3 <- lm(df3$H3[which(df3$deltaT3<0)] ~ df3$deltaT3[which(df3$deltaT3<0)])
lm4 <- lm(df4$H4[which(df4$deltaT4<0)] ~ df4$deltaT4[which(df4$deltaT4<0)])
par(mai=c(0.6,0.6,0,0.1))
plot(df1$deltaT1[which(df1$deltaT1<0)],df1$H1[which(df1$deltaT1<0)],
     pch=19,col=cols[1],
     xlab='',ylab='',cex=0.2,cex.axis=1,
     cex.lab=1,xlim=c(-0.5,0),xaxt='n',yaxt='n')
axis(side=2,at=c(-40,-20,0,20),
     labels=c(paste("\u2212",40,sep=""),paste("\u2212",20,sep=""),
              0,20),cex.axis=1)
axis(side=1,at=c(-0.5,-0.4,-0.3,-0.2,-0.1,0),
     labels=c(paste("\u2212",0.5,sep=""),paste("\u2212",0.4,sep=""),
              paste("\u2212",0.3,sep=""),paste("\u2212",0.2,sep=""),
              paste("\u2212",0.1,sep=""),0),cex.axis=1)
text(x=-0.48,y=35,labels='d)',cex=1.2)
mtext(side=2,'H',line=2,cex=1)
mtext(side=1,expression(paste(Delta,'T')),line=2.2,cex=1)
points(df2$deltaT2[which(df1$deltaT1<0)],df2$H2[which(df1$deltaT1<0)],
       pch=19,col=cols[2],cex=0.2)
points(df3$deltaT3[which(df1$deltaT1<0)],df3$H3[which(df1$deltaT1<0)],
       pch=19,col=cols[3],cex=0.2)
points(df4$deltaT4[which(df1$deltaT1<0)],df4$H4[which(df1$deltaT1<0)],
       pch=19,col=cols[4],cex=0.2)
minor.tick(nx=2,ny=2)
abline(lm1,lwd=4,lty=1,col=cols[1])
abline(lm2,lwd=4,lty=2,col=cols[2])
abline(lm3,lwd=4,lty=5,col=cols[3])
abline(lm4,lwd=4,lty=4,col=cols[4])

rm(lm1,lm2,lm3,lm4)
dev.off()

#### Fig. 6 udeltaE vs ASL stability wind categories ####
# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/lake/figs/wind_figs/fig_6.jpg')
jpeg(file=path_fig,width=5, height=10,res=360,units='in')
plot.new()

# a) For wind category 1 deltaE
plot1 <- ggplot(na.omit(df1),aes(x=factor(no_stability1),y=udeltaE1)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability1))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=14.8,label="a)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(1,1,4,-2.5),"mm")) + 
  scale_y_continuous(breaks=seq(-5,15,by=5),labels=c(paste("\u2212",5,sep=""),0,5,10,15),limits=c(-5,15)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

# b) For wind category 2 deltaE
plot2 <- ggplot(na.omit(df2),aes(x=factor(no_stability2),y=udeltaE2)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability2))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y=expression(paste('U',Delta,'e'))) + theme_bw() + annotate("text",x=0.8,y=14.8,label="b)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01,hjust=0.07),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-10,1,15,-2.5),"mm")) +
  scale_y_continuous(breaks=seq(-5,15,by=5),labels=c(paste("\u2212",5,sep=""),0,5,10,15),limits=c(-5,15)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

# c) For wind category 3 deltaE
plot3 <- ggplot(na.omit(df3),aes(x=factor(no_stability3),y=udeltaE3)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability3))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=14.8,label="c)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-20,1,25,-2.5),"mm")) + 
  scale_y_continuous(breaks=seq(-5,15,by=5),labels=c(paste("\u2212",5,sep=""),0,5,10,15),limits=c(-5,15)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

# d) For wind category 4 deltaE
plot4 <- ggplot(na.omit(df4),aes(x=factor(no_stability4),y=udeltaE4)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability4))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=14.8,label="d)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),axis.text.x=element_text(angle=90,size=16,hjust=1,family='Times',vjust=0.5),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-30,1,4,-2.5),"mm")) + 
  scale_y_continuous(breaks=seq(-5,15,by=5),labels=c(paste("\u2212",5,sep=""),0,5,10,15),limits=c(-5,15)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) +
  scale_x_discrete(labels=names_boxplot)

multiplot2(plot1,plot2,plot3,plot4,
           cols=1,labs=list("ASL stability ranges",""))

dev.off()

#### Fig. 6a udeltaT vs ASL stability wind categories ####
# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/lake/figs/wind_figs/fig_6a.jpg')
jpeg(file=path_fig,width=5, height=10,res=360,units='in')
plot.new()

# a) For wind category 1 udeltaT
plot1 <- ggplot(na.omit(df1),aes(x=factor(no_stability1),y=udeltaT1)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability1))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=98,label="e)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(1,1,4,-2.5),"mm")) + 
  scale_y_continuous(breaks=seq(-50,100,by=50),labels=c(paste("\u2212",50,sep=""),0,50,100),limits=c(-50,100)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

# b) For wind category 2 deltaT
plot2 <- ggplot(na.omit(df2),aes(x=factor(no_stability2),y=udeltaT2)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability2))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y=expression(paste('U',Delta,'T'))) + theme_bw() + annotate("text",x=0.8,y=98,label="f)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01,hjust=0.07),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-10,1,15,-2.5),"mm")) +
  scale_y_continuous(breaks=seq(-50,100,by=50),labels=c(paste("\u2212",50,sep=""),0,50,100),limits=c(-50,100)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

# c) For wind category 3 deltaT
plot3 <- ggplot(na.omit(df3),aes(x=factor(no_stability3),y=udeltaT3)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability3))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=98,label="g)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-20,1,25,-2.5),"mm")) + 
  scale_y_continuous(breaks=seq(-50,100,by=50),labels=c(paste("\u2212",50,sep=""),0,50,100),limits=c(-50,100)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

# d) For wind category 4 deltaT
plot4 <- ggplot(na.omit(df4),aes(x=factor(no_stability4),y=udeltaT4)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability4))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=98,label="h)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),axis.text.x=element_text(angle=90,size=16,hjust=1,family='Times',vjust=0.5),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-30,1,4,-2.5),"mm")) + 
  scale_y_continuous(breaks=seq(-50,100,by=50),labels=c(paste("\u2212",50,sep=""),0,50,100),limits=c(-50,100)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) +
  scale_x_discrete(labels=names_boxplot)

multiplot2(plot1,plot2,plot3,plot4,
           cols=1,labs=list("ASL stability ranges",""))

dev.off()

#### Fig. 7 C_D stability wind categories ####
# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/lake/figs/wind_figs/fig_7.jpg')
jpeg(file=path_fig,width=5, height=10,res=360,units='in')
plot.new()

# a) For wind category 1 C_D
plot1 <- ggplot(na.omit(df1),aes(x=factor(no_stability1),y=C_D1)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability1))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=0.005,label="a)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(1,1,4,-1.4),"mm")) + 
  scale_y_continuous(breaks=seq(-0.003,0.005,by=0.002),limits=c(-0.003,0.005),
                     labels=c(paste("\u2212",0.003,sep=""),paste("\u2212",0.001,sep=""),
                              0.001,0.003,0.005)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

# b) For wind category 2 C_D
plot2 <- ggplot(na.omit(df2),aes(x=factor(no_stability2),y=C_D2)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability2))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y=expression('C'['D'])) + theme_bw() + annotate("text",x=0.8,y=0.005,label="b)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01,hjust=0.07),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-10,1,15,-2),"mm")) +
  scale_y_continuous(breaks=seq(-0.003,0.005,by=0.002),limits=c(-0.003,0.005),
                     labels=c(paste("\u2212",0.003,sep=""),paste("\u2212",0.001,sep=""),
                              0.001,0.003,0.005)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

# c) For wind category 3 C_D
plot3 <- ggplot(na.omit(df3),aes(x=factor(no_stability3),y=C_D3)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability3))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=0.005,label="c)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-20,1,25,-1.4),"mm")) + 
  scale_y_continuous(breaks=seq(-0.003,0.005,by=0.002),limits=c(-0.003,0.005),
                     labels=c(paste("\u2212",0.003,sep=""),paste("\u2212",0.001,sep=""),
                              0.001,0.003,0.005)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

# d) For wind category 4 C_D
plot4 <- ggplot(na.omit(df4),aes(x=factor(no_stability4),y=C_D4)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability4))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=0.005,label="d)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),axis.text.x=element_text(angle=90,size=16,hjust=1,family='Times',vjust=0.5),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-30,1,4,-1.4),"mm")) + 
  scale_y_continuous(breaks=seq(-0.003,0.005,by=0.002),limits=c(-0.003,0.005),
                     labels=c(paste("\u2212",0.003,sep=""),paste("\u2212",0.001,sep=""),
                              0.001,0.003,0.005)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) +
  scale_x_discrete(labels=names_boxplot)

multiplot2(plot1,plot2,plot3,plot4,
           cols=1,labs=list("ASL stability ranges",""))

dev.off()

#### Fig. 8 C_E stability wind categories ####
# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/lake/figs/wind_figs/fig_8.jpg')
jpeg(file=path_fig,width=5, height=10,res=360,units='in')
plot.new()

# a) For wind category 1 C_E
plot1 <- ggplot(na.omit(df1),aes(x=factor(no_stability1),y=C_E1)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability1))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=0.005,label="a)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(1,1,4,-2.5),"mm")) + 
  scale_y_continuous(breaks=seq(-0.003,0.005,by=0.002),limits=c(-0.003,0.005),
                     labels=c(paste("\u2212",0.003,sep=""),paste("\u2212",0.001,sep=""),
                              0.001,0.003,0.005)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

# b) For wind category 2 C_E
plot2 <- ggplot(na.omit(df2),aes(x=factor(no_stability2),y=C_E2)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability2))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y=expression('C'['E'])) + theme_bw() + annotate("text",x=0.8,y=0.005,label="b)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01,hjust=0.07),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-10,1,15,-2.5),"mm")) +
  scale_y_continuous(breaks=seq(-0.003,0.005,by=0.002),limits=c(-0.003,0.005),
                     labels=c(paste("\u2212",0.003,sep=""),paste("\u2212",0.001,sep=""),
                              0.001,0.003,0.005)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

# c) For wind category 3 C_E
plot3 <- ggplot(na.omit(df3),aes(x=factor(no_stability3),y=C_E3)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability3))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=0.005,label="c)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-20,1,25,-2.5),"mm")) + 
  scale_y_continuous(breaks=seq(-0.003,0.005,by=0.002),limits=c(-0.003,0.005),
                     labels=c(paste("\u2212",0.003,sep=""),paste("\u2212",0.001,sep=""),
                              0.001,0.003,0.005)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

# d) For wind category 4 C_E
plot4 <- ggplot(na.omit(df4),aes(x=factor(no_stability4),y=C_E4)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability4))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=0.005,label="d)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),axis.text.x=element_text(angle=90,size=16,hjust=1,family='Times',vjust=0.5),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-30,1,4,-2.5),"mm")) + 
  scale_y_continuous(breaks=seq(-0.003,0.005,by=0.002),limits=c(-0.003,0.005),
                     labels=c(paste("\u2212",0.003,sep=""),paste("\u2212",0.001,sep=""),
                              0.001,0.003,0.005)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) +
  scale_x_discrete(labels=names_boxplot)

multiplot2(plot1,plot2,plot3,plot4,
           cols=1,labs=list("ASL stability ranges",""))

dev.off()

#### Fig. 9 C_H stability wind categories ####
# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/lake/figs/wind_figs/fig_9.jpg')
jpeg(file=path_fig,width=5, height=10,res=360,units='in')
plot.new()

# a) For wind category 1 C_H
plot1 <- ggplot(na.omit(df1),aes(x=factor(no_stability1),y=C_H1)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability1))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=0.0047,label="e)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(1,1,4,-2.5),"mm")) + 
  scale_y_continuous(breaks=seq(-0.003,0.005,by=0.002),limits=c(-0.003,0.005),
                     labels=c(paste("\u2212",0.003,sep=""),paste("\u2212",0.001,sep=""),
                              0.001,0.003,0.005)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

# b) For wind category 2 C_H
plot2 <- ggplot(na.omit(df2),aes(x=factor(no_stability2),y=C_H2)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability2))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y=expression('C'['H'])) + theme_bw() + annotate("text",x=0.8,y=0.0047,label="f)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01,hjust=0.07),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-10,1,15,-2.5),"mm")) +
  scale_y_continuous(breaks=seq(-0.003,0.005,by=0.002),limits=c(-0.003,0.005),
                     labels=c(paste("\u2212",0.003,sep=""),paste("\u2212",0.001,sep=""),
                              0.001,0.003,0.005)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

# c) For wind category 3 C_H
plot3 <- ggplot(na.omit(df3),aes(x=factor(no_stability3),y=C_H3)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability3))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=0.0047,label="g)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-20,1,25,-2.5),"mm")) + 
  scale_y_continuous(breaks=seq(-0.003,0.005,by=0.002),limits=c(-0.003,0.005),
                     labels=c(paste("\u2212",0.003,sep=""),paste("\u2212",0.001,sep=""),
                              0.001,0.003,0.005)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

# d) For wind category 4 C_H
plot4 <- ggplot(na.omit(df4),aes(x=factor(no_stability4),y=C_H4)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability4))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=0.0047,label="h)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),axis.text.x=element_text(angle=90,size=16,hjust=1,family='Times',vjust=0.5),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-30,1,4,-2.5),"mm")) + 
  scale_y_continuous(breaks=seq(-0.003,0.005,by=0.002),limits=c(-0.003,0.005),
                     labels=c(paste("\u2212",0.003,sep=""),paste("\u2212",0.001,sep=""),
                              0.001,0.003,0.005)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) +
  scale_x_discrete(labels=names_boxplot)

multiplot2(plot1,plot2,plot3,plot4,
           cols=1,labs=list("ASL stability ranges",""))

dev.off()

#### Fig. 10 Slope plots #####
path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/lake/figs/wind_figs/fig_10.jpg')
jpeg(file=path_fig,width=5, height=10,res=360,units='in')
plot.new()

par(family='Times',mfrow=c(2,1),oma=c(0.4,0.1,1.3,0.1))



# Slope data for regressions between LE (H) and deltaE (deltaT)
x <- 1:4 # Wind-class days

# Overall slope values for unstable and stable conditions
slope_LEdE <- c(64.901,78.076,100.584,166.4871)
slope_HdT <- c(7.02,9.7742,9.8642,14.5791)

slope_LEunst <- c(65.108,74.168,99.447,170.423)
slope_LEsta <- c(59.208,79.849,93.708,88.050)
slope_Hunst <- c(7.1339,9.716,9.9419,14.9332)
slope_Hstapos <- c(4.0402,-13.487,-1.2638,6.418) # positive deltaT
slope_Hstaneg <- c(1.2715,1.0684,1.3850,6.7102) # negative deltaT


# Plots
# a) Regression slope of LE and deltaE for unstable and stable conditions
par(mai=c(0.7,1,0,0.1))
plot(x,slope_LEunst,pch=19,ylim=c(50,180),xlab='',
     ylab=expression(paste('Slope of LE and ',Delta,'e')),
     xaxt='n',cex.axis=2,cex=2,cex.lab=2)
text(1.05,178,'a)',cex=2)
axis(side=1,at=c(1,2,3,4),labels = c('','','',''))
lines(x,slope_LEunst,lty=1,lwd=2)
points(x,slope_LEsta,pch=21,cex=2)
lines(x,slope_LEsta,lty=2,lwd=2)

# b) Regression slope of H and deltaT for unstable and stable conditions
par(mai=c(1,1,0,0.1))
plot(x,slope_Hunst,pch=19,ylim = c(0,15),xlab='Wind-class Day',
     ylab=expression(paste('Slope of H and ',Delta,'T')),
     xaxt='n',cex=2,cex.axis=2,cex.lab=2)
text(1.05,14.5,'b)',cex=2)
axis(side=1,at=c(1,2,3,4),labels=c('I','II','III','IV'),cex.axis=2)
lines(x,slope_Hunst,lty=1,lwd=2)
points(x,slope_Hstaneg,pch=21,cex=2)
lines(x,slope_Hstaneg,lty=2,lwd=2)

dev.off()



#### Plotting mean LE with ASL ####
LE1 <- c(mean(df1$LE1[which(df1$no_stability1 == 1)],na.rm=TRUE),
         mean(df1$LE1[which(df1$no_stability1 == 2)],na.rm=TRUE),
         mean(df1$LE1[which(df1$no_stability1 == 3)],na.rm=TRUE),
         mean(df1$LE1[which(df1$no_stability1 == 4)],na.rm=TRUE),
         mean(df1$LE1[which(df1$no_stability1 == 5)],na.rm=TRUE),
         mean(df1$LE1[which(df1$no_stability1 == 6)],na.rm=TRUE),
         mean(df1$LE1[which(df1$no_stability1 == 7)],na.rm=TRUE),
         mean(df1$LE1[which(df1$no_stability1 == 8)],na.rm=TRUE),
         mean(df1$LE1[which(df1$no_stability1 == 9)],na.rm=TRUE),
         mean(df1$LE1[which(df1$no_stability1 == 10)],na.rm=TRUE))

LE2 <- c(mean(df2$LE2[which(df2$no_stability2 == 1)],na.rm=TRUE),
         mean(df2$LE2[which(df2$no_stability2 == 2)],na.rm=TRUE),
         mean(df2$LE2[which(df2$no_stability2 == 3)],na.rm=TRUE),
         mean(df2$LE2[which(df2$no_stability2 == 4)],na.rm=TRUE),
         mean(df2$LE2[which(df2$no_stability2 == 5)],na.rm=TRUE),
         mean(df2$LE2[which(df2$no_stability2 == 6)],na.rm=TRUE),
         mean(df2$LE2[which(df2$no_stability2 == 7)],na.rm=TRUE),
         mean(df2$LE2[which(df2$no_stability2 == 8)],na.rm=TRUE),
         mean(df2$LE2[which(df2$no_stability2 == 9)],na.rm=TRUE),
         mean(df2$LE2[which(df2$no_stability2 == 10)],na.rm=TRUE))

LE3 <- c(mean(df3$LE3[which(df3$no_stability3 == 1)],na.rm=TRUE),
         mean(df3$LE3[which(df3$no_stability3 == 2)],na.rm=TRUE),
         mean(df3$LE3[which(df3$no_stability3 == 3)],na.rm=TRUE),
         mean(df3$LE3[which(df3$no_stability3 == 4)],na.rm=TRUE),
         mean(df3$LE3[which(df3$no_stability3 == 5)],na.rm=TRUE),
         mean(df3$LE3[which(df3$no_stability3 == 6)],na.rm=TRUE),
         mean(df3$LE3[which(df3$no_stability3 == 7)],na.rm=TRUE),
         mean(df3$LE3[which(df3$no_stability3 == 8)],na.rm=TRUE),
         mean(df3$LE3[which(df3$no_stability3 == 9)],na.rm=TRUE),
         mean(df3$LE3[which(df3$no_stability3 == 10)],na.rm=TRUE))

LE4 <- c(mean(df4$LE4[which(df4$no_stability4 == 1)],na.rm=TRUE),
         mean(df4$LE4[which(df4$no_stability4 == 2)],na.rm=TRUE),
         mean(df4$LE4[which(df4$no_stability4 == 3)],na.rm=TRUE),
         mean(df4$LE4[which(df4$no_stability4 == 4)],na.rm=TRUE),
         mean(df4$LE4[which(df4$no_stability4 == 5)],na.rm=TRUE),
         mean(df4$LE4[which(df4$no_stability4 == 6)],na.rm=TRUE),
         mean(df4$LE4[which(df4$no_stability4 == 7)],na.rm=TRUE),
         mean(df4$LE4[which(df4$no_stability4 == 8)],na.rm=TRUE),
         mean(df4$LE4[which(df4$no_stability4 == 9)],na.rm=TRUE),
         mean(df4$LE4[which(df4$no_stability4 == 10)],na.rm=TRUE))
# Plot the mean against ASL stability ranges
plot(c(1:10),LE1,type='l',ylim=c(-10,180),xlab='ASL stability ranges',ylab='LE',
     lwd=2)
lines(c(1:10),LE2,type='l',col='blue',lwd=2)
lines(c(1:10),LE3,type='l',col='red',lwd=2)
lines(c(1:10),LE4,type='l',col='purple',lwd=2)
legend(x=8.5,y=180,legend=c('1','2','3','4'),lty=c(1,1,1,1),
       col=c('black','blue','red','purple'))

#### Plotting mean H with ASL ####
H1 <- c(mean(df1$H1[which(df1$no_stability1 == 1)],na.rm=TRUE),
         mean(df1$H1[which(df1$no_stability1 == 2)],na.rm=TRUE),
         mean(df1$H1[which(df1$no_stability1 == 3)],na.rm=TRUE),
         mean(df1$H1[which(df1$no_stability1 == 4)],na.rm=TRUE),
         mean(df1$H1[which(df1$no_stability1 == 5)],na.rm=TRUE),
         mean(df1$H1[which(df1$no_stability1 == 6)],na.rm=TRUE),
         mean(df1$H1[which(df1$no_stability1 == 7)],na.rm=TRUE),
         mean(df1$H1[which(df1$no_stability1 == 8)],na.rm=TRUE),
         mean(df1$H1[which(df1$no_stability1 == 9)],na.rm=TRUE),
         mean(df1$H1[which(df1$no_stability1 == 10)],na.rm=TRUE))

H2 <- c(mean(df2$H2[which(df2$no_stability2 == 1)],na.rm=TRUE),
         mean(df2$H2[which(df2$no_stability2 == 2)],na.rm=TRUE),
         mean(df2$H2[which(df2$no_stability2 == 3)],na.rm=TRUE),
         mean(df2$H2[which(df2$no_stability2 == 4)],na.rm=TRUE),
         mean(df2$H2[which(df2$no_stability2 == 5)],na.rm=TRUE),
         mean(df2$H2[which(df2$no_stability2 == 6)],na.rm=TRUE),
         mean(df2$H2[which(df2$no_stability2 == 7)],na.rm=TRUE),
         mean(df2$H2[which(df2$no_stability2 == 8)],na.rm=TRUE),
         mean(df2$H2[which(df2$no_stability2 == 9)],na.rm=TRUE),
         mean(df2$H2[which(df2$no_stability2 == 10)],na.rm=TRUE))

H3 <- c(mean(df3$H3[which(df3$no_stability3 == 1)],na.rm=TRUE),
         mean(df3$H3[which(df3$no_stability3 == 2)],na.rm=TRUE),
         mean(df3$H3[which(df3$no_stability3 == 3)],na.rm=TRUE),
         mean(df3$H3[which(df3$no_stability3 == 4)],na.rm=TRUE),
         mean(df3$H3[which(df3$no_stability3 == 5)],na.rm=TRUE),
         mean(df3$H3[which(df3$no_stability3 == 6)],na.rm=TRUE),
         mean(df3$H3[which(df3$no_stability3 == 7)],na.rm=TRUE),
         mean(df3$H3[which(df3$no_stability3 == 8)],na.rm=TRUE),
         mean(df3$H3[which(df3$no_stability3 == 9)],na.rm=TRUE),
         mean(df3$H3[which(df3$no_stability3 == 10)],na.rm=TRUE))

H4 <- c(mean(df4$H4[which(df4$no_stability4 == 1)],na.rm=TRUE),
         mean(df4$H4[which(df4$no_stability4 == 2)],na.rm=TRUE),
         mean(df4$H4[which(df4$no_stability4 == 3)],na.rm=TRUE),
         mean(df4$H4[which(df4$no_stability4 == 4)],na.rm=TRUE),
         mean(df4$H4[which(df4$no_stability4 == 5)],na.rm=TRUE),
         mean(df4$H4[which(df4$no_stability4 == 6)],na.rm=TRUE),
         mean(df4$H4[which(df4$no_stability4 == 7)],na.rm=TRUE),
         mean(df4$H4[which(df4$no_stability4 == 8)],na.rm=TRUE),
         mean(df4$H4[which(df4$no_stability4 == 9)],na.rm=TRUE),
         mean(df4$H4[which(df4$no_stability4 == 10)],na.rm=TRUE))
# Plot the mean against ASL stability ranges
plot(c(1:10),H1,type='l',ylim=c(-50,100),xlab='ASL stability ranges',ylab='H',
     lwd=2)
lines(c(1:10),H2,type='l',col='blue',lwd=2)
lines(c(1:10),H3,type='l',col='red',lwd=2)
lines(c(1:10),H4,type='l',col='purple',lwd=2)
legend(x=8.5,y=100,legend=c('1','2','3','4'),lty=c(1,1,1,1),
       col=c('black','blue','red','purple'))

#### Plotting median C_E with ASL ####
CE1 <- c(median(df1$C_E1[which(df1$no_stability1 == 1)],na.rm=TRUE),
         median(df1$C_E1[which(df1$no_stability1 == 2)],na.rm=TRUE),
         median(df1$C_E1[which(df1$no_stability1 == 3)],na.rm=TRUE),
         median(df1$C_E1[which(df1$no_stability1 == 4)],na.rm=TRUE),
         median(df1$C_E1[which(df1$no_stability1 == 5)],na.rm=TRUE),
         median(df1$C_E1[which(df1$no_stability1 == 6)],na.rm=TRUE),
         median(df1$C_E1[which(df1$no_stability1 == 7)],na.rm=TRUE),
         median(df1$C_E1[which(df1$no_stability1 == 8)],na.rm=TRUE),
         median(df1$C_E1[which(df1$no_stability1 == 9)],na.rm=TRUE),
         median(df1$C_E1[which(df1$no_stability1 == 10)],na.rm=TRUE))

CE2 <- c(median(df2$C_E2[which(df2$no_stability2 == 1)],na.rm=TRUE),
         median(df2$C_E2[which(df2$no_stability2 == 2)],na.rm=TRUE),
         median(df2$C_E2[which(df2$no_stability2 == 3)],na.rm=TRUE),
         median(df2$C_E2[which(df2$no_stability2 == 4)],na.rm=TRUE),
         median(df2$C_E2[which(df2$no_stability2 == 5)],na.rm=TRUE),
         median(df2$C_E2[which(df2$no_stability2 == 6)],na.rm=TRUE),
         median(df2$C_E2[which(df2$no_stability2 == 7)],na.rm=TRUE),
         median(df2$C_E2[which(df2$no_stability2 == 8)],na.rm=TRUE),
         median(df2$C_E2[which(df2$no_stability2 == 9)],na.rm=TRUE),
         median(df2$C_E2[which(df2$no_stability2 == 10)],na.rm=TRUE))

CE3 <- c(median(df3$C_E3[which(df3$no_stability3 == 1)],na.rm=TRUE),
         median(df3$C_E3[which(df3$no_stability3 == 2)],na.rm=TRUE),
         median(df3$C_E3[which(df3$no_stability3 == 3)],na.rm=TRUE),
         median(df3$C_E3[which(df3$no_stability3 == 4)],na.rm=TRUE),
         median(df3$C_E3[which(df3$no_stability3 == 5)],na.rm=TRUE),
         median(df3$C_E3[which(df3$no_stability3 == 6)],na.rm=TRUE),
         median(df3$C_E3[which(df3$no_stability3 == 7)],na.rm=TRUE),
         median(df3$C_E3[which(df3$no_stability3 == 8)],na.rm=TRUE),
         median(df3$C_E3[which(df3$no_stability3 == 9)],na.rm=TRUE),
         median(df3$C_E3[which(df3$no_stability3 == 10)],na.rm=TRUE))

CE4 <- c(median(df4$C_E4[which(df4$no_stability4 == 1)],na.rm=TRUE),
         median(df4$C_E4[which(df4$no_stability4 == 2)],na.rm=TRUE),
         median(df4$C_E4[which(df4$no_stability4 == 3)],na.rm=TRUE),
         median(df4$C_E4[which(df4$no_stability4 == 4)],na.rm=TRUE),
         median(df4$C_E4[which(df4$no_stability4 == 5)],na.rm=TRUE),
         median(df4$C_E4[which(df4$no_stability4 == 6)],na.rm=TRUE),
         median(df4$C_E4[which(df4$no_stability4 == 7)],na.rm=TRUE),
         median(df4$C_E4[which(df4$no_stability4 == 8)],na.rm=TRUE),
         median(df4$C_E4[which(df4$no_stability4 == 9)],na.rm=TRUE),
         median(df4$C_E4[which(df4$no_stability4 == 10)],na.rm=TRUE))
# Plot the median against ASL stability ranges
plot(c(1:10),CE1,type='l',ylim=c(0,0.003),xlab='ASL stability ranges',ylab='C_E',
     lwd=2)
lines(c(1:10),CE2,type='l',col='blue',lwd=2)
lines(c(1:10),CE3,type='l',col='red',lwd=2)
lines(c(1:10),CE4,type='l',col='purple',lwd=2)
legend(x=8.5,y=0.003,legend=c('1','2','3','4'),lty=c(1,1,1,1),
       col=c('black','blue','red','purple'))

#### Plotting median C_H with ASL ####
CH1 <- c(median(df1$C_H1[which(df1$no_stability1 == 1)],na.rm=TRUE),
        median(df1$C_H1[which(df1$no_stability1 == 2)],na.rm=TRUE),
        median(df1$C_H1[which(df1$no_stability1 == 3)],na.rm=TRUE),
        median(df1$C_H1[which(df1$no_stability1 == 4)],na.rm=TRUE),
        median(df1$C_H1[which(df1$no_stability1 == 5)],na.rm=TRUE),
        median(df1$C_H1[which(df1$no_stability1 == 6)],na.rm=TRUE),
        median(df1$C_H1[which(df1$no_stability1 == 7)],na.rm=TRUE),
        median(df1$C_H1[which(df1$no_stability1 == 8)],na.rm=TRUE),
        median(df1$C_H1[which(df1$no_stability1 == 9)],na.rm=TRUE),
        median(df1$C_H1[which(df1$no_stability1 == 10)],na.rm=TRUE))

CH2 <- c(median(df2$C_H2[which(df2$no_stability2 == 1)],na.rm=TRUE),
        median(df2$C_H2[which(df2$no_stability2 == 2)],na.rm=TRUE),
        median(df2$C_H2[which(df2$no_stability2 == 3)],na.rm=TRUE),
        median(df2$C_H2[which(df2$no_stability2 == 4)],na.rm=TRUE),
        median(df2$C_H2[which(df2$no_stability2 == 5)],na.rm=TRUE),
        median(df2$C_H2[which(df2$no_stability2 == 6)],na.rm=TRUE),
        median(df2$C_H2[which(df2$no_stability2 == 7)],na.rm=TRUE),
        median(df2$C_H2[which(df2$no_stability2 == 8)],na.rm=TRUE),
        median(df2$C_H2[which(df2$no_stability2 == 9)],na.rm=TRUE),
        median(df2$C_H2[which(df2$no_stability2 == 10)],na.rm=TRUE))

CH3 <- c(median(df3$C_H3[which(df3$no_stability3 == 1)],na.rm=TRUE),
        median(df3$C_H3[which(df3$no_stability3 == 2)],na.rm=TRUE),
        median(df3$C_H3[which(df3$no_stability3 == 3)],na.rm=TRUE),
        median(df3$C_H3[which(df3$no_stability3 == 4)],na.rm=TRUE),
        median(df3$C_H3[which(df3$no_stability3 == 5)],na.rm=TRUE),
        median(df3$C_H3[which(df3$no_stability3 == 6)],na.rm=TRUE),
        median(df3$C_H3[which(df3$no_stability3 == 7)],na.rm=TRUE),
        median(df3$C_H3[which(df3$no_stability3 == 8)],na.rm=TRUE),
        median(df3$C_H3[which(df3$no_stability3 == 9)],na.rm=TRUE),
        median(df3$C_H3[which(df3$no_stability3 == 10)],na.rm=TRUE))

CH4 <- c(median(df4$C_H4[which(df4$no_stability4 == 1)],na.rm=TRUE),
        median(df4$C_H4[which(df4$no_stability4 == 2)],na.rm=TRUE),
        median(df4$C_H4[which(df4$no_stability4 == 3)],na.rm=TRUE),
        median(df4$C_H4[which(df4$no_stability4 == 4)],na.rm=TRUE),
        median(df4$C_H4[which(df4$no_stability4 == 5)],na.rm=TRUE),
        median(df4$C_H4[which(df4$no_stability4 == 6)],na.rm=TRUE),
        median(df4$C_H4[which(df4$no_stability4 == 7)],na.rm=TRUE),
        median(df4$C_H4[which(df4$no_stability4 == 8)],na.rm=TRUE),
        median(df4$C_H4[which(df4$no_stability4 == 9)],na.rm=TRUE),
        median(df4$C_H4[which(df4$no_stability4 == 10)],na.rm=TRUE))
# Plot the median against ASL stability ranges
plot(c(1:10),CH1,type='l',ylim=c(0,0.003),xlab='ASL stability ranges',ylab='C_H',
     lwd=2)
lines(c(1:10),CH2,type='l',col='blue',lwd=2)
lines(c(1:10),CH3,type='l',col='red',lwd=2)
lines(c(1:10),CH4,type='l',col='purple',lwd=2)
legend(x=8.5,y=0.003,legend=c('1','2','3','4'),lty=c(1,1,1,1),
       col=c('black','blue','red','purple'))

#### Plotting median C_D with ASL ####
CD1 <- c(median(df1$C_D1[which(df1$no_stability1 == 1)],na.rm=TRUE),
         median(df1$C_D1[which(df1$no_stability1 == 2)],na.rm=TRUE),
         median(df1$C_D1[which(df1$no_stability1 == 3)],na.rm=TRUE),
         median(df1$C_D1[which(df1$no_stability1 == 4)],na.rm=TRUE),
         median(df1$C_D1[which(df1$no_stability1 == 5)],na.rm=TRUE),
         median(df1$C_D1[which(df1$no_stability1 == 6)],na.rm=TRUE),
         median(df1$C_D1[which(df1$no_stability1 == 7)],na.rm=TRUE),
         median(df1$C_D1[which(df1$no_stability1 == 8)],na.rm=TRUE),
         median(df1$C_D1[which(df1$no_stability1 == 9)],na.rm=TRUE),
         median(df1$C_D1[which(df1$no_stability1 == 10)],na.rm=TRUE))

CD2 <- c(median(df2$C_D2[which(df2$no_stability2 == 1)],na.rm=TRUE),
         median(df2$C_D2[which(df2$no_stability2 == 2)],na.rm=TRUE),
         median(df2$C_D2[which(df2$no_stability2 == 3)],na.rm=TRUE),
         median(df2$C_D2[which(df2$no_stability2 == 4)],na.rm=TRUE),
         median(df2$C_D2[which(df2$no_stability2 == 5)],na.rm=TRUE),
         median(df2$C_D2[which(df2$no_stability2 == 6)],na.rm=TRUE),
         median(df2$C_D2[which(df2$no_stability2 == 7)],na.rm=TRUE),
         median(df2$C_D2[which(df2$no_stability2 == 8)],na.rm=TRUE),
         median(df2$C_D2[which(df2$no_stability2 == 9)],na.rm=TRUE),
         median(df2$C_D2[which(df2$no_stability2 == 10)],na.rm=TRUE))

CD3 <- c(median(df3$C_D3[which(df3$no_stability3 == 1)],na.rm=TRUE),
         median(df3$C_D3[which(df3$no_stability3 == 2)],na.rm=TRUE),
         median(df3$C_D3[which(df3$no_stability3 == 3)],na.rm=TRUE),
         median(df3$C_D3[which(df3$no_stability3 == 4)],na.rm=TRUE),
         median(df3$C_D3[which(df3$no_stability3 == 5)],na.rm=TRUE),
         median(df3$C_D3[which(df3$no_stability3 == 6)],na.rm=TRUE),
         median(df3$C_D3[which(df3$no_stability3 == 7)],na.rm=TRUE),
         median(df3$C_D3[which(df3$no_stability3 == 8)],na.rm=TRUE),
         median(df3$C_D3[which(df3$no_stability3 == 9)],na.rm=TRUE),
         median(df3$C_D3[which(df3$no_stability3 == 10)],na.rm=TRUE))

CD4 <- c(median(df4$C_D4[which(df4$no_stability4 == 1)],na.rm=TRUE),
         median(df4$C_D4[which(df4$no_stability4 == 2)],na.rm=TRUE),
         median(df4$C_D4[which(df4$no_stability4 == 3)],na.rm=TRUE),
         median(df4$C_D4[which(df4$no_stability4 == 4)],na.rm=TRUE),
         median(df4$C_D4[which(df4$no_stability4 == 5)],na.rm=TRUE),
         median(df4$C_D4[which(df4$no_stability4 == 6)],na.rm=TRUE),
         median(df4$C_D4[which(df4$no_stability4 == 7)],na.rm=TRUE),
         median(df4$C_D4[which(df4$no_stability4 == 8)],na.rm=TRUE),
         median(df4$C_D4[which(df4$no_stability4 == 9)],na.rm=TRUE),
         median(df4$C_D4[which(df4$no_stability4 == 10)],na.rm=TRUE))
# Plot the median against ASL stability ranges
plot(c(1:10),CD1,type='l',ylim=c(0,0.003),xlab='ASL stability ranges',ylab='C_D',
     lwd=2)
lines(c(1:10),CD2,type='l',col='blue',lwd=2)
lines(c(1:10),CD3,type='l',col='red',lwd=2)
lines(c(1:10),CD4,type='l',col='purple',lwd=2)
legend(x=8.5,y=0.003,legend=c('1','2','3','4'),lty=c(1,1,1,1),
       col=c('black','blue','red','purple'))

#### Plotting C_E, C_H, C_D for class 1 #########
plot(c(1:10),CD1,type='l',ylim=c(0,0.003),xlab='ASL stability ranges',ylab='C',
     lwd=2)
lines(c(1:10),CH1,type='l',col='blue',lwd=2)
lines(c(1:10),CE1,type='l',col='red',lwd=2)
legend(x=8.5,y=0.003,legend=c('CD1','CH1','CE1'),lty=c(1,1,1),
       col=c('black','blue','red'))

#### Plotting C_E, C_H, C_D for class 2 #########
plot(c(1:10),CD2,type='l',ylim=c(0,0.003),xlab='ASL stability ranges',ylab='C',
     lwd=2)
lines(c(1:10),CH2,type='l',col='blue',lwd=2)
lines(c(1:10),CE2,type='l',col='red',lwd=2)
legend(x=8.5,y=0.003,legend=c('CD2','CH2','CE2'),lty=c(1,1,1),
       col=c('black','blue','red'))

#### Plotting C_E, C_H, C_D for class 3 #########
plot(c(1:10),CD3,type='l',ylim=c(0,0.003),xlab='ASL stability ranges',ylab='C',
     lwd=2)
lines(c(1:10),CH3,type='l',col='blue',lwd=2)
lines(c(1:10),CE3,type='l',col='red',lwd=2)
legend(x=8.5,y=0.003,legend=c('CD3','CH3','CE3'),lty=c(1,1,1),
       col=c('black','blue','red'))

#### Plotting C_E, C_H, C_D for class 4 #########
plot(c(1:10),CD4,type='l',ylim=c(0,0.003),xlab='ASL stability ranges',ylab='C',
     lwd=2)
lines(c(1:10),CH4,type='l',col='blue',lwd=2)
lines(c(1:10),CE4,type='l',col='red',lwd=2)
legend(x=8.5,y=0.003,legend=c('CD4','CH4','CE4'),lty=c(1,1,1),
       col=c('black','blue','red'))


## Plotting LE versus deltaE and U
plot(df1$deltaE1,df1$LE1,xlim=c(-0.5,3),ylim=c(-100,450))
plot(df2$deltaE2,df2$LE2,col='blue',xlim=c(-0.5,3),ylim=c(-100,450))
plot(df3$deltaE3,df3$LE3,col='red',xlim=c(-0.5,3),ylim=c(-100,450))
plot(df4$deltaE4,df4$LE4,col='orange',xlim=c(-0.5,3),ylim=c(-100,450))

plot(df1$U1,df1$LE1,xlim=c(0,15),ylim=c(-150,450))
plot(df2$U2,df2$LE2,col='blue',xlim=c(0,15),ylim=c(-150,450))
plot(df3$U3,df3$LE3,col='red',xlim=c(0,15),ylim=c(-150,450))
plot(df4$U4,df4$LE4,col='orange',xlim=c(0,15),ylim=c(-150,450))

udE1 <- df1$U1 * df1$deltaE1
udE2 <- df2$U2 * df2$deltaE2
udE3 <- df3$U3 * df3$deltaE3
udE4 <- df4$U4 * df4$deltaE4

plot(udE1,df1$LE1,xlim=c(-5,15),ylim=c(-150,450))
points(udE2,df2$LE2,col='blue',xlim=c(-5,15),ylim=c(-150,450))
points(udE3,df3$LE3,col='red',xlim=c(-5,15),ylim=c(-150,450))
points(udE4,df4$LE4,col='orange',xlim=c(-5,15),ylim=c(-150,450))

## Plotting H versus deltaT and U
plot(df1$deltaT1,df1$H1,xlim=c(-12,12),ylim=c(-150,300))
plot(df2$deltaT2,df2$H2,col='blue',xlim=c(-12,12),ylim=c(-150,300))
plot(df3$deltaT3,df3$H3,col='red',xlim=c(-12,12),ylim=c(-150,300))
plot(df4$deltaT4,df4$H4,col='orange',xlim=c(-12,12),ylim=c(-150,300))

plot(df1$U1,df1$H1,xlim=c(0,15),ylim=c(-150,300))
plot(df2$U2,df2$H2,col='blue',xlim=c(0,15),ylim=c(-150,300))
plot(df3$U3,df3$H3,col='red',xlim=c(0,15),ylim=c(-150,300))
plot(df4$U4,df4$H4,col='orange',xlim=c(0,15),ylim=c(-150,300))

udT1 <- df1$U1 * df1$deltaT1
udT2 <- df2$U2 * df2$deltaT2
udT3 <- df3$U3 * df3$deltaT3
udT4 <- df4$U4 * df4$deltaT4

plot(udT1,df1$H1,xlim=c(-100,100),ylim=c(-150,300))
plot(udT2,df2$H2,col='blue',xlim=c(-100,100),ylim=c(-150,300))
plot(udT3,df3$H3,col='red',xlim=c(-100,100),ylim=c(-150,300))
plot(udT4,df4$H4,col='orange',xlim=c(-100,100),ylim=c(-150,300))

plot(df1$U1,df1$deltaE1)
points(df2$U2,df2$deltaE2,col='blue')
points(df3$U3,df3$deltaE3,col='red')
points(df4$U4,df4$deltaE4,col='orange')

boxplot(df1$deltaE1,df2$deltaE2,df3$deltaE3,df4$deltaE4)
boxplot(df1$deltaT1,df2$deltaT2,df3$deltaT3,df4$deltaT4)

boxplot(df1$U1,df2$U2,df3$U3,df4$U4)

#### Fig. 4 smoothed all parameters ############
path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/lake/figs/wind_figs/fig_4_smoothed.jpg')
jpeg(file=path_fig,width=8, height=10,res=360,units='in')
plot.new()

## a) U with ASL ranges 
plot1 <- ggplot(data = df1, aes(x = no_stability1, y = U1)) +
  stat_smooth(colour = 'black', lwd = 1.5, lty = 1) +
  stat_smooth(data = df2, aes(x = no_stability2, y = U2), lty = 2, colour = 'black', lwd = 1.5) +
  stat_smooth(data = df3, aes(x = no_stability3, y = U3), lty = 5, colour = 'black', lwd = 1.5) + 
  stat_smooth(data = df4, aes(x = no_stability4, y = U4), lty = 4, colour = 'black', lwd = 1.5) +
  theme_bw() + 
  labs(x = '', y = 'U',vjust = 10) +
  annotate("text",x=1,y=7.8,label="a)",size=7,family='Times') +
  theme(panel.grid.major=element_line(size=0),
        panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',margin=margin(0,20,0,0)),
        axis.text.y=element_text(size=16,family='Times'),
        axis.text.x=element_blank(),
        plot.margin=unit(c(1,1,5,2),"mm")) +
  scale_y_continuous(breaks = seq(0, 8, by = 2), 
                     limits = c(0,8)) +
  scale_x_continuous(breaks=seq(1, 10, by = 1),
                     limits = c(1, 10))

## b) deltaE with ASL ranges 
plot2 <- ggplot(data = df1, aes(x = no_stability1, y = deltaE1)) +
  stat_smooth(colour = 'black', lwd = 1.5, lty = 1, alpha = 0.2) +
  stat_smooth(data = df2, aes(x = no_stability2, y = deltaE2), 
              lty = 2, colour = 'black', lwd = 1.5, alpha = 0.2) +
  stat_smooth(data = df3, aes(x = no_stability3, y = deltaE3), 
              lty = 5, colour = 'black', lwd = 1.5, alpha = 0.2) + 
  stat_smooth(data = df4, aes(x = no_stability4, y = deltaE4), 
              lty = 4, colour = 'black', lwd = 1.5, alpha = 0.2) +
  theme_bw() +
  xlab('') + ylab(expression(paste(Delta,'e'))) +
  annotate("text",x=1,y=1.08,label="b)",size=7,family='Times') +
  theme(panel.grid.major=element_line(size=0),
        panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),
        axis.text.y=element_text(size=16,family='Times'),
        axis.text.x=element_blank(),
        plot.margin=unit(c(-7,1,9,2),"mm")) +
  scale_y_continuous(breaks = seq(0, 1.2, by = 0.2)) +
  scale_x_continuous(breaks=seq(1, 10, by = 1),
                     limits = c(1, 10))

## e) deltaT with ASL ranges
plot3 <- ggplot(data = df1, aes(x = no_stability1, y = deltaT1)) +
  stat_smooth(colour = 'black', lwd = 1.5, lty = 1) +
  stat_smooth(data = df2, aes(x = no_stability2, y = deltaT2), 
              lty = 2, colour = 'black', lwd = 1.5) +
  stat_smooth(data = df3, aes(x = no_stability3, y = deltaT3), 
              lty = 5, colour = 'black', lwd = 1.5) + 
  stat_smooth(data = df4, aes(x = no_stability4, y = deltaT4), 
              lty = 4, colour = 'black', lwd = 1.5) +
  theme_bw() +
  xlab('') + ylab(expression(paste(Delta,'T'))) +
  annotate("text",x=1,y=5.8,label="e)",size=7,family='Times') +
  theme(panel.grid.major=element_line(size=0),
        panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',margin=margin(0,10,0,0)),
        axis.text.y=element_text(size=16,family='Times'),
        axis.text.x=element_blank(),
        plot.margin=unit(c(-7,1,9,2),"mm")) +
  scale_y_continuous(breaks = seq(-6, 6, by = 2),
                     labels=c(paste("\u2212",6,sep=""),
                              paste("\u2212",4,sep=""),
                              paste("\u2212",2,sep=""),
                              0,2,4,6)) +
  scale_x_continuous(breaks=seq(1, 10, by = 1),
                     limits = c(1, 10))

## c) UdeltaE with ASL ranges
plot4 <- ggplot(data = df1, aes(x = no_stability1, y = udeltaE1)) +
  stat_smooth(colour = 'black', lwd = 1.5, lty = 1, alpha = 0.2) +
  stat_smooth(data = df2, aes(x = no_stability2, y = udeltaE2), 
              lty = 2, colour = 'black', lwd = 1.5, alpha = 0.2) +
  stat_smooth(data = df3, aes(x = no_stability3, y = udeltaE3), 
              lty = 5, colour = 'black', lwd = 1.5, alpha = 0.2) + 
  stat_smooth(data = df4, aes(x = no_stability4, y = udeltaE4), 
              lty = 4, colour = 'black', lwd = 1.5, alpha = 0.2) +
  theme_bw() +
  xlab('') + ylab(expression(paste('U',Delta,'e'))) +
  annotate("text",x=1,y=5.7,label="c)",size=7,family='Times') +
  theme(panel.grid.major=element_line(size=0),
        panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',margin=margin(0,10,0,0)),
        axis.text.y=element_text(size=16,family='Times'),
        axis.text.x=element_blank(),
        plot.margin=unit(c(-11,1,15,1.8),"mm")) +
  scale_y_continuous(breaks = seq(-1, 6, by = 1),
                     labels = c(paste("\u2212",1,sep=""),
                                0,1,2,3,4,5,6)) +
  scale_x_continuous(breaks=seq(1, 10, by = 1),
                     limits = c(1, 10))

## f) UdeltaT with ASL ranges
plot5 <- ggplot(data = df1, aes(x = no_stability1, y = udeltaT1)) +
  stat_smooth(colour = 'black', lwd = 1.5, lty = 1) +
  stat_smooth(data = df2, aes(x = no_stability2, y = udeltaT2), 
              lty = 2, colour = 'black', lwd = 1.5) +
  stat_smooth(data = df3, aes(x = no_stability3, y = udeltaT3), 
              lty = 5, colour = 'black', lwd = 1.5) + 
  stat_smooth(data = df4, aes(x = no_stability4, y = udeltaT4), 
              lty = 4, colour = 'black', lwd = 1.5) +
  theme_bw() + 
  xlab('') + ylab(expression(paste('U',Delta,'T'))) +
  annotate("text",x=1,y=32,label="f)",size=7,family='Times') +
  theme(panel.grid.major=element_line(size=0),
        panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-11,1,15,0),"mm")) +
  scale_y_continuous(breaks = seq(-40, 40, by = 10),
                     labels = c(paste("\u2212",40,sep=""),
                                paste("\u2212",30,sep=""),
                                paste("\u2212",20,sep=""),
                                paste("\u2212",10,sep=""),
                                0,10,20,30,40)) +
  scale_x_continuous(breaks=seq(1, 10, by = 1),
                     limits = c(1, 10))


## d) LE with ASL ranges
plot6 <- ggplot(data = df1, aes(x = no_stability1, y = LE1)) +
  stat_smooth(colour = 'black', lwd = 1.5, lty = 1, alpha = 0.2) +
  stat_smooth(data = df2, aes(x = no_stability2, y = LE2), 
              lty = 2, colour = 'black', lwd = 1.5, alpha = 0.2) +
  stat_smooth(data = df3, aes(x = no_stability3, y = LE3), 
              lty = 5, colour = 'black', lwd = 1.5, alpha = 0.2) + 
  stat_smooth(data = df4, aes(x = no_stability4, y = LE4), 
              lty = 4, colour = 'black', lwd = 1.5, alpha = 0.2) +
  theme_bw() + 
  xlab('') + ylab('LE') +
  annotate("text",x=1,y=170,label="d)",size=7,family='Times') +
  theme(panel.grid.major=element_line(size=0),
        panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),
        axis.text.y=element_text(size=16,family='Times'),
        axis.text.x=element_text(angle=90,size=12,hjust=1,family='Times',vjust=0.5),
        plot.margin=unit(c(-17,1,0,0),"mm")) +
  scale_y_continuous(breaks = seq(-50, 200, by = 50)) +
  scale_x_continuous(breaks=seq(1, 10, by = 1),
                     labels = names_boxplot)

## g) H with ASL ranges
plot7 <- ggplot(data = df1, aes(x = no_stability1, y = H1)) +
  stat_smooth(colour = 'black', lwd = 1.5, lty = 1, alpha = 0.2) +
  stat_smooth(data = df2, aes(x = no_stability2, y = H2), 
              lty = 2, colour = 'black', lwd = 1.5, alpha = 0.2) +
  stat_smooth(data = df3, aes(x = no_stability3, y = H3), 
              lty = 5, colour = 'black', lwd = 1.5, alpha = 0.2) + 
  stat_smooth(data = df4, aes(x = no_stability4, y = H4), 
              lty = 4, colour = 'black', lwd = 1.5, alpha = 0.2) +
  theme_bw() +
  xlab('') + ylab('H') +
  annotate("text",x=1,y=77,label="g)",size=7,family='Times') +
  theme(panel.grid.major=element_line(size=0),
        panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),
        axis.text.y=element_text(size=16,family='Times'),
        axis.text.x=element_text(angle=90,size=12,hjust=1,family='Times',vjust=0.5),
        plot.margin=unit(c(-17,1,0,0),"mm")) +
  scale_y_continuous(breaks = seq(-60, 100, by = 20),
                     labels = c(paste("\u2212",60,sep=""),
                                paste("\u2212",40,sep=""),
                                paste("\u2212",20,sep=""),
                                0,20,40,60,80,100)) +
  scale_x_continuous(breaks=seq(1, 10, by = 1),
                     labels = names_boxplot)

plot_empty <- ggplot() + theme_bw() + theme(panel.border = element_blank())

multiplot2(plot1,plot2,plot4,plot6,plot_empty,plot3,plot5,plot7,
           cols=2,labs=list("ASL stability ranges"," "))
rm(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot_empty)
dev.off()

#### Smoothed coefficient plots ####
path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/lake/figs/wind_figs/fig6_smoothed.jpg')
jpeg(file=path_fig,width=5, height=10,res=360,units='in')
plot.new()

## a) CD with ASL ranges
plot8 <- ggplot(data = df1, aes(x = no_stability1, y = C_D1)) +
  stat_smooth(colour = 'black', lwd = 1.5, lty = 1, alpha = 0.2) +
  stat_smooth(data = df2, aes(x = no_stability2, y = C_D2), 
              lty = 2, colour = 'black', lwd = 1.5, alpha = 0.2) +
  stat_smooth(data = df3, aes(x = no_stability3, y = C_D3), 
              lty = 5, colour = 'black', lwd = 1.5, alpha = 0.2) + 
  stat_smooth(data = df4, aes(x = no_stability4, y = C_D4), 
              lty = 4, colour = 'black', lwd = 1.5, alpha = 0.2) +
  theme_bw() + 
  xlab('') + ylab(expression('C'['D'])) +
  annotate("text",x=1,y=0.03,label="a)",size=7,family='Times') +
  theme(panel.grid.major=element_line(size=0),
        panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times'),
        axis.text.y=element_text(size=16,family='Times'),
        axis.text.x=element_blank(),
        plot.margin=unit(c(1,1,12,2),"mm")) +
  scale_y_continuous(breaks = seq(-0.01, 0.03, by = 0.01))

## b) CE with ASL ranges
plot9 <- ggplot(data = df1, aes(x = no_stability1, y = C_E1)) +
  stat_smooth(colour = 'black', lwd = 1.5, lty = 1, alpha = 0.2) +
  stat_smooth(data = df2, aes(x = no_stability2, y = C_E2), 
              lty = 2, colour = 'black', lwd = 1.5, alpha = 0.2) +
  stat_smooth(data = df3, aes(x = no_stability3, y = C_E3), 
              lty = 5, colour = 'black', lwd = 1.5, alpha = 0.2) + 
  stat_smooth(data = df4, aes(x = no_stability4, y = C_E4), 
              lty = 4, colour = 'black', lwd = 1.5, alpha = 0.2) +
  theme_bw() +
  xlab('') + ylab(expression('C'['E'])) +
  annotate("text",x=1,y=0.003,label="b)",size=7,family='Times') +
  theme(panel.grid.major=element_line(size=0),
        panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',margin=margin(0,-1,0,0)),
        axis.text.y=element_text(size=16,family='Times'),
        axis.text.x=element_blank(),
        plot.margin=unit(c(-10,1,22,2),"mm")) +
  scale_y_continuous(breaks = seq(0, 0.003, by = 0.001),
                     limits = c(0,0.003))

## c) CH with ASL ranges
plot10 <- ggplot(data = df1, aes(x = no_stability1, y = C_H1)) +
  stat_smooth(colour = 'black', lwd = 1.5, lty = 1, alpha = 0.2) +
  stat_smooth(data = df2, aes(x = no_stability2, y = C_H2), 
              lty = 2, colour = 'black', lwd = 1.5, alpha = 0.2) +
  stat_smooth(data = df3, aes(x = no_stability3, y = C_H3), 
              lty = 5, colour = 'black', lwd = 1.5, alpha = 0.2) + 
  stat_smooth(data = df4, aes(x = no_stability4, y = C_H4), 
              lty = 4, colour = 'black', lwd = 1.5, alpha = 0.2) +
  theme_bw() + 
  xlab('') + ylab(expression('C'['H'])) +
  annotate("text",x=1,y=0.003,label="c)",size=7,family='Times') +
  theme(panel.grid.major=element_line(size=0),
        panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',margin=margin(0,-1,0,0)),
        axis.text.y=element_text(size=16,family='Times'),
        axis.text.x=element_text(angle=90,size=14,hjust=1,family='Times',vjust=0.5),
        plot.margin=unit(c(-20,1,7,2),"mm")) +
  scale_y_continuous(breaks = seq(0, 0.003, by = 0.001),
                     limits = c(0,0.003)) +
  scale_x_continuous(breaks=seq(1, 10, by = 1),
                     labels = names_boxplot)


multiplot2(plot8,plot9,plot10,
           cols=1,labs=list("ASL stability ranges"," "))
rm(plot8,plot9,plot10)
dev.off()

#### ustar with u under different wind-class ####
# Path where the plots will be saved
path_fig <- 
  file.path('/Users/Yusri/Documents/Work/Data_analysis/lake/figs/wind_figs/fig_ustar.jpg')
jpeg(file=path_fig,width=8, height = 8,res=360,units='cm')
plot.new()

par(family='Times',oma=c(0.5,0.4,0.1,0.1))

# a) For positive deltaE
# Linear regression lines
lm1 <- lm(df1$ustar1 ~ df1$U1)
lm2 <- lm(df2$ustar2 ~ df2$U2)
lm3 <- lm(df3$ustar3 ~ df3$U3)
lm4 <- lm(df4$ustar4 ~ df4$U4)

par(mai=c(0.5,0.6,0,0.1))
library(RColorBrewer)
cols <- brewer.pal(4,"Set1")
plot(df1$U1,df1$ustar1,pch=19,col=cols[1],
     xlab='', ylab='',cex=0.2,cex.axis=1,
     cex.lab=1)

mtext(side=2,expression(paste('u'['*'])),line=2,cex=1)
mtext(side = 1, 'U', line = 2, cex = 1)
points(df2$U2,df2$ustar2,
       pch=19,col = cols[2],cex=0.2)
points(df3$U3,df3$ustar3,
       pch=19,col = cols[3],cex=0.2)
points(df4$U4,df4$ustar4,
       pch=19,col = cols[4],cex=0.2)
minor.tick(nx=2,ny=2)

abline(lm1,lwd=4,lty=1,col=cols[1])
abline(lm2,lwd=4,lty=2,col=cols[2])
abline(lm3,lwd=4,lty=5,col=cols[3])
abline(lm4,lwd=4,lty=4,col=cols[4])

dev.off()

#### Delete temp variables #########
rm(path_fig,names_boxplot,plot1,plot2,plot3,plot4)
rm(CD1,CD2,CD3,CD4,CE1,CE2,CE3,CE4,CH1,CH2,CH3,CH4)
rm(udE1,udE2,udE3,udE4,udT1,udT2,udT3,udT4)
