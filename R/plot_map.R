# Plot map

library(openair)

# Input coordinates of site
siteDetails <- data.frame(x=1,latitude=32.43823,longitude=-90.03000)

# Plot Google Map
GoogleMapsPlot(siteDetails,lat='latitude',long='longitude',zoom=13,fontsize=18,
               plot.transparent = 1,cex=0.5,col='black')

## TESTING 
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/Test_footp_sta.png')
png(file=path_fig,width=7200, height=7200,res=400,bg='transparent')
# Polar plot without border
polarPlot(angle_stable,pollutant='LE',x='Distance',wd='WD',
          par.settings = list(axis.line = list(col = 0)),statistic='frequency',
          scales=list(x=list(at=NULL)),key=TRUE,smooth=TRUE,fontsize=60,
          key.position='left',resolution='fine',ylim=c(-10000,10000),
          xlim=c(-10000,10000),upper=10000,key.footer='90% FLUX',
          key.header='FREQUENCY',
          exclude.missing = FALSE)

dev.off()

# X % and peak footprint
angle_unstable<-data.frame(data$LE[which(data$Z.L<0)],data$Distance[which(data$Z.L<0)],
                           data$sonic_WD[which(data$Z.L<0)],data$Peak[which(data$Z.L<0)])
angle_stable<-data.frame(data$LE[which(data$Z.L>0 & data$Z.L< 10)],data$Distance[which(data$Z.L>0 & data$Z.L< 10)],
                         data$sonic_WD[which(data$Z.L>0 & data$Z.L< 10)],data$Peak[which(data$Z.L>0 & data$Z.L< 10)])
names(angle_unstable)<-c('LE','Distance','WD','Peak')
names(angle_stable)<-c('LE','Distance','WD','Peak')


polarPlot(angle_stable,pollutant='Distance',x='Distance',wd='WD',
          par.settings = list(axis.line = list(col = 0)),statistic='frequency',
          scales=list(x=list(at=NULL)),key=TRUE,smooth=TRUE,
          key.position='left',resolution='fine',
          key.footer='70% FLUX',upper=4000,k=100,
          key.header='FREQUENCY',
          exclude.missing = FALSE,limits=c(0,5))

## FINAL
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/footp_stable70_01.png')
png(file=path_fig,width=7200, height=7200,res=400,bg='transparent')
# Polar plot without border
polarPlot(angle_stable,pollutant='Distance',x='Distance',wd='WD',
          par.settings = list(axis.line = list(col = 0)),statistic='frequency',
          scales=list(x=list(at=NULL)),key=TRUE,smooth=TRUE,fontsize=60,
          key.position='left',resolution='fine',ylim=c(-4200,4200),
          xlim=c(-4200,4200),upper=10000,key.footer='70% FLUX',k=550,
          key.header='FREQ',limits=c(1,6),cols='increment',
          exclude.missing = FALSE)

dev.off()