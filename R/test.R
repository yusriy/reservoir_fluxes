path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/draft_figs/time_series.jpg')
jpeg(file=path_fig,width=2900,height=3600,res=360)
## Creating 5 panels of plots
plot.new()
par(mfrow=c(5,1),oma=c(5.1,0.1,0.1,0.1))

vert_line <- c(-10,250)
horiz_line <- c(0,24)

# a) LE and H
par(mai=c(0,0.6,0.1,0.1))
plot(data_group$hour[data_group$month == '09'],
     data_group$LE[data_group$month == '09'],
     ylab='LE and H',xlab='',type='l',ylim=c(0,200),lwd=2,xaxt='n',cex.lab=2)
lines(data_group$hour[data_group$month == '09'],
      data_group$H[data_group$month == '09'],lty=2,lwd=2)
lines(c(18,18),vert_line,lwd=2,lty=3)
lines(c(14,14),vert_line,lwd=2,lty=3)
lines(c(13,13),vert_line,lwd=2,lty=3)
lines(c(7,7),vert_line,lwd=2,lty=3)
text(0,196,'a)',cex=2)
legend(20,195,y.intersp=0.8,bty='n',lty=c(1,2),lwd=c(2,2),c('LE','H'),cex=2)
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
lines(c(18,18),vert_line,lwd=2,lty=3)
lines(c(14,14),vert_line,lwd=2,lty=3)
lines(c(13,13),vert_line,lwd=2,lty=3)
lines(c(7,7),vert_line,lwd=2,lty=3)
text(0,4.8,'b)',cex=2)
legend(10,1.6,y.intersp=0.5,bty='n',lty=c(2,1,3),lwd=c(2,2,2),
       c(expression('e'['s']),expression('e'['a']),expression(paste(Delta,'e'))),cex=2)
minor.tick(ny=2,nx=5,tick.ratio=0.5)

# c) zeta
par(mai=c(0,0.6,0.1,0.1))
plot(data_group$hour[data_group$month == '09'],
     data_group$Z.L[data_group$month == '09'],
     ylab=expression(zeta),xlab='',type='l',lwd=2,ylim=c(-1,0.5),xaxt ='n',cex.lab=2)
text(0,0.45,'c)',cex=2)
lines(c(18,18),vert_line,lwd=2,lty=3)
lines(c(14,14),vert_line,lwd=2,lty=3)
lines(c(13,13),vert_line,lwd=2,lty=3)
lines(c(7,7),vert_line,lwd=2,lty=3)
lines(horiz_line,c(0,0),lty=3,lwd=2)
lines(horiz_line,c(-0.05,-0.05),lty=3,lwd=2)
lines(horiz_line,c(-0.1,-0.1),lty=3,lwd=2)
lines(horiz_line,c(-0.5,-0.5),lty=3,lwd=2)
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
lines(c(18,18),vert_line,lwd=2,lty=3)
lines(c(14,14),vert_line,lwd=2,lty=3)
lines(c(13,13),vert_line,lwd=2,lty=3)
lines(c(7,7),vert_line,lwd=2,lty=3)
text(0,34,'d)',cex=2)

# e) Wind speed
par(mai=c(0,0.6,0.1,0.1))
plot(data_group$hour[data_group$month == '09'],
     data_group$WS[data_group$month == '09'],
     ylab='U',xlab='',type='l',lwd=2,ylim=c(2,5),cex.lab=2)
text(0,4.9,'e)',cex=2)
lines(c(18,18),vert_line,lwd=2,lty=3)
lines(c(14,14),vert_line,lwd=2,lty=3)
lines(c(13,13),vert_line,lwd=2,lty=3)
lines(c(7,7),vert_line,lwd=2,lty=3)
minor.tick(ny=5,nx=5,tick.ratio=0.5)

title(xlab='Hour (local time)',ylab='',outer=TRUE,cex.lab=2)

rm(path_fig,horiz_line,vert_line)

dev.off()