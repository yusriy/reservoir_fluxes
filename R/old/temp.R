# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/fig_5new.jpg')
jpeg(file=path_fig,width=1346, height=3600,res=360)
## Creating 6 panels of plots
plot.new()
par(mfrow=c(3,1),mar=c(4.1,4.6,1,1.1),family='Times')

#Figure 5 a) LE and deltaE
plot(d_mean$deltaE_mean,d_mean$LE_mean,type='l',xlim=c(0,1),ylim=c(0,120),
     lwd=2,xlab=expression(paste(Delta,'e')),ylab='LE',cex.axis=2.2,cex.lab=2.2,
     yaxt='n')
mid1 <- midpoint(d_mean$deltaE_mean[5],d_mean$LE_mean[5],d_mean$deltaE_mean[6],
                 d_mean$LE_mean[6],ratio1=2,ratio2=3)
mid2 <- midpoint(d_mean$deltaE_mean[6],d_mean$LE_mean[6],d_mean$deltaE_mean[5],
                 d_mean$LE_mean[5],ratio1=2,ratio2=3)
arrows(x0=d_mean$deltaE_mean[5],y0=d_mean$LE_mean[5],x1=mid1[1],y1=mid1[2],
       lwd=3,length=0.15)
arrows(x0=d_mean$deltaE_mean[6],y0=d_mean$LE_mean[6],x1=mid2[1],y1=mid2[2],
       lwd=3,length=0.15)
points(d_mean$deltaE_mean,d_mean$LE_mean,pch=19,cex=2)
axis(side=2,at=c(0,50,100),cex.axis=2.2)
text(0,120,'a)',cex=2.2)
text(0.3,30,'stable',cex=2)
text(0.4,75,'near-neutral',cex=2)
text(0.82,120,'unstable',cex=2)

#Figure 5 b) LE and U
plot(d_mean$WS_mean,d_mean$LE_mean,type='l',xlim=c(0,6),ylim=c(0,120),
     lwd=2,xlab='U',ylab='LE',cex.axis=2.2,cex.lab=2.2,
     yaxt='n')
mid1 <- midpoint(d_mean$WS_mean[5],d_mean$LE_mean[5],d_mean$WS_mean[6],
                 d_mean$LE_mean[6],ratio1=2,ratio2=3)
mid2 <- midpoint(d_mean$WS_mean[6],d_mean$LE_mean[6],d_mean$WS_mean[5],
                 d_mean$LE_mean[5],ratio1=2,ratio2=3)
arrows(x0=d_mean$WS_mean[5],y0=d_mean$LE_mean[5],x1=mid1[1],y1=mid1[2],
       lwd=3,length=0.15)
arrows(x0=d_mean$WS_mean[6],y0=d_mean$LE_mean[6],x1=mid2[1],y1=mid2[2],
       lwd=3,length=0.15)
points(d_mean$WS_mean,d_mean$LE_mean,pch=19,cex=2)
axis(side=2,at=c(0,50,100),cex.axis=2.2)
text(0,120,'b)',cex=2.2)
text(3.8,20,'stable',cex=2)
text(3.4,70,'near-neutral',cex=2)
text(4.5,120,'unstable',cex=2)

#Figure 5 c) LE and UdeltaE
plot(d_mean$udeltaE_mean,d_mean$LE_mean,type='l',xlim=c(0,5),ylim=c(0,120),
     lwd=2,xlab=expression(paste('U',Delta,'e')),ylab='LE',cex.axis=2.2,cex.lab=2.2,
     yaxt='n')
mid1 <- midpoint(d_mean$udeltaE_mean[5],d_mean$LE_mean[5],d_mean$udeltaE_mean[6],
                 d_mean$LE_mean[6],ratio1=2,ratio2=3)
mid2 <- midpoint(d_mean$udeltaE_mean[6],d_mean$LE_mean[6],d_mean$udeltaE_mean[5],
                 d_mean$LE_mean[5],ratio1=2,ratio2=3)
arrows(x0=d_mean$udeltaE_mean[5],y0=d_mean$LE_mean[5],x1=mid1[1],y1=mid1[2],
       lwd=3,length=0.15)
arrows(x0=d_mean$udeltaE_mean[6],y0=d_mean$LE_mean[6],x1=mid2[1],y1=mid2[2],
       lwd=3,length=0.15)
points(d_mean$udeltaE_mean,d_mean$LE_mean,pch=19,cex=2)
axis(side=2,at=c(0,50,100),cex.axis=2.2)
text(0,120,'c)',cex=2.2)
text(2,20,'stable',cex=2)
text(4,70,'near-neutral',cex=2)
text(2.5,110,'unstable',cex=2)

# To turn off plotting in a different window
dev.off()
## Cleaning up
rm(mid1,mid2)
