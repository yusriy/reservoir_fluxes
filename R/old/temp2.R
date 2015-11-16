names_boxplot = c('-10\u2264\u03B6<-1','-1\u2264\u03B6<-0.5','-0.5\u2264\u03B6<-0.1','-0.1\u2264\u03B6<-0.05',
                  '-0.05\u2264\u03B6<0','0\u2264\u03B6<0.05','0.05\u2264\u03B6<0.1','0.1\u2264\u03B6<0.5','0.5\u2264\u03B6<1',
                  '1\u2264\u03B6<10')



# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/fig_6a.jpg')
jpeg(file=path_fig,width=5,height=10,res=360,units='in')


library(gridBase)
library(grid)
library(ggplot2)

#### First base graphic plot

par(mfrow=c(2,1),mar=c(4.1,4.6,1,1.1),family='Times')

#### a) First ggplot plot
plot.new()
# To combine ggplot and base graphics
vps <- baseViewports()
pushViewport(vps$figure)
vp1 <- plotViewport(margins=c(-0.5,0.5,-0.5,0))

plot5 <- ggplot(na.omit(data[,c('U.','stability_no')]), aes(factor(stability_no),U.)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/20),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='black',geom='text',label='---',size=9) + 
  labs(x="ASL stability ranges",y=expression("u"["*"])) + theme_bw() +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=18,family='Times'),axis.title.x=element_text(size=18,family='Times'),
        axis.text.y=element_text(size=18,family='Times'),axis.text.x=element_text(angle=90,size=18,vjust=1,family='Times')) + 
  scale_x_discrete(labels=names_boxplot) +
  scale_y_continuous(breaks=seq(0,0.7,by=0.1)) + annotate("text",x=0.8,y=0.7,label="a)",family='Times',size=7) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) 
print(plot5,vp = vp1)

#### b) Second base graphic plot

plot(d_mean$WS_mean,d_mean$ustar_mean,type='l',xlim=c(0,6),ylim=c(0,0.25),
     lwd=2,xlab='U',ylab=expression('u'['*']),cex.axis=1.5,cex.lab=1.5,
     yaxt='n',las=1,tck=-0.015,lwd.ticks=1.5)
mid1 <- midpoint(d_mean$WS_mean[3],d_mean$ustar_mean[3],d_mean$WS_mean[4],
                 d_mean$ustar_mean[4],ratio1=2,ratio2=3)
mid2 <- midpoint(d_mean$WS_mean[7],d_mean$ustar_mean[7],d_mean$WS_mean[6],
                 d_mean$ustar_mean[6],ratio1=2,ratio2=3)
arrows(x0=d_mean$WS_mean[3],y0=d_mean$ustar_mean[3],x1=mid1[1],y1=mid1[2],
       lwd=3,length=0.15)
arrows(x0=d_mean$WS_mean[7],y0=d_mean$ustar_mean[7],x1=mid2[1],y1=mid2[2],
       lwd=3,length=0.15)
points(d_mean$WS_mean,d_mean$ustar_mean,pch=19,cex=2)
axis(side=2,tck=-0.015,lwd.ticks=1.5,labels=NA)
axis(side=2,at=c(0,0.05,0.1,0.15,0.2,0.25),cex.axis=1.5,las=1,lwd=0,line=-0.5)
text(0,0.25,'b)',cex=1.5)
text(3.8,0.1,'stable',cex=1.2)
text(5,0.24,'near-neutral',cex=1.2)
text(2.5,0.15,'unstable',cex=1.2)

dev.off()