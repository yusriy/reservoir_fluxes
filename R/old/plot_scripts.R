
names_boxplot = c('-10\u2264z/L<-1','-1\u2264z/L<-0.5','-0.5\u2264z/L<-0.1','-0.1\u2264z/L<-0.05',
                  '-0.05\u2264z/L<0','0\u2264z/L<0.05','0.05\u2264z/L<0.1','0.1\u2264z/L<0.5','0.5\u2264z/L<1',
                  '1\u2264z/L<10')
#Boxplot of C_E vs z/L in different stability categories
boxplot(data$C_E[which(data$stability_no==1)],data$C_E[which(data$stability_no==2)],
        data$C_E[which(data$stability_no==3)],data$C_E[which(data$stability_no==4)],
        data$C_E[which(data$stability_no==5)],data$C_E[which(data$stability_no==6)],
        data$C_E[which(data$stability_no==7)],data$C_E[which(data$stability_no==8)],
        data$C_E[which(data$stability_no==9)],data$C_E[which(data$stability_no==10)],
        names=names_boxplot,ylab=expression('C'[E]),xlab='Atmospheric stability category',ylim=c(-50,100))

### FINAL PLOTS FOR PUBLICATION: EVAPORATION AND STABILITY
## Using ggplot2
library(ggplot2)
#LE_vs_stability_final.jpg
plot1 <- ggplot(na.omit(data[,c('LE','stability_no')]), aes(factor(stability_no),LE)) + geom_boxplot(outlier.size=0,aes(fill=factor(stability_no))) + geom_jitter(alpha=I(1/8),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='white',geom='text',label='-----',size=10,color='white') + 
  scale_x_discrete(labels=names_boxplot) + labs(x='Atmospheric stability category') + 
  theme_bw() + theme(legend.position='none',panel.grid.major=element_line(size=0.75),panel.grid.minor=element_line(size=0.50,color='gray'),
                     axis.title.x=element_text(size=24),axis.title.y=element_text(size=24),
                     axis.text.x=element_text(angle=90,size=20,vjust=1),axis.text.y=element_text(size=20)) + 
  scale_y_continuous(breaks=seq(-100,500,by=50)) +
  scale_fill_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))
# Wilcoxon rank sum test unpaired between LE in stable and unstable conditions
wilcox.test(data$LE[which(data$Z.L>=0)],data$LE[which(data$Z.L<0)])
# Median and number of data of LE in stable conditions
median(data$LE[which(data$Z.L>=0)],na.rm=TRUE)
sum(data$Z.L>=0,na.rm=TRUE)
# Median and number of data of LE in unstable conditions
median(data$LE[which(data$Z.L<0)],na.rm=TRUE)
sum(data$Z.L<0,na.rm=TRUE)

#deltaE_vs_stability_final.jpg
plot2 <- ggplot(na.omit(data[,c('deltaE','stability_no')]), aes(factor(stability_no),deltaE)) + geom_boxplot(outlier.size=0,aes(fill=factor(stability_no))) + geom_jitter(alpha=I(1/8),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='white',geom='text',label='-----',size=10,color='white') + 
  scale_x_discrete(labels=names_boxplot) + labs(x='Atmospheric stability category',y=expression(paste(Delta,'e'))) + 
  theme_bw() + theme(legend.position='none',panel.grid.major=element_line(size=0.75),panel.grid.minor=element_line(size=0.50,color='gray'),
                     axis.title.x=element_text(size=24),axis.title.y=element_text(size=24),
                     axis.text.x=element_text(size=20,angle=90,vjust=1),axis.text.y=element_text(size=20)) + 
  scale_y_continuous(breaks=seq(-3,3,by=0.5)) +
  scale_fill_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))  

#General plot of deltaE vs z/L, NEED TO CHECK HOW TO DRAW BOXPLOTS using ggplot2 without categories
plot3 <- ggplot(na.omit(data[,c('deltaE','Z.L')]),aes(Z.L,deltaE))
plot3 + geom_boxplot()

#Wilcoxon rank sum test unpaired between deltaE in stable and unstable conditions
wilcox.test(data$deltaE[which(data$Z.L>=0)],data$deltaE[which(data$Z.L<0)])
#Median and number of data of deltaE in stable conditions
median(data$deltaE[which(data$Z.L>=0)],na.rm=TRUE)
#Median and number of data of deltaE in unstable conditions
median(data$deltaE[which(data$Z.L<0)],na.rm=TRUE)


#U_vs_stability_final.jpg
plot4 <- ggplot(na.omit(data[,c('WS_Spd_WVT','stability_no')]), aes(factor(stability_no),WS_Spd_WVT)) + geom_boxplot(outlier.size=0,aes(fill=factor(stability_no))) + geom_jitter(alpha=I(1/8),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='white',geom='text',label='-----',size=10,color='white') + 
  scale_x_discrete(labels=names_boxplot) + labs(x='Atmospheric stability category',y='U') + 
  theme_bw() + theme(legend.position='none',panel.grid.major=element_line(size=0.75),panel.grid.minor=element_line(size=0.50,color='gray'),
                     axis.title.x=element_text(size=24),axis.title.y=element_text(size=24),
                     axis.text.x=element_text(size=20,vjust=1,angle=90),axis.text.y=element_text(size=20)) + 
  scale_y_continuous(breaks=seq(0,15,by=1)) +
  scale_fill_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))  
#Wilcoxon rank sum test unpaired between deltaP_q in stable and unstable conditions
wilcox.test(data$WS_Spd_WVT[which(data$Z.L>=0)],data$WS_Spd_WVT[which(data$Z.L<0)])
#Median and number of data of U in stable conditions
median(data$WS_Spd_WVT[which(data$Z.L>=0)],na.rm=TRUE)
#Median and number of data of U in unstable conditions
summary(data$WS_Spd_WVT[which(data$Z.L<0)],na.rm=TRUE)

#u_deltaE_vs_stability_final.jpg, deltaE is in Pa
plot5 <- ggplot(na.omit(data[,c('u_deltaE','stability_no')]), aes(factor(stability_no),u_deltaE)) + geom_boxplot(outlier.size=0,aes(fill=factor(stability_no))) + geom_jitter(alpha=I(1/8),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='white',geom='text',label='-----',size=10,color='white') + 
  scale_x_discrete(labels=names_boxplot) + labs(x='Atmospheric stability category',y=expression(paste('U',Delta,'e'))) + 
  theme_bw() + theme(legend.position='none',panel.grid.major=element_line(size=0.75),panel.grid.minor=element_line(size=0.50,color='gray'),
                     axis.title.x=element_text(size=24),axis.title.y=element_text(size=24),
                     axis.text.x=element_text(size=20,vjust=1,angle=90),axis.text.y=element_text(size=20)) + 
  scale_y_continuous(breaks=seq(-5,15,by=2.5)) +
  scale_fill_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))  
#Wilcoxon rank sum test unpaired between deltaP_q in stable and unstable conditions
wilcox.test(data$u_deltaE[which(data$Z.L>=0)],data$u_deltaE[which(data$Z.L<0)])
#Median and number of data of deltaP_q in stable conditions
median(data$u_deltaE[which(data$Z.L>=0)],na.rm=TRUE)
#Median and number of data of deltaP_q in unstable conditions
median(data$u_deltaE[which(data$Z.L<0)],na.rm=TRUE)

multiplot(plot1,plot4,plot2,plot5)

#regression R^2 results plot between LE and U
plot5 <- ggplot(data=data_rsq,aes(x=cat_no,y=r_LE_U)) +
  geom_bar(stat='identity',fill='skyblue',color='black',position='identity') +
  xlab('Atmospheric stability category') + ylab('r') + ylim(-0.05,1.0) +
  scale_x_discrete(labels=names_boxplot) + 
  theme(axis.title.x=element_text(size=20),axis.title.y=element_text(size=20),
        axis.text.x=element_text(color='black',size=20,vjust=1,angle=90), axis.text.y=element_text(color='black',size=20),
        panel.background=element_rect(fill='white',color='black'),title=element_text(size=20)) +
  ggtitle('(a) LE and U')

#regression R^2 results plot between LE and deltaE
plot6 <- ggplot(data=data_rsq,aes(x=cat_no,y=r_LE_deltaE)) +
  geom_bar(stat='identity',fill='skyblue',color='black',position='identity') +
  xlab('Atmospheric stability category') + ylab('r') + ylim(-0.05,1.0) +
  scale_x_discrete(labels=names_boxplot) +
  theme(axis.title.x=element_text(size=20),axis.title.y=element_text(size=20),
        axis.text.x=element_text(size=20,angle=90,vjust=1,color='black'), axis.text.y=element_text(size=20,color='black'),
        panel.background=element_rect(fill='white',color='black'),title=element_text(size=20)) +
  ggtitle(expression(paste('(b) LE and ',Delta,'e')))

#regression R^2 results plot between LE and deltaE
plot7 <- ggplot(data=data_rsq,aes(x=cat_no,y=r_LE_udeltaE)) +
  geom_bar(stat='identity',fill='skyblue',color='black',position='identity') +
  xlab('Atmospheric stability category') + ylab('r') + ylim(-0.05,1.0) +
  scale_x_discrete(labels=names_boxplot) +
  theme(axis.title.x=element_text(size=20),axis.title.y=element_text(size=20),
        axis.text.x=element_text(size=20,color='black',vjust=1,angle=90), axis.text.y=element_text(size=20,color='black'),
        panel.background=element_rect(fill='white',color='black'),title=element_text(size=20)) +
  ggtitle(expression(paste('(c) LE and ','U',Delta,'e')))

#Plotting all three above together in one plot
#The 'multiplot.R' function must be in working directory to plot many panels in plot
source('multiplot.R')
multiplot(plot5,plot6,plot7,cols=1)

#Plotting C_E against U, deltaE, and u_deltaE
#Plotting C_E against LE
plot8 <- ggplot(data=data,aes(x=LE,y=C_E,group=Stability,color=Stability,shape=Stability)) + geom_point(size=3,na.rm=TRUE) + scale_color_manual(values=c('blue','red')) +
  scale_y_continuous(breaks=seq(-100,450,by=50),limits=c(-100,450)) + scale_x_continuous(breaks=seq(-200,450,by=50)) +
  theme(panel.background=element_rect(fill='white',color='black'), panel.grid.major=element_line(size=0.30,color='gray'),
        panel.grid.minor=element_line(size=0.10,color='gray'),axis.title.x=element_text(size=20),axis.title.y=element_text(size=20),
        axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),legend.position='none') + 
  ylab(expression(paste('C'[E],'.L'))) + xlab('LE') + ggtitle('(a)')
plot8

#Plotting C_E against U
plot9 <- ggplot(data=data,aes(x=WS_Spd_WVT,y=C_E,group=Stability,color=Stability,shape=Stability)) + geom_point(size=3, na.rm=TRUE) + scale_color_manual(values=c('blue','red')) +
  scale_y_continuous(breaks=seq(-100,450,by=50),limits=c(-100,450)) + scale_x_continuous(breaks=seq(0,14,by=1)) +
  theme(panel.background = element_rect(fill='white',color='black'), panel.grid.major=element_line(size=0.30,color='gray'), 
        panel.grid.minor=element_line(size=0.10,color='gray'),axis.title.x=element_text(size=20),axis.title.y=element_text(size=20),
        axis.text.x=element_text(size=15), axis.text.y=element_text(size=15),legend.position='none') +
  ylab(expression(paste('C'[E],'.L'))) + xlab('U') + ggtitle('(b)')
plot9

#Plotting C_E against deltaE
plot10 <- ggplot(data=data,aes(x=deltaE,y=C_E,group=Stability,color=Stability,shape=Stability)) + geom_point(size=3,na.rm=TRUE) + scale_color_manual(values=c('blue','red')) +
  scale_y_continuous(breaks=seq(-100,450,by=50),limits=c(-100,450)) + scale_x_continuous(breaks=seq(-1,4,by=0.5)) +
  theme(panel.background=element_rect(fill='white',color='black'), panel.grid.major=element_line(size=0.30,color='gray'),
        panel.grid.minor=element_line(size=0.10,color='gray'),axis.title.x=element_text(size=20),axis.title.y=element_text(size=20),
        axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),legend.position='none') + 
  ylab(expression(paste('C'[E],'.L'))) + xlab(expression(paste(Delta,'e'))) + ggtitle('(c)')
plot10

#Plotting C_E against u_deltaE
plot11 <- ggplot(data=data,aes(x=u_deltaE,y=C_E,group=Stability,color=Stability,shape=Stability)) + geom_point(size=3,na.rm=TRUE) + scale_color_manual(values=c('blue','red')) +
  scale_y_continuous(breaks=seq(-100,450,by=50),limits=c(-100,450)) + scale_x_continuous(breaks=seq(-10,18,by=1)) +
  theme(panel.background=element_rect(fill='white',color='black'), panel.grid.major=element_line(size=0.30,color='gray'),
        panel.grid.minor=element_line(size=0.10,color='gray'),axis.title.x=element_text(size=20),axis.title.y=element_text(size=20),
        axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),legend.position='none') + 
  ylab(expression(paste('C'[E],'.L'))) + xlab(expression(paste('U',Delta,'e'))) + ggtitle('(d)')
plot11


multiplot(plot8,plot9,plot10,plot11)
