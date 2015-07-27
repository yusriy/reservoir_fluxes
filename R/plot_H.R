# Plotting H versus ASL stability ranges, U, deltaT, UdeltaT
# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/figs_V3/sensible_heat.jpg')
jpeg(file=path_fig,width=700,height=700)
## Creating a new plot
plot.new()
plot1 <- ggplot(na.omit(data[,c('H','stability_no')]), aes(factor(stability_no),H)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/32),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='black',geom='text',label='---',size=9) + 
  labs(x="",y="H") + theme_bw() + annotate("text",x=0.75,y=150,label="a)") +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=11),
        axis.text.y=element_text(size=11),axis.text.x=element_blank(),
        plot.margin=unit(c(4,2,5,0),"mm")) + 
  scale_y_continuous(breaks=seq(-50,150,by=50),limits=c(-50,150)) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

plot2 <- ggplot(na.omit(data[,c('deltaT','stability_no')]), aes(factor(stability_no),deltaT)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/32),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='black',geom='text',label='---',size=9) + 
  labs(x='',y=expression(paste(Delta,'T'))) + theme_bw() + annotate("text",x=0.75,y=15,label="b)") +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.x=element_text(size=11),axis.title.y=element_text(size=11),
        axis.text.x=element_text(angle=90,size=11,vjust=1),axis.text.y=element_text(size=11),
        plot.margin=unit(c(-10,2,4,1),"mm")) + 
  scale_y_continuous(breaks=seq(-12,16,by=2)) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) +
  scale_x_discrete(labels=names_boxplot)

plot3 <- ggplot(na.omit(data[,c('WS_Spd_WVT','stability_no')]), aes(factor(stability_no),WS_Spd_WVT)) + geom_boxplot(outlier.size=0,fill='white') + 
  geom_jitter(alpha=I(1/32),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='black',geom='text',label='---',size=9) + 
  labs(x='',y='U') + theme_bw() + annotate("text",x=0.75,y=13,label="c)") +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=11),
        axis.text.y=element_text(size=11),axis.text.x=element_blank(),
        plot.margin=unit(c(4,2,5,0),"mm")) + 
  scale_y_continuous(breaks=seq(0,15,by=1)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))  

plot4 <- ggplot(na.omit(data[,c('u_deltaT','stability_no')]), aes(factor(stability_no),u_deltaT)) + geom_boxplot(outlier.size=0,fill='white') + 
  geom_jitter(alpha=I(1/32),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='black',geom='text',label='---',size=9) + 
  labs(x='',y=expression(paste('U',Delta,'T'))) + theme_bw() + annotate("text",x=0.75,y=100,label="d)") +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.x=element_text(size=11),axis.title.y=element_text(size=11),
        axis.text.x=element_text(angle=90,size=11,vjust=1),axis.text.y=element_text(size=11),
        plot.margin=unit(c(-10,2,4,-3),"mm")) + 
  scale_y_continuous(breaks=seq(-50,100,by=50),limits=c(-50,100)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) +
  scale_x_discrete(labels=names_boxplot)

multiplot2(plot1,plot2,plot3,plot4,cols=2,labs=list("ASL stability ranges",""))
dev.off()

