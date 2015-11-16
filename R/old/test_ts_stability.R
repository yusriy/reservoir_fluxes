plot1 <- ggplot(na.omit(data[,c('Water.surface.temperature','stability_no')]), 
                aes(factor(stability_no),Water.surface.temperature)) + 
  geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/32),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='black',geom='text',label='---',size=7) + 
  labs(x="",y="TS") + theme_bw()
  #theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
  #      axis.title.y=element_text(size=16,family='Times',vjust=0.01),
  #      axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
  #      plot.margin=unit(c(1,1,4,-2.5),"mm")) + 
  #scale_y_continuous(breaks=seq(-100,400,by=100),labels=c(paste("\u2212",100,sep=""),0,100,200,300,400)) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) +
  #coord_cartesian(ylim = c(-100, 450))