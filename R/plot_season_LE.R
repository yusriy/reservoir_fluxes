## Plotting different season LE and H
# Run 0. Preliminaries first in Lake Analysis first

data1 <- cbind(data$H[which(data$season==0)],data$stability_no[which(data$season==0)])
data1 <- as.data.frame(data1)
colnames(data1) <- c('LE','stability_no')

plot1 <- ggplot(na.omit(data1[,c('LE','stability_no')]), aes(factor(stability_no),LE)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/32),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='black',geom='text',label='---',size=7) + 
  labs(x='ASL stability ranges',y="H") + theme_bw() + annotate("text",x=1,y=450,label="a) Winter") +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=12),
        axis.text.y=element_text(size=11),axis.text.x=element_blank(),
        plot.margin=unit(c(1,1,-2,-1.2),"mm")) + 
  scale_y_continuous(breaks=seq(-100,500,by=100),limits=c(-100,450)) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))
plot1

rm(data1,plot1)