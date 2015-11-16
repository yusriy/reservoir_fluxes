# Plot C_H
plot <- ggplot(na.omit(data[,c('C_H','stability_no')]), aes(factor(stability_no),C_H)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/8),aes(color=factor(stability_no))) + 
  stat_summary(fun.y="mean",colour='black',geom='text',label='-----',size=10) + 
  labs(x="ASL stability ranges",y=expression("C"["H"])) + theme_bw() +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=12),axis.title.x=element_text(size=12),
        axis.text.y=element_text(size=12),axis.text.x=element_text(size=12)) + 
  scale_x_discrete(labels=names_boxplot) +
  scale_y_continuous(breaks=seq(-5,10,by=1),limits=c(-5,10)) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

