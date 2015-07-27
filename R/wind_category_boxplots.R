##### Prelim #########
# Boxplots LE and H in different z/L ranges
library(ggplot2)
library(Hmisc)
library(MASS)
source("R/multiplot2.R")

##### Data preparation ######
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
#C_com1 <- data1$C_com[which(data$wind_category_day==1)]
#C_u.1 <- data1$C_u.[which(data$wind_category_day==1)]

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
#C_com2 <- data1$C_com[which(data$wind_category_day==2)]
#C_u.2 <- data1$C_u.[which(data$wind_category_day==2)]

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
#C_com3 <- data1$C_com[which(data$wind_category_day==3)]
#C_u.3 <- data1$C_u.[which(data$wind_category_day==3)]

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
#C_com4 <- data1$C_com[which(data$wind_category_day==4)]
#C_u.4 <- data1$C_u.[which(data$wind_category_day==4)]

df1 <- data.frame(no_stability1,LE1,H1,U1,deltaE1,ea1,es31,es21,es1,deltaT1,udeltaT1,udeltaE1,C_D1,
                  C_E1,C_H1)#,C_com1,C_u.1)
df2 <- data.frame(no_stability2,LE2,H2,U2,deltaE2,ea2,es32,es22,es2,deltaT2,udeltaT2,udeltaE2,C_D2,
                  C_E2,C_H2)#,C_com2,C_u.2)
df3 <- data.frame(no_stability3,LE3,H3,U3,deltaE3,ea3,es33,es23,es3,deltaT3,udeltaT3,udeltaE3,C_D3,
                  C_E3,C_H3)#,C_com3,C_u.3)
df4 <- data.frame(no_stability4,LE4,H4,U4,deltaE4,ea4,es34,es24,es4,deltaT4,udeltaT4,udeltaE4,C_D4,
                  C_E4,C_H4)#,C_com4,C_u.4)
rm(no_stability1,no_stability2,no_stability3,no_stability4,LE1,LE2,LE3,LE4,H1,H2,H3,H4,
   U1,U2,U3,U4,deltaE1,deltaE2,deltaE3,deltaE4,ea1,es1,ea2,es2,ea3,es3,ea4,es4,
   es31,es21,es32,es22,es33,es23,es34,es24,deltaT1,deltaT2,deltaT3,deltaT4,
   udeltaT1,udeltaE1,udeltaT2,udeltaE2,udeltaT3,udeltaE3,udeltaT4,udeltaE4,
   C_D1,C_D2,C_D3,C_D4,C_E1,C_E2,C_E3,C_E4,C_H1,C_H2,C_H3,C_H4)#,C_com1,C_com2,
   C_com3,C_com4,C_u.1,C_u.2,C_u.3,C_u.4)

##### Names of various figures and box plots ####
names_boxplot = c('\u221210\u2264\u03B6<\u22121','\u22121\u2264\u03B6<\u22120.5','\u22120.5\u2264\u03B6<\u22120.1','\u22120.1\u2264\u03B6<\u22120.05',
                  '\u22120.05\u2264\u03B6<0','0\u2264\u03B6<0.05','0.05\u2264\u03B6<0.1','0.1\u2264\u03B6<0.5','0.5\u2264\u03B6<1',
                  '1\u2264\u03B6<10')

##### Fig. 1: LE vs ASL stability ranges for 4 ws categories ####

# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/wind_figs/fig_1.jpg')
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
           cols=1,labs=list("","LE"))

dev.off()

#### Fig. 1a: H vs ASL for 4 ws categories ####
# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/wind_figs/fig_1a.jpg')
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

#### Fig. 2: U vs ASL wind categories #######
# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/wind_figs/fig_2.jpg')
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
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/wind_figs/fig_3.jpg')
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
  scale_y_continuous(breaks=seq(-1,3,by=1),limits=c(-1,3)) + 
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
  scale_y_continuous(breaks=seq(-1,3,by=1),limits=c(-1,3)) + 
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
  scale_y_continuous(breaks=seq(-1,3,by=1),limits=c(-1,3)) + 
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
  scale_y_continuous(breaks=seq(-1,3,by=1),limits=c(-1,3)) + 
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) +
  scale_x_discrete(labels=names_boxplot)

multiplot2(plot1,plot2,plot3,plot4,
           cols=1,labs=list("ASL stability ranges",""))

dev.off()

#### Fig. 3a deltaT vs ASL wind categories ####
# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/wind_figs/fig_3a.jpg')
jpeg(file=path_fig,width=5, height=10,res=360,units='in')
plot.new()

# a) For wind category 1 deltaE
plot1 <- ggplot(na.omit(df1),aes(x=factor(no_stability1),y=deltaT1)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability1))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=9.8,label="a)",size=7,family='Times') +
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
  labs(x="",y=expression(paste(Delta,'T'))) + theme_bw() + annotate("text",x=0.8,y=9.7,label="b)",size=7,family='Times') +
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
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=9.7,label="c)",size=7,family='Times') +
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
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=9.7,label="d)",size=7,family='Times') +
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

#### Fig. 4 udeltaE vs ASL stability wind categories ####
# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/wind_figs/fig_4.jpg')
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

#### Fig. 4a udeltaT vs ASL stability wind categories ####
# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/wind_figs/fig_4a.jpg')
jpeg(file=path_fig,width=5, height=10,res=360,units='in')
plot.new()

# a) For wind category 1 udeltaT
plot1 <- ggplot(na.omit(df1),aes(x=factor(no_stability1),y=udeltaT1)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability1))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=98,label="a)",size=7,family='Times') +
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
  labs(x="",y=expression(paste('U',Delta,'T'))) + theme_bw() + annotate("text",x=0.8,y=98,label="b)",size=7,family='Times') +
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
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=98,label="c)",size=7,family='Times') +
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
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=98,label="d)",size=7,family='Times') +
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

#### Fig. 5 C_D stability wind categories ####
# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/wind_figs/fig_5.jpg')
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
        plot.margin=unit(c(1,1,4,-2.5),"mm")) + 
  scale_y_continuous(breaks=seq(-0.003,0.005,by=0.002),limits=c(-0.003,0.005)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

# b) For wind category 2 C_D
plot2 <- ggplot(na.omit(df2),aes(x=factor(no_stability2),y=C_D2)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability2))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y=expression('C'['D'])) + theme_bw() + annotate("text",x=0.8,y=0.005,label="b)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01,hjust=0.07),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-10,1,15,-3.5),"mm")) +
  scale_y_continuous(breaks=seq(-0.003,0.005,by=0.002),limits=c(-0.003,0.005)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

# c) For wind category 3 C_D
plot3 <- ggplot(na.omit(df3),aes(x=factor(no_stability3),y=C_D3)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability3))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=0.005,label="c)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-20,1,25,-2.5),"mm")) + 
  scale_y_continuous(breaks=seq(-0.003,0.005,by=0.002),limits=c(-0.003,0.005)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

# d) For wind category 4 C_D
plot4 <- ggplot(na.omit(df4),aes(x=factor(no_stability4),y=C_D4)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability4))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=0.005,label="d)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),axis.text.x=element_text(angle=90,size=16,hjust=1,family='Times',vjust=0.5),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-30,1,4,-2.5),"mm")) + 
  scale_y_continuous(breaks=seq(-0.003,0.005,by=0.002),limits=c(-0.003,0.005)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) +
  scale_x_discrete(labels=names_boxplot)

multiplot2(plot1,plot2,plot3,plot4,
           cols=1,labs=list("ASL stability ranges",""))

dev.off()

#### Fig. 6 C_E stability wind categories ####
# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/wind_figs/fig_6.jpg')
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
  scale_y_continuous(breaks=seq(-0.003,0.005,by=0.002),limits=c(-0.003,0.005)) +
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
  scale_y_continuous(breaks=seq(-0.003,0.005,by=0.002),limits=c(-0.003,0.005)) +
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
  scale_y_continuous(breaks=seq(-0.003,0.005,by=0.002),limits=c(-0.003,0.005)) +
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
  scale_y_continuous(breaks=seq(-0.003,0.005,by=0.002),limits=c(-0.003,0.005)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) +
  scale_x_discrete(labels=names_boxplot)

multiplot2(plot1,plot2,plot3,plot4,
           cols=1,labs=list("ASL stability ranges",""))

dev.off()

#### Fig. 7 C_H stability wind categories ####
# Path where the plots will be saved
path_fig <- file.path('/Users/Yusri/Documents/Work/Data analysis/lake/figs/wind_figs/fig_7.jpg')
jpeg(file=path_fig,width=5, height=10,res=360,units='in')
plot.new()

# a) For wind category 1 C_H
plot1 <- ggplot(na.omit(df1),aes(x=factor(no_stability1),y=C_H1)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability1))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=0.005,label="a)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(1,1,4,-2.5),"mm")) + 
  scale_y_continuous(breaks=seq(-0.003,0.005,by=0.002),limits=c(-0.003,0.005)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

# b) For wind category 2 C_H
plot2 <- ggplot(na.omit(df2),aes(x=factor(no_stability2),y=C_H2)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability2))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y=expression('C'['H'])) + theme_bw() + annotate("text",x=0.8,y=0.005,label="b)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01,hjust=0.07),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-10,1,15,-2.5),"mm")) +
  scale_y_continuous(breaks=seq(-0.003,0.005,by=0.002),limits=c(-0.003,0.005)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

# c) For wind category 3 C_H
plot3 <- ggplot(na.omit(df3),aes(x=factor(no_stability3),y=C_H3)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability3))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=0.005,label="c)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-20,1,25,-2.5),"mm")) + 
  scale_y_continuous(breaks=seq(-0.003,0.005,by=0.002),limits=c(-0.003,0.005)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue'))

# d) For wind category 4 C_H
plot4 <- ggplot(na.omit(df4),aes(x=factor(no_stability4),y=C_H4)) + geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(alpha=I(1/16),aes(color=factor(no_stability4))) + 
  stat_summary(fun.y="median",geom='line',aes(group=1),size=1) + 
  labs(x="",y="") + theme_bw() + annotate("text",x=0.8,y=0.005,label="d)",size=7,family='Times') +
  theme(legend.position='none',panel.grid.major=element_line(size=0),panel.grid.minor=element_line(size=0),
        axis.title.y=element_text(size=16,family='Times',vjust=0.01),axis.text.x=element_text(angle=90,size=16,hjust=1,family='Times',vjust=0.5),
        axis.text.y=element_text(size=16,family='Times'),axis.text.x=element_blank(),
        plot.margin=unit(c(-30,1,4,-2.5),"mm")) + 
  scale_y_continuous(breaks=seq(-0.003,0.005,by=0.002),limits=c(-0.003,0.005)) +
  scale_color_manual(values=c('red','red','red','red','red','blue','blue','blue','blue','blue')) +
  scale_x_discrete(labels=names_boxplot)

multiplot2(plot1,plot2,plot3,plot4,
           cols=1,labs=list("ASL stability ranges",""))

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

#### Delete temp variables #########
rm(path_fig,names_boxplot,plot1,plot2,plot3,plot4)