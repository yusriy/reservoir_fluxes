# R squared between H and U

names_boxplot = c('-10\u2264\u03B6<-1','-1\u2264\u03B6<-0.5','-0.5\u2264\u03B6<-0.1','-0.1\u2264\u03B6<-0.05',
                  '-0.05\u2264\u03B6<0','0\u2264\u03B6<0.05','0.05\u2264\u03B6<0.1','0.1\u2264\u03B6<0.5','0.5\u2264\u03B6<1',
                  '1\u2264\u03B6<10')
cat_no <- as.factor(1:10)

# Stability no 1
lmH_U_1 <- lm(data$H[which(data$stability_no==1)]~data$WS_Spd_WVT[which(data$stability_no==1)])
# Stability no 2
lmH_U_2 <- lm(data$H[which(data$stability_no==2)]~data$WS_Spd_WVT[which(data$stability_no==2)])
# Stability no 3
lmH_U_3 <- lm(data$H[which(data$stability_no==3)]~data$WS_Spd_WVT[which(data$stability_no==3)])
# Stability no 4
lmH_U_4 <- lm(data$H[which(data$stability_no==4)]~data$WS_Spd_WVT[which(data$stability_no==4)])
# Stability no 5
lmH_U_5 <- lm(data$H[which(data$stability_no==5)]~data$WS_Spd_WVT[which(data$stability_no==5)])
# Stability no 6
lmH_U_6 <- lm(data$H[which(data$stability_no==6)]~data$WS_Spd_WVT[which(data$stability_no==6)])
# Stability no 7
lmH_U_7 <- lm(data$H[which(data$stability_no==7)]~data$WS_Spd_WVT[which(data$stability_no==7)])
# Stability no 8
lmH_U_8 <- lm(data$H[which(data$stability_no==8)]~data$WS_Spd_WVT[which(data$stability_no==8)])
# Stability no 9
lmH_U_9 <- lm(data$H[which(data$stability_no==9)]~data$WS_Spd_WVT[which(data$stability_no==9)])
# Stability no 10
lmH_U_10 <- lm(data$H[which(data$stability_no==10)]~data$WS_Spd_WVT[which(data$stability_no==10)])

r_H_U <- c(summary(lmH_U_1)$adj.r.squared,summary(lmH_U_2)$adj.r.squared,
           summary(lmH_U_3)$adj.r.squared,summary(lmH_U_4)$adj.r.squared,
           summary(lmH_U_5)$adj.r.squared,summary(lmH_U_6)$adj.r.squared,
           summary(lmH_U_7)$adj.r.squared,summary(lmH_U_8)$adj.r.squared,
           summary(lmH_U_9)$adj.r.squared,summary(lmH_U_10)$adj.r.squared)
p_H_U <- c(summary(lmH_U_1)$coefficients[8],summary(lmH_U_2)$coefficients[8],
           summary(lmH_U_3)$coefficients[8],summary(lmH_U_4)$coefficients[8],
           summary(lmH_U_5)$coefficients[8],summary(lmH_U_6)$coefficients[8],
           summary(lmH_U_7)$coefficients[8],summary(lmH_U_8)$coefficients[8],
           summary(lmH_U_9)$coefficients[8],summary(lmH_U_10)$coefficients[8])

rm(lmH_U_1,lmH_U_2,lmH_U_3,lmH_U_4,lmH_U_5,lmH_U_6,lmH_U_7,lmH_U_8,lmH_U_9,
   lmH_U_10)

# R squared between H and deltaT

# Stability no 1
lmH_dT_1 <- lm(data$H[which(data$stability_no==1)]~data$deltaT[which(data$stability_no==1)])
# Stability no 2
lmH_dT_2 <- lm(data$H[which(data$stability_no==2)]~data$deltaT[which(data$stability_no==2)])
# Stability no 3
lmH_dT_3 <- lm(data$H[which(data$stability_no==3)]~data$deltaT[which(data$stability_no==3)])
# Stability no 4
lmH_dT_4 <- lm(data$H[which(data$stability_no==4)]~data$deltaT[which(data$stability_no==4)])
# Stability no 5
lmH_dT_5 <- lm(data$H[which(data$stability_no==5)]~data$deltaT[which(data$stability_no==5)])
# Stability no 6
lmH_dT_6 <- lm(data$H[which(data$stability_no==6)]~data$deltaT[which(data$stability_no==6)])
# Stability no 7
lmH_dT_7 <- lm(data$H[which(data$stability_no==7)]~data$deltaT[which(data$stability_no==7)])
# Stability no 8
lmH_dT_8 <- lm(data$H[which(data$stability_no==8)]~data$deltaT[which(data$stability_no==8)])
# Stability no 9
lmH_dT_9 <- lm(data$H[which(data$stability_no==9)]~data$deltaT[which(data$stability_no==9)])
# Stability no 10
lmH_dT_10 <- lm(data$H[which(data$stability_no==10)]~data$deltaT[which(data$stability_no==10)])

r_H_dT <- c(summary(lmH_dT_1)$adj.r.squared,summary(lmH_dT_2)$adj.r.squared,
           summary(lmH_dT_3)$adj.r.squared,summary(lmH_dT_4)$adj.r.squared,
           summary(lmH_dT_5)$adj.r.squared,summary(lmH_dT_6)$adj.r.squared,
           summary(lmH_dT_7)$adj.r.squared,summary(lmH_dT_8)$adj.r.squared,
           summary(lmH_dT_9)$adj.r.squared,summary(lmH_dT_10)$adj.r.squared)
p_H_dT <- c(summary(lmH_dT_1)$coefficients[8],summary(lmH_dT_2)$coefficients[8],
            summary(lmH_dT_3)$coefficients[8],summary(lmH_dT_4)$coefficients[8],
            summary(lmH_dT_5)$coefficients[8],summary(lmH_dT_6)$coefficients[8],
            summary(lmH_dT_7)$coefficients[8],summary(lmH_dT_8)$coefficients[8],
            summary(lmH_dT_9)$coefficients[8],summary(lmH_dT_10)$coefficients[8])
rm(lmH_dT_1,lmH_dT_2,lmH_dT_3,lmH_dT_4,lmH_dT_5,lmH_dT_6,lmH_dT_7,lmH_dT_8,lmH_dT_9,
   lmH_dT_10)

# R squared between H and udeltaT

# Stability no 1
lmH_UdT1 <- lm(data$H[which(data$stability_no==1)]~data$u_deltaT[which(data$stability_no==1)])
# Stability no 2
lmH_UdT2 <- lm(data$H[which(data$stability_no==2)]~data$u_deltaT[which(data$stability_no==2)])
# Stability no 3
lmH_UdT3 <- lm(data$H[which(data$stability_no==3)]~data$u_deltaT[which(data$stability_no==3)])
# Stability no 4
lmH_UdT4 <- lm(data$H[which(data$stability_no==4)]~data$u_deltaT[which(data$stability_no==4)])
# Stability no 5
lmH_UdT5 <- lm(data$H[which(data$stability_no==5)]~data$u_deltaT[which(data$stability_no==5)])
# Stability no 6
lmH_UdT6 <- lm(data$H[which(data$stability_no==6)]~data$u_deltaT[which(data$stability_no==6)])
# Stability no 7
lmH_UdT7 <- lm(data$H[which(data$stability_no==7)]~data$u_deltaT[which(data$stability_no==7)])
# Stability no 8
lmH_UdT8 <- lm(data$H[which(data$stability_no==8)]~data$u_deltaT[which(data$stability_no==8)])
# Stability no 9
lmH_UdT9 <- lm(data$H[which(data$stability_no==9)]~data$u_deltaT[which(data$stability_no==9)])
# Stability no 10
lmH_UdT10 <- lm(data$H[which(data$stability_no==10)]~data$u_deltaT[which(data$stability_no==10)])

r_H_UdT <- c(summary(lmH_UdT1)$adj.r.squared,summary(lmH_UdT2)$adj.r.squared,
            summary(lmH_UdT3)$adj.r.squared,summary(lmH_UdT4)$adj.r.squared,
            summary(lmH_UdT5)$adj.r.squared,summary(lmH_UdT6)$adj.r.squared,
            summary(lmH_UdT7)$adj.r.squared,summary(lmH_UdT8)$adj.r.squared,
            summary(lmH_UdT9)$adj.r.squared,summary(lmH_UdT10)$adj.r.squared)
p_H_UdT <- c(summary(lmH_UdT1)$coefficients[8],summary(lmH_UdT2)$coefficients[8],
             summary(lmH_UdT3)$coefficients[8],summary(lmH_UdT4)$coefficients[8],
             summary(lmH_UdT5)$coefficients[8],summary(lmH_UdT6)$coefficients[8],
             summary(lmH_UdT7)$coefficients[8],summary(lmH_UdT8)$coefficients[8],
             summary(lmH_UdT9)$coefficients[8],summary(lmH_UdT10)$coefficients[8])

rm(lmH_UdT1,lmH_UdT2,lmH_UdT3,lmH_UdT4,lmH_UdT5,lmH_UdT6,lmH_UdT7,lmH_UdT8,lmH_UdT9,
   lmH_UdT10)

data_rsq2<-data.frame(cat_no,names_boxplot,r_H_U,p_H_U,r_H_dT,p_H_dT,r_H_UdT,p_H_UdT)
rm(r_H_U,r_H_dT,r_H_UdT,p_H_UdT,p_H_U,p_H_dT)

