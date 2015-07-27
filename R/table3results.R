## Table 3 results

# R2 and p-values for LE with U, deltaE, and u_deltaE under different ASL ranges

lm1 <- lm(data$LE[which(data$stability_no==1)] ~ 
            data$WS_Spd_WVT[which(data$stability_no==1)])
lm2 <- lm(data$LE[which(data$stability_no==1)] ~ 
            data$deltaE[which(data$stability_no==1)])
lm3 <- lm(data$LE[which(data$stability_no==1)] ~ 
            data$u_deltaE[which(data$stability_no==1)])

lm4 <- lm(data$LE[which(data$stability_no==2)] ~ 
            data$WS_Spd_WVT[which(data$stability_no==2)])
lm5 <- lm(data$LE[which(data$stability_no==2)] ~ 
            data$deltaE[which(data$stability_no==2)])
lm6 <- lm(data$LE[which(data$stability_no==2)] ~ 
            data$u_deltaE[which(data$stability_no==2)])

lm7 <- lm(data$LE[which(data$stability_no==3)] ~ 
            data$WS_Spd_WVT[which(data$stability_no==3)])
lm8 <- lm(data$LE[which(data$stability_no==3)] ~ 
            data$deltaE[which(data$stability_no==3)])
lm9 <- lm(data$LE[which(data$stability_no==3)] ~ 
            data$u_deltaE[which(data$stability_no==3)])

lm10 <- lm(data$LE[which(data$stability_no==4)] ~ 
            data$WS_Spd_WVT[which(data$stability_no==4)])
lm11 <- lm(data$LE[which(data$stability_no==4)] ~ 
            data$deltaE[which(data$stability_no==4)])
lm12 <- lm(data$LE[which(data$stability_no==4)] ~ 
            data$u_deltaE[which(data$stability_no==4)])

lm13 <- lm(data$LE[which(data$stability_no==5)] ~ 
            data$WS_Spd_WVT[which(data$stability_no==5)])
lm14 <- lm(data$LE[which(data$stability_no==5)] ~ 
            data$deltaE[which(data$stability_no==5)])
lm15 <- lm(data$LE[which(data$stability_no==5)] ~ 
            data$u_deltaE[which(data$stability_no==5)])

lm16 <- lm(data$LE[which(data$stability_no==6)] ~ 
            data$WS_Spd_WVT[which(data$stability_no==6)])
lm17 <- lm(data$LE[which(data$stability_no==6)] ~ 
            data$deltaE[which(data$stability_no==6)])
lm18 <- lm(data$LE[which(data$stability_no==6)] ~ 
            data$u_deltaE[which(data$stability_no==6)])

lm19 <- lm(data$LE[which(data$stability_no==7)] ~ 
            data$WS_Spd_WVT[which(data$stability_no==7)])
lm20 <- lm(data$LE[which(data$stability_no==7)] ~ 
            data$deltaE[which(data$stability_no==7)])
lm21 <- lm(data$LE[which(data$stability_no==7)] ~ 
            data$u_deltaE[which(data$stability_no==7)])

lm22 <- lm(data$LE[which(data$stability_no==8)] ~ 
            data$WS_Spd_WVT[which(data$stability_no==8)])
lm23 <- lm(data$LE[which(data$stability_no==8)] ~ 
            data$deltaE[which(data$stability_no==8)])
lm24 <- lm(data$LE[which(data$stability_no==8)] ~ 
            data$u_deltaE[which(data$stability_no==8)])

lm25 <- lm(data$LE[which(data$stability_no==9)] ~ 
            data$WS_Spd_WVT[which(data$stability_no==9)])
lm26 <- lm(data$LE[which(data$stability_no==9)] ~ 
            data$deltaE[which(data$stability_no==9)])
lm27 <- lm(data$LE[which(data$stability_no==9)] ~ 
            data$u_deltaE[which(data$stability_no==9)])

lm28 <- lm(data$LE[which(data$stability_no==10)] ~ 
            data$WS_Spd_WVT[which(data$stability_no==10)])
lm29 <- lm(data$LE[which(data$stability_no==10)] ~ 
            data$deltaE[which(data$stability_no==10)])
lm30 <- lm(data$LE[which(data$stability_no==10)] ~ 
            data$u_deltaE[which(data$stability_no==10)])



r2 <- c(summary(lm1)$adj.r.squared,summary(lm2)$adj.r.squared,
        summary(lm3)$adj.r.squared,summary(lm4)$adj.r.squared,
        summary(lm5)$adj.r.squared,summary(lm6)$adj.r.squared,
        summary(lm7)$adj.r.squared,summary(lm8)$adj.r.squared,
        summary(lm9)$adj.r.squared,summary(lm10)$adj.r.squared,
        summary(lm11)$adj.r.squared,summary(lm12)$adj.r.squared,
        summary(lm13)$adj.r.squared,summary(lm14)$adj.r.squared,
        summary(lm15)$adj.r.squared,summary(lm16)$adj.r.squared,
        summary(lm17)$adj.r.squared,summary(lm18)$adj.r.squared,
        summary(lm19)$adj.r.squared,summary(lm20)$adj.r.squared,
        summary(lm21)$adj.r.squared,summary(lm22)$adj.r.squared,
        summary(lm23)$adj.r.squared,summary(lm24)$adj.r.squared,
        summary(lm25)$adj.r.squared,summary(lm26)$adj.r.squared,
        summary(lm27)$adj.r.squared,summary(lm28)$adj.r.squared,
        summary(lm29)$adj.r.squared,summary(lm30)$adj.r.squared)

p <- c(summary(lm1)$coefficients[8],summary(lm2)$coefficients[8],
       summary(lm3)$coefficients[8],summary(lm4)$coefficients[8],
       summary(lm5)$coefficients[8],summary(lm6)$coefficients[8],
       summary(lm7)$coefficients[8],summary(lm8)$coefficients[8],
       summary(lm9)$coefficients[8],summary(lm10)$coefficients[8],
       summary(lm11)$coefficients[8],summary(lm12)$coefficients[8],
       summary(lm13)$coefficients[8],summary(lm14)$coefficients[8],
       summary(lm15)$coefficients[8],summary(lm16)$coefficients[8],
       summary(lm17)$coefficients[8],summary(lm18)$coefficients[8],
       summary(lm19)$coefficients[8],summary(lm20)$coefficients[8],
       summary(lm21)$coefficients[8],summary(lm22)$coefficients[8],
       summary(lm23)$coefficients[8],summary(lm24)$coefficients[8],
       summary(lm25)$coefficients[8],summary(lm26)$coefficients[8],
       summary(lm27)$coefficients[8],summary(lm28)$coefficients[8],
       summary(lm29)$coefficients[8],summary(lm30)$coefficients[8])

table3 <- data.frame(r2,p)

rm(lm1,lm2,lm3,lm4,lm5,lm6,lm7,lm8,lm9,lm10,lm11,lm12,lm13,lm14,lm15,lm16,lm17,
   lm18,lm19,lm20,lm21,lm22,lm23,lm24,lm25,lm26,lm27,lm28,lm29,lm30,r2,p)