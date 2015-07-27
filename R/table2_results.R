## Regression results between deltaE > 0 and < 0 and deltaT > 0 and < 0
# LE (unstable)
lm1 <- lm(data$LE[which(data$deltaE < 0 & data$Z.L < 0)] 
          ~ data$WS_Spd_WVT[which(data$deltaE < 0 & data$Z.L < 0)])
lm2 <- lm(data$LE[which(data$deltaE > 0 & data$Z.L < 0)] 
          ~ data$WS_Spd_WVT[which(data$deltaE > 0 & data$Z.L < 0)])

lm3 <- lm(data$LE[which(data$deltaE < 0 & data$Z.L < 0)] 
          ~ data$deltaE[which(data$deltaE < 0 & data$Z.L < 0)])
lm4 <- lm(data$LE[which(data$deltaE > 0 & data$Z.L < 0)] 
          ~ data$deltaE[which(data$deltaE > 0 & data$Z.L < 0)])

lm5 <- lm(data$LE[which(data$deltaE < 0 & data$Z.L < 0)] 
          ~ data$u_deltaE[which(data$deltaE < 0 & data$Z.L < 0)])
lm6 <- lm(data$LE[which(data$deltaE > 0 & data$Z.L < 0)] 
          ~ data$u_deltaE[which(data$deltaE > 0 & data$Z.L < 0)])

summary(lm1)$adj.r.squared
summary(lm1)$coefficients
summary(lm2)$adj.r.squared
summary(lm2)$coefficients
summary(lm3)$adj.r.squared
summary(lm3)$coefficients
summary(lm4)$adj.r.squared
summary(lm4)$coefficients
summary(lm5)$adj.r.squared
summary(lm5)$coefficients
summary(lm6)$adj.r.squared
summary(lm6)$coefficients

# LE (stable)
lm1 <- lm(data$LE[which(data$deltaE < 0 & data$Z.L > 0)] 
          ~ data$WS_Spd_WVT[which(data$deltaE < 0 & data$Z.L > 0)])
lm2 <- lm(data$LE[which(data$deltaE > 0 & data$Z.L > 0)] 
          ~ data$WS_Spd_WVT[which(data$deltaE > 0 & data$Z.L > 0)])

lm3 <- lm(data$LE[which(data$deltaE < 0 & data$Z.L > 0)] 
          ~ data$deltaE[which(data$deltaE < 0 & data$Z.L > 0)])
lm4 <- lm(data$LE[which(data$deltaE > 0 & data$Z.L > 0)] 
          ~ data$deltaE[which(data$deltaE > 0 & data$Z.L > 0)])

lm5 <- lm(data$LE[which(data$deltaE < 0 & data$Z.L > 0)] 
          ~ data$u_deltaE[which(data$deltaE < 0 & data$Z.L > 0)])
lm6 <- lm(data$LE[which(data$deltaE > 0 & data$Z.L > 0)] 
          ~ data$u_deltaE[which(data$deltaE > 0 & data$Z.L > 0)])

summary(lm1)$adj.r.squared
summary(lm1)$coefficients
summary(lm2)$adj.r.squared
summary(lm2)$coefficients
summary(lm3)$adj.r.squared
summary(lm3)$coefficients
summary(lm4)$adj.r.squared
summary(lm4)$coefficients
summary(lm5)$adj.r.squared
summary(lm5)$coefficients
summary(lm6)$adj.r.squared
summary(lm6)$coefficients

# H (unstable)
lm1 <- lm(data$H[which(data$deltaT < 0 & data$Z.L < 0)] 
          ~ data$WS_Spd_WVT[which(data$deltaT < 0 & data$Z.L < 0)])
lm2 <- lm(data$H[which(data$deltaT > 0 & data$Z.L < 0)] 
          ~ data$WS_Spd_WVT[which(data$deltaT > 0 & data$Z.L < 0)])

lm3 <- lm(data$H[which(data$deltaT < 0 & data$Z.L < 0)] 
          ~ data$deltaT[which(data$deltaT < 0 & data$Z.L < 0)])
lm4 <- lm(data$H[which(data$deltaT > 0 & data$Z.L < 0)] 
          ~ data$deltaT[which(data$deltaT > 0 & data$Z.L < 0)])

lm5 <- lm(data$H[which(data$deltaT < 0 & data$Z.L < 0)] 
          ~ data$u_deltaT[which(data$deltaT < 0 & data$Z.L < 0)])
lm6 <- lm(data$H[which(data$deltaT > 0 & data$Z.L < 0)] 
          ~ data$u_deltaT[which(data$deltaT > 0 & data$Z.L < 0)])

summary(lm1)$adj.r.squared
summary(lm1)$coefficients
summary(lm2)$adj.r.squared
summary(lm2)$coefficients
summary(lm3)$adj.r.squared
summary(lm3)$coefficients
summary(lm4)$adj.r.squared
summary(lm4)$coefficients
summary(lm5)$adj.r.squared
summary(lm5)$coefficients
summary(lm6)$adj.r.squared
summary(lm6)$coefficients

# H (stable)
lm1 <- lm(data$H[which(data$deltaT < 0 & data$Z.L > 0)] 
          ~ data$WS_Spd_WVT[which(data$deltaT < 0 & data$Z.L > 0)])
lm2 <- lm(data$H[which(data$deltaT > 0 & data$Z.L > 0)] 
          ~ data$WS_Spd_WVT[which(data$deltaT > 0 & data$Z.L > 0)])

lm3 <- lm(data$H[which(data$deltaT < 0 & data$Z.L > 0)] 
          ~ data$deltaT[which(data$deltaT < 0 & data$Z.L > 0)])
lm4 <- lm(data$H[which(data$deltaT > 0 & data$Z.L > 0)] 
          ~ data$deltaT[which(data$deltaT > 0 & data$Z.L > 0)])

lm5 <- lm(data$H[which(data$deltaT < 0 & data$Z.L > 0)] 
          ~ data$u_deltaT[which(data$deltaT < 0 & data$Z.L > 0)])
lm6 <- lm(data$H[which(data$deltaT > 0 & data$Z.L > 0)] 
          ~ data$u_deltaT[which(data$deltaT > 0 & data$Z.L > 0)])

summary(lm1)$adj.r.squared
summary(lm1)$coefficients
summary(lm2)$adj.r.squared
summary(lm2)$coefficients
summary(lm3)$adj.r.squared
summary(lm3)$coefficients
summary(lm4)$adj.r.squared
summary(lm4)$coefficients
summary(lm5)$adj.r.squared
summary(lm5)$coefficients
summary(lm6)$adj.r.squared
summary(lm6)$coefficients

rm(lm1,lm2,lm3,lm4,lm5,lm6)
