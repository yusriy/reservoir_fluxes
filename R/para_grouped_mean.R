LE_mean <- c(mean(data$LE[which(data$stability_no==1)],na.rm=TRUE),
               mean(data$LE[which(data$stability_no==2)],na.rm=TRUE),
               mean(data$LE[which(data$stability_no==3)],na.rm=TRUE),
               mean(data$LE[which(data$stability_no==4)],na.rm=TRUE),
               mean(data$LE[which(data$stability_no==5)],na.rm=TRUE),
               mean(data$LE[which(data$stability_no==6)],na.rm=TRUE),
               mean(data$LE[which(data$stability_no==7)],na.rm=TRUE),
               mean(data$LE[which(data$stability_no==8)],na.rm=TRUE),
               mean(data$LE[which(data$stability_no==9)],na.rm=TRUE),
               mean(data$LE[which(data$stability_no==10)],na.rm=TRUE))

deltaE_mean <-c(mean(data$deltaE[which(data$stability_no==1)],na.rm=TRUE),
                  mean(data$deltaE[which(data$stability_no==2)],na.rm=TRUE),
                  mean(data$deltaE[which(data$stability_no==3)],na.rm=TRUE),
                  mean(data$deltaE[which(data$stability_no==4)],na.rm=TRUE),
                  mean(data$deltaE[which(data$stability_no==5)],na.rm=TRUE),
                  mean(data$deltaE[which(data$stability_no==6)],na.rm=TRUE),
                  mean(data$deltaE[which(data$stability_no==7)],na.rm=TRUE),
                  mean(data$deltaE[which(data$stability_no==8)],na.rm=TRUE),
                  mean(data$deltaE[which(data$stability_no==9)],na.rm=TRUE),
                  mean(data$deltaE[which(data$stability_no==10)],na.rm=TRUE))

WS_mean <-c(mean(data$WS_Spd_WVT[which(data$stability_no==1)],na.rm=TRUE),
              mean(data$WS_Spd_WVT[which(data$stability_no==2)],na.rm=TRUE),
              mean(data$WS_Spd_WVT[which(data$stability_no==3)],na.rm=TRUE),
              mean(data$WS_Spd_WVT[which(data$stability_no==4)],na.rm=TRUE),
              mean(data$WS_Spd_WVT[which(data$stability_no==5)],na.rm=TRUE),
              mean(data$WS_Spd_WVT[which(data$stability_no==6)],na.rm=TRUE),
              mean(data$WS_Spd_WVT[which(data$stability_no==7)],na.rm=TRUE),
              mean(data$WS_Spd_WVT[which(data$stability_no==8)],na.rm=TRUE),
              mean(data$WS_Spd_WVT[which(data$stability_no==9)],na.rm=TRUE),
              mean(data$WS_Spd_WVT[which(data$stability_no==10)],na.rm=TRUE))

udeltaE_mean <-c(mean(data$u_deltaE[which(data$stability_no==1)],na.rm=TRUE),
                   mean(data$u_deltaE[which(data$stability_no==2)],na.rm=TRUE),
                   mean(data$u_deltaE[which(data$stability_no==3)],na.rm=TRUE),
                   mean(data$u_deltaE[which(data$stability_no==4)],na.rm=TRUE),
                   mean(data$u_deltaE[which(data$stability_no==5)],na.rm=TRUE),
                   mean(data$u_deltaE[which(data$stability_no==6)],na.rm=TRUE),
                   mean(data$u_deltaE[which(data$stability_no==7)],na.rm=TRUE),
                   mean(data$u_deltaE[which(data$stability_no==8)],na.rm=TRUE),
                   mean(data$u_deltaE[which(data$stability_no==9)],na.rm=TRUE),
                   mean(data$u_deltaE[which(data$stability_no==10)],na.rm=TRUE))

H_mean <- c(mean(data$H[which(data$stability_no==1)],na.rm=TRUE),
              mean(data$H[which(data$stability_no==2)],na.rm=TRUE),
              mean(data$H[which(data$stability_no==3)],na.rm=TRUE),
              mean(data$H[which(data$stability_no==4)],na.rm=TRUE),
              mean(data$H[which(data$stability_no==5)],na.rm=TRUE),
              mean(data$H[which(data$stability_no==6)],na.rm=TRUE),
              mean(data$H[which(data$stability_no==7)],na.rm=TRUE),
              mean(data$H[which(data$stability_no==8)],na.rm=TRUE),
              mean(data$H[which(data$stability_no==9)],na.rm=TRUE),
              mean(data$H[which(data$stability_no==10)],na.rm=TRUE))

deltaT_mean <-c(mean(data$deltaT[which(data$stability_no==1)],na.rm=TRUE),
                  mean(data$deltaT[which(data$stability_no==2)],na.rm=TRUE),
                  mean(data$deltaT[which(data$stability_no==3)],na.rm=TRUE),
                  mean(data$deltaT[which(data$stability_no==4)],na.rm=TRUE),
                  mean(data$deltaT[which(data$stability_no==5)],na.rm=TRUE),
                  mean(data$deltaT[which(data$stability_no==6)],na.rm=TRUE),
                  mean(data$deltaT[which(data$stability_no==7)],na.rm=TRUE),
                  mean(data$deltaT[which(data$stability_no==8)],na.rm=TRUE),
                  mean(data$deltaT[which(data$stability_no==9)],na.rm=TRUE),
                  mean(data$deltaT[which(data$stability_no==10)],na.rm=TRUE))

udeltaT_mean <-c(mean(data$u_deltaT[which(data$stability_no==1)],na.rm=TRUE),
                   mean(data$u_deltaT[which(data$stability_no==2)],na.rm=TRUE),
                   mean(data$u_deltaT[which(data$stability_no==3)],na.rm=TRUE),
                   mean(data$u_deltaT[which(data$stability_no==4)],na.rm=TRUE),
                   mean(data$u_deltaT[which(data$stability_no==5)],na.rm=TRUE),
                   mean(data$u_deltaT[which(data$stability_no==6)],na.rm=TRUE),
                   mean(data$u_deltaT[which(data$stability_no==7)],na.rm=TRUE),
                   mean(data$u_deltaT[which(data$stability_no==8)],na.rm=TRUE),
                   mean(data$u_deltaT[which(data$stability_no==9)],na.rm=TRUE),
                   mean(data$u_deltaT[which(data$stability_no==10)],na.rm=TRUE))

ustar_mean <- c(mean(data$U.[which(data$stability_no==1)],na.rm=TRUE),
                mean(data$U.[which(data$stability_no==2)],na.rm=TRUE),
                mean(data$U.[which(data$stability_no==3)],na.rm=TRUE),
                mean(data$U.[which(data$stability_no==4)],na.rm=TRUE),
                mean(data$U.[which(data$stability_no==5)],na.rm=TRUE),
                mean(data$U.[which(data$stability_no==6)],na.rm=TRUE),
                mean(data$U.[which(data$stability_no==7)],na.rm=TRUE),
                mean(data$U.[which(data$stability_no==8)],na.rm=TRUE),
                mean(data$U.[which(data$stability_no==9)],na.rm=TRUE),
                mean(data$U.[which(data$stability_no==10)],na.rm=TRUE))

LE_U_mean <- c(mean(data$LE_U[which(data$stability_no==1)],na.rm=TRUE),
             mean(data$LE_U[which(data$stability_no==2)],na.rm=TRUE),
             mean(data$LE_U[which(data$stability_no==3)],na.rm=TRUE),
             mean(data$LE_U[which(data$stability_no==4)],na.rm=TRUE),
             mean(data$LE_U[which(data$stability_no==5)],na.rm=TRUE),
             mean(data$LE_U[which(data$stability_no==6)],na.rm=TRUE),
             mean(data$LE_U[which(data$stability_no==7)],na.rm=TRUE),
             mean(data$LE_U[which(data$stability_no==8)],na.rm=TRUE),
             mean(data$LE_U[which(data$stability_no==9)],na.rm=TRUE),
             mean(data$LE_U[which(data$stability_no==10)],na.rm=TRUE))

LE_deltaE_mean <- c(mean(data$LE_deltaE[which(data$stability_no==1)],na.rm=TRUE),
               mean(data$LE_deltaE[which(data$stability_no==2)],na.rm=TRUE),
               mean(data$LE_deltaE[which(data$stability_no==3)],na.rm=TRUE),
               mean(data$LE_deltaE[which(data$stability_no==4)],na.rm=TRUE),
               mean(data$LE_deltaE[which(data$stability_no==5)],na.rm=TRUE),
               mean(data$LE_deltaE[which(data$stability_no==6)],na.rm=TRUE),
               mean(data$LE_deltaE[which(data$stability_no==7)],na.rm=TRUE),
               mean(data$LE_deltaE[which(data$stability_no==8)],na.rm=TRUE),
               mean(data$LE_deltaE[which(data$stability_no==9)],na.rm=TRUE),
               mean(data$LE_deltaE[which(data$stability_no==10)],na.rm=TRUE))

LE_udeltaE_mean <- c(mean(data$LE_udeltaE[which(data$stability_no==1)],na.rm=TRUE),
                       mean(data$LE_udeltaE[which(data$stability_no==2)],na.rm=TRUE),
                       mean(data$LE_udeltaE[which(data$stability_no==3)],na.rm=TRUE),
                       mean(data$LE_udeltaE[which(data$stability_no==4)],na.rm=TRUE),
                       mean(data$LE_udeltaE[which(data$stability_no==5)],na.rm=TRUE),
                       mean(data$LE_udeltaE[which(data$stability_no==6)],na.rm=TRUE),
                       mean(data$LE_udeltaE[which(data$stability_no==7)],na.rm=TRUE),
                       mean(data$LE_udeltaE[which(data$stability_no==8)],na.rm=TRUE),
                       mean(data$LE_udeltaE[which(data$stability_no==9)],na.rm=TRUE),
                       mean(data$LE_udeltaE[which(data$stability_no==10)],na.rm=TRUE))

CE_mean <- c(mean(data$C_E[which(data$stability_no==1)],na.rm=TRUE),
                     mean(data$C_E[which(data$stability_no==2)],na.rm=TRUE),
                     mean(data$C_E[which(data$stability_no==3)],na.rm=TRUE),
                     mean(data$C_E[which(data$stability_no==4)],na.rm=TRUE),
                     mean(data$C_E[which(data$stability_no==5)],na.rm=TRUE),
                     mean(data$C_E[which(data$stability_no==6)],na.rm=TRUE),
                     mean(data$C_E[which(data$stability_no==7)],na.rm=TRUE),
                     mean(data$C_E[which(data$stability_no==8)],na.rm=TRUE),
                     mean(data$C_E[which(data$stability_no==9)],na.rm=TRUE),
                     mean(data$C_E[which(data$stability_no==10)],na.rm=TRUE))

CH_mean <- c(mean(data$C_H[which(data$stability_no==1)],na.rm=TRUE),
             mean(data$C_H[which(data$stability_no==2)],na.rm=TRUE),
             mean(data$C_H[which(data$stability_no==3)],na.rm=TRUE),
             mean(data$C_H[which(data$stability_no==4)],na.rm=TRUE),
             mean(data$C_H[which(data$stability_no==5)],na.rm=TRUE),
             mean(data$C_H[which(data$stability_no==6)],na.rm=TRUE),
             mean(data$C_H[which(data$stability_no==7)],na.rm=TRUE),
             mean(data$C_H[which(data$stability_no==8)],na.rm=TRUE),
             mean(data$C_H[which(data$stability_no==9)],na.rm=TRUE),
             mean(data$C_H[which(data$stability_no==10)],na.rm=TRUE))

H_U_mean <- c(mean(data$H_U[which(data$stability_no==1)],na.rm=TRUE),
              mean(data$H_U[which(data$stability_no==2)],na.rm=TRUE),
              mean(data$H_U[which(data$stability_no==3)],na.rm=TRUE),
              mean(data$H_U[which(data$stability_no==4)],na.rm=TRUE),
              mean(data$H_U[which(data$stability_no==5)],na.rm=TRUE),
              mean(data$H_U[which(data$stability_no==6)],na.rm=TRUE),
              mean(data$H_U[which(data$stability_no==7)],na.rm=TRUE),
              mean(data$H_U[which(data$stability_no==8)],na.rm=TRUE),
              mean(data$H_U[which(data$stability_no==9)],na.rm=TRUE),
              mean(data$H_U[which(data$stability_no==10)],na.rm=TRUE))

H_deltaT_mean <-c(mean(data$H_deltaT[which(data$stability_no==1)],na.rm=TRUE),
                  mean(data$H_deltaT[which(data$stability_no==2)],na.rm=TRUE),
                  mean(data$H_deltaT[which(data$stability_no==3)],na.rm=TRUE),
                  mean(data$H_deltaT[which(data$stability_no==4)],na.rm=TRUE),
                  mean(data$H_deltaT[which(data$stability_no==5)],na.rm=TRUE),
                  mean(data$H_deltaT[which(data$stability_no==6)],na.rm=TRUE),
                  mean(data$H_deltaT[which(data$stability_no==7)],na.rm=TRUE),
                  mean(data$H_deltaT[which(data$stability_no==8)],na.rm=TRUE),
                  mean(data$H_deltaT[which(data$stability_no==9)],na.rm=TRUE),
                  mean(data$H_deltaT[which(data$stability_no==10)],na.rm=TRUE))

H_udeltaT_mean <-c(mean(data$H_udeltaT[which(data$stability_no==1)],na.rm=TRUE),
                   mean(data$H_udeltaT[which(data$stability_no==2)],na.rm=TRUE),
                   mean(data$H_udeltaT[which(data$stability_no==3)],na.rm=TRUE),
                   mean(data$H_udeltaT[which(data$stability_no==4)],na.rm=TRUE),
                   mean(data$H_udeltaT[which(data$stability_no==5)],na.rm=TRUE),
                   mean(data$H_udeltaT[which(data$stability_no==6)],na.rm=TRUE),
                   mean(data$H_udeltaT[which(data$stability_no==7)],na.rm=TRUE),
                   mean(data$H_udeltaT[which(data$stability_no==8)],na.rm=TRUE),
                   mean(data$H_udeltaT[which(data$stability_no==9)],na.rm=TRUE),
                   mean(data$H_udeltaT[which(data$stability_no==10)],na.rm=TRUE))



# Linear regressions
# LE
lmLE1 <- lm(LE_mean ~ deltaE_mean)
lmLE2 <- lm(LE_mean ~ WS_mean) 
lmLE3 <- lm(LE_mean ~ udeltaE_mean)
# Unstable
lmLE11 <- lm(LE_mean[1:5] ~ deltaE_mean[1:5])
lmLE21 <- lm(LE_mean[1:5] ~ WS_mean[1:5])
lmLE31 <- lm(LE_mean[1:5] ~ udeltaE_mean[1:5])

# Stable
lmLE12 <- lm(LE_mean[6:10] ~ deltaE_mean[6:10])
lmLE22 <- lm(LE_mean[6:10] ~ WS_mean[6:10])
lmLE32 <- lm(LE_mean[6:10] ~ udeltaE_mean[6:10])

# H
lmH1 <- lm(H_mean ~ deltaT_mean)
lmH2 <- lm(H_mean ~ WS_mean) 
lmH3 <- lm(H_mean ~ udeltaT_mean)

# Unstable
lmH11 <- lm(H_mean[1:5] ~ deltaT_mean[1:5])
lmH21 <- lm(H_mean[1:5] ~ WS_mean[1:5])
lmH31 <- lm(H_mean[1:5] ~ udeltaT_mean[1:5])

# Stable
lmH12 <- lm(H_mean[6:10] ~ deltaT_mean[6:10])
lmH22 <- lm(H_mean[6:10] ~ WS_mean[6:10])
lmH32 <- lm(H_mean[6:10] ~ udeltaT_mean[6:10])

d_mean_cor <- data.frame(allLE = c(summary(lmLE1)$adj.r.squared,summary(lmLE2)$adj.r.squared,summary(lmLE3)$adj.r.squared),
                           unstableLE = c(summary(lmLE11)$adj.r.squared,summary(lmLE21)$adj.r.squared,summary(lmLE31)$adj.r.squared),
                           stableLE = c(summary(lmLE12)$adj.r.squared,summary(lmLE22)$adj.r.squared,summary(lmLE32)$adj.r.squared),
                           allH = c(summary(lmH1)$adj.r.squared,summary(lmH2)$adj.r.squared,summary(lmH3)$adj.r.squared),
                           unstableH = c(summary(lmH11)$adj.r.squared,summary(lmH21)$adj.r.squared,summary(lmH31)$adj.r.squared),
                           stableH = c(summary(lmH12)$adj.r.squared,summary(lmH22)$adj.r.squared,summary(lmH32)$adj.r.squared))

d_mean <- data.frame(deltaE_mean,WS_mean,udeltaE_mean,LE_mean,deltaT_mean,udeltaT_mean,H_mean,
                     ustar_mean,LE_U_mean,LE_deltaE_mean,LE_udeltaE_mean,CE_mean,H_U_mean,
                     H_deltaT_mean,H_udeltaT_mean,CH_mean)

# To delete all created variables in this script
rm(lmH11,lmH21,lmH31,lmH12,lmH22,lmH32,lmH1,lmH2,lmH3)
rm(lmLE1,lmLE11,lmLE12,lmLE2,lmLE21,lmLE22,lmLE3,lmLE31,lmLE32)

rm(LE_mean,deltaE_mean,WS_mean,udeltaE_mean,H_mean,deltaT_mean,udeltaT_mean,ustar_mean,
   LE_U_mean,LE_deltaE_mean,LE_udeltaE_mean,CE_mean,CH_mean,H_U_mean,
   H_deltaT_mean,H_udeltaT_mean)