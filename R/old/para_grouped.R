LE_median <- c(median(data$LE[which(data$stability_no==1)],na.rm=TRUE),
               median(data$LE[which(data$stability_no==2)],na.rm=TRUE),
               median(data$LE[which(data$stability_no==3)],na.rm=TRUE),
               median(data$LE[which(data$stability_no==4)],na.rm=TRUE),
               median(data$LE[which(data$stability_no==5)],na.rm=TRUE),
               median(data$LE[which(data$stability_no==6)],na.rm=TRUE),
               median(data$LE[which(data$stability_no==7)],na.rm=TRUE),
               median(data$LE[which(data$stability_no==8)],na.rm=TRUE),
               median(data$LE[which(data$stability_no==9)],na.rm=TRUE),
               median(data$LE[which(data$stability_no==10)],na.rm=TRUE))

deltaE_median <-c(median(data$deltaE[which(data$stability_no==1)],na.rm=TRUE),
                  median(data$deltaE[which(data$stability_no==2)],na.rm=TRUE),
                  median(data$deltaE[which(data$stability_no==3)],na.rm=TRUE),
                  median(data$deltaE[which(data$stability_no==4)],na.rm=TRUE),
                  median(data$deltaE[which(data$stability_no==5)],na.rm=TRUE),
                  median(data$deltaE[which(data$stability_no==6)],na.rm=TRUE),
                  median(data$deltaE[which(data$stability_no==7)],na.rm=TRUE),
                  median(data$deltaE[which(data$stability_no==8)],na.rm=TRUE),
                  median(data$deltaE[which(data$stability_no==9)],na.rm=TRUE),
                  median(data$deltaE[which(data$stability_no==10)],na.rm=TRUE))

WS_median <-c(median(data$WS_Spd_WVT[which(data$stability_no==1)],na.rm=TRUE),
              median(data$WS_Spd_WVT[which(data$stability_no==2)],na.rm=TRUE),
              median(data$WS_Spd_WVT[which(data$stability_no==3)],na.rm=TRUE),
              median(data$WS_Spd_WVT[which(data$stability_no==4)],na.rm=TRUE),
              median(data$WS_Spd_WVT[which(data$stability_no==5)],na.rm=TRUE),
              median(data$WS_Spd_WVT[which(data$stability_no==6)],na.rm=TRUE),
              median(data$WS_Spd_WVT[which(data$stability_no==7)],na.rm=TRUE),
              median(data$WS_Spd_WVT[which(data$stability_no==8)],na.rm=TRUE),
              median(data$WS_Spd_WVT[which(data$stability_no==9)],na.rm=TRUE),
              median(data$WS_Spd_WVT[which(data$stability_no==10)],na.rm=TRUE))

udeltaE_median <-c(median(data$u_deltaE[which(data$stability_no==1)],na.rm=TRUE),
                   median(data$u_deltaE[which(data$stability_no==2)],na.rm=TRUE),
                   median(data$u_deltaE[which(data$stability_no==3)],na.rm=TRUE),
                   median(data$u_deltaE[which(data$stability_no==4)],na.rm=TRUE),
                   median(data$u_deltaE[which(data$stability_no==5)],na.rm=TRUE),
                   median(data$u_deltaE[which(data$stability_no==6)],na.rm=TRUE),
                   median(data$u_deltaE[which(data$stability_no==7)],na.rm=TRUE),
                   median(data$u_deltaE[which(data$stability_no==8)],na.rm=TRUE),
                   median(data$u_deltaE[which(data$stability_no==9)],na.rm=TRUE),
                   median(data$u_deltaE[which(data$stability_no==10)],na.rm=TRUE))

H_median <- c(median(data$H[which(data$stability_no==1)],na.rm=TRUE),
              median(data$H[which(data$stability_no==2)],na.rm=TRUE),
              median(data$H[which(data$stability_no==3)],na.rm=TRUE),
              median(data$H[which(data$stability_no==4)],na.rm=TRUE),
              median(data$H[which(data$stability_no==5)],na.rm=TRUE),
              median(data$H[which(data$stability_no==6)],na.rm=TRUE),
              median(data$H[which(data$stability_no==7)],na.rm=TRUE),
              median(data$H[which(data$stability_no==8)],na.rm=TRUE),
              median(data$H[which(data$stability_no==9)],na.rm=TRUE),
              median(data$H[which(data$stability_no==10)],na.rm=TRUE))

deltaT_median <-c(median(data$deltaT[which(data$stability_no==1)],na.rm=TRUE),
                  median(data$deltaT[which(data$stability_no==2)],na.rm=TRUE),
                  median(data$deltaT[which(data$stability_no==3)],na.rm=TRUE),
                  median(data$deltaT[which(data$stability_no==4)],na.rm=TRUE),
                  median(data$deltaT[which(data$stability_no==5)],na.rm=TRUE),
                  median(data$deltaT[which(data$stability_no==6)],na.rm=TRUE),
                  median(data$deltaT[which(data$stability_no==7)],na.rm=TRUE),
                  median(data$deltaT[which(data$stability_no==8)],na.rm=TRUE),
                  median(data$deltaT[which(data$stability_no==9)],na.rm=TRUE),
                  median(data$deltaT[which(data$stability_no==10)],na.rm=TRUE))

udeltaT_median <-c(median(data$u_deltaT[which(data$stability_no==1)],na.rm=TRUE),
                   median(data$u_deltaT[which(data$stability_no==2)],na.rm=TRUE),
                   median(data$u_deltaT[which(data$stability_no==3)],na.rm=TRUE),
                   median(data$u_deltaT[which(data$stability_no==4)],na.rm=TRUE),
                   median(data$u_deltaT[which(data$stability_no==5)],na.rm=TRUE),
                   median(data$u_deltaT[which(data$stability_no==6)],na.rm=TRUE),
                   median(data$u_deltaT[which(data$stability_no==7)],na.rm=TRUE),
                   median(data$u_deltaT[which(data$stability_no==8)],na.rm=TRUE),
                   median(data$u_deltaT[which(data$stability_no==9)],na.rm=TRUE),
                   median(data$u_deltaT[which(data$stability_no==10)],na.rm=TRUE))

ustar_median <- c(median(data$U.[which(data$stability_no==1)],na.rm=TRUE),
                median(data$U.[which(data$stability_no==2)],na.rm=TRUE),
                median(data$U.[which(data$stability_no==3)],na.rm=TRUE),
                median(data$U.[which(data$stability_no==4)],na.rm=TRUE),
                median(data$U.[which(data$stability_no==5)],na.rm=TRUE),
                median(data$U.[which(data$stability_no==6)],na.rm=TRUE),
                median(data$U.[which(data$stability_no==7)],na.rm=TRUE),
                median(data$U.[which(data$stability_no==8)],na.rm=TRUE),
                median(data$U.[which(data$stability_no==9)],na.rm=TRUE),
                median(data$U.[which(data$stability_no==10)],na.rm=TRUE))

LE_U_median <- c(median(data$LE_U[which(data$stability_no==1)],na.rm=TRUE),
               median(data$LE_U[which(data$stability_no==2)],na.rm=TRUE),
               median(data$LE_U[which(data$stability_no==3)],na.rm=TRUE),
               median(data$LE_U[which(data$stability_no==4)],na.rm=TRUE),
               median(data$LE_U[which(data$stability_no==5)],na.rm=TRUE),
               median(data$LE_U[which(data$stability_no==6)],na.rm=TRUE),
               median(data$LE_U[which(data$stability_no==7)],na.rm=TRUE),
               median(data$LE_U[which(data$stability_no==8)],na.rm=TRUE),
               median(data$LE_U[which(data$stability_no==9)],na.rm=TRUE),
               median(data$LE_U[which(data$stability_no==10)],na.rm=TRUE))

LE_deltaE_median <- c(median(data$LE_deltaE[which(data$stability_no==1)],na.rm=TRUE),
                    median(data$LE_deltaE[which(data$stability_no==2)],na.rm=TRUE),
                    median(data$LE_deltaE[which(data$stability_no==3)],na.rm=TRUE),
                    median(data$LE_deltaE[which(data$stability_no==4)],na.rm=TRUE),
                    median(data$LE_deltaE[which(data$stability_no==5)],na.rm=TRUE),
                    median(data$LE_deltaE[which(data$stability_no==6)],na.rm=TRUE),
                    median(data$LE_deltaE[which(data$stability_no==7)],na.rm=TRUE),
                    median(data$LE_deltaE[which(data$stability_no==8)],na.rm=TRUE),
                    median(data$LE_deltaE[which(data$stability_no==9)],na.rm=TRUE),
                    median(data$LE_deltaE[which(data$stability_no==10)],na.rm=TRUE))

LE_udeltaE_median <- c(median(data$LE_udeltaE[which(data$stability_no==1)],na.rm=TRUE),
                      median(data$LE_udeltaE[which(data$stability_no==2)],na.rm=TRUE),
                      median(data$LE_udeltaE[which(data$stability_no==3)],na.rm=TRUE),
                      median(data$LE_udeltaE[which(data$stability_no==4)],na.rm=TRUE),
                      median(data$LE_udeltaE[which(data$stability_no==5)],na.rm=TRUE),
                      median(data$LE_udeltaE[which(data$stability_no==6)],na.rm=TRUE),
                      median(data$LE_udeltaE[which(data$stability_no==7)],na.rm=TRUE),
                      median(data$LE_udeltaE[which(data$stability_no==8)],na.rm=TRUE),
                      median(data$LE_udeltaE[which(data$stability_no==9)],na.rm=TRUE),
                      median(data$LE_udeltaE[which(data$stability_no==10)],na.rm=TRUE))

CE_median <- c(median(data$C_E[which(data$stability_no==1)],na.rm=TRUE),
             median(data$C_E[which(data$stability_no==2)],na.rm=TRUE),
             median(data$C_E[which(data$stability_no==3)],na.rm=TRUE),
             median(data$C_E[which(data$stability_no==4)],na.rm=TRUE),
             median(data$C_E[which(data$stability_no==5)],na.rm=TRUE),
             median(data$C_E[which(data$stability_no==6)],na.rm=TRUE),
             median(data$C_E[which(data$stability_no==7)],na.rm=TRUE),
             median(data$C_E[which(data$stability_no==8)],na.rm=TRUE),
             median(data$C_E[which(data$stability_no==9)],na.rm=TRUE),
             median(data$C_E[which(data$stability_no==10)],na.rm=TRUE))

CH_median <- c(median(data$C_H[which(data$stability_no==1)],na.rm=TRUE),
               median(data$C_H[which(data$stability_no==2)],na.rm=TRUE),
               median(data$C_H[which(data$stability_no==3)],na.rm=TRUE),
               median(data$C_H[which(data$stability_no==4)],na.rm=TRUE),
               median(data$C_H[which(data$stability_no==5)],na.rm=TRUE),
               median(data$C_H[which(data$stability_no==6)],na.rm=TRUE),
               median(data$C_H[which(data$stability_no==7)],na.rm=TRUE),
               median(data$C_H[which(data$stability_no==8)],na.rm=TRUE),
               median(data$C_H[which(data$stability_no==9)],na.rm=TRUE),
               median(data$C_H[which(data$stability_no==10)],na.rm=TRUE))

H_U_median <- c(median(data$H_U[which(data$stability_no==1)],na.rm=TRUE),
              median(data$H_U[which(data$stability_no==2)],na.rm=TRUE),
              median(data$H_U[which(data$stability_no==3)],na.rm=TRUE),
              median(data$H_U[which(data$stability_no==4)],na.rm=TRUE),
              median(data$H_U[which(data$stability_no==5)],na.rm=TRUE),
              median(data$H_U[which(data$stability_no==6)],na.rm=TRUE),
              median(data$H_U[which(data$stability_no==7)],na.rm=TRUE),
              median(data$H_U[which(data$stability_no==8)],na.rm=TRUE),
              median(data$H_U[which(data$stability_no==9)],na.rm=TRUE),
              median(data$H_U[which(data$stability_no==10)],na.rm=TRUE))

H_deltaT_median <-c(median(data$H_deltaT[which(data$stability_no==1)],na.rm=TRUE),
                  median(data$H_deltaT[which(data$stability_no==2)],na.rm=TRUE),
                  median(data$H_deltaT[which(data$stability_no==3)],na.rm=TRUE),
                  median(data$H_deltaT[which(data$stability_no==4)],na.rm=TRUE),
                  median(data$H_deltaT[which(data$stability_no==5)],na.rm=TRUE),
                  median(data$H_deltaT[which(data$stability_no==6)],na.rm=TRUE),
                  median(data$H_deltaT[which(data$stability_no==7)],na.rm=TRUE),
                  median(data$H_deltaT[which(data$stability_no==8)],na.rm=TRUE),
                  median(data$H_deltaT[which(data$stability_no==9)],na.rm=TRUE),
                  median(data$H_deltaT[which(data$stability_no==10)],na.rm=TRUE))

H_udeltaT_median <-c(median(data$H_udeltaT[which(data$stability_no==1)],na.rm=TRUE),
                   median(data$H_udeltaT[which(data$stability_no==2)],na.rm=TRUE),
                   median(data$H_udeltaT[which(data$stability_no==3)],na.rm=TRUE),
                   median(data$H_udeltaT[which(data$stability_no==4)],na.rm=TRUE),
                   median(data$H_udeltaT[which(data$stability_no==5)],na.rm=TRUE),
                   median(data$H_udeltaT[which(data$stability_no==6)],na.rm=TRUE),
                   median(data$H_udeltaT[which(data$stability_no==7)],na.rm=TRUE),
                   median(data$H_udeltaT[which(data$stability_no==8)],na.rm=TRUE),
                   median(data$H_udeltaT[which(data$stability_no==9)],na.rm=TRUE),
                   median(data$H_udeltaT[which(data$stability_no==10)],na.rm=TRUE))


# Linear regressions
# LE
lmLE1 <- lm(LE_median ~ deltaE_median)
lmLE2 <- lm(LE_median ~ WS_median) 
lmLE3 <- lm(LE_median ~ udeltaE_median)
# Unstable
lmLE11 <- lm(LE_median[1:5] ~ deltaE_median[1:5])
lmLE21 <- lm(LE_median[1:5] ~ WS_median[1:5])
lmLE31 <- lm(LE_median[1:5] ~ udeltaE_median[1:5])

# Stable
lmLE12 <- lm(LE_median[6:10] ~ deltaE_median[6:10])
lmLE22 <- lm(LE_median[6:10] ~ WS_median[6:10])
lmLE32 <- lm(LE_median[6:10] ~ udeltaE_median[6:10])

# H
lmH1 <- lm(H_median ~ deltaT_median)
lmH2 <- lm(H_median ~ WS_median) 
lmH3 <- lm(H_median ~ udeltaT_median)

# Unstable
lmH11 <- lm(H_median[1:5] ~ deltaT_median[1:5])
lmH21 <- lm(H_median[1:5] ~ WS_median[1:5])
lmH31 <- lm(H_median[1:5] ~ udeltaT_median[1:5])

# Stable
lmH12 <- lm(H_median[6:10] ~ deltaT_median[6:10])
lmH22 <- lm(H_median[6:10] ~ WS_median[6:10])
lmH32 <- lm(H_median[6:10] ~ udeltaT_median[6:10])

d_median <- data.frame(deltaE_median,WS_median,udeltaE_median,LE_median,deltaT_median,udeltaT_median,H_median,
                     ustar_median,LE_U_median,LE_deltaE_median,LE_udeltaE_median,CE_median,
                     H_U_median,H_deltaT_median,H_udeltaT_median,CH_median)

d_median_cor <- data.frame(allLE = c(summary(lmLE1)$adj.r.squared,summary(lmLE2)$adj.r.squared,summary(lmLE3)$adj.r.squared),
                           unstableLE = c(summary(lmLE11)$adj.r.squared,summary(lmLE21)$adj.r.squared,summary(lmLE31)$adj.r.squared),
                           stableLE = c(summary(lmLE12)$adj.r.squared,summary(lmLE22)$adj.r.squared,summary(lmLE32)$adj.r.squared),
                           allH = c(summary(lmH1)$adj.r.squared,summary(lmH2)$adj.r.squared,summary(lmH3)$adj.r.squared),
                           unstableH = c(summary(lmH11)$adj.r.squared,summary(lmH21)$adj.r.squared,summary(lmH31)$adj.r.squared),
                           stableH = c(summary(lmH12)$adj.r.squared,summary(lmH22)$adj.r.squared,summary(lmH32)$adj.r.squared))
# To delete all created variables in this script
rm(lmH11,lmH21,lmH31,lmH12,lmH22,lmH32,lmH1,lmH2,lmH3)
rm(lmLE1,lmLE11,lmLE12,lmLE2,lmLE21,lmLE22,lmLE3,lmLE31,lmLE32)
rm(deltaE_median,deltaT_median,H_median,LE_median,udeltaE_median,udeltaT_median,WS_median,
   ustar_median,LE_U_median,LE_deltaE_median,LE_udeltaE_median,CE_median,H_U_median,H_udeltaT_median,H_deltaT_median,CH_median)
