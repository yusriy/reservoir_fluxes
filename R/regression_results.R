##### TABLING REGRESSION RESULTS #######################

## Adding regression results between LE and deltaE, U, ustar, and u_deltaE
## to a dataframe called 'data_rsq'

## Adding p-value results between U and deltaE to a dataframe called 'data_rsq'

# From category 1 to 10 in sequence
# z/L = -10 to -1 = cat1
# z/L = -1 to -0.5 = cat2
# z/L = -0.5 to -0.1 = cat3
# z/L = -0.1 to -0.05 = cat4
# z/L = -0.05 to 0 = cat5
# z/L =  0 to 0.05 = cat6
# z/L =  0.05 to 0.10 = cat7
# z/L =  0.10 to 0.50 = cat8
# z/L =  0.50 to 1.00 = cat9
# z/L =  1 to 10 = cat10
# R squared between H and U

names_boxplot = c('-10\u2264\u03B6<-1','-1\u2264\u03B6<-0.5','-0.5\u2264\u03B6<-0.1','-0.1\u2264\u03B6<-0.05',
                  '-0.05\u2264\u03B6<0','0\u2264\u03B6<0.05','0.05\u2264\u03B6<0.1','0.1\u2264\u03B6<0.5','0.5\u2264\u03B6<1','1\u2264\u03B6<10')
cat_no <- as.factor(1:10)

# Stability no 1
lmLE_U_1 <- lm(data$LE[which(data$stability_no==1)]~data$WS_Spd_WVT[which(data$stability_no==1)])
# Stability no 2
lmLE_U_2 <- lm(data$LE[which(data$stability_no==2)]~data$WS_Spd_WVT[which(data$stability_no==2)])
# Stability no 3
lmLE_U_3 <- lm(data$LE[which(data$stability_no==3)]~data$WS_Spd_WVT[which(data$stability_no==3)])
# Stability no 4
lmLE_U_4 <- lm(data$LE[which(data$stability_no==4)]~data$WS_Spd_WVT[which(data$stability_no==4)])
# Stability no 5
lmLE_U_5 <- lm(data$LE[which(data$stability_no==5)]~data$WS_Spd_WVT[which(data$stability_no==5)])
# Stability no 6
lmLE_U_6 <- lm(data$LE[which(data$stability_no==6)]~data$WS_Spd_WVT[which(data$stability_no==6)])
# Stability no 7
lmLE_U_7 <- lm(data$LE[which(data$stability_no==7)]~data$WS_Spd_WVT[which(data$stability_no==7)])
# Stability no 8
lmLE_U_8 <- lm(data$LE[which(data$stability_no==8)]~data$WS_Spd_WVT[which(data$stability_no==8)])
# Stability no 9
lmLE_U_9 <- lm(data$LE[which(data$stability_no==9)]~data$WS_Spd_WVT[which(data$stability_no==9)])
# Stability no 10
lmLE_U_10 <- lm(data$LE[which(data$stability_no==10)]~data$WS_Spd_WVT[which(data$stability_no==10)])

r_LE_U <- c(summary(lmLE_U_1)$adj.r.squared,summary(lmLE_U_2)$adj.r.squared,
            summary(lmLE_U_3)$adj.r.squared,summary(lmLE_U_4)$adj.r.squared,
            summary(lmLE_U_5)$adj.r.squared,summary(lmLE_U_6)$adj.r.squared,
            summary(lmLE_U_7)$adj.r.squared,summary(lmLE_U_8)$adj.r.squared,
            summary(lmLE_U_9)$adj.r.squared,summary(lmLE_U_10)$adj.r.squared)
p_LE_U <- c(summary(lmLE_U_1)$coefficients[8],summary(lmLE_U_2)$coefficients[8],
            summary(lmLE_U_3)$coefficients[8],summary(lmLE_U_4)$coefficients[8],
            summary(lmLE_U_5)$coefficients[8],summary(lmLE_U_6)$coefficients[8],
            summary(lmLE_U_7)$coefficients[8],summary(lmLE_U_8)$coefficients[8],
            summary(lmLE_U_9)$coefficients[8],summary(lmLE_U_10)$coefficients[8])

rm(lmLE_U_1,lmLE_U_2,lmLE_U_3,lmLE_U_4,lmLE_U_5,lmLE_U_6,lmLE_U_7,lmLE_U_8,lmLE_U_9,lmLE_U_10)

# R squared between LE and deltaE

# Stability no 1
lmLE_dE_1 <- lm(data$LE[which(data$stability_no==1)]~data$deltaE[which(data$stability_no==1)])
# Stability no 2
lmLE_dE_2 <- lm(data$LE[which(data$stability_no==2)]~data$deltaE[which(data$stability_no==2)])
# Stability no 3
lmLE_dE_3 <- lm(data$LE[which(data$stability_no==3)]~data$deltaE[which(data$stability_no==3)])
# Stability no 4
lmLE_dE_4 <- lm(data$LE[which(data$stability_no==4)]~data$deltaE[which(data$stability_no==4)])
# Stability no 5
lmLE_dE_5 <- lm(data$LE[which(data$stability_no==5)]~data$deltaE[which(data$stability_no==5)])
# Stability no 6
lmLE_dE_6 <- lm(data$LE[which(data$stability_no==6)]~data$deltaE[which(data$stability_no==6)])
# Stability no 7
lmLE_dE_7 <- lm(data$LE[which(data$stability_no==7)]~data$deltaE[which(data$stability_no==7)])
# Stability no 8
lmLE_dE_8 <- lm(data$LE[which(data$stability_no==8)]~data$deltaE[which(data$stability_no==8)])
# Stability no 9
lmLE_dE_9 <- lm(data$LE[which(data$stability_no==9)]~data$deltaE[which(data$stability_no==9)])
# Stability no 10
lmLE_dE_10 <- lm(data$LE[which(data$stability_no==10)]~data$deltaE[which(data$stability_no==10)])

r_LE_dE <- c(summary(lmLE_dE_1)$adj.r.squared,summary(lmLE_dE_2)$adj.r.squared,
            summary(lmLE_dE_3)$adj.r.squared,summary(lmLE_dE_4)$adj.r.squared,
            summary(lmLE_dE_5)$adj.r.squared,summary(lmLE_dE_6)$adj.r.squared,
            summary(lmLE_dE_7)$adj.r.squared,summary(lmLE_dE_8)$adj.r.squared,
            summary(lmLE_dE_9)$adj.r.squared,summary(lmLE_dE_10)$adj.r.squared)
p_LE_dE <- c(summary(lmLE_dE_1)$coefficients[8],summary(lmLE_dE_2)$coefficients[8],
             summary(lmLE_dE_3)$coefficients[8],summary(lmLE_dE_4)$coefficients[8],
             summary(lmLE_dE_5)$coefficients[8],summary(lmLE_dE_6)$coefficients[8],
             summary(lmLE_dE_7)$coefficients[8],summary(lmLE_dE_8)$coefficients[8],
             summary(lmLE_dE_9)$coefficients[8],summary(lmLE_dE_10)$coefficients[8])

rm(lmLE_dE_1,lmLE_dE_2,lmLE_dE_3,lmLE_dE_4,lmLE_dE_5,lmLE_dE_6,lmLE_dE_7,lmLE_dE_8,lmLE_dE_9,lmLE_dE_10)

# R squared between LE and udeltaE

# Stability no 1
lmLE_UdE1 <- lm(data$LE[which(data$stability_no==1)]~data$u_deltaE[which(data$stability_no==1)])
# Stability no 2
lmLE_UdE2 <- lm(data$LE[which(data$stability_no==2)]~data$u_deltaE[which(data$stability_no==2)])
# Stability no 3
lmLE_UdE3 <- lm(data$LE[which(data$stability_no==3)]~data$u_deltaE[which(data$stability_no==3)])
# Stability no 4
lmLE_UdE4 <- lm(data$LE[which(data$stability_no==4)]~data$u_deltaE[which(data$stability_no==4)])
# Stability no 5
lmLE_UdE5 <- lm(data$LE[which(data$stability_no==5)]~data$u_deltaE[which(data$stability_no==5)])
# Stability no 6
lmLE_UdE6 <- lm(data$LE[which(data$stability_no==6)]~data$u_deltaE[which(data$stability_no==6)])
# Stability no 7
lmLE_UdE7 <- lm(data$LE[which(data$stability_no==7)]~data$u_deltaE[which(data$stability_no==7)])
# Stability no 8
lmLE_UdE8 <- lm(data$LE[which(data$stability_no==8)]~data$u_deltaE[which(data$stability_no==8)])
# Stability no 9
lmLE_UdE9 <- lm(data$LE[which(data$stability_no==9)]~data$u_deltaE[which(data$stability_no==9)])
# Stability no 10
lmLE_UdE10 <- lm(data$LE[which(data$stability_no==10)]~data$u_deltaE[which(data$stability_no==10)])

r_LE_UdE <- c(summary(lmLE_UdE1)$adj.r.squared,summary(lmLE_UdE2)$adj.r.squared,
             summary(lmLE_UdE3)$adj.r.squared,summary(lmLE_UdE4)$adj.r.squared,
             summary(lmLE_UdE5)$adj.r.squared,summary(lmLE_UdE6)$adj.r.squared,
             summary(lmLE_UdE7)$adj.r.squared,summary(lmLE_UdE8)$adj.r.squared,
             summary(lmLE_UdE9)$adj.r.squared,summary(lmLE_UdE10)$adj.r.squared)
p_LE_UdE <- c(summary(lmLE_UdE1)$coefficients[8],summary(lmLE_UdE2)$coefficients[8],
              summary(lmLE_UdE3)$coefficients[8],summary(lmLE_UdE4)$coefficients[8],
              summary(lmLE_UdE5)$coefficients[8],summary(lmLE_UdE6)$coefficients[8],
              summary(lmLE_UdE7)$coefficients[8],summary(lmLE_UdE8)$coefficients[8],
              summary(lmLE_UdE9)$coefficients[8],summary(lmLE_UdE10)$coefficients[8])
rm(lmLE_UdE1,lmLE_UdE2,lmLE_UdE3,lmLE_UdE4,lmLE_UdE5,lmLE_UdE6,lmLE_UdE7,lmLE_UdE8,lmLE_UdE9,lmLE_UdE10)

data_rsq <-data.frame(cat_no,names_boxplot,
                      r_LE_U,p_LE_U,
                      r_LE_dE,p_LE_dE,
                      r_LE_UdE,p_LE_UdE)
rm(r_LE_U,r_LE_dE,r_LE_UdE,p_LE_U,p_LE_dE,p_LE_UdE)


#names_boxplot <- as.character(names_boxplot)
#rsquared_LE_U <- c(0.1706,0.2519,0.4324,0.4472,0.2209,0.00455,
#                   -0.001988,-0.001209,0.02215,-0.0004387)
#r_LE_U <- sapply(abs(rsquared_LE_U),sqrt)
#rsquared_LE_ustar <- c(0.2303,0.3316,0.4805,0.4589,0.2163,
#                       0.002817,-0.00296,0.003586,-0.004045,-0.007646)
#r_LE_ustar <- sapply(abs(rsquared_LE_ustar),sqrt)
#rsquared_LE_deltaE <- c(0.4505,0.4615,0.4055,0.3414,0.3754,
#                        0.6396,0.6718,0.7359,0.6883,0.7090)
#r_LE_deltaE <- sapply(abs(rsquared_LE_deltaE),sqrt)
#rsquared_LE_udeltaE <- c(0.6666,0.7746,0.8571,0.8471,0.7599,
#                         0.7676,0.724,0.7562,0.6185,0.5614)
#r_LE_udeltaE <- sapply(abs(rsquared_LE_udeltaE),sqrt)

# Adding p-value results
#p_U_deltaE <- c(0.0116,0.0385,0.08409,0.6096,0.0001853,
#                3.35E-7,0.001012,6.767E-6,6.794E-6,0.00037)

#cat_no <- as.factor(1:10)

#data_rsq <- cbind.data.frame(cat_no,names_boxplot,r_LE_U,r_LE_ustar,
#                  r_LE_deltaE,r_LE_udeltaE,rsquared_LE_U,rsquared_LE_ustar,
#                  rsquared_LE_deltaE,rsquared_LE_udeltaE,
#                  p_U_deltaE,stringsAsFactors = FALSE)

# Deleting temporary data
#rm(rsquared_LE_U,rsquared_LE_ustar,rsquared_LE_deltaE,rsquared_LE_udeltaE,
#   r_LE_U,r_LE_ustar,r_LE_deltaE,r_LE_udeltaE,p_U_deltaE,cat_no)
