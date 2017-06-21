# Sample unit root test 
# y <- 1:240
# E <- rnorm(240) + y
# X <- cumsum(E)
# plot(X, type ='l')
# lags <- 0
# z <- diff(X)
# n <- length(z)
# z.diff <- embed(z, lags+1)[,1]
# z.lag.1=X[(lags+1):n]
# summary(lm(z.diff~0+z.lag.1 ))
# 
# summary(lm(z.diff~0+z.lag.1 ))$coefficients[1,3]
# 
# library(urca)
# df=ur.df(X,type="none",lags=0)
# df

# Unit root test for each day to test for statioary
library(urca)

wind_cat1 <- data[which(data$wind_category_day == 1),]
wind_cat2 <- data[which(data$wind_category_day == 2),]
wind_cat3 <- data[which(data$wind_category_day == 3),]
wind_cat4 <- data[which(data$wind_category_day == 4),]




#Wind cat #
# Algorithm to test for each day 
summary(as.factor(wind_cat1$day))
cat1_u <- wind_cat1$WS_Spd_WVT[wind_cat1$day == 3]
plot(cat1_u, type = 'l')
plot(diff(cat1_u), type = 'l')
summary(ur.df(na.omit(cat1_u), type = 'trend', lags = 0))


#Wind cat 1
days1 <- as.numeric(levels(as.factor(wind_cat1$day)))
test_stat1 <- 0
for (i in 1:length(days1)){
  cat1_u <- wind_cat1$WS_Spd_WVT[wind_cat1$day == days1[i]]
  test_stat1[i] <- ur.df(na.omit(cat1_u), type = 'trend', lags = 0)@teststat[1]
}
length(days1[which(test_stat1 < -3.18)])/length(days1)

#Wind cat 2
days2 <- as.numeric(levels(as.factor(wind_cat2$day)))
test_stat2 <- 0
for (i in 1:length(days2)){
  cat2_u <- wind_cat2$WS_Spd_WVT[wind_cat2$day == days2[i]]
  test_stat2[i] <- ur.df(na.omit(cat2_u), type = 'trend', lags = 0)@teststat[1]
}
length(days2[which(test_stat2 < -3.18)])/length(days2)

#Wind cat 3
days3 <- as.numeric(levels(as.factor(wind_cat3$day)))
test_stat3 <- 0
for (i in 1:length(days3)){
  cat3_u <- wind_cat3$WS_Spd_WVT[wind_cat3$day == days3[i]]
  test_stat3[i] <- ur.df(na.omit(cat3_u), type = 'trend', lags = 0)@teststat[1]
}
length(days3[which(test_stat3 < -3.24)])/length(days3)


#Wind cat 4
days4 <- as.numeric(levels(as.factor(wind_cat4$day)))
test_stat4 <- 0
for (i in 1:length(days4)){
  cat4_u <- wind_cat4$WS_Spd_WVT[wind_cat4$day == days4[i]]
  test_stat4[i] <- ur.df(na.omit(cat4_u), type = 'trend', lags = 0)@teststat[1]
}
length(days4[which(test_stat4 < -3.18)])/length(days4)

