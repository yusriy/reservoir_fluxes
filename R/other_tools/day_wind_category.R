
day_wind <- numeric(length=177)
for (i in 1:177){ # 177 total days in dataset
  cat1 <- sum(data$wind_category==1 & data$day==i,na.rm=TRUE)
  cat2 <- sum(data$wind_category==2 & data$day==i,na.rm=TRUE)
  cat3 <- sum(data$wind_category==3 & data$day==i,na.rm=TRUE)
  cat4 <- sum(data$wind_category==4 & data$day==i,na.rm=TRUE)
  day_wind[i] <- which.max(c(cat1,cat2,cat3,cat4))
}

rm(cat1,cat2,cat3,cat4,i)

wind_category_day <- numeric(length=nrow(data))

for (i in 1:length(day_wind)) {
  for (k in 1:nrow(data)) {
    if (is.na(data$day[k])) {
      wind_category_day[k] <- NA
    } else if (data$day[k]==i){
      wind_category_day[k] <- day_wind[i]
    }
  }
}

data <- cbind(data,wind_category_day)
rm(day_wind,k,wind_category_day)