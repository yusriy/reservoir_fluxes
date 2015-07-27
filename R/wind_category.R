wind_category <- numeric(length=nrow(data))

for (i in 1:nrow(data)){
  # A failsafe if time_stamp is NA
  if(is.na(data$WS_Spd_WVT[i])){
    # Assigned NA if WS_Spd_WVT is NA
    wind_category[i] <- NA
  } else {
    if(data$WS_Spd_WVT[i] <= quartile1){
      wind_category[i] <- 1
    } else if (data$WS_Spd_WVT[i] > quartile1 & data$WS_Spd_WVT[i] <= quartile2){
      wind_category[i] <- 2
    } else if (data$WS_Spd_WVT[i] > quartile2 & data$WS_Spd_WVT[i] <= quartile3){
      wind_category[i] <- 3
    } else if (data$WS_Spd_WVT[i] > quartile3){
      wind_category[i] <- 4
    }
  }
}

