# Wind category 1
plot(data$time_stamp[which(data$wind_category_day == 1 & data$day < 5)],
     data$WS_Spd_WVT[which(data$wind_category_day == 1 & data$day < 5)], 
     type='l', xlab = '', ylab = '', lwd = 2)
mtext(side = 1, 'Time (local time)', line = 2.5)
mtext(side = 2, 'U', line = 2.5)

# Wind category 4
plot(data$time_stamp[which(data$wind_category_day == 4 & data$day < 23)],
     data$WS_Spd_WVT[which(data$wind_category_day == 4 & data$day < 23)], 
     type='l', xlab = '', ylab = '', lwd = 2)
mtext(side = 1, 'Time (local time)', line = 2.5)
mtext(side = 2, 'U', line = 2.5)

# Barplot

wind_days <- c(49,31,46,48)
barplot(wind_days, ylim =c(0,60), ylab = 'Days', names.arg = c('I', 'II', 'III', 'IV'),
        xlab = 'Wind-class', col = 'orange')
box()
