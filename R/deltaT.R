plot(data$WS_Spd_WVT[data$deltaT < 0 & data$Z.L > 0],data$C_E[data$deltaT < 0 & data$Z.L > 0],ylim=c(-0.008,0.008),xlim=c(0,4))
points(data$WS_Spd_WVT[data$deltaT > 1 & data$Z.L > 0],data$C_E[data$deltaT > 1 & data$Z.L > 0],col='red')
points(data$WS_Spd_WVT[data$deltaT > 4 & data$Z.L > 0],data$C_E[data$deltaT > 4 & data$Z.L > 0],col='blue')


plot(data$WS_Spd_WVT[data$deltaT < 0],data$C_E[data$deltaT < 0],ylim=c(-0.008,0.008),xlim=c(0,4))
points(data$WS_Spd_WVT[data$deltaT < -0.5],data$C_E[data$deltaT < -0.5],col='red')
points(data$WS_Spd_WVT[data$deltaT < -1],data$C_E[data$deltaT < -1],col='blue')

plot(data$WS_Spd_WVT[data$deltaT < -1 & data$Z.L > 0 & data$Water.surface.temperature < 10],data$C_E[data$deltaT < -1 & data$Z.L > 0 & data$Water.surface.temperature < 10],ylim=c(-0.008,0.008),xlim=c(0,4))

