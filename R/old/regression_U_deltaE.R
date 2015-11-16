# To calculate p-values of U with deltaE
lm_U_deltaE1 <- lm(deltaE[stability_no==1]~WS_Spd_WVT[stability_no==1],data=data)
lm_U_deltaE2 <- lm(deltaE[stability_no==2]~WS_Spd_WVT[stability_no==2],data=data)
lm_U_deltaE3 <- lm(deltaE[stability_no==3]~WS_Spd_WVT[stability_no==3],data=data)
lm_U_deltaE4 <- lm(deltaE[stability_no==4]~WS_Spd_WVT[stability_no==4],data=data)
lm_U_deltaE5 <- lm(deltaE[stability_no==5]~WS_Spd_WVT[stability_no==5],data=data)
lm_U_deltaE6 <- lm(deltaE[stability_no==6]~WS_Spd_WVT[stability_no==6],data=data)
lm_U_deltaE7 <- lm(deltaE[stability_no==7]~WS_Spd_WVT[stability_no==7],data=data)
lm_U_deltaE8 <- lm(deltaE[stability_no==8]~WS_Spd_WVT[stability_no==8],data=data)
lm_U_deltaE9 <- lm(deltaE[stability_no==9]~WS_Spd_WVT[stability_no==9],data=data)
lm_U_deltaE10 <- lm(deltaE[stability_no==10]~WS_Spd_WVT[stability_no==10],data=data)

# To calculate p-values of u* with deltaE
lm_ustar_deltaE1 <- lm(deltaE[stability_no==1]~U.[stability_no==1],data=data)
lm_ustar_deltaE2 <- lm(deltaE[stability_no==2]~U.[stability_no==2],data=data)
lm_ustar_deltaE3 <- lm(deltaE[stability_no==3]~U.[stability_no==3],data=data)
lm_ustar_deltaE4 <- lm(deltaE[stability_no==4]~U.[stability_no==4],data=data)
lm_ustar_deltaE5 <- lm(deltaE[stability_no==5]~U.[stability_no==5],data=data)
lm_ustar_deltaE6 <- lm(deltaE[stability_no==6]~U.[stability_no==6],data=data)
lm_ustar_deltaE7 <- lm(deltaE[stability_no==7]~U.[stability_no==7],data=data)
lm_ustar_deltaE8 <- lm(deltaE[stability_no==8]~U.[stability_no==8],data=data)
lm_ustar_deltaE9 <- lm(deltaE[stability_no==9]~U.[stability_no==9],data=data)
lm_ustar_deltaE10 <- lm(deltaE[stability_no==10]~U.[stability_no==10],data=data)

#for (i in 1:10){
#  summary(paste('lm_ustar_deltaE',i,sep=""))
#}

# Deleting temporary values
rm(lm_U_deltaE1,lm_U_deltaE2,lm_U_deltaE3,lm_U_deltaE4,lm_U_deltaE5,lm_U_deltaE6,
   lm_U_deltaE7,lm_U_deltaE8,lm_U_deltaE9,lm_U_deltaE10)
rm(lm_ustar_deltaE1,lm_ustar_deltaE2,lm_ustar_deltaE3,lm_ustar_deltaE4,lm_ustar_deltaE5,
   lm_ustar_deltaE6,lm_ustar_deltaE7,lm_ustar_deltaE8,lm_ustar_deltaE9,lm_ustar_deltaE10)