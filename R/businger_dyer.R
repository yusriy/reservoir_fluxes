# Create a profile 

z_L <- seq(-10,10,1)

trident_func_m <- 0

# Dimensionless wind shear
for (i in 1:length(z_L)){
  if (z_L[i] < 0){
    # Function for unstable conditions, z_L < 0
    x <- (1 - (15 * z_L[i]))^0.25
    trident_func_m[i] <- (-2 * log((1+x)/2)) - log((1+x^2)/2) + (2*atan(x)) - (pi/2)
  } else if (z_L[i] == 0){
    trident_func_m[i] <- 0
  }
  else if (z_L[i] > 0){
    # Function for stable conditions, z_L > 0
    trident_func_m[i] <- (4.7 * z_L[i])
  }
}
plot(z_L,trident_func_m,type = 'l',lwd=2, col = 'blue', xlim = c(-10,10), 
     ylim = c(-3,8), ylab = 'trident function m and H or E')

# Dimensionless temperature gradient
trident_func_h <- 0
for (i in 1:length(z_L)){
  if (z_L[i] < 0){
    # Function for unstable conditions, z_L < 0
    x <- 0.74 * (1 - (9 * z_L[i]))^0.25
    trident_func_h[i] <- (-2 * log((1+x)/2)) - log((1+x^2)/2) + (2*atan(x)) - (pi/2)
  } else if (z_L[i] == 0){
    trident_func_h[i] <- 0
  }
  else if (z_L[i] > 0){
    # Function for stable conditions, z_L > 0
    trident_func_h[i] <- (4.7 * z_L[i])
  }
}
lines(z_L,trident_func_h,type = 'l',lwd=2, col = 'red')


# Drag coefficient, CD
#z0 <- 0.001 # [m] = 1 [mm]
ustar <- 0.16 # Friction velocity [m s-1]
z0 <- 0.015 * ustar^2 / 9.81
k <- 0.35 # von Karman constant
z = 4 # [m]; measurment height
CD <- k^2 / ((log(z/z0)) + trident_func_m)^2
CH <- k^2 / (log(z/z0) + trident_func_m) / (log(z/z0) + trident_func_h)

plot(z_L,CD, type = 'l', lwd = 2, col = 'blue', ylim =c(0,0.002), ylab = 'C_D and C_H or E')
lines(z_L,CH, lwd = 2, col = 'red')
