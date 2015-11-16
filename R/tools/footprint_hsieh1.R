footprint_hsieh1 <- function (ustar,H,Ta,zm,zo,perc) {
  # Flux contribution
  flux_footprint = perc # 80% flux footprint
  
  # Constants
  k = 0.4 # von Karman constant [dimensionless]
  Cp = 1005 # Specific heat capacity of dry air at constant pressure [J kg-1 K-1]
  g = 9.81 # Gravitational constant [m s-2]
  Ta_K = Ta + 273.15 # Convert from deg C to K [K]
  rho = 1.3079 - 0.0045 * Ta # Density of air [kg m-3]
  eps = 0 # Not sure what this is
  
  
  # Calculate Obukhov length [m]
  L = ((-rho) * (ustar + eps)^3) / ((k * g * (H + eps)) / (Cp * Ta_K))
  
  # Equations (15) to (17) from Hsieh et al. (2000)
  zu = zm * (log(zm/zo) - 1 + (zo/zm))
  stab = zu / L
  
  # To determine model constants based on stability
  thresh = 0.04
  if (is.na(stab)) {
    P_set = NA
    D_set = NA
    Mu_set = NA
  } 
  else if (stab < (-thresh)) { # Unstable
    P_set = 0.59
    D_set = 0.28
    Mu_set = 100
  } 
  else if (abs(stab) < thresh) { # Near-neutral
    P_set = 1.00
    D_set = 0.97
    Mu_set = 500
  } 
  else if (stab > thresh) { # Stable
    P_set = 1.33
    D_set = 2.44
    Mu_set = 2000
  } 
  
  # Model constants
  #P <- c(0.59,1.00,1.33)
  #D <- c(0.28,0.97,2.44)
  #Mu <- c(100,500,2000) # To determine distance of plot
  
  #D_set = D[i]
  #P_set = P[i]
  #Mu_set = Mu[i] # Would not be used unless plotting the curve
  
  # Setting the distance for plot, would not be used
  # min_x = (Mu_set/100) * zo
  # max_x = Mu_set * zm
  # x_bin = (max_x - min_x) / 1000 # 1000 points
  
  # if (is.na(Mu_set)) x = NA
  # else x = seq(from = min_x, to = max_x, by = x_bin)
  
  # c1 = (-1/(k*k) * (D_set * zu^P_set * abs(L)^(1 - P_set))) / x
  # Fc = exp(c1)
  # xp = (1 / (2 * k * k)) * (D_set * zu^P_set * abs(L)^(1 - P_set)) # Peak distance
  # Fetch to height at 90% flux contribution, must multiply with measurement
  # height to get actual distance [m]
  F2H = (D_set / (abs(log(flux_footprint)) * k * k)) * (zm^(-1) * abs(L)^(1 - P_set) * zu^P_set)
  
  return(F2H)

}