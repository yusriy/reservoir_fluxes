C_E_calc <- function(height,roughness_length,z_L) {
# Estimate CE bulk transfer coefficient of heat/moisture 
# using similarity (Monin-Obukhov) equations and Businger-Dyer relations
# CE = k^2[ln(z/z0)-f_M(z/L)]^-1[ln(z/z0)-f_H(z/L)]^-1
# k = 0.35 (Businger-Dyer), but we used k = 0.4 here
# z0 = roughness length, calculated using Charnock's equation (1955)
# z/L = dimensionless stability parameter
# f_M or f_H = stability correction factor
# k_H/k_M = 0.74 (Businger et al., 1971)
  
  # For stable conditions
  if (!is.na(z_L)){
    if (z_L >= 0) {
      # Businger et al., (1971)
      wind_shear = 4.7 * z_L
      heat_shear = 4.7 * z_L
      C_E <- (0.16) * (log(height/roughness_length) + wind_shear)^-1 * (log(height/roughness_length) * 0.74 + heat_shear)^-1
    }
    else if (z_L < 0){
      # Businger et al., (1971)
      x_wind_shear = (1 - (9 * z_L))^0.25
      x_heat_shear = (1 - (16 * z_L))^0.50
      wind_shear = -2 * log((1 + x_wind_shear)/2) - log((1 + x_wind_shear^2)/2) + 
        2 * atan(x_wind_shear) - pi/2
      heat_shear = -2 * log((1 + x_heat_shear)/2) - log((1 + x_heat_shear^2)/2) + 
        2 * atan(x_heat_shear) - pi/2
      C_E <- (0.16) * (log(height/roughness_length) + wind_shear)^-1 * (log(height/roughness_length) * 0.74 + heat_shear)^-1
    }
  }
  else {
    return(NA)
  }  
}