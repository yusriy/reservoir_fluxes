wat_vap_diff <- function (z,qs,qa,density,LE) {
# Calculating K (m2/s) water vapor turbulent diffusivity
# at a certain height using Henderson-Sellers (1986) equation.
#   LE = K L (rho) (dq/dz)
#     L = 2,540,000 J/kg = temperature-dependent latent heat of vaporization of fresh water (lake)
#     rho = air density (kg/m3)
#     dq = qs - qa = vapor pressure difference (kg/kg)
#     dz = za - zs = height of measurement above water (m)
# This is a 'gradient' instead of 'bulk' method.
  
  # Calculating U (qs - qa)
  L = 2540000
  deltaP_q <- qs - qa
  
  # Note: For K only, only include qs - qa > 0.0005 and qs - qa < -0.0005 to remove too
  # high K values. 
  
  if (deltaP_q < 0.0005 || deltaP_q > -0.0005)
    K <- (LE * z) / (deltaP_q * density * L)
  else
    K <- NA
  
  return(K)
}
