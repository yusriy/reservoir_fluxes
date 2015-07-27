bulk_moist_coef <- function(LE,ws,deltaQ) {
# E.  Calculate C_E (bulk transfer coefficient) [(s/m)2] using the bulk aerodynamic mass transfer
#     equation
#       LE = L * C_E * U (es - ea) (W/m2)
#         L = 2,540,000 J/kg = latent heat of vaporization
#         U = wind speed (m/s) 
#         e = vapor pressure (kPa) so C_EL would be a larger value
#         C_E = bulk transfer coefficient [(s/m)2] but C_E . L is dimensionless
  
# Note: C_EL here is product of C_E and L = 2,540,000 J/kg

# Note: Only include C_E when deltaQ a certain value
#  if (deltaQ > -0.0000004 || deltaQ < -0.0000011)
    C_E = LE/(ws * deltaQ)
#  else
#    C_E = NA
  
  return(C_E)
  
}