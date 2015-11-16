ideal_gas_eq_rho <- function (pres, temp) {
# Calculating atmospheric air density (kg/m3) using the ideal gas law density 
#     rho = P/RT, 
#     R = universal gas constant = 286.9 J/kg K 
#     T = absolute temperature (K) at 5.46 m will be converted from deg C
  rho <- pres / (286.9 * (temp))
}