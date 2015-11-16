pot_temp <- function (temp,press) {
# Function to calculate potential temperature
# T_potential = T * (Patm / P)^(2/7)
#   T = temperature (deg C)
#   P = pressure (Pa)
  Patm = 101325
  pot_temp1 <- temp * (Patm / press)^(2/7)
}