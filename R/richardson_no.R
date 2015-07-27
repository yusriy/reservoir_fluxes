richardson_no <- function (T1,T2,z1,z2,U1,U2) {
# Function to calculate bulk Richardson number
# Ri = [(g / T2) * (T2 - T1) * (z2 - z1)] / [(U2 - U1)^2]
#   Ri = Richardson number (dimensionless)
#   g = gravity acceleration = 9.81 m/s2
#   T1 = temperature at height 1 (deg C)
#   T2 = temperature at height 2 (deg C)
#   z1 = height 1 (m)
#   z2 = height 2 (m)
#   U1 = wind speed at height 1 (m/s)
#   U2 = wind speed at height 2 (m/s)
  
  g = 9.81
  Ri <- ((g / T2) * (T2 - T1) * (z2 - z1)) / ((U2 - U1)^2)
  
  return(Ri)
  
}