roughness_length <- function (height,wind_speed,ustar){
# To calculate roughness length over land with no stability correction
# in neutral conditions
  z0 <- height/exp(wind_speed*0.4/ustar)
  return(z0)
}