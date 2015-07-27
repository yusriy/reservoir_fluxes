## Function to calculate footprint max distance and area using Schmid (1994) model
# FSAM (Flux Surface Area Model)
footprint_schmid <- function(zm = 4,zo,z.L,sigV = 1,ustar = 1,
                             method = 'distance') {
  
  # Function based on Schmid (1994). Source areas for scalars and scalar fluxes.
  # Boundary-Layer Meteorology.
  # 
  # Method 'distance' is max footprint distance, xm [m]
  # Method 'area' is total footprint area, Arf [m2]
  # Method 'minX' is minimum distance of footprint, af [m]
  # Method 'maxX' is maximum distance of footprint, ef [m]
  
  if (method == 'distance'){
    # Constants for stable conditions
    alpha1a = 1.58
    alpha2a = 1.25
    alpha3a = 2.91
    alpha4a = 1.02
    alpha5a = 0
    # Constants for unstable conditions
    alpha1b = 1.72
    alpha2b = 1.24
    alpha3b = 8.65
    alpha4b = -0.746
    alpha5b = 0
  } else if (method == 'area') {
    # Constants for stable conditions
    alpha1a = 51.3
    alpha2a = 1.86
    alpha3a = 7.29
    alpha4a = 1.05
    alpha5a = 1
    # Constants for unstable conditions
    alpha1b = 31.4
    alpha2b = 1.93
    alpha3b = 17.8
    alpha4b = -0.642
    alpha5b = 1
  } else if (method == 'minX') {
    # Constants for stable conditions
    alpha1a = 3.28
    alpha2a = 1.09
    alpha3a = 3.53
    alpha4a = 1.05
    alpha5a = 0
    # Constants for unstable conditions
    alpha1b = 2.79
    alpha2b = 1.11
    alpha3b = 14.1
    alpha4b = -0.399
    alpha5b = 0
  } else if (method == 'maxX') {
    # Constants for stable conditions
    alpha1a = 10.1
    alpha2a = 1.08
    alpha3a = 3.84
    alpha4a = 1.07
    alpha5a = 0
    # Constants for unstable conditions
    alpha1b = 8.54
    alpha2b = 1.11
    alpha3b = 12.8
    alpha4b = -0.390
    alpha5b = 0
  }
  
  # To choose the different 'Dimensions'
  if (method =='distance' | method == 'minX' | method == 'maxX') {
    if (is.na(z.L) | is.na(zo)) {
      output <- NA
    } else if (z.L > 0) {
      # Equation to calculate max footprint distance for stable conditions
      output = alpha1a * ((zm/zo)^alpha2a) * exp(alpha3a * (z.L^alpha4a)) * 
        ((sigV / ustar)^alpha5a) * zo
    }
    else if (z.L < 0) {
      # Equation to calculate max footprint distance for unstable conditions
      output = alpha1b * ((zm/zo)^alpha2b) * ((1 - (alpha3b * z.L))^alpha4b) *
        ((sigV / ustar)^alpha5a) * zo
    }
  } else if (method == 'area') {
    if (is.na(z.L) | is.na(zo) | is.na(sigV) | is.na(ustar)) {
      output <- NA
    } else if (z.L > 0) {
      # Equation to calculate max footprint distance for stable conditions
      output = alpha1a * ((zm/zo)^alpha2a) * exp(alpha3a * (z.L^alpha4a)) * 
        ((sigV / ustar)^alpha5a) * zo * zo
    }
    else if (z.L < 0) {
      # Equation to calculate max footprint distance for unstable conditions
      output = alpha1b * ((zm/zo)^alpha2b) * ((1 - (alpha3b * z.L))^alpha4b) *
        ((sigV / ustar)^alpha5a) * zo * zo
    }
    
  }
  
  return(output)
  
}