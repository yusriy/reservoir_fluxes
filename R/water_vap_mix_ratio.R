water_vap_mix_ratio <- function(vap, pres) {
# Calculating water vapor mixing ratio (kg/kg) using formula:
#   q = 0.622 (e / [Patm - e])
#     e = vap = water vapor pressure at a height (Pa)
#     Patm = press = atmospheric pressure (Pa)
  q <- 0.622 * (vap / (pres - vap))
}