# Footprint (m) calculations using Schmid (1994) FSA model
d1 <- numeric()
z.L <- seq(0.1,10,by=0.1)
for (i in 1:length(z.L)) {
  d1[i] <- footprint_schmid(zm=4,zo=0.001,z.L[i],method='minX')
  # 4 [m] is height of EC system 
}

d2 <- numeric()
z.L <- seq(0.1,10,by=0.1)
for (i in 1:length(z.L)) {
  d2[i] <- footprint_schmid(zm=4,zo=0.001,z.L[i],method='distance')
  # 4 [m] is height of EC system 
}

d3 <- numeric()
z.L <- seq(0.1,10,by=0.1)
for (i in 1:length(z.L)) {
  d3[i] <- footprint_schmid(zm=4,zo=0.001,z.L[i],method='maxX')
  # 4 [m] is height of EC system 
}

area <- numeric()
z.L <- seq(0.1,10,by=0.1)
for (i in 1:length(z.L)) {
  area[i] <- footprint_schmid(zm=4,zo=0.001,z.L[i],method='area')
  # 4 [m] is height of EC system 
}

plot(z.L,d1,col='black',type='l',ylim=c(0,2000),xlim=c(0,2))
lines(z.L,d2,col='blue')
lines(z.L,d3, col='red')