#### Generating 3-D matrix for surface plots ####

# Initializing the matrix
grid_LE_zeros <- rep(0,100*100)
grid_LE <- array(grid_LE_zeros,c(100,100))
title <- 'Stable'
zL1 <- 8
zL2 <- 8

# Setting the boundaries of the grid
# x-axis
min_x <- -10
max_x <- 14.75
dx <- 25/100
# y-axis
min_y <- -0.0064
max_y <- 0.006272
dy <- 0.0128/100

# Creating the grid
gridx <- seq(min_x,max_x,by=dx)
gridy <- seq(min_y,max_y,by=dy)

for (i in 1:100) {
  for (k in 1:100) {  
      grid_LE[i,k] <- mean(data$LE[which(data$u_deltaE >= gridx[i] & data$u_deltaE < gridx[i+1] &
                                    data$C_E >= gridy[k] & data$C_E < gridy[k+1] &
                                      (data$stability_no==zL1 | data$stability_no==zL2))],na.rm=TRUE)   
    }   
}

# Replace all NaNs with 0
grid_LE[is.nan(grid_LE)] = 0

# Plot contour plots (filled)
filled.contour(gridx,gridy,grid_LE,color.palette=topo.colors,main=title,
               zlim=c(-150,450),key.title=title(main='LE'))
# Note to plot 3-D surface plot use function: persp {graphics}

#Write out data to Matrix into default Matlab folder
#write.table(grid_LE,"/Users/Yusri/Documents/MATLAB/grid_LE.csv",sep=',')

# Deleting temporary variables
rm(grid_LE_zeros,grid_LE,min_x,max_x,dx,min_y,max_y,dy,i,k,gridx,gridy)
