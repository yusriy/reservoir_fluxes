## EC data filtration
# Read from EC processed data using Foken (2004) chapter
# Skip to line 12 to only include data from 2007-08-24 17:45 (or 17:30)
temp <-read.csv('data/Reservoir_data_08242007-02162008.csv')
header <- names(temp)
dataEC <- read.csv('data/Reservoir_data_08242007-02162008.csv',skip=36)
dataEC <- dataEC[-c(8449:nrow(dataEC)),]
names(dataEC) <- header
rm(temp,header)

# Sigma W
dataEC$W_std[which(dataEC$W_std < -2000 | dataEC$QCFlag_t > 6)] <- NA
# Friction velocity
dataEC$U_star[which(dataEC$QCFlag_t > 6 | dataEC$U_star < -2000)] <- NA

# Surface roughness
# Charnock's relation (1955)
z0 <- 0.015 * dataEC$U_star^2 / 9.81
dataEC <- cbind(dataEC,z0)

dataEC$z0[which(dataEC$z0 > 0.0008)] <- NA

rm(z0)

