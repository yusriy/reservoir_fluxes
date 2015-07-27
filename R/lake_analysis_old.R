# Remove row.names
row.names(old_data)<-NULL
# Install package "stringr" and load library
#install.packages('stringr')
library(stringr)
# Split Variable into 2 columns
date_time <- str_split_fixed(old_data$Variable, " ", 2)
# Combine data_time and data
old_data <- cbind(date_time,old_data)
# Remove "Variable" column
old_data$Variable = NULL
# Rename column to "date" and "time"
colnames(old_data)[1]<-"date"
colnames(old_data)[2]<-"time"
# Delete "date_time" variable
rm(date_time)
# Convert from factor to numeric using the function convert_magic.R
source('convert_magic.R')
source('convert_magic_num.R')
# Convert to character first
data2 <- convert_magic(old_data,c('character','character','character','character','character','character','character','character','character','character','character','character','character','character','character','character','character','character','character','character','character','character','character','character','character','character','character','character','character'))
# Convert to numeric second
data3 <- convert_magic_num(data2,c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))
old_data <- data3
# Remove temporary 'data'
rm(data2,data3)

# Changing all the '-999' (missing data) to NA
for (i in 1:length(old_data)){
  old_data[i][old_data[i] == -999] <- NA
}
# Remove all 'NA's from dataframe
data2 <- old_data[complete.cases(old_data),]
old_data <- data2
rm(data2,i)