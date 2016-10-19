#### A script to import more detailed EC data from Zhongming ####

# Read data from original csv file
data_EC <- read.csv(file.choose())

# Remove the first four rows because they are empty rows or non-data rows
data_EC <- data_EC[c(-1),]

# Rename column to "date" and "time"
colnames(data_EC)[1] <- 'time_stamp'
# Paste date and time into one column
time_stamp <- paste(data_EC$time_stamp,data_EC$time)
# Convert from factor to numeric using the function 'convert_magic.R'
# Convert data to character first
data_EC <- convert_magic(data_EC,c(rep('character',times = 46)))
# Then, convert to numeric
data_EC <- convert_magic_num(data_EC,c('character',rep('numeric',times = 46)))
# Convert "date" from character to date
time_stamp <-strptime(time_stamp,"%Y%m%d %H:%M")
# Combine time_stamp into dataframe
data_EC <- cbind(time_stamp, data_EC)
# Remove 2nd and 3rd column of data
data_EC <- data_EC[,c(-2,-3)]
# Changing all the '-999' (missing data) to NA
for (i in 1:length(data_EC)){
  data_EC[i][data_EC[i] == -10000.000 | data_EC[i] == -9999.0 | 
               data_EC[i] == -9999] <- NA
}
rm(i,time_stamp)

# Adding atmospheric stability categories
stability_no <- 0
stability <- 0
stability_no <- sapply(data_EC$zL,unst_stab_category)
stability <- sapply(data_EC$zL,unst_stab_category2)

data_EC <- cbind(data_EC, stability_no, stability)

plot_w_std <- ggplot(na.omit(data_EC[,c('W_std','stability_no')]), 
                     aes(factor(stability_no),W_std)) + 
  geom_boxplot(outlier.size=0,fill="white") + 
  coord_cartesian(ylim = c(0, 1))

plot_u_std <- ggplot(na.omit(data_EC[, c('U_std', 'stability_no')]), 
                     aes(factor(stability_no), U_std)) +
  geom_boxplot(outlier.size = 0, fill = 'white') +
  coord_cartesian(ylim = c(0,1))
  
