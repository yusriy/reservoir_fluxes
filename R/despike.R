##### SUBSETTING INTO DAYS ################
# To create a subset of data based on day
# Used to create daily time series data for bad data determination
day_2008_02_24 <- subset(data,data$time_stamp >= as.POSIXct('2007-12-20 17:30:00', format="%Y-%m-%d %H:%M:%S") & 
                data$time_stamp < as.POSIXct('2007-12-21 17:30:00', format="%Y-%m-%d %H:%M:%S"))

# To find the minimum or maximum of a certain variable such as z/L (Z.L) in the subset
index_no <- rownames(day_2008_02_24)[which.max(day_2008_02_24$Z.L)]

