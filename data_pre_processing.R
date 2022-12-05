# MATH2021-1 - High-dimensional statistics
# University of Liège
# Academic year 2022-2023
# Project : Exploratory data analysis
# File : data_pre_processing.R
# Authors :
#     - Merle Corentin 
#     - Jad Akkawi

air_quality_data = read.table('AirQuality.csv', dec=",", sep=";", 
                              header=TRUE, stringsAsFactors=TRUE)
attach(air_quality_data)

dim(air_quality_data)

# # Select the 500 random samples
# air_quality_data<-air_quality_data[sample(nrow(air_quality_data), 500), ]

# Select the 500 first samples
air_quality_data = air_quality_data[0:500,]

dim(air_quality_data)

# Summary 
summary(air_quality_data)

# We can notice that : 
# - there are two useless columns X & X.1 which can be deleted
# - missing value are labeled -200 (as said in the data description) and we must 
#   replace -200 by NA in order to use library that handle missing values.

# Remove column x & x.1
air_quality_data <- subset (air_quality_data, select = -X)
air_quality_data <- subset (air_quality_data, select = -X.1)

# Change missing values (-200 -> NA)
air_quality_data[air_quality_data == -200]<- NA

dim(air_quality_data)
summary(air_quality_data)

# Good! 

# Generate the binary attribute from the quantitative attribute AH
threshold = 0.8
air_quality_data$AH_bin[air_quality_data$AH < threshold] = 0
air_quality_data$AH_bin[air_quality_data$AH >= threshold] = 1



attach(air_quality_data)

# Save the new dataset
write.table(air_quality_data, file="preProcessAirQuality.csv", sep = ";", 
            dec = ".", row.names = TRUE, col.names = TRUE)

hist(CO.GT.)
hist(PT08.S1.CO.)
hist(NMHC.GT.)
hist(C6H6.GT.)
hist(PT08.S2.NMHC.)
hist(NOx.GT.)
hist(PT08.S3.NOx.)
hist(NO2.GT.)
hist(PT08.S4.NO2.)
hist(PT08.S5.O3.)
hist(T)
hist(RH)
hist(AH)
barplot(table(AH_bin))

detach(air_quality_data)

