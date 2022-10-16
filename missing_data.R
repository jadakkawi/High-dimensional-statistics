# MATH2021-1 - High-dimensional statistics
# University of Liège
# Academic year 2022-2023
# Project : Exploratory data analysis
# File : missing_data.R
# Authors :
#     - Merle Corentin 
#     - Jad Akkawi

air_quality_data <- read.table('preProcessAirQuality.csv', sep=";" , 
                               header=TRUE, stringsAsFactors=TRUE)
attach(air_quality_data)

vis_miss(air_quality_data)
vis_dat(air_quality_data)
gg_miss_upset(air_quality_data)

ggplot(air_quality_data,aes(x=NMHC.GT.,y=C6H6.GT.)) + geom_miss_point() 

## We can see from the plot that most missing values are "non metanic hydrocarbure" 103/133 cases 
## NOx and NO2  are missing together 22/133 times
## CO is missing 18/133 times

## if we plot the observations where NO2 and NOX are abscent 
air_quality_data[which(is.na(NO2.GT.)),]
## we realize that during the month of march at 3:00 AM the reference center 
## did not  measure any data regarding NO2 and NOx maybe because the machines were resetting

# plotting NMHC missing values for inferring reasons
air_quality_data[which(is.na(NMHC.GT.)),]
## we realize that from 18th till 20th of Mars these elements weren't measured
