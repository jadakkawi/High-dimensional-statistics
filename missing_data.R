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
