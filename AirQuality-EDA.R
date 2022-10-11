air_quality_data<-read.table("AirQuality.csv",header=TRUE,sep=";",stringsAsFactors=TRUE)
attach(air_quality_data)

dim(air_quality_datat)

# Select the 500 random samples
air_quality_datat<-air_quality_datat[sample(nrow(air_quality_datat), 500), ]
dim(air_quality_data)

summary(air_quality_data)

# Remove column x & x.1
air_quality_datat <- subset (air_quality_data, select = -X)
air_quality_datat <- subset (air_quality_data, select = -X.1)

# "Missing values are tagged with -200 value" => -200<-NA
air_quality_datat[air_quality_datat == -200]<- NA

summary(air_quality_datat)

# Vizualize missing values
library(visdat)
vis_miss(air_quality_data)
vis_dat(air_quality_data)
gg_miss_upset(air_quality_data)

library(ggplot2)
library(naniar)
ggplot(air_quality_data,aes(x=AH,y=RH))+ geom_miss_point()



detach(air_quality_datat)


