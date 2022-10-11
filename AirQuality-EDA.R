air_quality_datat<-read.table("AirQuality.csv",header=TRUE,sep=";",stringsAsFactors=TRUE)
attach(air_quality_datat)

dim(air_quality_datat)

# Select the 500 random samples
air_quality_datat<-air_quality_datat[sample(nrow(air_quality_datat), 500), ]
dim(air_quality_datat)

summary(air_quality_datat)

# Remove column x & x.1
air_quality_datat <- subset (air_quality_datat, select = -X)
air_quality_datat <- subset (air_quality_datat, select = -X.1)

# "Missing values are tagged with -200 value" => -200<-NA
air_quality_datat[air_quality_datat == -200]<- NA

summary(air_quality_datat)

# Vizualize missing values
library(visdat)
vis_miss(air_quality_datat)
vis_dat(air_quality_datat)
gg_miss_upset(air_quality_datat)

library(ggplot2)
library(naniar)
ggplot(air_quality_datat,aes(x=AH,y=RH))+ geom_miss_point()
ggplot(data,aes(x=Height,y=Loss))+ geom_miss_point()


detach(air_quality_datat)


