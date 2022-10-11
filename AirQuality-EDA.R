library(ggplot2)
library(naniar)
library(visdat)

air_quality_data<-read.table("AirQuality.csv",header=TRUE,sep=";",stringsAsFactors=TRUE)
attach(air_quality_data)

dim(air_quality_data)

# Select the 500 random samples
air_quality_data<-air_quality_data[sample(nrow(air_quality_data), 500), ]
dim(air_quality_data)

summary(air_quality_data)

# Remove column x & x.1
air_quality_data <- subset (air_quality_data, select = -X)
air_quality_data <- subset (air_quality_data, select = -X.1)

# "Missing values are tagged with -200 value" => -200<-NA
air_quality_data[air_quality_data == -200]<- NA

summary(air_quality_data)

# Vizualize missing values
vis_miss(air_quality_data)
vis_dat(air_quality_data)
gg_miss_upset(air_quality_data)

step = 40
AH_x = air_quality_data$AH[seq(0, 500, by=step)]
RH_y = air_quality_data$RH[seq(0, 500, by=step)]
plt = ggplot(air_quality_data,aes(x=AH,y=RH), angle=45) + geom_miss_point() 

plt + scale_x_discrete(breaks=AH_x) + scale_y_discrete(labels=seq(0, 500, by=step))
 
plt + element_text(size = 14)
                                                                     
air_quality_data$RH


detach(air_quality_data)


