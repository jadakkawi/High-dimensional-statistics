library(ggplot2)
library(naniar)
library(visdat)
library(moments)
library(corrplot)

#air_quality_data <- read.table("AirQuality.csv",header=TRUE,sep=";",stringsAsFactors=TRUE)
air_quality_data <- read.table('AirQuality.csv', dec=",", sep=";" , header=TRUE, stringsAsFactors=TRUE)
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
dim(air_quality_data)

#apply(apply(air_quality_data, 2, gsub, patt=",", replace="."), 2, as.numeric)
#gsub(",", ".",air_quality_data[str_contains(air_quality_data,",")])
#format(var1, decimal.mark = '.')
#air_quality_data[air_quality_data == '']<- NA
#air_quality_data <- replace(air_quality_data, air_quality_data=='', NA)

dim(air_quality_data)

summary(air_quality_data)

# Vizualize missing values
vis_miss(air_quality_data)
vis_dat(air_quality_data)
gg_miss_upset(air_quality_data)

step = 40
AH_x = air_quality_data$AH[seq(0, 500, by=step)]
RH_y = air_quality_data$RH[seq(0, 500, by=step)]
plt = ggplot(air_quality_data,aes(x=NMHC.GT.,y=C6H6.GT.)) + geom_miss_point() 

plt = ggplot(air_quality_data,aes(x=NMHC.GT.,y=NOx.GT.)) + geom_miss_point() 

plt + scale_x_discrete(breaks=AH_x) + scale_y_discrete(labels=seq(0, 500, by=step))
 
plt + element_text(size = 14)
                                                                     
air_quality_data$RH

plt

# -----------------------------------------

attach(air_quality_data)

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

# -----------------------------------------

cor1<-cor(cbind(CO.GT.,PT08.S1.CO.,NMHC.GT.,C6H6.GT.,PT08.S2.NMHC., NOx.GT., PT08.S3.NOx., NO2.GT., PT08.S4.NO2., PT08.S5.O3., RH, AH, T),use="complete.obs")
cor2<-cor(cbind(Length,Width,Height,Price,Loss),use="pairwise.complete.obs")
cor1
cor2

corrplot(cor1)
corrplot(cor2)

skewness(cbind(CO.GT.,PT08.S1.CO.,NMHC.GT.))
kurtosis(cbind(CO.GT.,PT08.S1.CO.,NMHC.GT.))


detach(air_quality_data)


