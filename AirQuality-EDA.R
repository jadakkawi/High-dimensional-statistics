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
summary(air_quality_data)

attach(air_quality_data)

#apply(apply(air_quality_data, 2, gsub, patt=",", replace="."), 2, as.numeric)
#gsub(",", ".",air_quality_data[str_contains(air_quality_data,",")])
#format(var1, decimal.mark = '.')
#air_quality_data[air_quality_data == '']<- NA
#air_quality_data <- replace(air_quality_data, air_quality_data=='', NA)

# Vizualize missing values
vis_miss(air_quality_data)
vis_dat(air_quality_data)
gg_miss_upset(air_quality_data)
ggplot(air_quality_data,aes(x=NMHC.GT.,y=C6H6.GT.)) + geom_miss_point() 

# -----------------------------------------

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
cor2<-cor(cbind(CO.GT.,PT08.S1.CO.,NMHC.GT.,C6H6.GT.,PT08.S2.NMHC., NOx.GT., PT08.S3.NOx., NO2.GT., PT08.S4.NO2., PT08.S5.O3., RH, AH, T),use="pairwise.complete.obs")
cor1
cor2

corrplot(cor1)
corrplot(cor2)

#------------------------------------------

#After the correlation analysis, we know that the most "favorable" variable 
#to use in order to get information on Loss is the variable Height, with a correlation of -0.43
#even though the correlation remains low.

Imputed<-NMHC.GT.
coef<-lm(NMHC.GT.~C6H6.GT.)$coefficients

#It was enought to look at the output of the function lm and directly use the 
#estimations of the slope and the intercept, instead of using the output "coefficients"

for (i in 1:length(Imputed))
{
  
  if (is.na(Imputed[i]))
  {Imputed[i]<-coef[1]+coef[2]*C6H6.GT.[i]}
}

#The loop might be replaced by
Imputed<-NMHC.GT.
Imputed[which(is.na(Imputed))]<-coef[1]+coef[2]*C6H6.GT.[which(is.na(Imputed))]
#where the use of the function which is explained by the fact that we need to identify 
#the coordinates of Height to use.

boxplot(NMHC.GT., Imputed)

summary(cbind(NMHC.GT., Imputed))

plot(NMHC.GT.,C6H6.GT.)
plot(Imputed,C6H6.GT.)

plot(PT08.S3.NOx., NOx.GT.)

plot(NMHC.GT., PT08.S4.NO2.)

plot(C6H6.GT., PT08.S1.CO.)

skewness(cbind(CO.GT.,PT08.S1.CO.,NMHC.GT.))
kurtosis(cbind(CO.GT.,PT08.S1.CO.,NMHC.GT.))

summary(air_quality_data)
detach(air_quality_data)


