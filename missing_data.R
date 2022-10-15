# MATH2021-1 - High-dimensional statistics
# University of Liège
# Academic year 2022-2023
# Project : Exploratory data analysis
# File : missing_data.R
# Authors :
#     - Merle Corentin 
#     - Jad Akkawi
library(visdat)
library(corrplot)

air_quality_data <- read.table('preProcessAirQuality.csv', sep=";" , 
                               header=TRUE, stringsAsFactors=TRUE)
attach(air_quality_data)

vis_miss(air_quality_data)
vis_dat(air_quality_data)
gg_miss_upset(air_quality_data)

ggplot(air_quality_data,aes(x=NMHC.GT.,y=C6H6.GT.)) + geom_miss_point() 

# Compare missingness correlation

cor1<-cor(cbind(CO.GT.,PT08.S1.CO.,NMHC.GT.,C6H6.GT.,PT08.S2.NMHC., NOx.GT., PT08.S3.NOx., NO2.GT., PT08.S4.NO2., PT08.S5.O3., RH, AH, T),use="complete.obs")
cor2<-cor(cbind(CO.GT.,PT08.S1.CO.,NMHC.GT.,C6H6.GT.,PT08.S2.NMHC., NOx.GT., PT08.S3.NOx., NO2.GT., PT08.S4.NO2., PT08.S5.O3., RH, AH, T),use="pairwise.complete.obs")

cor1
cor2

corrplot(cor1)
corrplot(cor2)

# There are missing values in CO.GT., NMHC.GT., NOx.GT. and NO2.GT.
# => Imputation with respect to the best correlation

## CO.GT. ~ C6H6.GT.

Imputed = CO.GT.
coef = lm(CO.GT.~C6H6.GT.)$coefficients

#It was enought to look at the output of the function lm and directly use the 
#estimations of the slope and the intercept, instead of using the output "coefficients"

for (i in 1:length(Imputed))
{
  
  if (is.na(Imputed[i]))
  {Imputed[i]<-coef[1]+coef[2]*C6H6.GT.[i]}
}

#The loop might be replaced by
Imputed<-CO.GT.
Imputed[which(is.na(Imputed))]<-coef[1]+coef[2]*C6H6.GT.[which(is.na(Imputed))]
#where the use of the function which is explained by the fact that we need to identify 
#the coordinates of Height to use.

boxplot(CO.GT., Imputed)

summary(cbind(CO.GT., Imputed))

## NMHC.GT. ~ ???

## NOx.GT. ~ ???

## NO2.GT. ~ ???



