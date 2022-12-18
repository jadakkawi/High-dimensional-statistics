air_quality_data <- read.table('preProcessAirQuality.csv', sep=";" , 
                               header=TRUE, stringsAsFactors=TRUE)
attach(air_quality_data)

dim(air_quality_data)
summary(air_quality_data)


#apply(apply(air_quality_data, 2, gsub, patt=",", replace="."), 2, as.numeric)
#gsub(",", ".",air_quality_data[str_contains(air_quality_data,",")])
#format(var1, decimal.mark = '.')
#air_quality_data[air_quality_data == '']<- NA
#air_quality_data <- replace(air_quality_data, air_quality_data=='', NA)

 

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


