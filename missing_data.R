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

# plotting CO missing values for inferring reasons
air_quality_data[which(is.na(CO.GT.)),]
## we realize that during the month of march at 4:00 AM the reference center 
## did not  measure any data regarding CO maybe because the machines were resetting

# Compare missingness correlation

cor1<-cor(cbind(CO.GT.,PT08.S1.CO.,NMHC.GT.,C6H6.GT.,PT08.S2.NMHC., NOx.GT., PT08.S3.NOx., NO2.GT., PT08.S4.NO2., PT08.S5.O3., RH, AH, AH_bin, T),use="complete.obs")
cor2<-cor(cbind(CO.GT.,PT08.S1.CO.,NMHC.GT.,C6H6.GT.,PT08.S2.NMHC., NOx.GT., PT08.S3.NOx., NO2.GT., PT08.S4.NO2., PT08.S5.O3., RH, AH, AH_bin, T),use="pairwise.complete.obs")

cor1
cor2

corrplot(cor1)
corrplot(cor2)


# => Imputation with respect to the best correlation

## CO.GT. ~ C6H6.GT. corr= 0.9810
Imputed_CO = CO.GT.
coef = lm(CO.GT.~C6H6.GT.)$coefficients
Imputed_CO[which(is.na(Imputed_CO))]<-coef[1]+coef[2]*C6H6.GT.[which(is.na(Imputed_CO))]
air_quality_data = cbind(air_quality_data,Imputed_CO)
summary(cbind(CO.GT., Imputed_CO))
ggplot(air_quality_data,aes(x=CO.GT.,y=Imputed_CO)) + geom_miss_point()
boxplot(CO.GT., Imputed_CO, col=(c("darkgreen","blue2")), names=(c("CO.GT.", "Imputed CO.GT.")))
#
##
## NMHC.GT. ~ C6H6.GT. corr = 0.8621
Imputed_NMHC = NMHC.GT.
coef = lm(NMHC.GT.~C6H6.GT.)$coefficients
Imputed_NMHC[which(is.na(Imputed_NMHC))]<-coef[1]+coef[2]*C6H6.GT.[which(is.na(Imputed_NMHC))]
air_quality_data = cbind(air_quality_data,Imputed_NMHC)
summary(cbind(NMHC.GT., Imputed_NMHC))
ggplot(air_quality_data,aes(x=NMHC.GT.,y=Imputed_NMHC)) + geom_miss_point()
boxplot(NMHC.GT., Imputed_NMHC, col=(c("darkgreen","blue2")), names=(c("NMHC.GT.", "Imputed NMHC.GT.")))
#
##
## NOx.GT. ~ CO.GT.(OR RATHER imputed_CO because it has no NA's) corr = 0.9575
Imputed_NOx = NOx.GT.
coef = lm(NOx.GT.~Imputed_CO)$coefficients
Imputed_NOx[which(is.na(Imputed_NOx))]<-coef[1]+coef[2]*Imputed_CO[which(is.na(Imputed_NOx))]
air_quality_data = cbind(air_quality_data,Imputed_NOx)
summary(cbind(NOx.GT., Imputed_NOx))
ggplot(air_quality_data,aes(x=NOx.GT.,y=Imputed_NOx)) + geom_miss_point()
boxplot(NOx.GT., Imputed_NOx, col=(c("darkgreen","blue2")), names=(c("NOx.GT.", "Imputed NOx.GT.")))
#
#
## NO2.GT. ~ PT08.S2.NMHC. corr = 0.9003
Imputed_NO2 = NO2.GT.
coef = lm(NO2.GT.~PT08.S2.NMHC.)$coefficients
Imputed_NO2[which(is.na(Imputed_NO2))]<-coef[1]+coef[2]*PT08.S2.NMHC.[which(is.na(Imputed_NO2))]
air_quality_data = cbind(air_quality_data,Imputed_NO2)
summary(cbind(NO2.GT., Imputed_NO2))
ggplot(air_quality_data,aes(x=NO2.GT.,y=Imputed_NO2)) + geom_miss_point()
boxplot(NO2.GT., Imputed_NO2, col=(c("darkgreen","blue2")), names=(c("NO2.GT.", "Imputed NO2.GT.")))

hist(Imputed_CO, col="blue2", 
     main = paste("CO.GT. & imputed CO.GT. histogram"), xlab="CO")
hist(CO.GT., col="darkgreen", add=TRUE)

hist(Imputed_NMHC, col="blue2", 
     main = paste("NMHC.GT. & imputed NMHC.GT. histogram"), xlab="NMHC")
hist(NMHC.GT., col="darkgreen", add=TRUE)
 
hist(Imputed_NOx, col="blue2", 
     main = paste("NOx.GT. & imputed NOx.GT. histogram"), xlab="NOx")
hist(NOx.GT., col="darkgreen", add=TRUE)
 
hist(Imputed_NO2, col="blue2", 
     main = paste("NO2.GT. & imputed NO2.GT. histogram"), xlab="NO2")
hist(NO2.GT., col="darkgreen", add=TRUE)

#air_quality_data$CO.GT. = Imputed_CO
#air_quality_data$NMHC.GT. = Imputed_NMHC
#air_quality_data$NOx.GT. = Imputed_NOx
#air_quality_data$NO2.GT. = Imputed_NO2

attach(air_quality_data)

# Save the new dataset
write.table(cbind(Date, Time, Imputed_CO, PT08.S1.CO., Imputed_NMHC, C6H6.GT., 
                  PT08.S2.NMHC., Imputed_NOx, PT08.S3.NOx., Imputed_NO2, 
                  PT08.S4.NO2., PT08.S5.O3., T, RH, AH_bin), 
            file="cleanAirQuality.csv", sep = ";", 
            dec = ".", 
            row.names = TRUE, 
            col.names = TRUE)

detach(air_quality_data)

