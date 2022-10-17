# MATH2021-1 - High-dimensional statistics
# University of Liège
# Academic year 2022-2023
# Project : Exploratory data analysis
# File : ???????????????????????????????????????????????
# Authors :
#     - Merle Corentin 
#     - Jad Akkawi
library(visdat)

air_quality_data <- read.table('cleanAirQuality.csv', sep=";" , 
                               header=TRUE, stringsAsFactors=TRUE)

summary(air_quality_data)
dim(air_quality_data)

vis_miss(air_quality_data)

# Historgrams + Normal 

## CO

hist_imputed = hist(Imputed_CO, col="blue2", 
                    main = paste("CO.GT. & imputed CO.GT. histogram"), xlab="CO")
hist = hist(CO.GT., col="darkgreen", add=TRUE)

x_values_imputed <- seq(min(Imputed_CO), max(Imputed_CO), length = 100)
y_values_imputed <- dnorm(x_values_imputed, mean = mean(Imputed_CO), sd = sd(Imputed_CO)) 
y_values_imputed <- y_values_imputed * diff(hist_imputed$mids[1:2]) * length(Imputed_CO) 

lines(x_values_imputed, y_values_imputed, lwd = 3, col="green")

x_values <- seq(min(na.omit(CO.GT.)), max(na.omit(CO.GT.)), length = 100)
y_values <- dnorm(x_values, mean = mean(na.omit(CO.GT.)), sd = sd(na.omit(CO.GT.))) 
y_values <- y_values * diff(hist$mids[1:2]) * length(CO.GT.) 

lines(x_values, y_values, lwd = 3, col="blue")

hist(CO.GT., freq=FALSE)
lines(density(na.omit(CO.GT.), bw=0.8), col="red")
(my.mle<-fitdistr(na.omit(CO.GT.), densfun="poisson"))
##      lambda  
##   20.6700000 
##  ( 0.4546427)
BIC(my.mle)

## NMHC

hist_imputed = hist(Imputed_NMHC, col="blue2", 
                    main = paste("NMHC.GT. & imputed NMHC.GT. histogram"), xlab="CO")
hist = hist(NMHC.GT., col="darkgreen", add=TRUE)

x_values_imputed <- seq(min(Imputed_NMHC), max(Imputed_NMHC), length = 100)
y_values_imputed <- dnorm(x_values_imputed, mean = mean(Imputed_NMHC), sd = sd(Imputed_NMHC)) 
y_values_imputed <- y_values_imputed * diff(hist_imputed$mids[1:2]) * length(Imputed_NMHC) 

lines(x_values_imputed, y_values_imputed, lwd = 3, col="green")

x_values <- seq(min(na.omit(NMHC.GT.)), max(na.omit(NMHC.GT.)), length = 100)
y_values <- dnorm(x_values, mean = mean(na.omit(NMHC.GT.)), sd = sd(na.omit(NMHC.GT.))) 
y_values <- y_values * diff(hist$mids[1:2]) * length(NMHC.GT.) 

lines(x_values, y_values, lwd = 3, col="blue")

## NOx

hist_imputed = hist(Imputed_NOx, col="blue2", 
                    main = paste("NOx.GT. & imputed NOx.GT. histogram"), xlab="NOx")
hist = hist(NOx.GT., col="darkgreen", add=TRUE)

x_values_imputed <- seq(min(Imputed_NOx), max(Imputed_NOx), length = 100)
y_values_imputed <- dnorm(x_values_imputed, mean = mean(Imputed_NOx), sd = sd(Imputed_NOx)) 
y_values_imputed <- y_values_imputed * diff(hist_imputed$mids[1:2]) * length(Imputed_NOx) 

lines(x_values_imputed, y_values_imputed, lwd = 3, col="green")

x_values <- seq(min(na.omit(NOx.GT.)), max(na.omit(NOx.GT.)), length = 100)
y_values <- dnorm(x_values, mean = mean(na.omit(NOx.GT.)), sd = sd(na.omit(NOx.GT.))) 
y_values <- y_values * diff(hist$mids[1:2]) * length(NOx.GT.) 

lines(x_values, y_values, lwd = 3, col="blue")

## NO2

hist_imputed = hist(Imputed_NO2, col="blue2", 
                    main = paste("NO2.GT. & imputed NO2.GT. histogram"), xlab="NO2")
hist = hist(NO2.GT., col="darkgreen", add=TRUE)

x_values_imputed <- seq(min(Imputed_NO2), max(Imputed_NO2), length = 100)
y_values_imputed <- dnorm(x_values_imputed, mean = mean(Imputed_NO2), sd = sd(Imputed_NO2)) 
y_values_imputed <- y_values_imputed * diff(hist_imputed$mids[1:2]) * length(Imputed_NO2) 

lines(x_values_imputed, y_values_imputed, lwd = 3, col="green")

x_values <- seq(min(na.omit(NO2.GT.)), max(na.omit(NO2.GT.)), length = 100)
y_values <- dnorm(x_values, mean = mean(na.omit(NO2.GT.)), sd = sd(na.omit(NO2.GT.))) 
y_values <- y_values * diff(hist$mids[1:2]) * length(NO2.GT.) 

lines(x_values, y_values, lwd = 3, col="blue")

# Histograms + Poisson 

## CO

hist(CO.GT., freq=FALSE, breaks=100)
lines(density(na.omit(CO.GT.), bw=0.8), col="red")
(my.mle<-fitdistr(na.omit(CO.GT.), densfun="poisson"))
##      lambda  
##   20.6700000 
##  ( 0.4546427)
BIC(my.mle)

# 
# hist(Imputed_NMHC, col="blue2", 
#      main = paste("NMHC.GT. & imputed NMHC.GT. histogram"), xlab="NMHC")
# hist(NMHC.GT., col="darkgreen", add=TRUE)
# 
# hist(Imputed_NOx, col="blue2", 
#      main = paste("NOx.GT. & imputed NOx.GT. histogram"), xlab="NOx")
# hist(NOx.GT., col="darkgreen", add=TRUE)
# 
# hist(Imputed_NO2, col="blue2", 
#      main = paste("NO2.GT. & imputed NO2.GT. histogram"), xlab="NO2")
# hist(NO2.GT., col="darkgreen", add=TRUE)
