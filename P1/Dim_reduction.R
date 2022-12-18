rm(list=ls())
library(visdat)
library(corrplot)

air_quality_data <- read.table('cleanAirQuality.csv', sep=";" , 
                               header=TRUE, stringsAsFactors=TRUE)

air_quality_data<-subset (air_quality_data, select = -AH_bin)
air_quality_data<-subset (air_quality_data, select = -Date)
air_quality_data<-subset (air_quality_data, select = -Time)
pairs(air_quality_data)
summary(air_quality_data)
dim(air_quality_data)
vis_miss(air_quality_data)


C <- cor(air_quality_data)
corrplot(C, is.corr=TRUE)

xbar<-apply(air_quality_data, 2, mean)
std<-apply(air_quality_data,2, sd)
pairs(air_quality_data)


# Trace: sum of diagonal values
TotCorr<-sum(diag(C))
TotCorr

resPCA<-eigen(C)
resPCA 

#Screeplot
plot(resPCA$values,type="b")

cbind(resPCA$values,resPCA$values/TotCorr,cumsum(resPCA$values/TotCorr))
j<-as.matrix(air_quality_data-matrix(xbar,nrow=dim(air_quality_data)[1],
                                     ncol=dim(air_quality_data)[2],byrow=TRUE))

y<-j %*% resPCA$vectors
plot(y[,1],y[,2])

k<-matrix(std, nrow=dim(air_quality_data)[1], ncol=dim(air_quality_data)[2],byrow=TRUE)

cor(as.matrix(j/k), y)

corrplot(cor(air_quality_data, as.matrix(air_quality_data) %*% resPCA$vectors))
corrplot(cor(air_quality_data, y))
corrplot(cor(j, y))
corrplot(cor(as.matrix(j/k), y))


library(ade4)
#Selection of the two PC, e.g. PC 1 and PC2: 
rescor<-cor(as.matrix(j/k), y)[,1:2]
s.corcircle(rescor)




plot(C6H6.GT., PT08.S2.NMHC., xlab= "C6H6.GT.", ylab="PT08.S2.NMHC.")


