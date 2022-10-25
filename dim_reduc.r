# MATH2021-1 - High-dimensional statistics
# University of Liège
# Academic year 2022-2023
# Project : Exploratory data analysis
# File : dim_reduc.r
# Authors :
#     - Merle Corentin 
#     - Jad Akkawi
library(corrplot)

air_quality_data = read.table('cleanAirQuality.csv', sep=";", 
                              header=TRUE, stringsAsFactors=TRUE)

air_quality_data = subset (air_quality_data, select = c(-AH_bin, -Date, -Time))

attach(air_quality_data)

summary(air_quality_data)

C <- cor(air_quality_data)
corrplot(C, is.corr=TRUE)
pairs(air_quality_data)

xbar = apply(air_quality_data, 2, mean)
std <- var(air_quality_data)

# Trace: sum of diagonal values
TotStd<-sum(diag(std))
TotStd

resPCA<-eigen(C)
resPCA 

cbind(resPCA$values,resPCA$values/TotStd,cumsum(resPCA$values/TotStd))
j<-as.matrix(air_quality_data-matrix(xbar,nrow=dim(air_quality_data)[1],
                                     ncol=dim(air_quality_data)[2],byrow=TRUE))


plot(resPCA$values,type="b")


y<-as.matrix(air_quality_data-matrix(xbar,nrow=dim(air_quality_data)[1],
                                     ncol=11,byrow=TRUE))

y<-y %*% resPCA$vectors
plot(y[,1],y[,2])

res <- princomp(air_quality_data)
summary(res)
res$loadings

k<-matrix(std, nrow=dim(air_quality_data)[1], ncol=dim(air_quality_data)[2],byrow=TRUE)

cor(as.matrix(j/k), y)

# corrplot(cor(air_quality_data, as.matrix(air_quality_data) %*% resPCA$vectors))
# corrplot(cor(air_quality_data, y))
# corrplot(cor(j, y))
# corrplot(cor(as.matrix(j/k), y))


library(ade4)
#Selection of the two PC, e.g. PC 1 and PC2: 
rescor<-cor(as.matrix(j/k), y)[,1:2]
s.corcircle(rescor)


