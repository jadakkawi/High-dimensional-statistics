# MATH2021-1 - High-dimensional statistics
# University of Liège
# Academic year 2022-2023
# Project : Exploratory data analysis
# File : dim_reduc.r
# Authors :
#     - Merle Corentin 
#     - Jad Akkawi

library(corrplot)

################################################################################

# Standardize dataframe function

standardizeDF <- function(x) {
  
  new_columns = c()
  
  for (name in names(x)) {
    new_col = (x[name]-mean(as.matrix(x[name])))/sd(as.matrix(x[name]))
    new_columns = append(new_columns, new_col, after=length(new_columns))
  }
  
  return(data.frame(new_columns))
}

################################################################################

air_quality_data = read.table('cleanAirQuality.csv', sep=";", 
                              header=TRUE, stringsAsFactors=TRUE)

air_quality_data = subset (air_quality_data, select = c(-AH_bin, -Date, -Time))

air_quality_data = standardizeDF(air_quality_data)

attach(air_quality_data)

summary(air_quality_data)

C <- cor(air_quality_data)
S <- var(air_quality_data)
corrplot(S, is.corr=FALSE)
pairs(air_quality_data)

# Trace: sum of diagonal values
TotS<-sum(diag(S))
TotS

resPCA<-eigen(S)
resPCA 

cbind(resPCA$values,resPCA$values/TotS,cumsum(resPCA$values/TotS))

plot(resPCA$values,type="b")

y<-as.matrix(air_quality_data) %*% resPCA$vectors
plot(y[,1],y[,2])

res <- princomp(air_quality_data)
summary(res)
res$loadings

k<-matrix(std, nrow=dim(air_quality_data)[1], ncol=dim(air_quality_data)[2],byrow=TRUE)

cor(as.matrix(j), y)

# corrplot(cor(air_quality_data, as.matrix(air_quality_data) %*% resPCA$vectors))
# corrplot(cor(air_quality_data, y))
# corrplot(cor(j, y))
# corrplot(cor(as.matrix(j/k), y))


library(ade4)
#Selection of the two PC, e.g. PC 1 and PC2: 
rescor<-cor(as.matrix(j), y)[,1:2]
s.corcircle(rescor)