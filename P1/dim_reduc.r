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

AH_bin = subset (air_quality_data, select = c(AH_bin))
date_filter = subset (air_quality_data, select = c(Date))
air_quality_data = subset (air_quality_data, select = c(-AH_bin, -Date, -Time))
air_quality_data = standardizeDF(air_quality_data)

summary(air_quality_data)

C <- cor(air_quality_data)
S <- var(air_quality_data)
corrplot(S, is.corr=FALSE)

# Save
# dev.copy(pdf, paste("Plots/corrplot_DR.pdf", sep=""), 
#         width=20, height=11) 
# dev.off()

pairs(air_quality_data)

# Save
# dev.copy(pdf, paste("Plots/pairs.pdf", sep=""), 
#          width=20, height=11) 
# dev.off()

# Trace: sum of diagonal values
TotS<-sum(diag(S))
TotS

resPCA<-eigen(S)
resPCA 

cbind(resPCA$values,resPCA$values/TotS,cumsum(resPCA$values/TotS))

plot(resPCA$values,type="b")

# Save
# dev.copy(pdf, paste("Plots/plot(resPCA$values).pdf", sep=""), 
#          width=20, height=11) 
# dev.off()
################ air_quality_data = air_quality_data which(date in 7 to 12 and 17 to 21)
y<-as.matrix(air_quality_data) %*% resPCA$vectors
plot(y[,1],y[,2], col=as.matrix(AH_bin)+3, pch = 18)
legend("topleft", legend = c("Low Humidity ", "High Humidity"), col = 3:4, pch = 18, bty = "n")

# identify(y[,1],y[,2]) 

# Save
# dev.copy(pdf, paste("Plots/plot(y[,1],y[,2]).pdf", sep=""), 
#          width=20, height=11) 
# dev.off()

res <- princomp(air_quality_data)
summary(res)
res$loadings

library(ade4)
#Selection of the two PC, e.g. PC 1 and PC2: 
rescor<-cor(as.matrix(air_quality_data), y)[,1:2]
s.corcircle(rescor)

# Save
# dev.copy(pdf, paste("Plots/s.corcircle(rescor).pdf", sep=""), 
#          width=20, height=11) 
# dev.off()

