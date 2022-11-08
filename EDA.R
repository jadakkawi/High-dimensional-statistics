# MATH2021-1 - High-dimensional statistics
# University of Liège
# Academic year 2022-2023
# Project : Exploratory data analysis
# File : EDA.R
# Authors :
#     - Merle Corentin 
#     - Jad Akkawi

library(visdat) 
library(MASS)
library(robustbase)
library(stargazer)

# Plot (histogram + distribution) functions

plotAndSaveHistNorm <- function(x, name=name) {
  
  x = as.numeric(unlist(x))
  
  hist = hist(x, col="darkgreen", freq=FALSE, breaks=20,
              main = paste("Histogram of", name), xlab=name)
  x_values <- seq(min(na.omit(x)), max(na.omit(x)), length = 100)
  y_values <- dnorm(x_values, mean = mean(na.omit(x)), sd = sd(na.omit(x))) 
  #y_values <- y_values * diff(hist$mids[1:2]) * length(x) 
  
  lines(x_values, y_values, lwd = 3, col="green")
  
  dev.copy(pdf, paste("Plots/hist_normal_",name,".pdf", sep=""), 
           width=20, height=11)
  dev.off()
  
  print(paste(name, " : MLE = ", BIC(fitdistr(na.omit(x), densfun="normal"))))
}

plotAndSaveHistPoisson <- function(x, name=name) {
  
  x = as.numeric(unlist(x))
  
  hist = hist(x, col="darkgreen", freq=FALSE, breaks=20,
              main = paste("Histogram of", name), xlab=name)
  x_values <- seq(floor(min(na.omit(x))), ceiling(max(na.omit(x))))
  
  estimate_param = fitdistr(na.omit(x), densfun="poisson")
  print(paste(name, " : MLE = ", BIC(estimate_param)))
  estimate_param = unlist(estimate_param)
  
  y_values <- dpois(x_values, 
                    lambda=estimate_param["estimate.lambda"]) 
  
  lines(x_values, y_values, lwd = 3, col="green")
  
  dev.copy(pdf, paste("Plots/hist_poisson_", name, ".pdf", sep=""), 
           width=20, height=11) 
  dev.off()
}

plotAndSaveHistLogNormal <- function(x, name=name) {
  
  x = as.numeric(unlist(x))
  
  hist = hist(x, col="darkgreen", freq=FALSE, breaks=20,
              main = paste("Histogram of", name), xlab=name)
  x_values <- seq(min(na.omit(x)), max(na.omit(x)), length = 100)
  
  estimate_param = fitdistr(na.omit(x), densfun="lognormal")
  print(paste(name, " : MLE = ", BIC(estimate_param)))
  estimate_param = unlist(estimate_param)
  print(paste("mean :", estimate_param["estimate.meanlog"]))
  print(paste("sd :", estimate_param["estimate.sdlog"]))
  
  y_values <- dlnorm(x_values, 
                     meanlog = estimate_param["estimate.meanlog"],
                     sdlog = estimate_param["estimate.sdlog"], 
                     log = FALSE) 
  
  lines(x_values, y_values, lwd = 3, col="green")
  
  dev.copy(pdf, paste("Plots/hist_logNormal_", name, ".pdf", sep=""), 
           width=20, height=11) 
  dev.off()
}

################################################################################

air_quality_data = read.table('cleanAirQuality.csv', sep=";" , 
                              header=TRUE, stringsAsFactors=TRUE)

attach(air_quality_data)

summary(air_quality_data)
stargazer(air_quality_data)
dim(air_quality_data)

# Boxplots

boxplot(cbind(air_quality_data["CO.GT."]), names=c("CO.GT."),
        col="darkgreen")
dev.copy(pdf, paste("Plots/boxplot_CO.GT.pdf", sep=""),
         width=20, height=11) 
dev.off()

boxplot(cbind(air_quality_data["NMHC.GT."]), names=c("NMHC.GT."),
        col="darkgreen")
dev.copy(pdf, paste("Plots/boxplot_NMHC.GT.pdf", sep=""),
         width=20, height=11) 
dev.off()

boxplot(cbind(air_quality_data["C6H6.GT."]), names=c("C6H6.GT."),
        col="darkgreen")
dev.copy(pdf, paste("Plots/boxplot_C6H6.GT.pdf", sep=""),
         width=20, height=11) 
dev.off()

boxplot(cbind(air_quality_data["NOx.GT."], 
              air_quality_data["NO2.GT."]),
        col="darkgreen")
dev.copy(pdf, paste("Plots/boxplot_NOx-NO2.GT.pdf", sep=""),
         width=20, height=11) 
dev.off()

boxplot(cbind(air_quality_data["PT08.S1.CO."], air_quality_data["PT08.S2.NMHC."],
              air_quality_data["PT08.S3.NOx."], 
              air_quality_data["PT08.S4.NO2."],
              air_quality_data["PT08.S5.O3."]), 
        names=c("S1.CO.", "S2.NMHC.", "S3.NOx.","S4.NO2.", "S5.O3."), 
        col="darkgreen")
dev.copy(pdf, paste("Plots/boxplot_S1-5.pdf", sep=""),
         width=20, height=11) 
dev.off()

## adjbox (apparently for skew distribution)

adjbox(cbind(air_quality_data["CO.GT."]), names=c("CO.GT."),
        col="darkgreen")
dev.copy(pdf, paste("Plots/adjboxplot_CO.GT.pdf", sep=""),
         width=20, height=11) 
dev.off()

adjbox(cbind(air_quality_data["NMHC.GT."]), names=c("NMHC.GT."),
        col="darkgreen")
dev.copy(pdf, paste("Plots/adjboxplot_NMHC.GT.pdf", sep=""),
         width=20, height=11) 
dev.off()

adjbox(cbind(air_quality_data["C6H6.GT."]), names=c("C6H6.GT."),
        col="darkgreen")
dev.copy(pdf, paste("Plots/adjboxplot_C6H6.GT.pdf", sep=""),
         width=20, height=11) 
dev.off()

adjbox(cbind(air_quality_data["NOx.GT."], 
              air_quality_data["NO2.GT."]),
        col="darkgreen")
dev.copy(pdf, paste("Plots/adjboxplot_NOx-NO2.GT.pdf", sep=""),
         width=20, height=11) 
dev.off()

adjbox(cbind(air_quality_data["PT08.S1.CO."], air_quality_data["PT08.S2.NMHC."],
              air_quality_data["PT08.S3.NOx."], 
              air_quality_data["PT08.S4.NO2."],
              air_quality_data["PT08.S5.O3."]), 
        names=c("S1.CO.", "S2.NMHC.", "S3.NOx.","S4.NO2.", "S5.O3."), 
        col="darkgreen")
dev.copy(pdf, paste("Plots/adjboxplot_S1-5.pdf", sep=""),
         width=20, height=11) 
dev.off()

# Plot histogram

## Historgrams + Normal 

for (name in names(air_quality_data)){
  plotAndSaveHistNorm(air_quality_data[name], name=name)
}

## Historgrams + Poisson

for (name in names(air_quality_data)){
  plotAndSaveHistPoisson(air_quality_data[name], name=name)
}

## Histograms + Log Normal

for (name in names(air_quality_data)){
  plotAndSaveHistLogNormal(air_quality_data[name], name=name)
}

# Outlying observations

new_air_quality_data = air_quality_data[ ,3:length(air_quality_data)-1]

m<-apply(new_air_quality_data,2,mean)
S<-cov(new_air_quality_data)
d3<-mahalanobis(new_air_quality_data,m,S)
plot(seq(1:dim(new_air_quality_data)[1]),d3, type="h", col="darkgreen", xlab="Index", ylab = "Mahalanobis Distance")
abline(h=qchisq(0.95,12),col="red")

identify(d3)   

detach(air_quality_data)

# Date -> uniform
# Time -> uniform
# CO.GT. -> poisson
# PT08.S1.CO. -> log normal
# NMHC.GT.-> log normal
# C6H6.GT. -> log normal
# PT08.S2.NMHC.-> log normal
# NOx.GT. -> log normal 
# PT08.S3.NOx. -> log normal
# NO2.GT. -> normal
# PT08.S4.NO2. -> log normal
# PT08.S5.O3. -> log normal
# T -> log normal
# RH -> ?
# AH -> ?

