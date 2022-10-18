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

plotAndSaveHistNorm <- function(x, name=name) {
  
  x = as.numeric(unlist(x))
  
  hist = hist(x, col="darkgreen", freq=FALSE, breaks=20,
              main = paste("Histogram of", name), xlab=name)
  x_values <- seq(min(na.omit(x)), max(na.omit(x)), length = 100)
  y_values <- dnorm(x_values, mean = mean(na.omit(x)), sd = sd(na.omit(x))) 
  #y_values <- y_values * diff(hist$mids[1:2]) * length(x) 
  
  lines(x_values, y_values, lwd = 3, col="green")
  
  dev.copy(pdf, paste("Plots/hist_normal_",name,".pdf"), width=20, height=11)
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
  
  dev.copy(pdf, paste("Plots/hist_poisson_", name, ".pdf"), width=20, height=11) 
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
  
  y_values <- dlnorm(x_values, 
                     meanlog = estimate_param["estimate.meanlog"],
                     sdlog = estimate_param["estimate.sdlog"], 
                     log = FALSE) 
  
  lines(x_values, y_values, lwd = 3, col="green")
  
  dev.copy(pdf, paste("Plots/hist_logNormal_", name, ".pdf"), width=20, height=11) 
  dev.off()
  
  
  
}

attach(air_quality_data)

summary(air_quality_data)
dim(air_quality_data)

vis_miss(air_quality_data)

# Historgrams + Normal 

for (name in names(air_quality_data)){
  plotAndSaveHistNorm(air_quality_data[name], name=name)
}

# Historgrams + Poisson

for (name in names(air_quality_data)){
  plotAndSaveHistPoisson(air_quality_data[name], name=name)
}

# Historgrams + Log Normal

for (name in names(air_quality_data)){
  plotAndSaveHistLogNormal(air_quality_data[name], name=name)
}

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

