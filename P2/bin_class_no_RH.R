# This is a test to check if binary classification is possible for pollution 
# dataset using electronic sensor heads.

# Classifying absolute humidity is not very interesting, yet it's the variable that we chose in project 1 and for now we will stick to it.

# Multi-colinearity issues may arise due to highly correlated variables.
# Eliminating some variables will weaken our classification study.
# Professor said that we do a logistic reg using PCs.

# Other possible binary variables to create:
# One might consider predicting whether it's a rush hour or not.
# One might also Predict absence or presence of a certain chemical compound in the air due to sensor responses
rm(list=ls())
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

air_quality_data = read.csv('Data/cleanAirQuality.csv', sep=";", 
                            header=TRUE, stringsAsFactors=TRUE)

AH_bin_factor <- as.factor(air_quality_data[['AH_bin']])

# date_filter = subset (air_quality_data, select = c(Date))

air_quality_data = subset (air_quality_data, select = c(-AH_bin, -Date, -Time, -RH))
air_quality_data = standardizeDF(air_quality_data)

air_quality_data = append(air_quality_data, data.frame(AH_bin_factor), after = length(air_quality_data))

air_quality_data = data.frame(air_quality_data)

summary(air_quality_data)

# We first try the logistic regression framework and fit using all the data

# We first try the logistic regression framework.
# We split the data into 80/20 ratio.

# AH_bin = as.factor(AH_bin)
set.seed(2)
sample <- sample(c(TRUE, FALSE), dim(air_quality_data)[1], replace=TRUE, prob=c(0.8,0.2))
train  <- air_quality_data[sample, ]
test   <- air_quality_data[!sample, ]

GLM1 <- glm(formula = AH_bin_factor ~ .,data = train, family=binomial(logit))
summary(GLM1)
ppv=1-pchisq(542.192-73.499, df=391-379)
ppv

glm0 <- glm(AH_bin_factor ~ 1, data = train,family=binomial(link="logit"))
anova(glm0, GLM1, test="Chisq")

plot(GLM1$linear.predictors, GLM1$fitted, col = as.numeric(train[['AH_bin_factor']]))
Classif1 <- GLM1$fitted.values >= 0.5
(tab1 <- table(train[['AH_bin_factor']], Classif1))
round((tab1[2,1]+tab1[1,2])/sum(tab1) * 100, 2)
library(MASS)
stepAIC(GLM1, direction="backward")

glm2 <- glm(AH_bin_factor ~ C6H6.GT. +PT08.S2.NMHC.+NOx.GT.
            +PT08.S3.NOx.+NO2.GT.+PT08.S4.NO2.+PT08.S5.O3.+T, data = train,family=binomial(link="logit"))
# glm2 <- glm(AH_bin_factor ~ PT08.S1.CO. + NMHC.GT. +PT08.S2.NMHC.+NOx.GT.
#             +PT08.S3.NOx.+PT08.S4.NO2.+T+RH, data = train,family=binomial(link="logit"))
summary(glm2)

Classif2 <- glm2$fitted.values >= 0.5
(tab2 <- table(train[['AH_bin_factor']], Classif2))
round((tab2[2,1]+tab2[1,2])/sum(tab2) * 100, 2)

plot(glm2$linear.predictors, glm2$fitted, col = as.numeric(train[['AH_bin_factor']]))
pairs(air_quality_data)

fitProb <- linearPredict <- NULL
for(i in 1:dim(test)[1])
{
  ind_i <- c(1, test$C6H6.GT.[i], test$PT08.S2.NMHC.[i],
             test$NOx.GT.[i], test$PT08.S3.NOx.[i],test$NO2.GT.[i], test$PT08.S4.NO2.[i], test$PT08.S5.O3.[i],
             test$T[i])
  # ind_i <- c(1, test$PT08.S1.CO.[i], test$NMHC.GT.[i], test$PT08.S2.NMHC.[i],
  #            test$NOx.GT.[i], test$PT08.S3.NOx.[i], test$PT08.S4.NO2.[i], 
  #            test$T[i], test$RH[i])
  linearPredict[i] <- ind_i %*% glm2$coefficients
  fitProb[i] <- exp(linearPredict[i]) / (1+exp(linearPredict[i]))
}
Classico <- fitProb >= 0.5
(tabos <- table(test$AH_bin_factor, Classico))
round((tabos[2,1]+tabos[1,2])/sum(tabos) * 100, 2)

library(car)
vif(glm2)

# 2. Further analysis (PCA) ----

dataPCA <- train[,-c(1, 2, 3, 12)]
# dataPCA <- train[,-c(1, 4, 8, 10, 13)]

res <- princomp(dataPCA, cor=TRUE)
summary(res)
plot(res,type="b")

mod2 <- glm(train$AH_bin_factor ~ res$scores[,1] + res$scores[,2] + res$scores[,3] +
              res$scores[,4] + res$scores[,5],family=binomial(logit))
summary(mod2)
#In order to simplify the model, different procedures are possible
#As illustration, two-way the automatic selection procedure  based on the AIC is used:
modPCA <- stepAIC(mod2, direction="both")

# And the 1st,2nd, 3rd, 4th and 5th PCs are kept in the final model.

#plot(obesity, modPCA$fitted, ylim=c(0,1), ylab="Fitted values")
#plot(sbp, modPCA$fitted, ylim=c(0,1), ylab="Fitted values")

#These plots show no clear patterns; this means that the probability of success
# does not increase/decrease with these two variables

mod3 <- glm(train$AH_bin_factor ~ res$scores[,1] +
              res$scores[,4] + res$scores[,5],family=binomial(logit))
summary(mod3)

plot(mod3$linear.predictors, mod3$fitted, col = as.numeric(train[['AH_bin_factor']]))

fitProb2 <- linearPredict2 <- NULL
for(i in 1:dim(test)[1]){ 
  # transform test into PCA
  ind_i <- c(test$C6H6.GT.[i], test$PT08.S2.NMHC.[i],
             test$NOx.GT.[i], test$PT08.S3.NOx.[i],test$NO2.GT.[i], test$PT08.S4.NO2.[i],test$PT08.S5.O3.[i],
             test$T[i])
  tranformed_i = append(1,ind_i %*% res$loadings[,c(1,4,5)])
  linearPredict2[i] <- tranformed_i %*% mod3$coefficients
  fitProb2[i] <- exp(linearPredict2[i]) / (1+exp(linearPredict2[i]))
}
Classico <- fitProb2 >= 0.5
(tabos <- table(test$AH_bin_factor, Classico))
round((tabos[2,1]+tabos[1,2])/sum(tabos) * 100, 2)
