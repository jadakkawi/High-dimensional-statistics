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
# Sensitivity:
Sens <- function(M)
{
  M[2,2]/sum(M[2,])
}
# Rem.: It is assumed that the truth is on the rows and the classification in the columns

# Specificity:
Spec <- function(M)
{
  M[1,1]/sum(M[1,])
}
# Rem.: It is assumed that the truth is on the rows and the classification in the columns


# 2. ROC curve  ----

ROCCurve <- function(score, class)
{
  x <- y <- NULL
  scoreOrd <- sort(score)
  
  x[1] <- 1 ; y[1] <- 1
  for(i in 2:length(score)){
    c <- scoreOrd[i]
    ConfMat <- table(class, score >= c)
    
    sens <- Sens(ConfMat)
    spec <- Spec(ConfMat)
    x[i] <- 1-spec
    y[i] <- sens
  }
  return(list(x=x, y=y))
}
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
air_quality_data$NOx.GT.<- air_quality_data$NOx.GT.>=100

NOx.GT._factor <- as.factor(air_quality_data[['NOx.GT.']])

# date_filter = subset (air_quality_data, select = c(Date))

air_quality_data = subset (air_quality_data, select = c(-NOx.GT., -Date, -Time, -PT08.S3.NOx., -AH_bin))
air_quality_data = standardizeDF(air_quality_data)

air_quality_data = append(air_quality_data, data.frame(NOx.GT._factor), after = length(air_quality_data))

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

GLM1 <- glm(formula = NOx.GT._factor ~ .,data = train, family=binomial(logit))
summary(GLM1)
ppv=1-pchisq(542.192-126.95, df=391-381)
ppv

glm0 <- glm(NOx.GT._factor ~ 1, data = train,family=binomial(link="logit"))
anova(glm0, GLM1, test="Chisq")

plot(GLM1$linear.predictors, GLM1$fitted, col = as.numeric(train[['NOx.GT._factor']]))
Classif1 <- GLM1$fitted.values >= 0.5
(tab1 <- table(train[['NOx.GT._factor']], Classif1))
round((tab1[2,1]+tab1[1,2])/sum(tab1) * 100, 2)
library(MASS)
jad <-stepAIC(GLM1, direction="backward")

glm2 <- glm(NOx.GT._factor ~ NMHC.GT. + NO2.GT. + PT08.S4.NO2. + T + RH, data = train,family=binomial(link="logit"))
# glm2 <- glm(AH_bin_factor ~ PT08.S1.CO. + NMHC.GT. +PT08.S2.NMHC.+NOx.GT.
#             +PT08.S3.NOx.+PT08.S4.NO2.+T+RH, data = train,family=binomial(link="logit"))
summary(glm2)

Classif2 <- glm2$fitted.values >= 0.5
(tab2 <- table(train[['NOx.GT._factor']], Classif2))
round((tab2[2,1]+tab2[1,2])/sum(tab2) * 100, 2)

plot(glm2$linear.predictors, glm2$fitted, col = as.numeric(train[['NOx.GT._factor']]))
pairs(air_quality_data)

fitProb <- linearPredict <- NULL
for(i in 1:dim(test)[1])
{
  ind_i <- c(1, test$NMHC.GT.[i], test$NO2.GT.[i], 
             test$PT08.S4.NO2.[i], test$T[i], test$RH[i])
  # ind_i <- c(1, test$PT08.S1.CO.[i], test$NMHC.GT.[i], test$PT08.S2.NMHC.[i],
  #            test$NOx.GT.[i], test$PT08.S3.NOx.[i], test$PT08.S4.NO2.[i], 
  #            test$T[i], test$RH[i])
  linearPredict[i] <- ind_i %*% glm2$coefficients
  fitProb[i] <- exp(linearPredict[i]) / (1+exp(linearPredict[i]))
}
Classico <- fitProb >= 0.5
(tabos <- table(test$NOx.GT._factor, Classico))
round((tabos[2,1]+tabos[1,2])/sum(tabos) * 100, 2)

library(car)
vif(glm2)
pairs(data.frame(train$NMHC.GT.,train$NO2.GT.,train$PT08.S4.NO2
                 ,train$T,train$RH))


# Discriminatory Boxplots

boxplot(train$NMHC.GT. ~ train$NOx.GT._factor)
boxplot(train$NO2.GT. ~ train$NOx.GT._factor)
boxplot(train$PT08.S4.NO2 ~ train$NOx.GT._factor)
boxplot(train$T ~ train$NOx.GT._factor)
boxplot(train$RH ~ train$NOx.GT._factor)
# 2. Further analysis (PCA) ----
dataPCA <- train[,-c(1, 2, 4, 5, 8, 11)]
res <- princomp(dataPCA, cor=TRUE)
summary(res)
plot(res,type="b")
mod2 <- glm(train$NOx.GT._factor ~ res$scores[,1] + res$scores[,2] + res$scores[,3] +
              res$scores[,4] ,family=binomial(logit))
summary(mod2)
#In order to simplify the model, different procedures are possible
#As illustration, two-way the automatic selection procedure  based on the AIC is used:
modPCA <- stepAIC(mod2, direction="both")

# And the 1st,2nd, 3rd, 4th PCs are kept in the final model.

#plot(obesity, modPCA$fitted, ylim=c(0,1), ylab="Fitted values")
#plot(sbp, modPCA$fitted, ylim=c(0,1), ylab="Fitted values")

#These plots show no clear patterns; this means that the probability of success
# does not increase/decrease with these two variables

mod3 <- glm(train$NOx.GT._factor ~ res$scores[,1] +
            res$scores[,2] + res$scores[,3] + res$scores[,4],family=binomial(logit))
summary(mod3)

plot(mod3$linear.predictors, mod3$fitted, col = as.numeric(train[['NOx.GT._factor']]))

fitProb2 <- linearPredict2 <- NULL
for(i in 1:dim(test)[1]){ 
  # transform test into PCA
  ind_i <- c(test$NMHC.GT.[i], test$NO2.GT.[i],
              test$PT08.S4.NO2.[i],
             test$T[i], test$RH[i])
  tranformed_i = append(1,ind_i %*% res$loadings[,c(1,2,3,4)])
  linearPredict2[i] <- tranformed_i %*% mod3$coefficients
  fitProb2[i] <- exp(linearPredict2[i]) / (1+exp(linearPredict2[i]))
}
Classico <- fitProb2 >= 0.5
(tabos <- table(test$NOx.GT._factor, Classico))
round((tabos[2,1]+tabos[1,2])/sum(tabos) * 100, 2)

# Faire un CV GLM fit puis appliquer ROC CURVE

# Fitted probabilities via leave-one-out:
cvfitProb <- cvlinearPredict <- NULL
for(i in 1:dim(air_quality_data)[1]){
  newdata <- air_quality_data[-i,]
  modcv <- glm(NOx.GT._factor ~ NMHC.GT. + NO2.GT. + PT08.S4.NO2. + T + RH, data = newdata,family=binomial(link="logit"))
  cvlinearPredict[i] <- predict(modcv, newdata=air_quality_data[i,])
  cvfitProb[i] <- exp(cvlinearPredict[i]) / (1+exp(cvlinearPredict[i]))
}
plot(cvlinearPredict, cvfitProb, col=as.numeric(NOx.GT._factor)+1, xlab="Linear predictor", ylab="Fitted probability")

# ROC curve:
ROC_LogRegr <- ROCCurve(score=cvfitProb, class=as.numeric(NOx.GT._factor))
plot(ROC_LogRegr$x, ROC_LogRegr$y, xlab="1-specificity", ylab="sensitivity",
     xaxt="n",yaxt="n", xlim=0:1, ylim=0:1)
axis(1, seq(0,1,by=0.2), seq(0,1,by=0.2))
axis(2, seq(0,1,by=0.2), seq(0,1,by=0.2))
abline(0,1, lty=2, col="grey")


# 2. Linear discriminant analysis ----
library(MASS)
ldafull <- lda(x=air_quality_data[,1:10], grouping=air_quality_data$NOx.GT._factor)
g <- 2 ; n <- dim(air_quality_data)[1]
l1 <- (g-1)*(ldafull$svd)^2 / (n-g)
( gamma1 <- l1/(1+l1) )

# Variable selection:
l <- gamma <- NULL
for(i in 1:10){
  var <- (1:10)[-i]
  ldaTest <- lda(x=air_quality_data[,var], grouping=air_quality_data$NOx.GT._factor)
  l[i] <- (g-1)*(ldaTest$svd)^2 / (n-g)
  gamma[i] <- l[i]/(1+l[i])
}
gamma
# CO.GT. could be removed
l <- gamma <- NULL
for(i in 1:9)
{
  var <- (2:10)[-i]
  ldaTest <- lda(x=air_quality_data[,var], grouping=air_quality_data$NOx.GT._factor)
  lTest <- (g-1)*(ldaTest$svd)^2 / (n-g)
  l <- c(l, lTest)
  gamma <- c(gamma, lTest/(1+lTest))
}
gamma
# T could be removed
l <- gamma <- NULL
for(i in 1:8)
{
  var <- c(2,3,4,5,6,7,8,10)[-i]
  ldaTest <- lda(x=air_quality_data[,var], grouping=air_quality_data$NOx.GT._factor)
  lTest <- (g-1)*(ldaTest$svd)^2 / (n-g)
  l <- c(l, lTest)
  gamma <- c(gamma, lTest/(1+lTest))
}
gamma
# PT08.S4.NO2. could be removed
l <- gamma <- NULL
for(i in 1:7)
{
  var <- c(2,3,4,5,6,8,10)[-i]
  ldaTest <- lda(x=air_quality_data[,var], grouping=air_quality_data$NOx.GT._factor)
  lTest <- (g-1)*(ldaTest$svd)^2 / (n-g)
  l <- c(l, lTest)
  gamma <- c(gamma, lTest/(1+lTest))
}
gamma
# NMHC.GT.  could be removed
l <- gamma <- NULL
for(i in 1:6)
{
  var <- c(2,4,5,6,8,10)[-i]
  ldaTest <- lda(x=air_quality_data[,var], grouping=air_quality_data$NOx.GT._factor)
  lTest <- (g-1)*(ldaTest$svd)^2 / (n-g)
  l <- c(l, lTest)
  gamma <- c(gamma, lTest/(1+lTest))
}
gamma


# After comparison of the powers of the different models, a final model is obtained by removing
# the variables CO.GT., T, PT08.S4.NO2., NMHC.GT.

# Posterior probabilities via leave-one-out:
# They can be obtained by applying a loop of the same type as for logistic regression
# or directly by using the option CV=TRUE of the lda function
final = c(2,4,5,6,8,10)
lda1 <- lda(x=air_quality_data[,final], grouping=air_quality_data$NOx.GT._factor, CV=TRUE)
postProb <- lda1$posterior[,2]

# ROC curve:
ROC_lda <- ROCCurve(score=postProb, class=air_quality_data$NOx.GT._factor)
plot(ROC_lda$x, ROC_lda$y, type="l", xlab="1-specificity", ylab="sensitivity",
     xaxt="n",yaxt="n", xlim=0:1, ylim=0:1)
axis(1, seq(0,1,by=0.2), seq(0,1,by=0.2))
axis(2, seq(0,1,by=0.2), seq(0,1,by=0.2))
abline(0,1, lty=2, col="grey")


# 3. Comparison of the two classifications ----

# Comparison of scores:
plot(cvfitProb, postProb, xlab="Prob. log. regr.", ylab="Prob. LDA") ; abline(a=0,b=1, lty=2, col="red")
plot(cvfitProb-postProb, ylab="Prob. log. regr. - Prob. LDA") ; abline(h=0, lty=2, col="red")
( tabComp <- table(cvfitProb >= 0.5, postProb >= 0.5) )
((tabComp[1,2]+tabComp[2,1]) / sum(tabComp)*100)
# Posterior probabilities are, for the most, quite closed and, using a cutoff
# equal to 0.5, 23 observations (i.e. 7.6%) are differently classified by the
# two techniques.

# Comparison of ROC curves:
plot(ROC_LogRegr$x, ROC_LogRegr$y, type="l", xlab="1-specificity", ylab="sensitivity",
     xaxt="n",yaxt="n", xlim=0:1, ylim=0:1)
axis(1, seq(0,1,by=0.2), seq(0,1,by=0.2))
axis(2, seq(0,1,by=0.2), seq(0,1,by=0.2))
abline(0,1, lty=2, col="grey")
lines(ROC_lda$x, ROC_lda$y, type="l", col=2, lty=2)
legend("bottomright",c("Logistic regression","LDA"), col=1:2, lty=1:2)
# Both curves are quite close and the areas under the curves are also quite similar.





# Specific cutoffs:
( ConfMat_LogRegr <- table(air_quality_data$NOx.GT._factor, cvfitProb >= 0.5) )
( ConfMat_LDA <- table(air_quality_data$NOx.GT._factor, postProb >= 0.5) )

sens.5_LogRegr <- Sens(ConfMat_LogRegr) ; spec.5_LogRegr <- Spec(ConfMat_LogRegr)
sens.5_LDA <- Sens(ConfMat_LDA) ; spec.5_LDA <- Spec(ConfMat_LDA)

(id_LogRegr <- which.min(abs((1-ROC_LogRegr$x) - ROC_LogRegr$y)))
sens_LogRegr <- ROC_LogRegr$y[id_LogRegr] ; spec_LogRegr <- 1-ROC_LogRegr$x[id_LogRegr]
(id_LDA <- which.min(abs((1-ROC_lda$x) - ROC_lda$y)))
sens_LDA <- ROC_lda$y[id_LDA] ; spec_LDA <- 1-ROC_lda$x[id_LDA]

(idYouden_LogRegr <- which.max(ROC_LogRegr$y - ROC_LogRegr$x))
sens.Youd_LogRegr <- ROC_LogRegr$y[idYouden_LogRegr] ; spec.Youd_LogRegr <- 1-ROC_LogRegr$x[idYouden_LogRegr]
(idYouden_LDA <- which.max(ROC_lda$y - ROC_lda$x))
sens.Youd_LDA <- ROC_lda$y[idYouden_LDA] ; spec.Youd_LDA <- 1-ROC_lda$x[idYouden_LDA]

points(c(1-spec.5_LogRegr, 1-spec_LogRegr, 1-spec.Youd_LogRegr),
       c(sens.5_LogRegr, sens_LogRegr, sens.Youd_LogRegr), pch=16, col=2:4)
legend("bottomright",
       legend=c("Cutoff = 0.5", "Cutoff achieving the same spec. and sens.", "Cutoff based on Youden's J statistics"),
       col=2:4, pch=16)


plot(ROC_LogRegr$x, ROC_LogRegr$y, type="l", xlab="1-specificity", ylab="sensitivity",
     xaxt="n",yaxt="n", xlim=0:1, ylim=0:1)
axis(1, seq(0,1,by=0.2), seq(0,1,by=0.2))
axis(2, seq(0,1,by=0.2), seq(0,1,by=0.2))
abline(0,1, lty=2, col="grey")

points(c(1-spec.5_LogRegr, 1-spec_LogRegr, 1-spec.Youd_LogRegr),
       c(sens.5_LogRegr, sens_LogRegr, sens.Youd_LogRegr), pch=16, col=c(3,4,6))
lines(ROC_lda$x, ROC_lda$y, type="l", col=2, lty=2)
points(c(1-spec.5_LDA, 1-spec_LDA, 1-spec.Youd_LDA),
       c(sens.5_LDA, sens_LDA, sens.Youd_LDA), pch=17, col=c(3,4,6))

legend("bottomright",
       legend=c("Cutoffs = 0.5", "Cutoffs achieving the same spec. and sens.", "Cutoffs based on Youden's J statistics"),
       col=c(3,4,6), pch=18)
# The cutoff based on Youden's statistic seems to be the best in terms of specificity
# while the cutoff achieving the same specificity and sensitivity seems to be the best in terms of
# sensitivity. The "default" cutoff "equal to 0.5" seems to be a compromise between the two others.

# 