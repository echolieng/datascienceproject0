library(MASS)
library(ISLR)
data(Boston)
names(Boston)
#simple linear regression model with medv as the response and lstat as the predictor
lm.fit=lm(medv~lstat, data=Boston)
#equivalently:
attach(Boston)
lm.fit=lm(medv~lstat)
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)
#prediction of medv for a given value of lstat
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval="confidence")
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval="prediction")
#plot medv and lstat along with the least squares regression line
plot(lstat,medv)
abline(lm.fit)
#increase line width by 3 times
abline(lm.fit, lwd=3)
#change line color
abline(lm.fit, lwd=3, col="red")
#create different plotting symbols
plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")
abline(lm.fit, lwd=3, col="red")
#plot(1:20,1:20,pch=1:20)

#diagnosistic plots
#divide plotting region into 2x2 grid of panels
par(mfrow=c(2,2))
plot(lm.fit)

#plot residuals
plot(predict(lm.fit), residuals(lm.fit))
#plot studentized residuals
plot(predict(lm.fit), rstudent(lm.fit))

#plot leverage statistics
plot(hatvalues(lm.fit))
#identify the index of the largest element of a vector (which obs. has the largest leverage statistics)
which.max(hatvalues(lm.fit))

#multiple linear regression
lm.fit=lm(medv~lstat+age, data=Boston)
summary(lm.fit)
#regression using all the predictors
lm.fit=lm(medv~.,data=Boston)
summary(lm.fit)
#R^2
summary(lm.fit)$r.sq
#RSE
summary(lm.fit)$sigma
#VIF (Variance Inflation Factor)
library(car)
vif(lm.fit)
#preform regression using all variables but one (age)
lm.fit1=lm(medv~.-age, data=Boston)
summary(lm.fit1)
#alternatively
lm.fit1=update(lm.fit, ~.-age)
summary(lm.fit1)

#include interaction term lstat*age (shorthand for lstat+age+lstat:age)
summary(lm(medv~lstat*age, data=Boston))

#nonliner transformations of predictors
#create a predictor X^2 using I(X^2)
lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)
#anova (analysis of variance)
lm.fit=lm(medv~lstat)
anova(lm.fit, lm.fit2)
par(mfrow=c(2,2))
plot(lm.fit2)
#create polynomial within lm
lm.fit5=lm(medv~poly(lstat,5))
summary(lm.fit5)
#log transformation
summary(lm(medv~log(rm)), data=Boston)

#qualitative predictors
data(Carseats)
names(Carseats)
lm.fit =lm(Sales ~ .+ Income :Advertising +Price :Age,data=Carseats)
summary(lm.fit)
#return coding R uses to create dummy variables for qualitative predictors
attach(Carseats)
contrasts(ShelveLoc)

#create function LoadLibraries
LoadLibraries=function (){
  library (ISLR)
  library (MASS)
  print (" The libraries have been loaded .")
  }