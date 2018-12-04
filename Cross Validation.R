# The Validation Set Approach

library(ISLR)
set.seed(1)
# Randomly subset 196 observations out of original 392 observations
train = sample(392, 196)
# Fit a linear regression using only the observations in the training set
lm.fit = lm(mpg~horsepower, data=Auto, subset=train)
# Estimate response for all 392 observations then calculate MSE of the 192 observations in the validation set
attach(Auto)
mean((mpg-predict(lm.fit, Auto))[-train]^2)
# Estimate test errors for poly and cubic regressions
lm.fit2=lm(mpg~poly(horsepower, 2), data=Auto, subset=train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)
lm.fit3=lm(mpg~poly(horsepower, 3), data=Auto, subset=train)
mean((mpg-predict(lm.fit3, Auto))[-train]^2)


# Leave-One-Out Cross-Validation

library(boot)
# Fit linear regression with glm
glm.fit = glm(mpg~horsepower, data=Auto)
# Use cv.glm() for cross-validation
cv.error = cv.glm(Auto, glm.fit)
cv.error$delta
# Cross-Validation for increasing polynomial fits
cv.error = rep(0,5)
for (i in 1:5){
  glm.fit = glm(mpg~poly(horsepower, i), data=Auto)
  cv.error[i] = cv.glm(Auto, glm.fit)$delta[1]
}
cv.error

# K-Fold Validation

set.seed(17)
glm.fit = glm(mpg~horsepower, data=Auto)
# Use cv.glm() for cross-validation
cv.error = cv.glm(Auto, glm.fit)
cv.error$delta
# Cross-Validation for increasing polynomial fits
cv.error.10 = rep(0,10)
for (i in 1:10){
  glm.fit = glm(mpg~poly(horsepower, i), data=Auto)
  cv.error.10[i] = cv.glm(Auto, glm.fit, K=10)$delta[1]
}
cv.error.10