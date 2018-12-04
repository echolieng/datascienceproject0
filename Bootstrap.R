library(ISLR)
# Create a function which takes as input the (X, Y) data as well as a vector indicating which observations should be used to estimate alpha.
alpha.fn = function(data, index){
  X = data$X[index]
  Y = data$Y[index]
  return((var(Y)-cov(X, Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
# ^outputs an estimate for alpha
alpha.fn(Portfolio, 1:100)
# Randomly select 100 observations from the range 1 to 100 with replacement.
set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace=T))
# Produce 1000 bootstrap estimates for alpha using boot()
library(boot)
boot(Portfolio, alpha.fn, R=1000)

# Estimate accuracy of a linear regression model
boot.fn = function(data, index){
  return(coef(lm(mpg~horsepower, data=data, subset=index)))
}
boot.fn(Auto, 1:392)

set.seed(1)
boot.fn(Auto, sample(392, 392, replace=T))
boot(Auto, boot.fn, 1000)