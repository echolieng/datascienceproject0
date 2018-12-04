library (ISLR)
names(Smarket )
dim(Smarket)
summary(Smarket)
# Produces a matrix that contains all of the pairwise correlations among the predictors (excpet Direction)
cor(Smarket[, -9])
# As one would expect, the correlations between the lag variables and today's returns are close to zero.
#In other words, there appears to be little
#correlation between today's returns and previous days' returns. The only
#substantial correlation is between Year and Volume. By plotting the data we
#see that Volume is increasing over time. In other words, the average number
#of shares traded daily increased from 2001 to 2005.
attach(Smarket)
plot(Volume)

#fit a logistic regression model in order to predict Direction using Lag1 through Lag5 and Volume.
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family = binomial)
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef
summary(glm.fit)$coef[, 4]
# Predict the probability that the market will go up, given the values of the predictors.
glm.probs = predict(glm.fit, type = 'response')
glm.probs[1:10]
contrasts(Direction)
# Create a vector of class predictions based whether the predicted probability of a market increase is greater than or less than 50%.
# Create a vector of 1,250 Down elements.
glm.pred = rep('Down', 1250)
glm.pred[glm.probs>0.5] = 'Up'
# Confusion Matrix
table(glm.pred, Direction)
mean(glm.pred == Direction)

# Use training data to fit model, then use future data to predict
train = (Year < 2005)
Smarket.2005 = Smarket[!train, ]
dim(Smarket.2005)
Direction.2005 = Direction[!train]
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial, subset=train)
glm.probs = predict(glm.fit, Smarket.2005, type='response')
# Compare predictions to actual classes
glm.pred = rep('Down', 252)
glm.pred[glm.probs>0.5] = 'Up'
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
mean(glm.pred != Direction.2005)

# Remove irrelevant predictors and refit using just Lag1 and Lag2 (predictors with smallest p values)
glm.fit = glm(Direction~Lag1+Lag2, data = Smarket, family = binomial, subset = train)
glm.probs = predict(glm.fit, Smarket.2005, type = 'response')
glm.pred = rep('Down', 252)
glm.pred[glm.probs<.5] = 'Up'
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
mean(glm.pred != Direction.2005)
# Predict returns associated with particular values of Lag1 and Lag2
predict(glm.fit, newdata=data.frame(Lag1=c(1.2, 1.5), Lag2=c(1.1, -0.8), type='response'))