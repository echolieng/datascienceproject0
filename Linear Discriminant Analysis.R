library (ISLR)
library(MASS)
attach(Smarket)
train = (Year < 2005)
Smarket.2005 = Smarket[!train, ]
Direction.2005 = Direction[!train]
lda.fit = lda(Direction~Lag1+Lag2, data=Smarket, subset=train)
lda.fit
# Plot the linear discrinimants (coefficientxpredictor)
plot(lda.fit)
# Predict class, posterior (a matrix whose kth column contains the posterior probability that the corresponding observation belongs to the kth class), and the linear discriminants
lda.predict = predict(lda.fit, Smarket.2005)
names(lda.predict)
lda.predict
lda.class = lda.predict$class
table(lda.class, Direction.2005)
# The LDA and logistic regression predictions are almost identical
mean(lda.class==Direction.2005)
# Apply a 50% threshold
sum(lda.predict$posterior[,1]>=.5)
sum(lda.predict$posterior[,1]<.5)
lda.predict$posterior[1:20, 1]
lda.class[1:20]
# Look at other thresholds
sum(lda.pedict$posterior[,1]>.9)
# No days in 2005 meet the threshold