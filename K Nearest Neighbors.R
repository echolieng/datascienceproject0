library(class)
attach(Smarket)
train = (Year < 2005)
# Create a matrix containing the predictors associated with the training data
train.X = cbind(Lag1, Lag2)[train,]
# Create a matrix containing the predictors associated with the test data
test.X = cbind(Lag1, Lag2)[!train,]
# Create a vector containing the class labels for the training observations
train.Direction = Direction[train]
# Set a random seed so R will randomly break the tie if several observations are tied as nearest neighbors
set.seed(1)
knn.predict = knn(train.X, test.X, train.Direction, k=1)
Direction.2005 = Direction[!train]
table(knn.predict, Direction.2005)
mean(knn.predict ==  Direction.2005)
# Results not very good using k=1^. Try k=3:
knn.predict = knn(train.X, test.X, train.Direction, k=3)
table(knn.predict, Direction.2005)
mean(knn.predict ==  Direction.2005)
# Slighly better results^

# Apply KNN approach to Caravan data:
attach(Caravan)
summary(Purchase)
mean(Purchase == "Yes")
# Standardize data so that all variables have mean of 0 and stdev of 1. (Exclude column 86(qualitative))
standardized.X = scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
# Now every column has stdev of 1 and mean of 0
var(standardized.X[,1])
var(standardized.X[,2])
# Split data into a test set and a training set
test = 1:1000
train.X = standardized.X[-test,]
test.X = standardized.X[test,]
train.Y = Purchase[-test]
test.Y = Purchase[test]
set.seed(1)
knn.pred = knn(train.X, test.X, train.Y, k=1)
mean(test.Y != knn.pred)
mean(test.Y != "No")
table(knn.pred, test.Y)
# success rate
9/(68+9)
# K=3
knn.pred = knn(train.X, test.X, train.Y, k=3)
mean(test.Y != knn.pred)
mean(test.Y != "No")
table(knn.pred, test.Y)
5/(21+5)
# success rate increases to 19%^
# try K=5
knn.pred = knn(train.X, test.X, train.Y, k=5)
mean(test.Y != knn.pred)
mean(test.Y != "No")
table(knn.pred, test.Y)
4/(11+4)
# success rate increases to 26.7%^

# Now try logistic regression
glm.fit = glm(Purchase~., data=Caravan, family=binomial, subset=-test)
glm.probs = predict(glm.fit, Caravan[test,], type='response')
glm.pred = rep("No", 1000)
glm.pred[glm.probs>.5] = "Yes"
table(glm.pred, test.Y)
# threshold of .5 didn't yield good results. try .25:
glm.pred = rep("No", 1000)
glm.pred[glm.probs>.25] = "Yes"
table(glm.pred, test.Y)
11/(11+22)
# threshold of .25 yields much better results