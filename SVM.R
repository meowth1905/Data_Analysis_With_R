setwd("C:\\Neethu\\Introduction to statistical learning with applications in R")

library(ISLR)
library(e1071)
library(ROCR)

# Support Vector Classifier
# -------------------------

set.seed(1)
x <- matrix(rnorm(20*2), ncol = 2) # random 40 values from N(0,1)
dim(x)
y <- c(rep(-1, 10), rep(1, 10))
length(y)
x[y==1, ] <- x[y==1, ] + 1

# x axis first column 
# y axis 2nd column 
# colurs 3 - -1 = 4 is blue
# 3 - 1 = 2 is red

# we can see data is not linearly separable
plot(x, col=(3-y))

# we have 2 classes
# fit support vector classifier

# prepare data
# response must be a factor for svm()
my.data <- data.frame(x=x, y=as.factor(y))

?svm()
# cost specify the cost of a violation to the margin (a data point is on the wrong side of the margin boundary).
# small cost: wide margins, more misclassifications
# large cost: narrow margin 
svm.fit <- svm(y~., data = my.data, kernel="linear", cost=10,
               scale=FALSE)
plot(svm.fit, my.data)

# Note 
# here the second feature is plotted on the# x-axis and
# the first feature is plotted on the y-axis
# usual plot has the opposite
# but we can specify which column like
plot(x[, 2], x[, 1], col=(3-y))

# support vectors are plotted as crosses
# remaining observations are plotted as circles
# their ideintities can be find by
svm.fit$index

summary(svm.fit)
# Number of Support Vectors:  7

#( 4 3 )
# 4 support vectors in first class and 3 in the second

# smaller cost#
# more support vectors since margin is wider
svm.fit <- svm(y~., data = my.data, kernel="linear", cost=.1,
               scale=FALSE)
plot(svm.fit, my.data)
svm.fit$index

# compare SVMs with a linear kernel for a range cost values
# tune() performs 10-fold cross validation
set.seed(1)
tune.out <- tune(svm, y~., data = my.data, kernel="linear",
                 ranges = list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100))) 

summary(tune.out)
# cost=0.1 results in the lowest cross-validation error rate
# to access the best model
best.model <- tune.out$best.model

x.test <- matrix(rnorm(20*2), ncol = 2)
y.test <- sample(c(-1, 1), 20, replace = TRUE)
x.test[y.test==1, ] <- x.test[y.test==1, ] + 1

data.test <- data.frame(x=x.test, y=as.factor(y.test))

# prediction
y.pred <- predict(best.model, data.test)
table(predict=y.pred, truth=data.test$y)
# with cost=0.1, 17 observations are correctly classified
# 3 are misclassified

# what of we use cost = 0.01
new.fit <- svm(y~., data = my.data, kernel="linear", cost=0.01,
               scale=FALSE)

new.pred <- predict(new.fit, data.test)
table(predict=new.pred, truth=data.test$y)
# we got 6 misclassification

# data is linearly separable
set.seed(1)
x <- matrix(rnorm(20*2), ncol = 2) 
y <- c(rep(-1, 10), rep(1, 10))
x[y==1, ] <- x[y==1, ] + 2

plot(x, col=(3-y))

# data is barely linearly separable
dat <- data.frame(x=x, y=as.factor(y))
svmfit <- svm(y~., data = dat, kernel="linear", cost=1e5,
              scale=F)
summary(svmfit)
# Number of Support Vectors:  3
plot(svmfit, dat)
# no training errors (reds points are all on one side)
# but margin is very narrow because non support vector points(o) are
# very close to the decision boundary

svmfit <- svm(y~., data = dat, kernel="linear", cost=1,
              scale=F)

summary(svmfit)
# Number of Support Vectors:  4
plot(svmfit, dat)
# wide margin
# non support vector points are much farther from decision boundary

# Support Vector Machine
# ----------------------
# which user kernel to expand feature space
set.seed (1)
x <- matrix(rnorm(200*2), ncol =2)
x[1:100 ,] <- x[1:100, ] + 2
x[101:150 ,] <-  x[101:150, ] - 2
y <- c(rep(1, 150), rep(2, 50))
colors <- c("blue", "red")

# class boundary is indeed nonlinear
plot(x, col=colors[y], pch=21)

dat <- data.frame(x=x, y=as.factor(y))

train <- sample(200, 100)
nonlin.svm <- svm(y~., data = dat[train, ], kernel="radial", gamma=1,
                  cost=1)

summary(nonlin.svm)

# some training errors
# we can reduce this by increasing cost
# but this will come at cost of more irregular decision boundary (risk of overfitting)
plot(nonlin.svm, dat[train, ])


nonlin.svm <- svm(y~., data = dat[train, ], kernel="radial", gamma=1,
                  cost=1e5)

# less training error
# non support vector points(o) very close to decision boundary
plot(nonlin.svm, dat[train, ])

# choose cost using tune()
set.seed(1)
tune.out <- tune(svm, y~., data = dat[train, ], kernel="radial",
                 ranges = list(cost=c(0.1, 1, 10, 100, 1000), gamma=c(0.5, 1, 2, 3, 4))) 

summary(tune.out)
#  cost gamma
#   1   0.5

table(predict = predict(tune.out$best.model, newdata = dat[-train, ]),
      truth = dat[-train, "y"])

# 12% misclassification

