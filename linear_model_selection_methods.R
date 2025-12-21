setwd("C:\\Neethu\\Introduction to statistical learning with applications in R")

library("ISLR")

data("Hitters")
names(Hitters)
# response variable is Salary
dim(Hitters)
# so we have number of predictors p = 19

attach(Hitters)

# 59 rows has no salary 
table(is.na(Salary))
# or use sum to get count of True
sum(is.na(Salary))

# to remove all of the rows that have missing values in any variable.
Hitters <- na.omit(Hitters)
# returns 0
sum(is.na(Hitters))

# Best Subset Selection
# ------------------------------
# install.packages("leaps")
library(leaps)
# syntax is same as lm()
# Salary~.: explain Salary using every other column in the data.
model.bs <- regsubsets(Salary~., data = Hitters)
summary(model.bs)
# asterisk indicates that a given variable is included in the corresponding model.
# for example
# best 2-variable model included predictors Hits and CRBI
# by default only report upto best 8-variable model
# if i want upto 19-variable model then
model.bs <- regsubsets(Salary~., data = Hitters, nvmax = 19)
summary(model.bs)
# summary also returns RSS, R2, adj R2, Cp, BIC
names(summary(model.bs))
# to select which model is best from (1-variable model to 19-variable model)
# using:
# R2 statistics
summary(model.bs)$rsq
# increases from 32% when only 1 predictor is used to 54% when all predictors are used
# this is expected as R2 statistic increases monotonically as more variables are included.

# Plotting adj R2, RSS Cp and BIC 
par(mfrow=c(2,2))
plot(summary(model.bs)$adjr2, xlab = "Number of predictors",
     ylab = "Adjusted R2", type = 'l')
plot(summary(model.bs)$rss, xlab = "Number of predictors",
     ylab = "RSS", type = 'l')
plot(summary(model.bs)$cp, xlab = "Number of predictors",
     ylab = "Cp", type = 'l')
plot(summary(model.bs)$bic, xlab = "Number of predictors",
     ylab = "BIC", type = 'l')

# model with 11 predictors has the highest asdjusted R2
which.max(summary(model.bs)$adjr2)
# model with 19 predictors has the lowest RSS
which.min(summary(model.bs)$rss)
# model with 10 predictors has the lowest Cp
which.min(summary(model.bs)$cp)
# model with 6 predictors has the lowest BIC
which.min(summary(model.bs)$bic)

# plot these points as well
plot(summary(model.bs)$adjr2, xlab="Number of predictors",
     ylab="Adjusted R2", type='l')
points(11, summary(model.bs)$adjr2[11], col='red', cex=2, pch=20)

plot(summary(model.bs)$rss, xlab="Number of predictors",
     ylab="RSS", type='l')
points(19, summary(model.bs)$rss[19], col='red', cex=2, pch=20)

plot(summary(model.bs)$cp, xlab="Number of predictors",
     ylab="Cp", type='l')
points(10, summary(model.bs)$cp[10], col='red', cex=2, pch=20)

plot(summary(model.bs)$bic, xlab="Number of predictors",
     ylab="BIC", type='l')
points(6, summary(model.bs)$bic[6], col='red', cex=2, pch=20)

# regsubsets() has a built-in plot function to show best mdoel ranked on the same quantities
?plot.regsubsets

par(mfrow=c(1,1))
plot(model.bs, scale = "adjr2")
plot(model.bs, scale = "r2")
plot(model.bs, scale = "Cp")
plot(model.bs, scale = "bic")

# model with the lowest BIC (-150) is the six-variable model
# that contains only AtBat, Hits, Walks, CRBI, DivisionW, and PutOuts
# to see coef estimates of the best 6-variable model
coef(model.bs, 6)

# model with 10 variables has the lowest Cp
# to see coef estimates of the best 10-variable model
coef(model.bs, 10)

# Forward and Backward Step wise Selection
# ----------------------------------------
model.fwd <- regsubsets(Salary~., data = Hitters, nvmax = 19, 
                        method = "forward")
summary(model.fwd)
# 1-variable model included only CRBI
# 2-variable model included CRBI and Hits
# 3-variable model included CRBI, Hits and PutOuts 

model.bwd <- regsubsets(Salary~., data = Hitters, nvmax = 19, 
                        method = "backward")
summary(model.bwd)

# best seven-variable models identified by 3 methods are different
coef(model.bs, 7)
coef(model.fwd, 7)
coef(model.bwd, 7)

# Above we chose model based on adj R2, Cp, BIC 
# Now we choose models using the validation set and cross-validation approaches
# only use training data to perform model fitting (including variable selection)
# among all possible  n-variable models, which is the best?
# for example which 2 variables are the best predictors
# we chose using training data only

# first step: splitting the observations into a training set and a test set
?sample
# sample(x, size, replace = FALSE, prob = NULL)

set.seed(1)
# allocate each observation to train or test
train <- sample(c(TRUE, FALSE), nrow(Hitters), replace = T)
# when train is True (observation is in train set) test is False (observation not in test set)
# and viceversa
test <- (!train)

# apply regsubsets() to the training set in order to perform best subset selection.
train.bs <- regsubsets(Salary~., data=Hitters[train, ], nvmax=19)

# Compute test MSE for the best model of each model size
?model.matrix
# for building "X" from data
# if there is a categorical variable model.matrix converts them into dummy variables
# Take the test data, use all variables except Salary to predict Salary, 
# and convert everything into a numeric matrix that a model can work with
test.mat <- model.matrix(Salary~., data=Hitters[test, ])

# empty vector to save validation error
test.mse <- rep(NA, 19)

# for each number of variables i
# get the coef estimates from train.bs for the best model using i predictors
# get predicted salary by multiplying coef estimates with corresponding columns
# compute test MSE
for (i in 1:19){
  coefi <- coef(train.bs, i)
  pred <- test.mat[, names(coefi)]%*% coefi
  test.mse[i] <- mean((Hitters$Salary[test] - pred)^2)
}

# Note
# %*% matric multiplication

which.min(test.mse)
# so model with 7 variables is the best one with lowest test MSE
coef(train.bs, 7)
# Note
# this changes when we set different observations as train and test
# so how we split data affects model selection which is bad

# After getting the coef estimates for these 7 variables we use full dataset
full.bs <- regsubsets(Salary~., data=Hitters, nvmax=19)
coef(full.bs, 7)
# we see that the best 7-variable model on the full data set has a different
# set of variables than the best 7-variable model on the training set.
# so we still don't know which 7 variables are the best
# but we know we should use 7 variables
# several different sets of 7 variables predict almost equally well.

# Above we did the validation set approach:
# Split data once → train vs test
# Use test MSE to choose model size
# Result depends heavily on which split you happened to get

# Now we use cross validation
# Splits data into many folds
# Trains and tests multiple times
# Averages the error
# Gives a much more stable choice of model size


# Note 
train.bs
names(train.bs)
train.bs$call
# to get formula used 
train.bs$call[[2]]

# regsubsets() has no predict method
# to get predictions

predict.regsubsets <- function(regsubsets_object, your_data, var_size, ...){
  model_formula <- as.formula(regsubsets_object$call[[2]])
  mat <- model.matrix(model_formula, your_data)
  coefi <- coef(regsubsets_object, var_size)
  var_names <- names(coefi)
  mat[, var_names]%*% coefi
}

# K fold Cross Validation
k <- 10
set.seed(1)
# create a vector that allocates each observation to one of k = 10 folds
folds <- sample(1:k, nrow(Hitters), replace=T)
# a matrix to store error
# each column has the error of best n-variable model for all k runs
cv.errors <- matrix (NA ,k,19, dimnames =list(NULL , paste (1:19) ))

# loop for cross validation
# to select all obs not in fold 1
folds != 1

for (j in 1:k){
  best.model <- regsubsets(Salary~., Hitters[folds!=j, ], nvmax=19)
  for (i in 1:19){
    pred <- predict.regsubsets(best.model, Hitters[folds==j, ], i)
    cv.errors[j, i] <- mean((Hitters[folds==j, ]$Salary - pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
plot(mean.cv.errors ,type='b')
which.min(mean.cv.errors)

# so model with 10 variables has the lowest cross validation error
# perform best subset selection on the full data set
# in order to obtain the 10-variable model.

best.bs <- regsubsets(Salary~., Hitters, nvmax=19)
coef(best.bs, 10)

# selected model size (number of variables) can change a lot depending on
# which single train/test split you use, whereas with cross-validation
# the chosen model size is much more stable
# repeat each method a 100 times and see how stable model size is each time

# single train test split repeated 100 times
set.seed(1)
B <- 100
best.size.single <- rep(NA, B)

for (b in 1:B){
  train <- sample(c(TRUE, FALSE), nrow(Hitters), replace = TRUE)
  test <- !train
  
  train.bs <- regsubsets(Salary ~ ., data = Hitters[train, ], nvmax = 19)
  test.mat <- model.matrix(Salary ~ ., data = Hitters[test, ])
  
  test.mse <- rep(NA, 19)
  
  for (i in 1:19){
    coefi <- coef(train.bs, i)
    pred <- test.mat[, names(coefi)] %*% coefi
    test.mse[i] <- mean((Hitters$Salary[test] - pred)^2)
  }
  
  best.size.single[b] <- which.min(test.mse)
}

table(best.size.single)

# CV  repeated 100 times
set.seed(1)
B <- 100
k <- 10
best.size.cv <- rep(NA, B)

for (b in 1:B){
  folds <- sample(1:k, nrow(Hitters), replace = TRUE)
  cv.errors <- matrix(NA, k, 19)
  
  for (j in 1:k){
    best.model <- regsubsets(Salary ~ ., Hitters[folds != j, ], nvmax = 19)
    for (i in 1:19){
      pred <- predict.regsubsets(best.model, Hitters[folds == j, ], i)
      cv.errors[j, i] <- mean((Hitters[folds == j, ]$Salary - pred)^2)
    }
  }
  
  mean.cv.errors <- apply(cv.errors, 2, mean)
  best.size.cv[b] <- which.min(mean.cv.errors)
}

table(best.size.cv)

par(mfrow=c(1,2))
barplot(table(best.size.single),
        main="Single train-test split: high variance in model selection",
        cex.main=0.7)

barplot(table(best.size.cv), 
        main="CV: low variance in model selection", cex.main=0.7)

# to do all this with forward or backwars stepwise selection
# use method = "forward" pr "backward" in regsubsets()
