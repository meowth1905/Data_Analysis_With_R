setwd("C:\\Neethu\\Introduction to statistical learning with applications in R")

library("ISLR")

data(Auto)
names(Auto)
attach(Auto)


# non linear relationship
plot(mpg, horsepower, pch=20)

lin.model <- lm(mpg ~ horsepower, data=Auto)
lin.model
summary(lin.model)
# horsepower is clearly an important variable to predict mpg

nonlin.model1 <- lm(mpg ~ poly(horsepower, 2), data=Auto)
summary(nonlin.model1)
# horsepower ^ 2 is a significant variable

nonlin.model2 <- lm(mpg ~ poly(horsepower, 3), data=Auto)
summary(nonlin.model2)
# horsepower ^ 3 is not a significant variable (p value=0.367)

anova(lin.model, nonlin.model1, nonlin.model2)
# nonlin.model1 is significantly better than lin.model
# nonlin.model2 is not significantly better than nonlin.model2

# Here we used p values 
# We can also do this using validation method

# Validation Set Approach
# -----------------------
# randomly dividing the available set of observations into
# a training set and a validation set or hold-out set (both with comparable size)
# model is fit on the training set and
# the fitted model is used to predict the responses for the observations in the validation set

set.seed(1)
dim(Auto)

# randomly choose half of the rows for training
train <- sample(392, 196)

# fit a linear regression model using only train data
lm.fit <- lm(mpg~horsepower, data = Auto, subset = train)
# OR
lm.fit <- lm(mpg~horsepower, data = Auto[train, ])

# get predicted mpg on test data
mpg.pred <- predict(lm.fit, newdata = Auto[-train, ])

# estimate test MSE
mean((mpg[-train] - mpg.pred) ^ 2) # 23

# Note if train is a logical vector use ! instead of -

# quadratic and cubic regressions.
quad.fit <- lm(mpg~poly(horsepower, 2), data = Auto[train, ])
mpg.pred <- predict(quad.fit, newdata = Auto[-train, ])
mean((mpg[-train] - mpg.pred) ^ 2) # 18.71646

cubic.fit <- lm(mpg~poly(horsepower, 3), data = Auto[train, ])
mpg.pred <- predict(cubic.fit, newdata = Auto[-train, ])
mean((mpg[-train] - mpg.pred) ^ 2) # 18.79401

# we get different values if we split data differently
set.seed(2)
train <- sample(392, 196)

lin.fit <- lm(mpg~horsepower, data = Auto[train, ])
mpg.pred <- predict(lin.fit, newdata = Auto[-train, ])
mean((mpg[-train] - mpg.pred) ^ 2) # 25.72651

quad.fit <- lm(mpg~poly(horsepower, 2), data = Auto[train, ])
mpg.pred <- predict(quad.fit, newdata = Auto[-train, ])
mean((mpg[-train] - mpg.pred) ^ 2) # 20.43036

cubic.fit <- lm(mpg~poly(horsepower, 3), data = Auto[train, ])
mpg.pred <- predict(cubic.fit, newdata = Auto[-train, ])
mean((mpg[-train] - mpg.pred) ^ 2) # 20.38533

# Even though MSE we got for different seed is different
# we got the same result
# quadratic model is better than linear model
# there is no improvement in cubic model compared to quadratic model

# lm() and glm()
lm.model <- lm(mpg~horsepower, data = Auto)
coef(lm.model)
# ==
glm.model <- glm(mpg~horsepower, data = Auto)
coef(glm.model)

# we can use glm to perform classification (logistic regression)
# using argument family="binomial"

# Leave-One-Out Cross-Validation
# ------------------------------
# instead of creating two subsets of comparable size, a single observation
# is used for the validation set, and the remaining observations
# make up the training set
# repeat the procedure n (number of observations) times and compute MSE each time
# test MSE = average MSE_1, MSE_2 ...., MSE_n

library(boot)

glm.model <- glm(mpg~horsepower, data = Auto)
cv.error <- cv.glm(Auto, glm.model)
names(cv.error)
# to get cross validation results
# we get 2 values, first is the standard and 2nd is the boas corrected version
cv.error$delta 

# do this for 5 polynomial regressions
cv.error <- rep(0, 5)

for (i in 1:5){
  glm.model <- glm(mpg~poly(horsepower, i), data = Auto)
  cv.error[i] <- cv.glm(Auto, glm.model)$delta[1]
}
cv.error
# quadratic fit is better than simple linear fit
# but higher-order polynomials bring no improvements

# k-Fold Cross-Validation
# -----------------------
# randomly divide the set of observations into k groups, or folds, of approximately same size
# first fold is treated as a validation set, and
# the method is fit on the remaining k -1 folds
# MSE_k is computed on the observations in the kth fold
# repeat this k times
# test MSE = average MSE_1, MSE_2 ...., MSE_k

# Leave-One-Out Cross-Validation is a special case where k = 1

set.seed(17)

# 10-fold CV (k = 10)
cv.error.ten <- rep(0, 5)

for (i in 1:5){
  glm.model <- glm(mpg~poly(horsepower, i), data = Auto)
  cv.error.ten[i] <- cv.glm(Auto, glm.model, K=10)$delta[1]
}
cv.error.ten
# Not much evidence that using higher-order polynomial terms 
# lower test error than simply using a quadratic fit.





# Bootstrap
# ---------
# assess the variability of the coefficient estimates and predictions from a statistical learning method.
# generate B bootstrap samples by sampling with replacement from the original sample.
# for each bootstrap sample, compute the estimator.
# we have now B estimates
# we can use them to compure average, standard deviation, CI 

# average of the B bootstrap estimates is usually very close to
# what you’d get from many real population samples.
# that is Bootstrap mimics the sampling variability.


# create a function that takes data and indices (a subset of data)
# fit a linear model on the rows = indices
# and return the coefs slope and intercept

boot.fun <- function(data, indices){ # {} are optional
  return(coef(lm(mpg~horsepower, data=data, subset=indices)))
}

dim(Auto)
boot.fun(Auto, 1:392)

# perform linear fit on observations randomly sampled from Auto with replacement
set.seed(1)
boot.fun(Auto, sample(392, 392, replace = T))

# if we run again we get a different coef estimates
boot.fun(Auto, sample(392, 392, replace = T))

# we can automate this and run this for any number of times 
# for example:
# 1000 bootstrap iterations
# for each iteration, boot() creates a vector of row indices
# of length = nrow(Auto)
# sampled with replacement

boot(Auto, boot.fun, 1000)
# bootstrap estimate for SE(ß0) = 0.840134476
# bootstrap estimate for SE(ß1) = 0.007327413

# standard errors for the regression coefficients in a linear model can also obtained from
summary(lm(mpg~horsepower, data=Auto))
# SE(ß0) = 0.717499
# SE(ß1) = 0.006446
# different from the estimates obtained using the bootstrap
# Note bootstrap estimates are consiederd more accurate

# bootstrap estimates of coef estimates for a quadratic fit
boot.fun <- function(data, indices){ # {} are optional
  return(coef(lm(mpg~poly(horsepower, 2), data=data, subset=indices)))
}

boot(Auto, boot.fun, 1000)

summary(lm(mpg~poly(horsepower, 2), data=Auto))
