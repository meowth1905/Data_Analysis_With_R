setwd("C:\\Neethu\\Introduction to statistical learning with applications in R")

library(ISLR)
library(MASS)
library(class)

# Load data
data("Default")
?Default

# Classification problem
# classify students students by default status based on their income and balance

attach(Default)
dim(Default)

# plot annual incomes and monthly credit card balance of some students
set.seed(2)
sampled_data <- Default[sample(nrow(Default), 1000), ]

with(sampled_data, 
     plot(balance, income,
          col = ifelse(default=="No", "skyblue", "coral"),
          pch = ifelse(default=="No", 21, 3),
          xlab = "Balance",
          ylab = "Income")
     )

legend("topright",
       legend = c("Default: No", "Default: Yes"),
       col = c("skyblue", "coral"),
       pch = c(21, 3),
       bty = "n",
       y.intersp = 0.6,
       cex = 0.7)

# distribution of income and balance split by default
boxplot(income~default,
        col = c("skyblue", "coral"),
        xlab = "Default",
        ylab = "Income")

boxplot(balance~default,
        col = c("skyblue", "coral"),
        xlab = "Default",
        ylab = "Balance")

# distribution of balance for student and non students
boxplot(balance~student,
        col = c("skyblue", "coral"),
        xlab = "Student Status",
        ylab = "Balance")

# distribution of income for student and non students
boxplot(income~student,
        col = c("skyblue", "coral"),
        xlab = "Student Status",
        ylab = "Income")



data("Smarket")
?Smarket
# cor need numeric columns (so removed 9th column)
corr <- cor(Smarket[, -9])
heatmap(corr)
# correlation between Year and Volume

attach(Smarket)
# Volume is increasing over time
plot(Volume)
plot(Year, Volume)

# Using Lag1 through Lag5 and Volume predict Direction
# fit Logistic Regression
#--------------------------

log.reg <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
               data = Smarket, family = binomial)

coef(log.reg) 
summary(log.reg)
names(summary(log.reg))
# get only the p values of coef estimates
summary(log.reg)$coefficients[, 4]
# none of the coef estimates have p value < 0.5
# no significant association between predictors and response variable

# type = "response" to output probabilities P(Y=1|X) instead of logits
# since no data is provided 
# probabilities are computed on training data that was used to fit log.reg
probs <- predict(log.reg, type = "response")
# probability that marker will go Up 
probs[1:10]
# why is this probability that market go Up not Down
contrasts(Direction)
# because R created dummy variable with a 1 for Up

# to convert these predicted probabilities into class labels, Up or Down
pred.class <- rep("Down", 1250)
# assign class Up for observation where P(class=Up) > 0.5
pred.class[probs > 0.5]="Up"
pred.class[1:10]

# Confusion matrix
table(pred.class, Direction)
(145 + 507) / 1250
# 0.5216
# OR use
mean(pred.class == Direction)
# 0.5216
# logistic regression correctly predicted market direction only 0.52% of the time
# but we trained and tested the model on the same 1250 observations

# to better assess the model,
# we need separate train and test data sets
unique(Year)
# we use 2005 data for test and rest for train
train <- Year < 2005
test.dat <- Smarket[!train, ]
dim(test.dat)
# 252 observations

# fit logistic regression model on train set
# subset = train
# train can be wither a logical Year < 2005, glm() uses only rows where train == True
# or
# a numerical vector of row indices which(Year < 2005) 
log.reg <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
               data = Smarket, family = binomial,
               subset = train)

# test model of data from 2005
pred.probs <- predict(log.reg, test.dat, type = "response")

pred.class <- rep("Down", 252)
pred.class[pred.probs > 0.5]="Up"

table(pred.class, test.dat$Direction)
mean(pred.class == test.dat$Direction)
#  0.4801587
# test error rate
mean(pred.class != test.dat$Direction)
# 0.5198413
# worse than random guessing so not good
# not all that surprising, given that one would not generally expect to be
# able to use previous days’ returns (Lag1 through 5) to predict future market performance

summary(log.reg)$coef[, 4]
sort(summary(log.reg)$coef[, 4])
# remove all variables other than Lag1 and 2 

log.reg <- glm(Direction ~ Lag1+Lag2,
               data = Smarket, family = binomial,
               subset = train)

pred.probs <- predict(log.reg, test.dat, type = "response")
pred.class <- rep("Down", 252)
pred.class[pred.probs > 0.5]="Up"
table(pred.class, test.dat$Direction)
mean(pred.class == test.dat$Direction)
# 0.5595238
# model prediction is correct 56% of the time
# test error rate is less than when all variables are used
mean(pred.class != test.dat$Direction)
0.4404762

# days where model predict market will go Up
106 + 76
# out of these days, market really go up in
# True positive / Predicted positives
# that is when the model predicts Up, how often is it actually Up?”
# precision = How reliable positive predictions are
106 / (106 + 76)

# given new values of Lag1 and Lag2 predict Direction
predict(log.reg, newdata = data.frame(Lag1=c(1.3, 1.5),Lag2=c(1.9, -0.9)),
        type="response")


# LDA
# ---
# train: observations before 2005 
# train <- Year < 2005
lda.fit <- lda(Direction ~ Lag1+Lag2, data = Smarket,subset = train)
lda.fit

# Prior probabilities of groups:
# Down       Up 
# 0.491984 0.508016 
# 49.2% of training observations correspond to days during which the market went down

# group means
# average of each predictor within each class

#   Lag1        Lag2
# Down  0.04279022  0.03389409
# Up   -0.03954635 -0.03132544

# this suggests there is a tendency for Lag1 and 2 (previous 2 days)
# returns to be
# negative on days when market increases (Up)
# and positive when market decreases (Down)

# Coefficients of linear discriminants:
# LD1 = −0.64 * Lag1 − 0.514 * Lag2 + intercept
# LD1 is a a linear score that measures how strongly a data point belongs to one class versus another.
# small then LDA classifier will predict Down
# large LDA will predict Up
# 

lda.pred <- predict(lda.fit, test.dat)
names(lda.pred)
lda.class <- lda.pred$class
table(Predicted = lda.class, Actual = test.dat$Direction)

# how model assigned Up and Down
# observations where model assigned Up
sum(lda.pred$posterior[,2] >=0.5)
# 76 + 106 = 182
# observations where model assigned Down
sum(lda.pred$posterior[,2] < 0.5)
#  35 + 35 = 70

# we can also use a threshold other than 0.5
mean(lda.class == test.dat$Direction)
# 0.5595238
# same as logistic regression

# Quadratic Discriminant Analysis (QDA)
#-----------------------------------------
qda.fit <- qda(Direction ~ Lag1+Lag2, data = Smarket,subset = train)
qda.fit

qda.pred <- predict(qda.fit, test.dat)
qda.class <- qda.pred$class
table(Predicted = qda.class, Actual = test.dat$Direction)
mean(qda.class == test.dat$Direction)
# 0.5992063
# almost 60% time model predict correctly, so performed better than LDA

# KNN
# ---
# we need 4 inputs
# 1. matrix containing the predictors associated with the training data
# 2. matrix containing the predictors associated with the test data
# 3. vector containing class labels for training observations
# 4. K, number of nearest neighbors to be used by the classifier

train.X <- cbind(Lag1, Lag2)[train, ]
test.X <- cbind(Lag1, Lag2)[!train, ]
train.labels <- Direction[train]

# set random seed before knn()
# because if several observations are tied as neighbors knn() choose randomly from them
set.seed(1)
knn.pred <- knn(train.X, test.X, train.labels, k=1)
table(Predicted = knn.pred, Actual = test.dat$Direction)
mean(knn.pred == test.dat$Direction)
# 0.5, only 50% of observations are correctly predicted

set.seed(1)
knn.pred <- knn(train.X, test.X, train.labels, k=3)
table(Predicted = knn.pred, Actual = test.dat$Direction)
mean(knn.pred == test.dat$Direction)
# 0.53

set.seed(1)
knn.pred <- knn(train.X, test.X, train.labels, k=10)
table(Predicted = knn.pred, Actual = test.dat$Direction)
mean(knn.pred == test.dat$Direction)
# 0.53
# increasing k shows no improvement
# so for Smarket QDA provides the best results (60%)

# KNN another example
#--------------------

data("Caravan")
?Caravan
fix(Caravan)
dim(Caravan)
#  5822   86
# 85 predictors
# response variable is Purchase
attach(Caravan)
summary(Caravan)
348 / (348 + 5474) # only 6% of people purchased Caravan policy

# Note 
# in KNN scale of variables matters
# bring all variables to comparable scale (mean=0 and sd=1)
# 86th is the response variable
standardized.X <- scale(Caravan[, -86])
var(Caravan[, 1])
var(standardized.X[, 1])

# split data into train and test
set.seed(1)
test <- sample(1:nrow(standardized.X), 1000)

train.X <- standardized.X[-test, ]
test.X <- standardized.X[test, ]
train.labels <- Purchase[-test]
test.labels <- Purchase[test]

knn.pred <- knn(train.X, test.X, train.labels, k=1)
table(Predicted = knn.pred, Actual = test.labels)
mean(knn.pred == test.labels)
# 89% of the time model prediction is correct