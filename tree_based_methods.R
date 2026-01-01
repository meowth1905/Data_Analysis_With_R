setwd("C:\\Neethu\\Introduction to statistical learning with applications in R")

library(ISLR)
library(MASS)
library(tree)
library (randomForest)
library(gbm)

# Load dataset
data("Carseats")
View(Carseats)
attach(Carseats)

any(is.na(Carseats))

hist(Sales, breaks = 30)

# Decision trees
# -----------------

# Fitting Classification Trees

# create a response variable
High <- factor(ifelse(Sales <= 10, "No", "Yes"))
# add this variable to Carseats
Carseats <- data.frame(Carseats, High)

# predict if sales is high or not (High) using all variables but Sales
# syntax of tree() is similar to lm()
tree1 <- tree(High~.-Sales, data = Carseats)

# lists the variables that are used as internal nodes in the tree
# number of terminal nodes
# training error rate
summary(tree1)

# plot the tree
plot(tree1)
text(tree1, pretty = 0)
# ShelveLoc is the most important variable because
# the first split separates good locations from medium and bad ones.

tree1

# to evaluate the performance of a classification tree on Carseats
# we need test error rate
# split the observations into a training set and a test set
# build the tree using train set
# evaluate its performance on the test data

dim(Carseats)
# we do a 50-50 split
set.seed(2)
train <- sample(1:nrow(Carseats), 200)
carseats.test <- Carseats[-train, ]
High.test <- High[-train]
tree.carseats <- tree(High~.-Sales, data = Carseats, subset = train)
tree.pred <- predict(tree.carseats, newdata = carseats.test, type = "class")
# type = "class" predict classes not probabilities

table(tree.pred, High.test)
(136 + 25) / 200
# model predicts correctly 80% of time

# Pruning Decision Trees
# Does Pruning help improve tree?

set.seed(3)

# cross-validation to see how pruning affects misclassification error.
# generates a sequence of subtrees from the largest tree to the root.
# for each subtree, it calculates the cross-validated error.
cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)
names(cv.carseats)

# number of terminal nodes (leaves) in the pruned tree.
cv.carseats$size

# cross-validated misclassification error
cv.carseats$dev

# complexity penalty used for pruning
cv.carseats$k

# how error was measured
cv.carseats$method

# plot the error rate as a function of both size and k
par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type='b')
plot(cv.carseats$k, cv.carseats$dev, type='b')

# prune the tree to obtain 5 node tree
prune.tree <- prune.misclass(tree.carseats, best = 5)

# plot pruned tree
plot(prune.tree)
text(prune.tree, pretty = 0)

# How well does this pruned tree perform on the test data set?
pruned.tree.pred <- predict(prune.tree, newdata = carseats.test, type = "class")
table(pruned.tree.pred, High.test)
(148 + 20) / 200
# now 84% of observations are correctly classified
# accuracy improved from 80% to 84%

# suppose i used 9 terminal nodes for pruning 
test.prune.tree <- prune.misclass(tree.carseats, best = 9)
test.pred <- predict(test.prune.tree, newdata = carseats.test, type = "class")
table(test.pred, High.test)
(139 + 22) / 200
# we get 80% same as tree without pruning


# Regression Trees
#

data(Boston)
View(Boston)
?Boston

any(is.na(Boston))
dim(Boston)

# fit a regression tree on Boston
# randomly sample half of rows in Boston
set.seed(1)
train <- sample(1: nrow(Boston), nrow(Boston) / 2)
length(train)

# goal:
# predict medv using all variables other than medv
# medv: median value of owner-occupied homes in $1000s.
boston.tree <- tree(medv ~ ., data = Boston, subset = train)
summary(boston.tree)

# variables used in building tree (predictors the tree found useful)
# "rm"    "lstat" "crim"  "age"  
# Note: this depends on which observations are in train
# for regression trees, deviance = MSE
# tree has 7 terminal nodes

# plot the tree
plot(boston.tree)
text(boston.tree, pretty = 0)

# check whether pruning help
boston.tree.cv <- cv.tree(boston.tree)
plot(boston.tree.cv$size, boston.tree.cv$dev, type = 'b')

# check with 6 nodes which gave the lowest cv error
boston.pruned <- prune.tree(boston.tree, best = 6)

plot(boston.pruned)
text(boston.pruned, pretty = 0)

# check which is better by predicting on test set
medv.pred <- predict(boston.tree, newdata = Boston[-train, ])
medv.test <- Boston[-train, "medv"]

plot(medv.pred, medv.test)
# minimal vertical spres, no systematic pattern, almost a straight line == good prediction 

mse <- mean((medv.test - medv.pred)^2)
mse

# using pruned tree
medv.pruned <- predict(boston.pruned, newdata = Boston[-train, ])

plot(medv.pruned, medv.test)

mse <- mean((medv.test - medv.pruned)^2)
mse
# not much improvement in MSE
# mse = 35
# rmse = 5.91
# predictions are off by about $5,900 on average.
medv.pruned[1]
# 21.38 + or - 5.9 thousand dollars (on average).

# Bagging 
# --------------------------
?randomForest
ncol(Boston)
# 14 - 1 (medv) 13 predictors 
# mtry number of variables to use at each split
# in bagging we use same set of predictors for all split
# here we use all predictors available
boston.bag <- randomForest(medv~., data = Boston, subset = train,
                           mtry=13, importance=T)
boston.bag

medv.bag <- predict(boston.bag, newdata = Boston[-train, ])

# %IncMSE:
# How much the mean squared error increases if you randomly permute that variable. Higher = more important.
# IncNodePurity:
# total decrease in node impurity that results from splits over that variable, averaged over all trees.
# Higher = more important

importance(boston.bag)
varImpPlot(boston.bag)
# rm and lstat are very important variables 

plot(medv.bag, medv.test)
abline(0,1)

mean((medv.test - medv.bag)^2)
# from 35(single tree) to 23 (using bagging - many trees and taking average)

# we can change number of trees to grow
# 25 trees 
boston.bag.25 <- randomForest(medv~., data = Boston, subset = train,
                           mtry=13, importance=T, ntree = 25)
medv.bag.25 <- predict(boston.bag.25, newdata = Boston[-train, ])
mean((medv.test - medv.bag.25)^2)
# from 23 to 22

# Random Forest
# be default uses p/3 variables for each split
set.seed(1)
rf.boston <- randomForest(medv~., data = Boston, subset = train,
                          mtry=6, importance=T)
medv.rf <- predict(rf.boston, newdata = Boston[-train, ])
mean((medv.test - medv.rf)^2)
# reduced to 19
# RF gives us an improvement over bagging

varImpPlot(rf.boston)
# rm and lstat are the two most important variables 

# Boosting = sequential trees on residuals.
# ---------
# each new tree is trained to predict the residuals (errors) of the current ensemble.
# The predictions are then combined (usually weighted) to improve overall fit.

# fit boosted regression trees
set.seed(1)
# for classification distribution = "bernoulli"
# 5000 trees of depth 4
# depth: longest path from the root to a leaf, so it’s the number of splits along that path.

boost.boston <- gbm(medv ~ ., data = Boston[train, ],
                    distribution = "gaussian",
                    n.trees = 5000,
                    interaction.depth = 4)

# rm and lstat are important variables 
summary(boost.boston)


# medv increases as rm increases
plot(boost.boston, i = "rm")

# medv decreases as lstat increases 
plot(boost.boston, i = "lstat")

# prediction on test set
medv.boost <- predict(boost.boston, newdata = Boston[-train, ],
                      n.trees = 5000)

# MSE
mean((medv.test - medv.boost)^2)
# 19 no improvement over RF

# shrinkage parameter lambda
# Smaller shrinkage → each tree makes a small adjustment, so the model learns more slowly.
# Larger shrinkage → each tree has more impact, learning faster 
# default is 0.1


boost.boston <- gbm(medv ~ ., data = Boston[train, ],
                    distribution = "gaussian",
                    n.trees = 5000,        # maximum number of trees
                    interaction.depth = 4, # depth of each tree
                    shrinkage = 0.2,       # learning rat  
                    cv.folds = 5)          # 5-fold cross-validation

# choose n.trees (number of trees)
best.ntree <- gbm.perf(boost.boston, method="cv")  # "OOB" for out-of-bag

medv.boost <- predict(boost.boston.test, newdata = Boston[-train, ],
                      n.trees = best.ntree)

mean((medv.test - medv.boost)^2)
# increased to 21 from 19

# we can also change n.trees used for prediction and
# interaction.depth 
# depth = 1 means additive models (no interactions)
# depth >=2 allowd interactions between predictors

set.seed(123)

shrinkages <- c(0.01, 0.05, 0.1, 0.2)
depths <- 1:5

results <- expand.grid(shrink=shrinkages, depth=depths, best.trees=NA, cv.error=NA)

for(i in 1:nrow(results)){
  model <- gbm(medv ~ ., data = Boston[train, ],
               distribution = "gaussian",
               n.trees = 5000,
               interaction.depth = results$depth[i],
               shrinkage = results$shrink[i],
               cv.folds = 5,  # 5-fold CV
               n.minobsinnode = 10)
  
  best_iter <- gbm.perf(model, method="cv")
  results$best.trees[i] <- best_iter
  results$cv.error[i] <- model$cv.error[best_iter]
}

# show best combination
results[order(results$cv.error),]

final.boost <- gbm(medv ~ ., data = Boston[train, ],
                    distribution = "gaussian",
                    n.trees = 839,        
                    interaction.depth = 3,
                    shrinkage = 0.05)          

medv.final <- predict(final.boost, newdata = Boston[-train, ],
                      n.trees = 839)

mean((medv.test - medv.final)^2)
# reduced to 18