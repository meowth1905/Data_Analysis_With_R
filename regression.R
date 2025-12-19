setwd("C:\\Neethu\\Data_Analysis_With_R\\R_learning")
setwd("D:\\Introduction to statistical learning with applications in R")

library(ISLR)
library(MASS)

load.libs <- function(){
  library(ISLR)
  library(MASS)
  print (" The libraries have been loaded .")
}
load.libs()

load.libraries <- function(...){
  for (each in list(...)) library(each, character.only = TRUE)
  print (" The libraries have been loaded .")
}
load.libraries("ISLR", "MASS")


old.par <- par()

?Boston
fix(Boston)
View(Boston)
names(Boston)
dim(Boston)

# to fit a simple linear regression model
# lm()
# lm(design = response ~ predictor, data = dataset.name)

attach(Boston)

# Simple Linear Regression
#
# 1 predictor
# predict medv using lstat
# since we attach dataset we can just use
model1 <- lm(medv~lstat)
summary(model1)

# infromation stored in model1
names(model1)

# to get coefficient estimates
model1$coefficients

# safer to use coef()
coef(model1)

# to get confidence interval of the estimates
confint(model1)

# to produce confidence intervals and prediction intervals 
# for the prediction of medv for a given value of lstat
# new value of lastat must be a data.frame

# What is the average price of houses when lstat = 5, 10 , 15?
# what is its CI
predict(model1, data.frame(lstat=c(5, 10, 15)), interval="confidence")

# What will the price be for one single house with lstat = 5, 10, 15?
# what is its PI
predict(model1, data.frame(lstat=c(5, 10, 15)), interval="prediction")

plot(lstat, medv, col=3, pch=20)
abline(model1, col=2, lwd=2)

# Diagnostic plots
par(mfrow= c(2,2))
plot(model1)

residuals <- residuals(model1)
# or model1$residuals

# to get studentized residuals
std.residuals <- rstudent(model1)

# residual plot against predictor (since there is only 1)
par(mfrow=c(1,1))
plot(lstat, residuals)

# there is pattern in residual plot
# proof of nonlinearity

# standardized residual plot
plot(lstat, std.residuals)

# residual plot against fitted values
plot(predict(model1), residuals(model1), xlab="predicted meedv", ylab="residuals")
names(model1)
# or
plot(model1$fitted.values, residuals(model1), xlab="predicted meedv", ylab="residuals")

# to get leverage   statistics
plot(hatvalues(model1))

# index of the observation with largest leverage statistic.
which.max(hatvalues(model1))

# Multiple Linear Regression
#
model2 <- lm(medv~lstat + age,data = Boston)
summary(model1)

# to use all 13 predictors 
model3 <- lm(medv~., data=Boston)
summary(model3)

# to get individual component of summary(model)
# to see what is available
?summary.lm

summary(model3)$r.squared	

# Residual standard error
summary(model3)$sigma

# VIF variance inflation factor
# VIF tells us whether a predictor is highly correlated with other predictors
# High VIF for a predictor -> its coefficient’s variance is much inflated
# due to correlation with other predictors.

# install.packages("car")
library(car)

vif(model3)
#
# Predictors with VIF > 5 (rad, tax) are likely highly correlated with other predictors.

# from summary(model3) age has a high p value
# keep all excluding age
model4 <- lm(medv~.-age, data=Boston)
summary(model4)

# excluding age and indus
model4 <- lm(medv ~ . -age -indus, data=Boston)
summary(model4)

# or 
model4 <- update(model3, ~.-age-indus)

# add interaction term
summary(lm(medv~lstat*age, data=Boston))

# Non linear transformation of predictors

# polynomial transformations
# to use lstat and lstat squared as predictors
model5 <- lm(medv ~ lstat + I(lstat^2))
summary(model5)

# model1 used only lstat
summary(model1)

# to compare model1 and 5 (which also used lstat^2)
anova(model1, model5)

par(mfrow=c(2,2))
plot(model1)
plot(model5)

par(mfrow=c(1,2))
plot(predict(model1), residuals(model1), xlab="predicted meedv", ylab="residuals", main="model1")
plot(predict(model5), residuals(model5), xlab="predicted meedv", ylab="residuals", main="model5")

# model5 residual plot only little pattern

# to add higher order polynomials
# poly()
# i need to add lstat ^ 1 2 3 4 and 5

model6 <- lm(medv ~ poly(lstat, 5))
summary(model6)

# after 5 not significant
summary(lm(medv ~ poly(lstat, 6)))

names(Boston)
?Boston
# rm: average number of rooms per dwelling.

# log transformation
model7 <- lm(medv~log(rm), data=Boston)
summary(model7)

# Qualitative Predictors
#
data("Carseats")
head(Carseats)
attach(Carseats)

names(Carseats)
?Carseats

unique(ShelveLoc)
# Given a qualitative variable
# R generates dummy variables automatically.

# predict Sales using all predictors + interaction between few
model8 <- lm(Sales ~. + Income:Advertising + Price:Age, data=Carseats)
summary(model8)

# to know coding used for qualitative predictors
contrasts(ShelveLoc)

# ShelveLocBad is the baseline
# from summary ShelveLocGood 4.8486762
# a good shelving location is associated with high sales (relative to a bad location).
# 
# ShelveLocMedium has a smaller positive coefficient 1.9532620
# a medium shelving location leads to higher sales than a bad shelving location.
# but lower sales than a good shelving location.



# To test whether a qualitative predictor as whole is significant
# example: ShelveLoc

# 1. fit a model without ShelveLoc
model.reduced <- lm(Sales ~ . + Income:Advertising + Price:Age - ShelveLoc, data = Carseats)

# 2. fit the full model (with ShelveLoc)
model8

# # Does the factor ShelveLoc (as a whole) improve the model?
# 3. Perform F test
anova(model.reduced, model8)

# get p value
anova(model.reduced, model8)[2, "Pr(>F)"]

# which is < 0.05 means ShelveLoc is a sig predictor

# For quantitative predictors,
# we can simply look at the individual coefficient p-values in the summary output.


test.model <- lm(Sales ~ Income, data=Carseats)
test.model   

# Income coef is 0.01533

scaled.income <- Income*10
test.model2 <- lm(Sales ~ scaled.income, data=Carseats)
test.model2

# Income coef is 0.001533

# scaling the variables has no effect in linear regression
# multiplying a variable by a factor of c will simply lead to
# multiplication of the corresponding coefficient estimate by a factor of 1/c