# R Lecture 8
# Neethu Raj
# Based on lecture series "Data analysis using R" by Prof. Danny Arends
# ==========================================

setwd("C:\\Data_Analysis_With_R")  # Set working directory

# Regression
# ==========================================

# Estimate relationship among Dependent and independent variables.
# Used to do predictions.
# 
# Dependent Y: output or the effect. What we want to predict.
# Independent X: input or the cause. What we use to predict.
# 
# Y ~ f(X,ß)
# function depends on parameters (coefficients) ß that must be estimated from data.
# Regression is about learning these parameters which define function f.
#
# Constants:
# N: number of observations
# k: number of unknown parameters (number of independent variables + the intercept.)
#

# Inputs can have any distributions.
# Normality assumption is on the error term.

# Assumptions for the model to be valid
# * indicate very important

# 1. Sample should be represent the population. *
# 2. Error (observed Y - predicted Y) should have a mean of 0 and normal distribution.*
# 3. Independent variables are measure without error: In reality never possible as we cannot measure with infinite precision. But do the utmost best.
# 4. Predictors (independent variables) must be linearly independent. * 
# 5. Errors are not correlated to any of the dependent or independent variables.

# 6. Homoscedasticity: variance of data should be approx equal to the range of predictors:
#    errors should not increase as X increases.
#    i.e, there is no correlation between X and error (Heteroscedasticity).
#    If we see this generally data is log transformed.
#  
# 
# Linear Regression
# ==========================================
#
?lm

data(airquality)
head(airquality)

# Predict Ozone using Temp
model <- lm(formula = Ozone ~ Temp, data = airquality)
model
# we get
# Coefficients:
#(Intercept)         Temp  
# -146.995        2.429 

intercept <- model$coefficients[1]
temp.coeff <- model$coefficients[2]

#
# Intercept is Y when all X = 0
# For every degree rise in the temperature, Ozone concentration will go up by 2.429.

# Ozone.predicted <- -146.995  + 2.429 * Temp
# Error <- Ozone.observed - Ozone.predicted

# Which predictors have the most influence on Ozone? (***).
# How good the model is overall? How well the model explains the variability in the data?
# That is how much of the variability in Ozone is explained by Temp. (look at Multiple R-squared)

summary(model)                             

with(airquality, {
  plot(Temp, Ozone,
       xlab = "Temperature (degrees F)",
       ylab = "Ozone (ppb)",
       col = "steelblue",
       pch = 20, 
       main = "Ozone depenency on Temperature",
       cex.main = 0.8)    # smaller title
  abline(a = intercept, b = temp.coeff, col = "red", lwd = 2)
})

# Confidence Interval (CI)
# ==========================================
#
#
# 1. Confidence Intervals for the regression coefficients (model parameters):
# The range in which the true coefficient (like the effect of Temp on Ozone) likely lies.
#
# ==========================================

# Calculate margin of error
# we need critical value and standard error (from model summary).
#
# To calculate critical value: 
#
# For a two-sided confidence interval, you use the quantile corresponding to 
# 1 − alpha/2  of the relevant distribution (for linear regression, we use T-distribution)
# or just alpha for one-sided CI

# For 96% CI, alpha = 0.05 (For 99%, alpha is 0.01)
#
# Degrees of freedom N -2, N is number of independent observations.
# why -2 because estimated 2 parameters (intercept and slope of Temp).

# critical.value <- qt(0.975, N -2) 
#
# Margin of error <- critical.value * standard error (from model$coefficients)

# CI of the Temp coefficient

model.summary <- summary(model)
temp.error <- model.summary$coefficients["Temp", "Std. Error"]

critical.value <- qt(0.975, nrow(airquality) - 2)
critical.value

margin.error <- critical.value * temp.error
margin.error

# CI is our estimate  - margin.error to estimate + margin.error
model.summary$coefficients["Temp", "Estimate"] + c(-1, 1) * margin.error

# OR in R by
?confint
confint(model, "Temp", level = 0.95) # 95% CI

# Confidence Intervals for the Predicted mean 
# a range where the true mean response for a given X likely lies.
# 
# ==========================================

temp.range <- seq(min(airquality$Temp, na.rm = TRUE),
                  max(airquality$Temp, na.rm = TRUE),
                  by = 1)

?predict
temp.range.df <-  data.frame(Temp = temp.range)      # because predict need data.frame

predicted.CI <- predict(model,temp.range.df,interval = "confidence")
head(predicted.CI)

with(airquality, {
  plot(Temp, Ozone,
       xlab = "Temperature (degrees F)",
       ylab = "Ozone (ppb)",
       col = "darkgrey",
       pch = 20, 
       main = "Ozone depenency on Temperature",
       cex.main = 0.8)    # smaller title
  
  # Add regression line
  abline(a = intercept, b = temp.coeff, col = "red", lwd = 2)
  
  # Add CI lines
  lines(temp.range, predicted.CI[, "lwr"], col = "forestgreen", lty = 2, lwd = 2)
  lines(temp.range, predicted.CI[, "upr"], col = "forestgreen", lty = 2, lwd = 2)
  
})

# Or use R package
library(visreg)
visreg(model, alpha = 0.05)


# Note
# For each X, Y values vary due to random error.
# Regression line shows the estimated mean Y for each X.
# CI shows Where do we expect the average value of Y to be for this X?

# We expect 95% of data within this CI but its not the case here
# Because Multiple R-squared:  0.4877
# so only 95% of the 48% end up within the interval

# why CI shape is not uniform and is narrow in the middle?
# CI is actually the infinite number of regression lines with slope or Temp coefficient
# ranging from [ 1.968082 2.889325] (from CI of the estimate if Temp coefficient)


# Residuals
# ==========================================
#
# Distance from the regression line
# Goodness of fit
# Measure of how well regression lines fit the data.
# Aim of regression is to minimize sum of square of residuals.
# sum of (observed y - predicted y)**2 for all observations
# By minimizing this we get a maximum likelihood model.

# Visualize Residuals
#
# In regression you either:
# 1. Remove rows with NA (complete-case analysis), or
# 2. Impute missing values before fitting the model.

airquality.clean <- airquality[rowSums(is.na(airquality)) == 0, ]  # keep only rows with no NA in any of the columns

lm.model <- lm(formula = Ozone ~ Temp, data = airquality.clean) # Fit the model
lm.model.predict <- predict(lm.model)                           # Predict

# Note
# If we want to predict for a new data which was not seen by the model
# predict(lm.model, newdata = list(Temp = 80))

with(airquality.clean, 
     {plot(Temp, Ozone, pch = 20, col = "grey57", )
       abline(lm.model, col = "forestgreen", lwd = 2)
       
       for (i in 1:nrow(airquality.clean)) {
         lines(x = c(airquality.clean[i, "Temp"], airquality.clean[i, "Temp"]),
               y = c(airquality.clean[i, "Ozone"], lm.model.predict[i]),
               col = "red", lty = 2)}
       })

# lines(x coordinates, y coordinates)
# X coordinates: same value twice, it starts and ends at the same X.
# Y coordinates: go from observed Y to predicted Y. This shows the residual.


# Multiple Linear Regression
# ==========================================
#
# More than 1 predictors
#
head(airquality)
mlm.model <- lm(formula = Ozone ~ Temp + Wind, data = airquality.clean)    # lm automatically remove any rows with NA
summary(mlm.model)

# Wind is also important predictor (***)
# Multiple R-squared:  0.5814 increases from 48% with just Temp as the only predictor.
# Estimate for Temp changed.
# Because there is a relationship between Temp and Wind as well.

with(airquality.clean,
     {
       ozone.predict <- predict(mlm.model)
       plot(Ozone, Temp)
       points(ozone.predict, Temp, pch = 2, col = "red")
       legend("bottomright",                  # coordinates in your plot units
              legend = c("Observed", "Predicted"),
              pch = c(1, 2),
              col = c(2, "red"),
              bty = "n",                         # no box
              y.intersp = 0.7)                   # reduce vertical spacing
       
     })

# To also add effects of interaction between predictors
# X1 and X2 are two predictors, interaction term is X1:X2
# to add effects of X1, X2 and their interaction
# formula = Y ~ X1 + X2 + X1:X2
# or juts formula = X1 * X2

new.model <- lm(formula = Ozone ~ Temp * Wind, data = airquality.clean)
summary(new.model)

# Quadratic Regression
# ==========================================
#
# Fits a linear model in the coefficients, but the predictor can include.
# So instead of Y = a + b * X
# Y = a + b1 * X + b2 * X squared
# In R we do this I(X^2)

summary(lm(formula = Ozone ~ Temp + I(Temp^2), data = airquality.clean))

# quadratic term is ***

plot(Ozone ~ Temp, data = airquality.clean)
curve(292.95885 + -9.22680 * x + 0.07602 * x^2, add = T, col = "red")


# Which model to choose?
#
# Should never include parameters unless it significantly improve predictions.
# AIC criteria
# AIC is not a statistical test, but a guideline
# more parameters, more model is penalized  
# lower AIC better model
# difference in AIC ΔAIC should be atleast 10 AIC points 
?AIC

lm1 <- lm(Ozone ~ Temp, airquality.clean)
lm2 <- lm(Ozone ~ Temp + I(Temp^2), airquality.clean)
lm3 <- lm(Ozone ~ Temp + Wind, airquality.clean)
lm4 <- lm(Ozone ~ Temp * Wind, airquality.clean)

AIC(lm1, lm2, lm3, lm4)

# here ΔAIC of lm4 is > 10 so lm4 is the best out of 4 models


# “All models are wrong, but some are useful.” — George E. P. Box