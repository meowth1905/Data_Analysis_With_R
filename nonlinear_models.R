setwd("C:\\Neethu\\Introduction to statistical learning with applications in R")

library("ISLR")

data(Wage)
attach(Wage)

View(Wage)
# no missing values in any column 
any(is.na(Wage))
# no NA in wage column
any(is.na(wage))

hist(wage, breaks = 100)

nrow(Wage)

# 79 high earners (out of 3000 people)
length(which(wage > 250))

# non linear relation between age and wage
plot(age, wage)


# Polynomial Regression
# ---------------------
poly.fit <- lm(wage~poly(age, 4), data=Wage)
summary(poly.fit)
names(summary(poly.fit))

# to get just the coefficients
coef(summary(poly.fit))
# OR
summary(poly.fit)$coefficients

# poly() returns a matrix, each column is a linear combination of
# age, age^2, age^3, age^4

# raw = T obtain age, age^2, age^3, age^4 directly
poly.fit2 <- lm(wage~poly(age, 4, raw = T), data=Wage)
coef(summary(poly.fit2))

# we get different coef estimates
# but predicted values will be the same

# we can also use
lm(wage~age, I(age^2), I(age^3), I(age^4), data=Wage)
# or
lm(wage~cbind(age, age^2, age^3, age^4), date=Wage)

# create a grid of values for age at which we want predictions
age.grid <- seq(from=min(age), to=max(age))

?predict.lm
preds <- predict(poly.fit, newdata = data.frame(age=age.grid),
                     se.fit = T)
names(predicted)
# 95% CI interval
se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)

# plot the data and fit
plot(age, wage, xlim = range(age), cex=0.5, col="darkgray")
title("Degree-4 polynomial", cex=0.7)
lines(age.grid, predicted$fit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)

preds2 <- predict(poly.fit2, newdata = data.frame(age=age.grid),
                 se.fit = T)
# using raw=T in poly will not affect fitted values
max(abs(preds$fit - preds2$fit)) # ~ 0

# When performing polynomial regression
# we need to decide which degree of polynomial to use 
# we can do this by
# 1. hypothesis test
# we fit models ranging from linear to a degree-5 polynomial (M1 to M5)
# null: M1 is sufficient to explain data
# alternate: M1 is not sufficient, more complex model is needed

M1 <- lm(wage~age, data=Wage)
M2 <- lm(wage~poly(age,2), data=Wage)
M3 <- lm(wage~poly(age,3), data=Wage)
M4 <- lm(wage~poly(age,4), data=Wage)
M5 <- lm(wage~poly(age,5), data=Wage)

anova(M1, M2, M3, M4, M5)
# p value comparing linear model to quadratic model is low, so
# M2 is better than M1 or M1 is not sufficient
# p value comparing quadratic to cubic model is also low
# M3 is better than M2
# p value comparing cubic to quartic model is also significant (.)
# we can conclude either M3 or M4 is a better choice than M1 and M2

# 2. Cross validation
library(boot)

set.seed(1)

cv.err <- rep(NA, 5)

for (d in 1:5) {
  fit <- glm(wage ~ poly(age, d), data = Wage)
  cv.err[d] <- cv.glm(Wage, fit, K = 10)$delta[1]
}

cv.err
# M4 and M5 got the lowest cv error

# Classification Task
# predict whether an individual earns more than $250,000 per year

# we need a qualitative response
I(age > 250)
# wage > 250 gives us a logical vector
# family='binomial' coerces TRUEs to 1 and FALSEs to 0
plot(age, I(wage > 250), xlim = range(age))

fit <- glm(I(wage > 250)~poly(age, 4), data=Wage, family = binomial)

# make predictions
preds <- predict(fit, newdata = data.frame(age=age.grid), se.fit = T)

# when we use glm(.., family='binomial'), the model does not directly predict probabilities.
# it predicts logits, logit = log(p/(1-p)) which range from -Inf to Inf
preds$fit
# probabilities are not -ve
# also se.fit gives the standard error of the logit, not the probability
preds$se.fit 

# so we need to compute p = exp(logit) / (1 + exp(logit))
pfit <- exp(preds$fit) / (1 + exp(preds$fit))
se.bands.logit <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
se.bands <- exp(se.bands.logit) / (1 + exp(se.bands.logit))

# plot the data and fit
# jitter() to jitter the age values a bit so that observations with same age
# do not cover each other up.
plot(jitter(age), I(wage > 250), xlab="Age", ylab="Pr(wage > 250)",
     col="darkgrey", pch="|", cex=0.5, ylim=c(0,1))
lines(age.grid, pfit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)

# Step function regression OR Piecewise constant regression
# -------------

# to fit a step function 
table(cut(age, 4))

# model to predict mean age
fit <- lm(wage~cut(age, 4), data=Wage)
summary(fit)

# Note:
# cut(age, 4) creates a factor with 4 levels: (17.9,33.5]   (33.5,49]   (49,64.5]   (64.5,80.1]
# one of the level is chosen as reference  (17.9,33.5]
# wage = β0  
# + β1 * I(age in (33.5,49])  
# + β2 * I(age in (49,64.5])  
# + β3 * I(age in (64.5,80.1])
# where ß0 = mean wage for the baseline group (17.9,33.5]
# ß1 = difference in mean wage between (33.5,49] and (17.9,33.5]
# β2 = difference in mean wage between (49,64.5] and (17.9,33.5]
# ...
coef(summary(fit))


fit.step <- glm(I(wage > 250) ~ cut(age, 4),
                data = Wage,
                family = binomial)

# this model gives us P(wage > 250 | age belonging to a bin j)
preds <- predict(fit.step,
                 newdata = data.frame(age = age.grid),
                 se.fit = TRUE)
# Convert logits → probabilities:
# plogis() is does exp(x)/(1+exp(x)).)
pfit <- plogis(preds$fit)

se.bands.logit <- cbind(preds$fit + 2 * preds$se.fit,
                        preds$fit - 2 * preds$se.fit)

se.bands <- plogis(se.bands.logit)

plot(jitter(age), I(wage > 250),
     xlab = "Age",
     ylab = "Pr(wage > 250)",
     col = "darkgrey",
     pch = "|",
     cex = 0.5,
     ylim = c(0, 1))

lines(age.grid, pfit, col = "red", lwd = 2, type = "s")   
matlines(age.grid, se.bands, col = "red", lty = 3, lwd = 1, type = "s")

# Regression Splines
# -------

library(splines)
# bs()
# turns one variable (like age) into several new variables so that
# a regression model can fit a smooth but flexible curve instead of a straight line.
# ordinary linear regression lm(y ~ age)
# as age increases, y changes in a straight line.

# polynomial regression lm(y ~ poly(age, 4))
# let y be a curved function of age.
# 
# instead of one big curve, split variable into regions, fit curves in each region
# force them to connect smoothly

fit <- lm(wage ~ bs(age, knots=c(25, 40, 60)), data=Wage)
pred <- predict(fit, newdata = data.frame(age = age.grid), se.fit=T)

plot(age, wage, col = "gray")
lines(age.grid, pred$fit, lwd=2, col = "blue")
lines(age.grid, pred$fit + 2* pred$se, lty ="dashed", col="blue")
lines(age.grid, pred$fit - 2* pred$se, lty ="dashed", col="blue")

# prespecified knots
dim(bs(age, knots = c(25, 40, 60)))
# produce a spline with knots at uniform quantiles of the data.
dim(bs(age, df=6))
attr(bs(age, df=6), "knots")
# knots at ages 33.8, 42.0, and 51.0, which correspond to the 25th, 50th, and 75th percentiles of age

# by default bs() uses cubic polynomial basis functions by default (which give us cubic splines)
# we can change this using degree argument

# fit a natural spline with four degrees of freedom
fit2 <- lm(wage ~ bs(age, df=4), data=Wage)
pred2 <- predict(fit2, newdata = data.frame(age = age.grid), se.fit=T)

plot(age, wage, col = "gray")
lines(age.grid, pred2$fit, lwd=2, col = "blue")
lines(age.grid, pred2$fit + 2* pred2$se, lty ="dashed", col="blue")
lines(age.grid, pred2$fit - 2* pred2$se, lty ="dashed", col="blue")

# Smoothing spline
# ----------------

plot(age, wage, col = "gray")
title("Smoothing spline", cex=0.7)

fit <- smooth.spline(age, wage, df=16)
names(fit)
fit$x   # sorted age values (or grid)
fit$y   # fitted spline values

fit2 <- smooth.spline(age, wage, df=4)

fit3 <- smooth.spline(age, wage, cv=TRUE)
fit3$df # 6.8

lines(fit, lwd=2, col = "red") # == lines(fit$x, fit$y)
lines(fit2, lwd=2, col = "darkgreen")
lines(fit3, lwd=2, col = "blue")

legend("topright", legend=c("16 DF", "4 DF", "6.8 DF"), col=c("red","darkgreen", "blue"),
       lty = 1, lwd = 2, cex = 0.8)

# Local regression
# ----------------
# fits many little regressions, each focused on a small neighborhood of the data.
# Then stitches them together to form a smooth curve.
plot(age, wage, col = "gray")
title ("Local Regression", cex=0.6)

# For each age, look at nearby ages, fit a tiny straight line (or low-degree polynomial), and predict the value for that age
# span controls how big the neighborhood is
# span = 0.2 use 20% of the data closest to each point to fit the local regression.
# span = 0.5 use 50% of the data.
fit <- loess(wage~age, span = .2, data=Wage)
fit2 <- loess(wage~age, span =.5, data=Wage)
fit3 <- loess(wage~age, span =.8, data=Wage)

lines(age.grid, predict(fit, data.frame(age=age.grid)),
        col="red", lwd=2)
lines(age.grid, predict(fit2, data.frame(age=age.grid)),
      col="blue", lwd=2)
lines(age.grid, predict(fit3, data.frame(age=age.grid)),
      col="orange", lwd=2)
legend ("topright", legend=c("Span=0.2", "Span =0.5", "Span =0.8") ,
          col=c("red", "blue", "orange"), lty=1, lwd=2, cex =.8)

# Small span → curve is very flexible, follows the data closely, can wiggle a lot.
# Large span → curve is smoother, less sensitive to small fluctuations.

# GAMs
# is like doing local regression / splines for multiple predictors at once, and then adding their effects together.
# predict wage using age year and education

# ns: natural spline. It expands one predictor into multiple smooth basis functions.
# breaks one variable into several smooth pieces so the regression can bend where needed but stay well-behaved at the edges.
# ns(age, 5) replaces age column with 5 new columns
# age -> b1(age), b2(age), b3(age), b4(age), b5(age)
# education is qualitative, so leave it as is
unique(education)

gam1 <- lm(wage ~ ns(year, 4) + ns(age, 5) + education, data = Wage)
summary(gam1)

par(mfrow=c(1,3))
plot.Gam(gam1, se=TRUE, col="blue")

# gam()
# when you want the model to automatically decide how smooth a predictor’s effect should be (penalized smoothing).
library(gam)
# ns(age, 5) means exactly 5 degrees of freedom
# s(age, 5) means dof up to 5, chosen automatically
gam2 <- gam(wage ~ s(year, 4) + s(age, 5) + education, data = Wage)

par(mfrow=c(1,3))
plot(gam2, se=TRUE, col="blue")

# holding age and education fixed, wage increases with year, may be due to inflation
# holding year and education fixed, wage is lowest for the very young and very old
# and highest for the middle aged people
# holding age and year fixed, wage increases with education.
# the more educated a person is, the higher their salary, on average

# gam1 and gam2 results are almost the same
# In most situations, the differences in the GAMs obtained using smoothing splines
# versus natural splines are small.


# compare multiple gams
M1 <- gam(wage ~ s(age, 5) + education, data = Wage)
M2 <- gam(wage ~ year + s(age, 5) + education, data = Wage)
M3 <- gam(wage ~ s(year, 4) + s(age, 5) + education, data = Wage)

anova(M1, M2, M3)
# M2 is better than other 2 models
# so model with year (linear function of year) is better than a model without
# year and model with s(year, 4) (non linear function of year)

summary(M3)
# coef estimate for s(year, 4) is not significant (p value is 0.3537)

summary(M2)
# while that of year is significant (p value 2.89e-06)

# above we use linear function, ns() and s() as building blocks 
# can also use local regression
gam.lo <- gam(wage ~ year + lo(age, span = 0.7) + education, data = Wage)
plot(gam.lo, se=TRUE, col="blue")

# can add interaction terms
# lo(age, year, span = 0.5) is the interaction between age and year
gam.lo.i <- gam(wage ~ lo(year, age, span = 0.5) + education, data = Wage)
summary(gam.lo.i)
