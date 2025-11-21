# R Lecture 9
# Neethu Raj
# Based on lecture series "Data analysis using R" by Prof. Danny Arends
# ==========================================

setwd("C:\\Data_Analysis_With_R")  # Set working directory


# Linear Mixed Models (LMMs)
# ==========================================
#
# Why do we need them?
#
# General linear models (GLMs) assume all observations are independent.
# But in many cases this assumption is violated:
#   i) repeated measurements from the same individual
#   ii) data from related individuals (e.g. family studies)
#
# Why it matters:
# If repeated measurements are wrongly treated as independent in a GLM,
# results may look significant due to relatedness when they are not.
# This leads to spurious relationships (FPs) and overestimation of statistical power.
#

# Linear Models: Response ~ Predictor
#
# example:
#
freq.data <- read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")
head(freq.data)

unique(freq.data$subject)

# frequency(of voice) ~ attitude (polite vs informal) + gender + error term
# why include gender ?
# To account for the fact that females have much higher frequency of voice compared to males.
#

boxplot(frequency ~ subject, data = freq.data,
        main = "Frequency by Subject",
        xlab = "Subject",
        ylab = "Frequency",
        cex.main = 0.8)

# Note
library(ggplot2)

ggplot(freq.data, aes(x = subject, y = frequency, fill = gender)) +
  geom_boxplot() +
  labs(title = "Frequency by Subject",
       x = "Subject",
       y = "Frequency") +
  scale_fill_manual(values = c("F" = "orange", "M" = "lightblue")) +
  theme_minimal()

# As per the design of the experiment, they had multiple measurements per subject.
# (subjects measured in different settings where they need to be polite or informal.)
# per-subject variation

# A GLM won't know this and will consider each observation independent.
#
# Random Effects
# ==========================================
#
# All responses from the same person will share similarities,
# and will differ from responses of other individuals.
# This makes repeated measures interdependent rather than independent.
# From the above box plot we can see clearly that there is variation between M3 M4 and M7
# between F1 and F2 and between F1 and F3

# Extending the Model
#
# In GLM, intercept is the predicted value for the response variable,
# when all predictors are equal to 0. Meaning:
# it’s the shared baseline response or prediction for all individuals,
# before accounting for predictors.
# - fixed intercept
#
# In LMM
# frequency ~ attitude + gender + (1|subject) + error term
# The extra term (1|subject) is a random intercept. 
# It allows each subject to have their own individual intercept,
# random intercept

# As per the design they also asked each subject different questions
# depending on question answers can be different
# and can have different words and letters and the frequency then depend on these as well
# per-item variation
# 

boxplot(frequency ~ scenario, data = freq.data,
        main = "Frequency by Scenario/Question",
        xlab = "Subject",
        ylab = "Frequency",
        cex.main = 0.8)

#Note
ggplot(freq.data, aes(x = scenario, y = frequency)) +  # won't work as expected
  geom_boxplot() 

# why?
levels(freq.data$scenario) # is NULL
# ggplot needs us to explicitly tell when a numeric variable should be treated as categorical.
# Base r does this automatically

ggplot(freq.data, aes(x = as.factor(scenario), y = frequency)) 
  geom_boxplot() 

# Extending the Model again:
# frequency ~ attitude + gender + (1|subject) + (1|item) + error term
#
# each individual is allowed to have its own intercept.
# each question is also allowed to have its own intercept.
# So we take into account variation within subjects and within questions.
