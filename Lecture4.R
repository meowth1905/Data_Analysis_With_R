# ==========================================
# R Lecture 4
# Neethu Raj
# Based on lecture series "Data analysis using R" by Prof. Danny Arends
# ==========================================

setwd("C:\\R_Tutorial")  # Set working directory

# Descriptive Statistics:
# Univariate               # describes one variable at a time 
                           # (mean, median, mode, frequency, etc.).

# Bivariate                # describes the relationship between two variables
                           # correlation, cross-tabulation, scatter plots

# Multivariate analysis    # describes relationships among three or more variables simultaneously
                           # multiple regression, MANOVA, factor analysis

# ==========================================
# Descriptive Univariate Statistics
# ==========================================

# 1. Central tendency: Mean, Median, Mode

# Mean
# Arithmetic mean: sum of all values divided by the number of values.
                   

# Harmonic mean:   # reciprocal of the average of reciprocals.
                   # useful for rates or ratios (e.g., speed).
                   # most accurate mean

# Geometric mean   # nth root of the product of n values, useful for growth rates or percentages.
                   # when comparing values with different numeric ranges.


# R only has function mean() for arithmetic mean.
mean(c(1,2,3,4,5))
mean(c(1,2,3,4,5,NA))
mean(c(1,2,3,4,5,NA), na.rm = TRUE)           

x <- c(2, 4, 6, 8, 10)
mean(x, trim = 0.1)
# trim = 0.1
# First sort x in ascending order.
# Remove 10% from both ends

# Median
median(c(1,2,3,4,5))
median(1,2,3)         # wrong way to pass values, median only sees the first value.
median(c(1,2,3,4,NA,5), na.rm = TRUE)

# Mode
# In Gaussian distribution, mean = median = mode
# Multiple modes are possible

# ==========================================
# Dispersion:Range, Quantile, Variance, Standard Deviation, Outliers

# Range:
# difference between largest and smallest.
range(c(1,2,3,4,5))                        # gives min and max 
diff(range(c(1,2,3,4,5)))                  # max - min

# Quantiles
# Divide a dataset into equal-sized parts.

# Examples:
# Percentiles → divide data into 100 equal parts
# Quartiles →   divide data into 4 equal parts Q1, Q2 and Q3:
                # Min → Q1
                # Q1  → Q2
                # Q2  → Q3
                # Q3  → Max

# ........ many more

quantile(runif(100))

# Variance
# measures the spread of data around the mean

# For a population:
# - Use denominator N (total number of values)
# - Formula: sum((x - mean)^2) / N

# For a sample:
# - Use denominator n-1 (sample size minus 1)
# - Reason: sample mean estimates population mean
#   → reduces degrees of freedom
#   → Bessel's correction makes sample variance unbiased
# - Formula: sum((x - mean)^2) / (n - 1)

# var() calculates sample variance by default
x <- c(2,4,6,8)
var(x)

# If you want population variance, you need to adjust manually:
var(x) * (length(x)-1)/length(x)

# Standard Variation
sqrt(var(x)) # or
sd(x)

# Outliers
# Measurements that are distant to other measurements.

# What to do with an oulier:
# Trimming: set to NA
# Winsorizing: set to whatever you want, but write down what you did. 

# ==========================================
# Shape: Skewness, Kurtosis

# Skewness
# Measure of the asymmetry of a distribution 

library(psych)
x <- c(2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 10, 15, 25)
skew(x)   # >0, mean is pulled to right by the larger values

# == 0 means distribution is symmetric
#  > 0 distribution is positively skewed (tail on the right).
#  < 0 distribution is negatively skewed (tail on the left).

# Kurtosis
# measures how heavy or light the tails are compared to a normal distribution.

kurtosi(x)
# A normal distribution has kurtosis ≈ 3
# < 3 distribution has lighter tails than a normal distribution.

# Why is shape of distribution important?
# Because most models in statistics assume a normal distribution

# Normality Test
# Is my data normally distributed?
# Shapiro-Wilk test of normality

x <- runif(1000)
shapiro.test(x)

# null hypothesis (H₀) is: The data come from a normal distribution.
# p > 0.05 → data does NOT significantly deviate from normal (looks normal).
# p < 0.05 → data significantly deviates from normal (not normal).Reject null.

# Goodness-of-Fit Test
# how well data fit ANY specified distribution, not just normal.


# Basic plots: 
# Visualize distribution, outliers, etc.

x <- runif(100)
y <- runif(10)

plot()
points() # add extra points on top
axis()
legend()

m <- matrix(c(1:20), 4, 5)
image(m) # 2D plots
heatmap(m)

x <- rnorm(50, mean=10, sd=2)
y <- rnorm(50, mean=12, sd=3)

par(mfrow=c(1,2))            # 2 images in 1 row

# Boxplot without notches
boxplot(x, y,
        notch=F,
        names=c("x values", "y values"),
        col=c("yellow", "orange"),
        ylab="Values")


# Boxplot with notches
# notches of 2 distributions do not overlap -> strong evidence that the 2 medians differ.
boxplot(x, y,
        notch=TRUE,
        names=c("x values", "y values"),
        col=c("yellow", "orange"),
        ylab="Values")

# varwidth = TRUE, width of box will be proportional to number of observations.

# Violin plots shows distribution better
# shows shape of the distribution

par(mfrow=c(1,1))

library("vioplot")
vioplot(x, y, 
        names=c("x values", "y values"), 
        col=c("yellow", "orange"))


# If we want to group data into categories

set.seed(20)
x <- c(
  rnorm(100,10,3),
  rnorm(100,3,8)
)

# y <-  round(runif(200), 0)
y <- c(rep("Male", 100), rep("Female", 100))

vioplot(x~y, 
        col=c("firebrick", "steelblue"))


# Histogram
hist(x, col = "coral")

hist(x, breaks = 20,        
     col = "coral",     
     xlab = "X values",
     ylab = "Density",
     freq = FALSE)

lines(density(x), col="steelblue", lwd=2)


# To configure plotting parameters
# use par()

old_par <- par()

# How to plot
# Ready plots for publocations

png("hist.png", width = 800, height = 600)

par(font.lab=2, font.axis=2)

hist(x, breaks = 20,        
     col = "coral",     
     xlab = "X values",
     ylab = "Density",
     freq = FALSE)

lines(density(x), col="steelblue", lwd=2)

dev.off()

par(mfrow = c(2,2))

x <- rnorm(100, mean = 2, sd = 4)
y <- rnorm(100, mean = 20, sd = 2)
z <- rnorm(100, mean = 8, sd = 10)
k <- rnorm(100, mean = 12, sd = 13)

hist(x)
hist(y)
hist(z)
hist(k)

par(old_par)

hist(y)


# For more complex layouts
png("histograms.png", width = 800, height = 600)

test <- layout(matrix(c(1,1,3,2), 2, 2, byrow = TRUE))
layout.show(test)
hist(x, col ="yellow")
hist(y, col = "orange")
hist(z, col = "violet")

dev.off()


col1 <- c(10,20,30)
col2 <- c(19,22,34)
col3 <- c(11,92,67)

mat <- cbind(col1, col2, col3)
colnames(mat) <- c("Jan", "Feb", "Mar")
mat

apply(mat, 2, mean)

# but only want mean of column Jan and Mar
apply(subset(mat, select = c(Jan, Mar)), 2, mean)

# also possible to use conditions inside subset

data("airquality")
head(airquality)

apply(subset(airquality, Temp > 70 ,select = c(Wind, Ozone)), 2, mean)
