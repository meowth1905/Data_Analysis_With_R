# ==========================================
# R Lecture 6
# Neethu Raj
# Based on lecture series "Data analysis using R" by Prof. Danny Arends
# ==========================================

setwd("C:\\R_Tutorial")  # Set working directory

# Project Planning
# ==========================================

# Consult statistician before experiment begins
# Plan statistical power at design stage
# Avoid post‑hoc regrets about sample size
# Right sample size = stronger hypothesis testing

# Normalization 
# ==========================================
# To remove technical variation from noisy data.
# Might also remove biologically driven variations.

# adjusting values measured on different scales to a common scale.
# bring everything on to a common range for a fair comparison.

cat <- c(2,3,4,5,6,7,9)
range(cat)                        # range 2–9

tiger <- c(40,50,67,89,90,120,137)
range(tiger)                      # range 40–137

# Min-max normalization to [0,1]
minMax <- function(x){
  (x - min(x)) / (max(x) - min(x)) 
}

# Note: last expression’s value is returned automatically. No need of return()

catNormalized <- minMax(cat)
tigerNormalized <- minMax(tiger)

range(catNormalized)             # range 0-1
range(tigerNormalized)           # range 0-1

# Z‑score normalization x - (mean(x))/sd(x)
# scale()

A <- c(1:5)
B <- seq(10, 50, 10)

zNorm <- function(x){           # can also use scale()
  (x-mean(x))/sd(x)
}

ANormalized <- zNorm(A)
BNormalized <- zNorm(B)

ANormalized == BNormalized

# z-score normalization removes shifts and scaling.
# Two vectors that differ only by a constant scale factor will have identical z-scores.

x <- c(1,3,5)
y <- c(48, 73, 109)
zNorm(x)
zNorm(y)

# Both are expressed in units of standard deviations from their own mean
# +1 means one standard deviation above the average
# Regardless of whether the original scale was 1–5 or 48–109

# Mean centering
# subtract the mean from each value 
x <- c(2, 39, 19, 10)
xNew <- x - mean(x)
mean(xNew)                           # = 0


# Quantile normalization
library(preprocessCore)
m <- matrix(c(5, 2, 3, 4, 1, 4, 3, 7), nrow = 4, byrow = TRUE)
colnames(m) <- c("Sample1", "Sample2")
rownames(m) <- c("gene1", "gene2", "gene3", "gene4")
m

mNorm <- normalize.quantiles(m)
colnames(mNorm) <- colnames(m)
rownames(mNorm) <- rownames(m)
mNorm

# Quantile normalization steps:
# 1. Sort each column (get ranks).
# 2. Average values across columns at each rank.
# 3. Replace each value with its rank-average.
# 4. Reorder values back to the original column order.

# quantile normalization forces all samples to have the same distribution.
# the mean, median, and overall shape of each sample’s distribution will be the same.
mean(mNorm[,1]) == mean(mNorm[,2])
median(mNorm[,1]) == median(mNorm[,2])

# Column-wise (per sample): All samples now have the same distribution
# Row-wise (per gene/probe): Differences remain.
# so gene-level differences across samples remain.
# but some difference between samples that make sense biologically will also get lost.

# Commonly used in microarrays


# Gene Expression Profiling
# ==========================================

# Goal: to identify genes that are differentially expressed between groups
# 2 groups: t-test
# >2: ANOVA
# Also non parametric tests (robust to outliers, less statistical power)

# Note
# We should choose the appropriate statistical test during experiment planning. Because:
# we need to know how much power we will have,
# and based on that we need to determine sample size

# Hypothesis Testing
# ==========================================
# We determine whether an observed effect is significant or not,
# i.e., whether it is unlikely to occur randomly by chance.
# If it is very unlikely, we consider the effect significant.

# # Null hypothesis: there is no effect; any difference observed between groups
# occurred by chance or due to random sampling of group members.

# p-value: probability of observing what you observed (or even more extreme)
# if the null hypothesis is true.

# Smaller p-value: probability that the observed effect happens when the null is true is very small,
# so the effect is considered significant.

# We need a threshold (alpha) to decide how small a p-value must be for an effect to be significant.
# In biology, a commonly used threshold is 0.05.
# In physics, a commonly used threshold is  0.0001.

# Statistical Testing
# ==========================================

# Assumption: samples are random and independent
# It's okay if samples are related when that is the focus (e.g., studying twins),
# but in general, participants should not be related.

# t.test
# 
# One sample (one sample t.test) and 2 samples (two sample t.test)
# 2 sample t.test: difference between 2 groups.
# one sample t.test: if mean of a single group is different to what is predicted or expected.

# 2 hypothesis
# single sided test if A > B or A < B
# more powerful
# used when we have an idea about the direction of the effect from prior data
# if not then use 2 sided test

# calculate test statistic, t (depend on 1 or 2 sample test)
# For a one sided test,
# p-value is the area in the t-distribution where x ≥ t (or ≤ t, depending on the direction)

# For a 2 sided test, 
# p-value is area in the t-distribution where x ≥ |t| * 2

# One sample t test
# test the hypothesis that population mean = some value mu_o
# test statistic, t = (sample mean - mu_o)/standard error
# standard error = standard deviation / sqrt(sample size)

# Two sample t test:

# 1. independent or unpaired samples
# example: case control
# preferred: both groups have the same size and variance

# 2. paired samples
# measurements from before and after treatment.
# two groups A and B is paired
# if A and B have measurements from the same person for diff conditions.
# eg: before and after treatment

# paired t test more powerful than independent sample t test
# t test is parametric
# assumes data is normal

# How to validate normality assumption?
?shapiro.test()
# If shapiro.test() fails, then switch to non parametric tests.

# standard t test also assume variance is equal between the groups.
# Welch’s t-test: when  independent samples have unequal variances.

# How to test if both groups have same variance?
library(car)
?leveneTest

# Welch’s t-test is the default
# if we know variances are the same then use t.test(..., var.equal = TRUE)

# Non parametric tests
#
# 1. Mann-Whitney U test
# non parametric equivalent of t-test
# R its called:
?wilcox.test()

# y is the numeric response, x is a grouping factor.
# use formula interface.
# wilcox.test(y~x)

# x and y are both numeric vectors
# use Vector interface.
# wilcox.test(y, x)
# can set paired = TRUE or FALSE as required

# 2. Kruskal Wallis Test
# non parametric equivalent of ANOVA
# >= 2 samples

?kruskal.test()
# kruskal.test(y~A), where y is numeric, A is factor (>=2 levels)

# what it tells?
#
# if there is any difference among groups.
# It does NOT tell which group differs.
# Use post-hoc tests to find specific group differences.

#3. Friedman test
# 
?friedman.test

# non parametric alternative to repeated measures ANOVA.
# use it when:
# there is 1 grouping factor (the thing you want to compare) and
# one blocking factor (what you want to control for, because it could confound the results.)

# friedman.test(y ~ grouping factor | blocking factor)

# example:
# We want to see how diet affects weight (y)
# and control for individual differences between people
# so those differences don’t confound the results


# Correlation
# ==========================================

?cor

# correlation does NOT mean there is causation.
# BUT
# causation can NOT occur without correlation.

# 1. Pearson correlation
#
# parametric
# doesnt do any transformations on the data
# measure linear relationships
# sensitive to outliers
# assume variables are normally distributed.
# so we need to check if this assumption holds or not
# reliable when there is >=30 measurements

# 2. Spearman correlation
#
# non parametric
# converts the data to ranks and then computes the correlation on those ranks.
# robust to outlier
# can measure  monotonic relationship
# monotonic relationship can be curved or non-linear,
# as long as it always moves in the same direction.
# for example sin(x) not monotonic because it goes up and down repeatedly.


# Multiple Testing
# ==========================================

# when we have 20k genes, we do 20k tests to see if any of them are differentially expressed
# When you test thousands of genes, even if no real difference exists,
# some will have p < 0.05 purely by chance.

# Bonferroni correction
# p_value should be < (0.05 / number of tests) 
# significant only if p_value * number of tests < alpha

?p.adjust
# p.adjust(p_value, "bonferroni", number of tests)
# the newly adjusted p_value is significant if it is < 0.05 

# Type 1 error
# False Positives
# calling an event significant when its not really
# avoid by Bonferroni correction

# Type 2 error
# False negative
# fail to see an event is significant
# avoid by Benjamini-Hochberg False Discovery Rate procedure 


