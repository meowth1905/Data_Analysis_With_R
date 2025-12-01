# R Lecture 10
# Neethu Raj
# Based on lecture series "Data analysis using R" by Prof. Danny Arends
# ==========================================

setwd("C:\\Data_Analysis_With_R")  # Set working directory

# Logistic Regression 
# When Response variable is not continuous (discrete, categorical)

# Load in the data
data <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
head(data)

# response variable: admit (0 / 1)
# predictor variables: gre, gpa and rank 

unique(data$rank)            # 1 2 3 4

# Contingency Table
#  for each rank (1–4), the counts of admit = 0 and admit = 1.

xtabs(~admit + rank, data = data)

# If for any rank, count = 0 for admit == 0 or 1 then 
# we cannot use that rank in logistic regression.
# each predictor category needs variation in the outcome (both 0 and 1) to estimate an effect.

# Note: need to do this when predictor is a category

#
levels(data$rank)                 # Not a factor
data$rank <- as.factor(data$rank) # change to a factor

levels(data$rank) 

# Model the data
?glm

glm.model <- glm(formula = admit ~ gre + gpa + rank,
                 data = data, 
                 family = "binomial")   # response variable is binary (0/1)

summary(glm.model)
# gra and gpe are significant predictors
# 
# rank2       -0.675443   0.316490  -2.134 0.032829 *  
# rank3       -1.340204   0.345306  -3.881 0.000104 ***
# rank4       -1.551464   0.417832  -3.713 0.000205 ***
#
# Why no rank1?
# rank1 is the reference category (since its the first level of rank)
# because for ranks 2 3 and 4 the parameters are given relative to that of rank1 (since its the first level)
#
# rank2       -0.675443
# means the log‑odds of admission for rank2 are 0.675 lower than rank1, holding other variables constant.
# and that for rank 3 and 4 are even lower.
# 
# But how significant is the rank as a whole to determine admit?
#
install.packages("aod")
library(aod)

coef(glm.model) # or
glm.model$coefficients

glm.model$coefficients[c("rank2", "rank3", "rank4")]
# or
glm.model$coefficients[4:6]

# Combine significance of  3 ranks into 1
wald.test(b = glm.model$coefficients, 
          Sigma = vcov(glm.model),   # variance–covariance matrix of the estimated coefficients of glm.model
          Terms = 4:6)

# After this we get a single p.value (0.00011) very significant
# Chi-squared test:
# X2 = 20.9, df = 3, P(> X2) = 0.00011

# 
# In linear regression, we report the coefficients directly.
# Each coefficient shows how much the outcome changes when that predictor increases by one unit (holding others constant).
# if coefficient = 2, then Y goes up by 2 for every +1 in X.


# In logistic regression, we report odds ratio.
# example: the chance of someone being admitted.

# so instead of reporting
glm.model$coefficients

# we report
exp(glm.model$coefficients)

# gre 1.0022670
# means: for every +1 increase in GRE,
# the odds of admission are multiplied by 1.0022670

# Percent change in odds = (Odds ratio − 1 ) × 100%
# (1.0022670 − 1) × 100 = 0.2267%
# So each +1 GRE point increases the odds of admission only by about 0.23%.

# gpa   2.2345448
# Higher gpa increases odds by approximately 123.45%.

# Note
# Percent change measures how much larger something becomes relative to the original.
# If something doubles, 
# Percent change = (2 - 1) * 100 = 100%
# If it goes from 1 to 3, (3 - 1) * 100 = 200%

# rank2   0.5089310
# rank 2 decreases the chance of getting admitted by 50% (percent change in odds = −49.1069%)
# rank3        0.2617923
# and rank 3 decreases the chance by 73.82077%

# To get a 95% Confidence Interval
exp(cbind(odds.ratio = glm.model$coefficients, confint(glm.model)))


# glm(...., family = "binomial"): if response variable is  binomial but 
# it can be:
# Gaussian
# gamma
# inverse.Gaussian
# Poisson

# glm(...., family = "gaussian") is exactly same as using lm()
#
# which distribution? depends on data
# if we have count data then Poisson 


# Long vs Wide Format
# ==========================================
# Wide format: each variable has its own column.
# most commonly used

# Long format: one column for the variable name, another for its values.

data("airquality")
head(airquality)      # Wide format, each variable has its own column.

library(ggplot2)
#  ggplot works best with long (tidy) format data.

# For a basic scatter plot, wide format will do
ggplot(airquality, aes(x = Temp, y = Ozone)) +
  geom_point() +
  labs(x = "Temperature", y = "Ozone") +
  theme_minimal()

# But to plot more than 2 variables, ggplot need long format

library(tidyr)

airquality.long <- airquality %>%          # pipe operator
  pivot_longer(
    cols = c(Ozone, Solar.R, Wind, Temp),  # columns to reshape
    names_to = "Variable",                 # new column name
    values_to = "Value"                    # new column for values
  )
head(airquality.long)

library(ggplot2)

ggplot(airquality.long, aes(x = Day, y = Value, color = Variable)) +
  geom_line() +
  facet_wrap(~ Month) +
  labs(x = "Day", y = "Value", title = "Airquality variables by day (May-Sep)") +
  theme_minimal()

# Back to wide format

airquality.wide <- airquality.long %>% pivot_wider(
  names_from = Variable,
  values_from = Value
)
head(airquality.wide)
head(airquality)

# Note
# there is reshape2 older package from the creator of tidyr
# tidyr is more easy to use

# Common Pitfalls in R Programming
# ==========================================
#
# 1. When converting a factor to numeric go through character
# factor -> character -> numeric
# ==========================================

my.fac <- as.factor(c("11", "10", "4", "3", "4"))
levels(my.fac)                                  # "10" "11" "3"  "4" 

as.numeric(my.fac)                              # 2 1 4 3 4, which is wrong

# use instead
as.numeric(as.character(my.fac))                # 11 10  4  3  4

# 2. When you know how many elements you have,instead of using list() use
# ==========================================

vector("list", 5)                               # creates a list of length 5, with each slot initialized to NULL.

# list() creates an empty list of length 0
# every time you grow the list, R may need to reallocate memory to make space
# slower
# using vector("list", length of list) is much faster.

# 3. Always try to work with row names and column names of a matrix instead of positions 
# ==========================================
#
# To remove a single column from a matrix
my.matrix <- my.matrix[, -which(colnames(my.matrix) == "name of the column you want to remove")]

c1 <- c(100,220, 300)
c2 <- c(289, 765, 0)

m <- rbind(c1,c2)
rownames(m) <- c("gene1", "gene2")
colnames(m) <- c("sample1", "sample2", "sample3")
m

which(colnames(m) == "sample2")                 # get the index of column with name "sample2"
m <- m[, -which(colnames(m) == "sample2") ]     # remove that column
m

# To remove multiple columns from a matrix
my.matrix <- my.matrix[, -which(colnames(my.matrix) %in% c("column1", "column3", "column5"))]

# To remove multiple rows from a matrix
my.matrix <- my.matrix[-which(rownames(my.matrix) %in% c("row1", "row3", "row5")), ]

m <- matrix(1:24, nrow = 3)                      # R will automatically set ncol = 8
rownames(m) <- c("row1", "row2", "row3")
colnames(m) <- paste("col", 1:8)
m

m <- m[, -which(colnames(m) %in% c("col 1", "col 3", "col 5"))]
m

# 4. To force a scientific notation use format()
format(0.2378345, scientific=T, digits = 3)       # 3 significant digits     

# Can also do the other way around
format(2.387e-11, scientific=F, digits = 2)       # 2 significant digits (Leading zeros (before the first non‑zero digit) are not significant.)

# 5. strsplit combined with lapply
# ==========================================

dates <- c(
  "01-05-1925",
  "12-14-2021",
  "03-23-1919",
  "11-30-1867",
  "05-20-1994",
  "07-02-1995",
  "10-20-1994"
)

split.dates <- strsplit(dates, "-")
split.dates                                      # returns a list

# [ is actually a function (called the subsetting operator).
# so select  3rd element 

years <- unlist(lapply(split.dates, "[", 3))  
years
# or 
# sapply(split.dates, "[", 3)

months <- sapply(split.dates, "[", 1)
days  <- sapply(split.dates, "[", 2)

# 6 grepl to do partial string match
# ==========================================
#
# get every date with a 20 in it

grepl("20", dates)                      # returns a logical vector
which(grepl("20", dates))
dates[which(grepl("20", dates))]

all(which(grepl("20", dates)))          # returns  TRUE if pattern is present in atleast 1 

# grep can return indices directly
grep("20", dates) == which(grepl("20", dates))
dates[grep("20", dates)]


# 7. Its best to use season as a predictor instead of month
# ==========================================

# Convert months to season
to.season <- function(dates, pos=1){ # position of month in your dates
  months <- as.numeric(sapply(strsplit(dates, "-"), "[", pos))
  
  ret <- rep(NA, length(months))
  ret[months >= 3 & months <= 5] <- "Spring"
  ret[months >= 6 & months <= 8] <- "Summer"
  ret[months >= 9 & months <= 11] <- "Fall"
  ret[months == 12 | months == 1 | months == 2] <- "Winter"
  
  return(ret)
}

to.season(dates)

# To do computation with dates
date1 <- as.Date("05/11/1993", format = "%d/%m/%Y")
date1 <- as.Date("05/11/93", format = "%d/%m/%y")
date2 <- as.Date("23-11-2025", format = "%d-%m-%Y")

class(date1)            # "Date"

# with "Date" objects we can do computations
date2 - date1           # difference in days

Sys.Date()              # todays date

# can use seq on dates

# give me dates for the next 10 weeks from today
seq(Sys.Date(), length.out = 10, by = "1 week")


# 8. Replace in text vectors
# gsub() global substitution tool
# ==========================================

tiger.texts <- c(
  "The tiger jumps around.",
  "A tiger is sitting quietly.",
  "The tiger runs through the grass.",
  "A tiger lies down to rest.",
  "The tiger plays with its tail."
)

cat.texts <- gsub("tiger", "cat", tiger.texts)
cat.texts

# 9. Order a matrix by column(s)

m <- matrix(
  c("Tiger", "Lion", "Dog", "Cat", "Lion",
    "Yawning", "Sleeping", "Barking", "Licking", "Hunting"),
  nrow = 5,
  ncol = 2
)
colnames(m) <- c("Animal", "Action")
m

ordering <- order(m[,"Animal"])   # ,desc=T
m[ordering, ]


ordering <- order(m[,"Animal"], m[, "Action"])
m[ordering, ]

# 10. Preallocate with matrix(), vector(), or data.frame() and fill it in
# ==========================================

# Go through a matrix and collect multiple statistics per row
gene.exp <- matrix(
  c(5.2, 3.1, 4.8,
    2.4, 6.7, 5.5,
    7.8, 8.2, 6.9,
    1.2, 2.3, 1.9),
  nrow = 4,
  ncol = 3,
  byrow = TRUE
)
rownames(gene.exp) <- c("GeneA", "GeneB", "GeneC", "GeneD")
colnames(gene.exp) <- c("Sample1", "Sample2", "Sample3")

exp.stats <- data.frame(Gene = rownames(gene.exp),
                        Mean = NA,
                        SD = NA)

for (i in 1:nrow(gene.exp)){
  exp.stats$Mean[i] <- mean(gene.exp[i,], na.rm = TRUE)
  exp.stats$SD[i] <- sd(gene.exp[i,], na.rm = TRUE)
}

exp.stats

rownames(exp.stats) <- exp.stats$Gene
exp.stats <- exp.stats[, -which(colnames(exp.stats) == "Gene")]
exp.stats


# More simple way to get the stats
results <- c()

for (i in 1:nrow(gene.exp)){
  mmean <- mean(gene.exp[i,], na.rm = TRUE)
  msd <- sd(gene.exp[i,], na.rm = TRUE)
  results <- rbind(results, c(mmean, msd))
}

rownames(results) <- rownames(gene.exp)
colnames(results) <- c("Mean", "SD")
results

# Always pre allocate 
big.m <- matrix(runif(1000000), nrow= 10000, ncol = 100)

system.time({
  results <- c()
  for (i in 1:nrow(big.m)){
    mmean <- mean(big.m[i,], na.rm = TRUE)
    msd <- sd(big.m[i,], na.rm = TRUE)
    results <- rbind(results, c(mmean, msd))
  }
})

# user  system elapsed 
# 0.42    0.33    0.73 

system.time({
  results <- matrix(NA, nrow = nrow(big.m), ncol = 2)
  
  for (i in 1:nrow(big.m)){
    mmean <- mean(big.m[i,], na.rm = TRUE)
    msd <- sd(big.m[i,], na.rm = TRUE)
    results[i, ] <- c(mmean, msd)
  }
})

# user  system elapsed 
# 0.34    0.00    0.34 

# Pre allocating saves time 
# So always preallocate with matrix(), vector(), or data.frame() and fill it in

# There is also proc.time

s <- proc.time()

results <- matrix(NA, nrow = nrow(big.m), ncol = 2)
for (i in 1:nrow(big.m)){
  mmean <- mean(big.m[i,], na.rm = TRUE)
  msd <- sd(big.m[i,], na.rm = TRUE)
  results[i, ] <- c(mmean, msd)
}

proc.time() - s

# 11. First convert to character before converting to numeric
# R sometimes convert to factor instead of numeric
# ==========================================

# Force first three columns of my.matrix into numeric
my.matrix <- matrix(rep("123", 30), ncol=6, nrow=5)
my.matrix

num.mat <- apply(my.matrix[,c(1,2,3)], 2, function(x){
  as.numeric(as.character(x))
})

class(num.mat[, 1])

# 12. pmatch: partial match
# ==========================================

example <- function(x, method){
  # we want to check if the method user input is actually supported by function or not
  supported <- c("pearson", "spearman", "kendall")
  method <- pmatch(method, supported)    # returns NA f no match, otherwise returns index of matched
  if (is.na(method)) stop("Method not supported")
  return(method)
  
  # rest of the logic
  #
}

example(c(1:10), "p") # 1, 1st method in supported
example(c(1:10), "pear")
example(c(1:10), "ken")

# 13. invisible()
# ==========================================

example <- function(x){
  rep(x, 1000000)
}
example(1)

# prevents a function’s return value from printing to the console,
# but still returns it so other code can use it

example <- function(x){
  invisible(rep(x, 1000000))
}
example(1)  # the value is still returned, just not printed.

# Pipelines
# R is a good lang to write pipelines
# ==========================================
#
# When running R from a server
# Always end every path with q()
q("no") # quit without saving the work space

?commandArgs
# read command line arguments also return script name
# with  trailingOnly = TRUE, only the arguments after your script name.

cmd.args <- commandArgs(trailingOnly = TRUE)
if (length(cmd.ar) < 1){
  stop("Atleast 1 argument is needed")
  q("no")  # # Always quit a script, otherwise R will continue to run
}

args <- as.character(cmd.args[1])
cat(args, "\n")

# Always quit a script, otherwise R will continue to run
q("no")

# To run on a server
# Rscript my.script.R

# How to run external commands?
# example: dir which is a windows command to show content of the current directory

system("dir") # not working? try
system("cmd /c dir")

files <- system("cmd /c dir", intern = T)

# In R we have similar to dir
list.files() # print all files in the current working directory.

