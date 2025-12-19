
setwd("C:\\Neethu\\Data_Analysis_With_R\\R_learning")

ls()
x <- c(1,2,3)
y <- c(4,5,6)

# list of all objects
ls()

# remove objects
rm(x,y) 
ls()

# remove all objects
rm(list = ls())

?rnorm
# rnorm(n, mean = 0, sd = 1)

x <- rnorm(50)
y <- x + rnorm (50, mean=50, sd=.1)
cor(x, y)

plot(x, y)
plot(x, y, xlab=" this is the x-axis", ylab=" this is the y-axis",
     main=" Plot of X vs Y")

pdf("figure.pdf")
plot(x, y, xlab=" this is the x-axis", ylab=" this is the y-axis",
     main=" Plot of X vs Y")
dev.off()

# to create jpeg use jpeg()

# to create sequence of numbers
seq(10, 100)
seq(100, 10)

# shorthanded way to do this
10:100

# 10 numbers that are equally spaced between 0 and 1.
seq(0, 1, length=10)

?outer
x <- c(1, 2, 3)
y <- c(4, 5)
outer(x, y, function(a, b) a + b)

# creates a matrix nrow = length(x), ncol = length(y)
# first column 4+1 4+2 4+3
# second column 5+1 5+2 5+3

?contour

A <- matrix(1:16, 4, 4)
A
# exclude first 2 rows, get all columns
A[-c(1,2),] 

data <- read.table("assig6_data\\testdata.txt", sep = "\t", row.names = 2, header = T)

# view loaded data in a spreadsheet like window.
fix(data)

# if the file has missing observations represented by a question mark ?
# then na.strings="?" tells R to treat ? as missing element of the dataset

data <- read.table("assig6_data\\testdata.txt",
                   sep = "\t",
                   row.names = 2,
                   header = T,
                   na.strings = "?")

fix(data)
dim(data)

# to remove rows with missing values 
data <- na.omit(data)
dim(data)

# column names
names(data)
# or
colnames(data)

# install.packages("ISLR")
library(ISLR)
data(Auto)

head(Auto)
# plot(cylinders, mpg)  will throw error

# to make the variables in this data frame available by name
attach(Auto)
plot(cylinders, mpg) 

unique(cylinders)

# converts quantitative variables into qualitative variables
?as.factor()

cylinders <- as.factor(cylinders)
levels(cylinders)

# If the variable plotted on the x-axis is categorial
# plot() will automatically produce boxplots
plot(cylinders, mpg, xlab="cylinders", ylab="mpg", col=12)

# histograms
hist(mpg, col=2)

# a scatterplot for every pair of variables for a given data set
pairs(Auto)

pairs(~ mpg + displacement + horsepower + weight +
         acceleration , Auto)

plot(displacement, horsepower)

# identify() pass x, y and which column value we need to show (example name)  
# click on data points and then finish
# then in plot we can see name of each clicked points
plot(horsepower, mpg)
identify(horsepower, mpg, name)

# numerical summary of each variable in a dataset
summary(Auto)

# a summary of just a single
variable.
summary(mpg)

# for qualitative variable, 
# number of observations that fall in each category
summary(name)
# or
table(name)

# to save a record of all of the commands that we typed
# in the most recent session
savehistory()

# to load that history
# loadhistory()

# to quit
q()
