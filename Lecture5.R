# ==========================================
# R Lecture 5
# Neethu Raj
# Based on lecture series "Data analysis using R" by Prof. Danny Arends
# ==========================================

setwd("C:\\R_Tutorial")  # Set working directory

# Plots Plots Plots

# ==========================================
# R Objects
# ==========================================

# are called S language objects
# S3 and S4 object system

# S3:

# R objects have classes.

class(12)
class("hello")

# Functions like summary(), plot(), print() are S3 generic functions.
# They are NOT one fixed function.
# They choose which method to run based on the class of the object you pass.

# For example: summary()

x <- factor(c(rep("male", 5), rep("female", 5)))
class(x)

summary(x)   # here R calls summary.factor()

x  <- rnorm(100, mean = 2, sd = 5)
summary(x)   # here R calls the default summary.numeric()

y <- c(21:30)
df <- data.frame(x,y)
summary(df)  # here R calls summary.data.frame()

# Why R do this?
# Because different objects need different kinds of summary:

# A numeric vector → min, median, mean, quartiles
# A factor → counts of each level
# A data frame → summary per column

# Define class
# In S3, a “class” is just a label you attach to an object.

x <- rnorm(10, 2, 3)
class(x)              # numeric

class(x) <- "myClass"
class(x )             # myClass

print(x)

# Define custom print method
print.myClass <- function(obj) {
  cat("This is my S3 object:\n", obj, "\nclass:", class(obj))
}

print(x)

summary(x)            # R here calls summary.default() because it cannot find 
# summary.myClass(), because it doesn't exist

summary.myClass <- function(object){
  cat("Mean:", mean(object),
      "\nMedian:", median(object),
      "\nQuantile:\n")
  print(quantile(object))
}

summary(x)


# In S3, you change an object’s class and define methods
# to override how generic functions behave for that object

# More examples

myObject <- list(chr = c("X" = 115, "Y" = 100),
                 genotypes = matrix(runif(250), 10, 25))
myObject

class(myObject) <- c("list", "newClass")

print.newClass <- function(object){
  cat("newClass Object\n")
  cat("Chromosomes:", names(object$chr))
  cat("\nMarkers:", unname(object$chr))
  cat("\nTotal markers:", sum(object$chr))
  cat("\nContent:", nrow(object$genotypes), "Individuals")
  
}
print(myObject) 
plot(myObject)               # R doesn't know how to plot this list

plot.newClass <- function(object){
  image(object$genotypes)
  
}

plot(myObject)               # Now it will plot

# S4:
# We can define classes, but also
# slots (data fields), type of slots and methods

# For Example:
# Define S4 class → setClass()
setClass("Student",
         slots = list(
           name = "character",
           age = "numeric",
           ID = "character"
         ))

# Create object → new()
student1 <- new("Student", name = "Elizabeth", age = 12, ID = "12038715")


# Access slots → @
student1@name
student1@age
student1@ID

# Show the object using built in show method
student1

# Write methods → setMethod()
setMethod("show", "Student",
          function(object){
            cat("Student: ", object@name, "\nAge:", object@age, "\nID:", object@ID)
          })
student1

# ==========================================
# Basic R plots
# ==========================================

data(cars)
head(cars)
?cars

plot(x = cars$speed, y = cars$dist, 
     xlab = "Speed (mph)", ylab = "Distance (ft)")


# OR

with(cars, plot(speed, dist))    # can use column names directly
with(cars, mean(speed))          # {} when there is >1 code lines

# Key Plot  Parameters
# ==========================================
?plot
?lines
?points

# pch
# lty
# lwd
# col
# xlab
# ylab

# Other parameters we can set using par()
# las: rotates axis labels (90°).
# bg: background color
# mar: margin size
# oma: outer margin size, used with multiple plots to set spacing between them.
# mfrow: number of plots per row, column (plots are filled row wise).
# mfcol: number of plots per row, column (plots are filled column wise).

# To get the current or default plot settings, use par()
par("col")
par("pch")
par("bg")
par("mar")  # bottom, left, top , right
par("mfrow")

# par() affects all of the plots until you close the window or open up a new one.

# old_par <- par()   # save current plot settings
# par()                   # change settings using par()
# par(old_par)       # revert back to old setting

# Functions we use to plot
# ==========================================
# plot()
# points()
# lines()
# text()             # adds text anywhere inside the plot region.
# title()
# mtext()          # adds text outside of plot window
# axis()             # adds axis ticks/labels


# R color ranges
# ==========================================
?rainbow
?heat.colors
?terrain.colors
?topo.colors
?cm.colors
?gray

# Set transparency rgb()
# for example to set transparency to 0.5
# col = rgb(1, 0, 0, alpha = 0.5)
# lower alpha more transparent

# Note:
# ?  → Open the manual page for this exact tool.
# ?? → Search the library for anything related to this word.

# Use packages to get more colors
# ==========================================

help(package = "RColorBrewer")           
library(RColorBrewer)
# mycols <- brewer.pal(N, "palette_name")         # N: how many colors you want, depends on which palette you chose

mycols <- brewer.pal(7,"Greens")
mycols <- brewer.pal(5,"Pastel1")               

# Each palette has a limited number of colors
# To make a smooth transition between then we can use

mycols <- brewer.pal(9,"Blues") 

# Blues got only 9 colors
barplot(rep(2, 18), col = mycols, main = "Barplot with Blues Palette")

# If we want many more shades of blues
# 20 is an example we can expand to any number of colors
barplot(rep(2, 18), col = colorRampPalette(mycols)(20), main = "Barplot with Blues Palette")

# Building up a plot
# ==========================================

data(airquality)
head(airquality)

# Always add units to axis labels
with(airquality, 
     plot(Wind, Ozone,
          main = "Wind and Ozone in NewYork"))

# To plot a subset of airquality
with(subset(airquality, Month == 9), 
     plot(Wind, Ozone,
          main = "Wind and Ozone in NewYork in Sep"))

# We can plot both in one
with(airquality, 
     plot(Wind, Ozone,
          main = "Wind and Ozone in NewYork"))

with(subset(airquality, Month == 9),
     points(Wind, Ozone, 
            col = 2))                  # col = 2 = "red"



# How to make this plot better

# 1. Empty plot
# Benefit: you avoid plotting default points (black dots) first
# so you don’t need to overwrite them.

with(airquality, 
     plot(Wind, Ozone,
          main = "Wind and Ozone in NewYork",
          type = "n"))

with(subset(airquality, Month == 9),
     points(Wind, Ozone, 
            col = "red",
            pch = 20))

with(subset(airquality, Month != 9),
     points(Wind, Ozone, 
            col = "blue",
            pch = 20))

# Add a legend
legend("topright",
       col = c("red", "blue"),
       pch = 20,
       legend = c("Sep", "Other months"))


old_par <- par()
# cex controls the size of text and plotting symbols
par(cex = 1.2)
# and then plot and see if you like that better

par(old_par)

par("mar")
par(mar = c(5.1, 5.1, 4.1, 1.1))

with(airquality, 
     plot(Wind, Ozone,
          main = "Wind and Ozone in NewYork",
          type = "n",
          xlab = "Wind (mph)",
          ylab = "Ozone (ppb)"))

with(subset(airquality, Month == 9),
     points(Wind, Ozone, 
            col = "orange",
            pch = 20))

with(subset(airquality, Month != 9),
     points(Wind, Ozone, 
            col = "grey",
            pch = 20))

# Highlight certain point
arrows(3.4,150, 3.4, 165,
       col = "black",
       lwd = 2)


# 3.4, 150 start position
# 3,4, 165 end position
# find by trying different values

# To fit best line y = intercept + slope * x
# we use models lm: linear model
# here y = Ozone, x = Wind

model <- with(subset(airquality, Month == 9), 
              lm(Ozone ~ Wind))

model

# a = intercept
# b = slope
abline(a = model$coefficients[1],
       b = model$coefficients[2],
       lty = 3,                    # dashed line
       lwd = 2,
       col = "orange"
) 

model <- with(subset(airquality, Month != 9), 
              lm(Ozone ~ Wind))

abline(a = model$coefficients[1],
       b = model$coefficients[2],
       lty = 3,                    
       lwd = 2,
       col = "grey"
) 


# Add a legend
legend(x = 13, y = 160,
       col = c("orange", "grey", "black"),
       legend = c("Sep", "Other Months", "Highlighted Point", "Best Fit Sep", "Best Fit Other Months"),
       pch = c(20,20,NA, NA, NA),
       lty = c(NA, NA, 1, 3, 3),
       lwd = c(NA,NA,2, 2, 2),
       bty    = "n",              # removes the surrounding box
       y.intersp = 0.5,        # reduce vertical spacing
       cex    = 0.8)             # reduce legend text size


# Multiple base plots
# ==========================================

par(mfrow = c(1,3), oma = c(0, 0, 2, 0))

with(airquality, {
  plot(Solar.R, Ozone, main = "Ozone vs Solar.R")     # No need for coma
  plot(Temp, Ozone, main = "Ozone vs Temp")
  plot(Wind, Ozone, main = "Ozone vs Wind")
  mtext("Ozone vs Weather Variables", outer = TRUE)   # global header
})

par(old_par)

# Pie chart
# ==========================================  

characters <- c("Gintoki", "Kagura", "Schipachi", "Katsura", "Madao")
popularity <- c(40, 30, 35, 36, 27)

pct <- round((popularity / sum(popularity)) * 100, 2)
labels <- paste(characters, pct, "%")

pie(popularity, labels = labels, main = "Gintama Popularity Polls", col = rainbow(length(characters)))

cols <- brewer.pal(5, "Pastel2")
pie(popularity,
    labels = labels,
    main = "Gintama Popularity Polls",
    col = cols,
    border = NA)

# Dendograms
# Show relationship between objects
# ========================================== 

data("mtcars")
head(mtcars)

# create hierarchical clusters (hc)
hc <- hclust(dist(mtcars))
plot(hc)

# pull all labels on to the same level
plot(hc, hang = -1) 

# Convert hc into a dendogram so we can choose triangle visualization
hcd <- as.dendrogram(hc)
plot(hcd, type = "triangle")#

# Color a Dendogram

cutree(hc, 4) # groups rows into 4 clusters
whichCluster <- cutree(hc, 4)
labelCols <- c("firebrick", "steelblue", "coral", "gold")

colLeaf <- function(object){
  if (is.leaf(object)) {
    a <- attributes(object)
    cluster <- whichCluster[which(names(whichCluster) == a$label)]
    attr(object, "nodePar") <- c(a$nodePar, lab.col = labelCols[cluster])
  }
  
  return(object)
}


coolhcd <- dendrapply(hcd, colLeaf)
plot(coolhcd, main = "Cooler Dendogram", type = "triangle")


# Phylogenetic trees
# ========================================== 

library(ape)

plot(as.phylo(hc))
plot(as.phylo(hc), type = "cladogram")    # most commonly used in papers
plot(as.phylo(hc), type = "unrooted")
plot(as.phylo(hc), type = "fan")







