# ==========================================
# Lecture 2 - Neethu Raj
# Reference: Based on lecture series by Prof. Danny Arends
# ==========================================

# ---------- Working Directory ----------
setwd("C:\\R_Tutorial")

# ---------- Variables ----------
name <- "Gintoki"                  # Create a character vector of length 1
length(name)                       # Number of elements in the vector
nchar(name)                        # Number of characters in the string

is.character(name)
is.numeric(name)

# In R, type coercion only works if the conversion “makes sense”.
as.numeric(name)                   # Will throw warning as conversion makes no sense

number <- "23"
is.numeric(number)
number <- as.numeric(number)       
is.numeric(number)

class(number)
class(name)

M <- matrix(1.:10, nrow = 5, ncol = 2)
nrow(M)
ncol(M)

# ==========================================
# Control Structures
# ---------- Branching ----------

### IF
random_number <- runif(1)          # Generate 1 random number
random_number

# Put else on same line as } of if, otherwise R throws an error
if (random_number < 0.5) {
  cat("random number is smaller than 0.5")
} else {
  cat("random number is not smaller than 0.5")
}

### Switch 
box <- "Zura"

switch(class(box),
       logical   = cat("box is a logical"),
       character = cat("box is character"),
       numeric   = cat("box is a numeric")
)

# Same can be done with if else if
box <- "Zura"

if (class(box) == "logical") {
  cat("box is a logical")
} else if (class(box) == "character") {
  cat("box is a character")
} else {
  cat("box is a numeric")
}

# ---------- Vector Comparisons ----------
x <- 10
numbers <- 0:4

# When we do if it should always evaluate a single TRUE or FALSE statement
x < 5       # Single comparison
numbers < 5 # Vector comparison returns multiple TRUE/FALSE

# Check if all elements satisfy a condition
if (all(numbers < 5)) {
  cat("All numbers are less than 5")
} else {
  cat("No number is less than 5")
}

# Check if any element satisfies a condition
if (any(numbers < 5)) {
  cat("At least one number is less than 5")
} else {
  cat("No number is less than 5")
}

# ==========================================
# Looping

### FOR loop
# Use FOR when you know how many times to iterate

names <- c("Gintoki", "Schinpachi", "Kagura", "Katsura")

for (name in names) {
  cat("I love ", name, " from Gintama anime.\n")
}

x <- 100
for (i in 1:10) {
  x <- x - i
  cat(x, "\n")
}

### WHILE loop
# Use WHILE when the number of iterations is unknown

x <- 100
counter <- 1
while (counter <= 10) {
  x <- x - counter
  cat(x, "\n")
  counter <- counter + 1 # Note: += is not valid in R
}

random_number <- runif(1)
count <- 0
while (random_number < 0.9) {
  count <- count + 1
  cat("count:", count, ", random number:", random_number, "\n")
  random_number <- runif(1)  # Without this, loop may run forever
}
random_number

# Sum of even numbers from 2 to 100
even_numbers <- seq(2, 100, 2)
total <- 0
for (number in even_numbers) {
  total <- total + number
}
cat("Sum of even numbers in 1 to 100 is ", total)
sum(even_numbers) # Alternative

# Sum of even numbers using conditional
numbers <- 1:100
total <- 0
for (number in numbers) {
  if (number %% 2 == 0) {
    total <- total + number
  }
}
total

# ==========================================
# Statements and Expressions

# IF statements:
# if (statement) {
#   expression
#   .
#   .
# }

# statement should return TRUE or FALSE
# multiple expressions possible; separate multiple statements on same line with ;

# & (element-wise AND) vs && (short-circuit AND)
# FALSE & some_function()  # evaluates second condition
# FALSE && some_function() # does not evaluate second condition

x <- 5
y <- 10
if (x > 0 && y > 0) {
  cat("Both x and y are positive\n")
}

# & vs && for vectors
v1 <- c(TRUE, FALSE, TRUE)
v2 <- c(TRUE, TRUE, FALSE)
v1 & v2

# | (element-wise OR) vs || (short-circuit OR)
# || skips second condition if first is TRUE

# Using logical conditions to select vector elements
x <- 10:1
x
x < 5
x[x < 5]                       # 4 3 2 1
x[x < 5 & x > 2]               # 4 3
x[x < 5 & x > 2 & x %% 2 == 0] # 4
x[x < 5 & x > 2 & x %% 2 != 0] # 3

x[c(TRUE, FALSE)] # Select even numbers
x[c(FALSE, TRUE)] # Select odd numbers

M <- matrix(1:9, 3, 3)
M
i <- M[, 1] < 3    # Which elements of column 1 < 3
M[i, ]             # Select rows where column 1 < 3

# ==========================================
# Special Control Structures

## Warnings
x <- -5
if (x < 0) {
  warning("x is negative, result may not be valid")
}
sqrt(x) # Still runs

## Errors
x <- -5
if (x < 0) {
  stop("x is negative, cannot compute square root") 
}
sqrt(x) # Never runs

## Try-catch (template)
# tryCatch(
#   expr = { ... },
#   warning = function(w){ ... },
#   error = function(e){ ... },
#   finally = { ... }
# )

# ==========================================
# Advanced Looping - lapply() & apply()

# lapply() apply a function to each element of a list or vector
# returns a list of results # lapply(X, FUN, ....) 
# X: A list or vector you want to iterate over. 
# FUN: The function to apply to each element. 
# ...: Optional arguments passed to FUN.

x <- 1:10
divide_by_2 <- lapply(x, "/", 2) # Divide elements by 2
unlist(divide_by_2)

square_roots <- lapply(x, sqrt)  
square_roots <- unlist(square_roots) # Convert list to vector
square_roots

l <- list(nums1 = 1:5, nums2 = c(1,2,3,4,NA))
lapply(l, mean)            # mean = NA if NA present
lapply(l, mean, na.rm = TRUE) # Remove NA

# if X is a matrix # apply(X, MARGIN, FUN, ...) 
# MARGIN: 1 = rows, 2 = columns, c(1,2) = rows and columns. 
# returns a vector or matrix, depending on the margin and function.

M <- matrix(1:9, 3, 3)
apply(M, 2, "*", 2) # Multiply each column by 2
apply(M, 1, mean)   # Row means
apply(M, 2, mean)   # Column means

# ==========================================
# Functions
# Variables inside a function are local
# Can return only one value, but that can be any type
# return() exits function immediately

myFunction <- function(val1, val2, val3) {
  fval <- val1 - val2 * val3
  
  if (val1 < val2) return(val1)
  if (val2 < val3) return(val2)
  
  return(fval)
}

myFunction(-1, -1, -3)

# Pass by value vs reference
# Copy-on-modify examples

x <- 10
change <- function(a) { a <- a + 5 }
change(x)
x # Still 10

x <- list(a = 1, b = 2)
use_it <- function(y) { print(y$a) } # Reading only, no copy triggered
use_it(x)

# Default parameters
anime <- function(name, genre = "Comedy") {
  cat("Anime", name, "is of genre", genre, "\n")
}
anime("Gintama")
anime("Demon slayer", "Action")

# Variadic functions (...)
mysum <- function(...) {
  count <- 0
  for (item in list(...)) count <- count + item
  return(count)
}
mysum(1,2,3,4,5,6,7,8)
mysum(1:3)
mysum(c(1,2,3))

variadic_test <- function(...) { return(list(...)) }
variadic_test(1,2,3)
variadic_test(1:10)
variadic_test(c(1,2,3))

# ==========================================
# Function Scope
test <- function(name) {
  new_var <- "Hello"
  cat(new_var, name, "!\n")
}
test("Katsura San")
# new_var # Error, local variable

global_var <- "Hello"
test <- function(name) { cat(global_var, name, "!\n") }
test("Kintoki")

# Local vs global variable
name <- "Nagumo"
test_func <- function() {
  name <- "Sakamoto" # Local variable
  cat(name, "is so cool!!\n")
}
test_func()
name # Global variable unchanged

# ==========================================
# Brackets in R
# () - function calls or control statements
# [] - index vectors/matrices/data frames
# [[]] - index a list
# {} - define block of code

# ---------- Printing ----------
print(paste("Zura", "ja", "nai", "Katsura", "da"))
cat(paste("Zura", "ja", "nai", "Katsura", "da\n"), file = "gintama_quotes.txt")
cat(paste("Sakata", "Gintoki", "wa", "nete", "matsu\n"), file = "gintama_quotes.txt", append = TRUE)

# Escape characters
cat("Hello\tWorld \"  \\ ")
cat("Hello\tWorld\b \"  \\ ")       # Backspaced 'd'
cat("Hello\tWorld\b\b \"  \\ ")     # Backspaced 'd' and 'l'

# ---------- Random Numbers ----------
runif(n, min=0, max=1) # Uniform
rnorm(n, mean=0, sd=1) # Normal
rpois(n, lambda)       # Poisson

# Reproducible randomness
set.seed(1)
round(runif(5, 0, 2))

# ---------- File Permissions ----------
Sys.chmod("gintama_quotes.txt", mode = "0444")
cat("can i write to this file?", file = "gintama_quotes.txt", append = TRUE)

# Note:
# Keep input files in an 'input' directory and write outputs to a separate 'output' directory
# to avoid accidentally overwriting input files.
