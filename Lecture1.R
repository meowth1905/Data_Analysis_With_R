# Before starting please note:
# > head(
#  + 
#  + 
#  + 
#  + 
#  + 
#  + 

# whenever you see this in your console press Esc to get back prompt >
# You got this +++++ because you forgot to close a parenthesis ().

# ==========================================
# Lecture 1 - Neethu Raj
# Reference: Based on lecture series by Prof. Danny Arends
# ==========================================

# ---------- Working Directory ----------
getwd()                       # Where does R save / retrieve files?
setwd("C:\\R_Tutorial")       # Set another place to save / retrieve files.
getwd()

# ---------- Files ----------
dir()                          # What files are there?
file.remove("Intro_R.R")       # Remove a file
dir()                          # Check files again

# ---------- Environment ----------
ls()                           # What is in the current environment?
numberX <- 10
ls()                           # List objects again

# ---------- Packages ----------
install.packages("qtl")        # Install package
library("qtl")                 # Load package

# ---------- Save and Load ----------
numberX                        # 10
save(numberX, file = "myNumberX.RData")   # Save an object as binary

numberX <- NA
numberX                        # NA

load("myNumberX.RData") 
numberX                        # 10

# ---------- Built-in Variables ----------
letters <- letters             # Lowercase letters
LETTERS <- LETTERS             # Uppercase letters
months <- month.abb            # Abbreviated month names
ls()                           # Check environment

# Save the entire environment
save.image("myEnv.RData")      

# Always answer NO to "Do you want to save your workspace or session?"
# Save explicitly using save() or save.image()
q("no")

# ---------- Help ----------
?seq                           # Opens help file for function seq
??"annotation"                 # Search for a term across help files

# ==========================================
# Basic Types
# 1. Numeric
# 2. Character
# 3. Logical
# Note: No string type in R. Text data is stored as character vectors.

# ---------- Vectors ----------
# Vectors are a data structure, not a type. All elements should be of the same type.

v1 <- c(1,2,3,4,5,6,7,8,9,10,11)                          # Numeric vector
v2 <- seq(200, 300, 10)                                   # Sequence including 200 and 300     
v3 <- c(rep("Gintoki", 4), rep("Katsura", 4), rep("Madao", 3)) # Character vector
v4 <- c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE) # Logical vector

length(v1)               # Number of elements
str(v1)                  # Structure of an object
class(v1)                # Class or type of an object (numeric, logical, or character)

# Type conversion examples
"1" + 1                  # Throws error
as.numeric("1") + 1      # Convert character to numeric
as.character(1) + 1      # Force to character, throws error

# Type checking
is.numeric("1")           # FALSE
is.numeric(1)             # TRUE
# is.numeric(Y)            # True if Y is defined as a numeric matrix (see below)
is.character(names)        # TRUE

# ==========================================
# 2D Matrices
Y <- matrix(1:20, nrow = 5, ncol = 4)                   # 5 x 4 numeric matrix
Y
matrix(1:20, nrow = 5, ncol = 4, byrow = TRUE)         # Fill matrix by row
matrix("", nrow = 5, ncol = 4)                         # Empty 5 x 4 character matrix
matrix(NA, nrow = 5, ncol = 4)                         # Empty 5 x 4 numeric matrix

cbind(v1, v2)     # Combine vectors as columns (length(v1) should = length(v2))
rbind(v1, v2)     # Combine vectors as rows (length(v1) should = length(v2))

nrow(Y)           # Number of rows
ncol(Y)           # Number of columns

# Set row and column names
rownames(Y)
rownames(Y) <- c("row1", "row2", "row3", "row4", "row5")
rownames(Y)

colnames(Y) <- c("col1", "col2", "col3", "col4")
colnames(Y)

Y["row1", "col2"]            # Access specific element

# Always name rows and columns logically
# Never assign numeric names 

t(Y)                         # Transpose of matrix

# ---------- Data Frame ----------
# A matrix can only contain one data type (all numeric, all character, etc.).
# A data frame can have different data types in different columns.
data.frame(v1, v2, v3, v4)

# ---------- List ----------
# Not a vector; can contain anything (vectors, matrices, etc.)
# Always name elements of a list

mylist <- list(names = c("Titania", "Jellal"),
               age = 23,
               kills = v2,
               matrix = Y)
mylist
str(mylist)                 # Structure of the list

# Accessing list elements
mylist[[1]]                 # First element
mylist$names                # Access by name

mylist[[1]][1]              # "Titania"
mylist[[1]][2]              # "Jellal"

mylist$age
mylist$kills[c(1:3, 11)]    # First 3 and 11th element
mylist$matrix[3,3]          # 3rd row, 3rd column value
mylist$matrix[,2]           # 2nd column of matrix

# ---------- Factor ----------
# Categorical variable
l <- c(rep("male", 10), rep("female", 10))
l
levels(l)                   # Only works if l is a factor

myfactor <- as.factor(c(rep("male", 10), rep("female", 10)))
levels(myfactor)

# ---------- Guess Data Types ----------
1e+11                        # 1 * 10^11, scientific notation
class(1e+11)                 # numeric

class(as.factor(TRUE))       # factor
class(is.character(1e+11))  # logical

0x89                         # hexadecimal 0x89 = 137 decimal
class(0x89)                  # numeric

class("TRUE")                # character

# ---------- Variables ----------
# <- and -> both work
bank <- 3000                # Put 3000 in bank
bank

4000 -> bank                # Put 4000 in bank
bank
