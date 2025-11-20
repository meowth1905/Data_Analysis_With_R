# R Lecture 7
# Neethu Raj
# Based on lecture series "Data analysis using R" by Prof. Danny Arends
# ==========================================

setwd("C:\\Data_Analysis_With_R")  # Set working directory

# Algorithm:
# finite list of discrete number of steps
# when followed will complete a certain task or fail

# State Diagram:
# used to describe the behavior of a system
# visualize an algorithm 

# Algorithm classifications based how it is implemented and what approach it used.

# Design patterns:
# patterns that are proven to work or the optimal solution to your problem
# avoid reinventing the wheel

# Examples:
# 1 Model-View-Controller (MVC) design pattern
# ==========================================

# MVC is widely used in web development.
# It separates an application into three parts: Model, View, Controller

# Model
# Responsible for data storage and management
# Represents the application's data and business rules
# Updates when the controller validates and allows changes

# View
# Handles presentation of data to the user
# Displays information from the model in a user-friendly format
# New views can be added easily without changing the logic

# Controller
# Contains the application logic
# Acts as the intermediary between user, model, and view
# All user interactions go through the controller
# Decides if a user action is valid and whether it can update the model

# Benefits
# Provides clear separation between data, model and view
# Easier to maintain and extend applications
# Increased security since every action is processed by the controller
# Flexible: new views can be added without altering model or controller

# 2. Monte Carlo Sampling
# ==========================================

# Monte Carlo sampling is used to simulate complex events.
# It considers all possible outcomes that might happen and then
# helps identify which outcomes are most likely
# Simulates many random scenarios to estimate probabilities.

# Applications
# Example in genetics: modeling chromosome recombination.
# Also used in agriculture, weather prediction, and economics.

# Genetics example
# Before you inherit X and Y from parents, chromosomes undergo random recombination.
# In mothers (XX): the two X chromosomes can recombine with each other.
# In fathers (XY): the X and Y chromosomes mostly do not recombine and
# recombination between X and Y occurs only in two small special regions.
# This recombination process is random.

# To Sum it up:
# We can simulate possible recombination events.
# Estimate which recombinations are more likely to occur.
# Provides a way to model genetic variation and inheritance patterns.

# 3. Facade or API
# ==========================================

# A facade or API provides a stable interface for external tools and applications.
# It allows developers to interact with a system without needing to know its internal details.
# The underlying code can change, but the interface remains consistent.

# Example: Twitter
# Many apps connect to Twitter.
# These apps do not need to know what programming language Twitter uses.
# Even if Twitter changes its internal implementation apps should still be able to connect.
# This is possible because Twitter uses a stable API.

# Facade concept
# A facade is an interface that provides high-level functions.
# Example: when you click "add a friend",
# You do not need to know how the code works internally.
# The API ensures that the friend is added through a simple, consistent command.

# Benefits
# Simplifies complex systems by exposing only necessary functions.
# Provides flexibility: internal code can change without breaking external apps.
# Makes integration easier for developers.
#


# 4. Scheduler
# ==========================================

# A scheduler organizes tasks to run within a certain amount of time.
# Different scheduling strategies are used depending on requirements.

# FIFO (First In, First Out)
# Queues processes in the CPU in the order they arrive.
# Simple but not suitable when deadlines are important.

# EDF (Earliest Deadline First)
# Selects the process with the earliest deadline and runs it first.
# Useful when tasks must be completed before specific deadlines.
# Example: similar to how students prioritize studying before exams

# SRT (Shortest Remaining Time)
# Chooses the process with the least estimated remaining time.
# If a task is already 90% complete, the scheduler runs the remaining 10% first.
# Helps finish shorter tasks quickly.

# RRS (Round Robin Scheduling)
# Assigns a fixed time slice to each process.
# Runs process 1 for a set time, then process 2, then process 3, cycling through all.
# Ensures fairness by giving each process CPU time in turn.

# Functions
# ==========================================

# It's not recommended to use global variables directly inside a function.
# Exception: when the data is very large and copying would waste RAM.
# In most cases, create a local copy inside the function or pass the variable as an argument.
# This keeps the function self-contained.
# Ensures the same output for the same input (predictable behavior).
# Makes the code easier to test, debug, and reuse.

# Define a variable in the global environment
parentVar <- 1000

# Function using <- (local assignment)
testFun <- function() {
  # This creates a new local variable 'parentVar' inside the function
  # It does not affect the global 'parentVar'
  parentVar <- parentVar + 1000
  return(parentVar)   # returns the local value
}

testFun()     # returns 2000 (local calculation)
parentVar     # still 1000, unchanged in the global environment


# Function using <<- (parent assignment)
test2Fun <- function() {
  # This modifies the 'parentVar' in the parent/global environment
  parentVar <<- parentVar + 1000
  return(parentVar)   # returns the updated global value
}

test2Fun()    # returns 2000 (global variable updated)
parentVar     # now 2000, changed in the global environment

#
# <<- updates the global (or parent) variable directly from inside the function.
# useful when the global variable is too large

# R Function can only return a single object
# for multiple, use a list

mul <- function(x, y){
  results <- c(x * 100, y * 100)
  return(results)
}

answers <- mul(5, 6)
answers

studentInfo <- function(name, score, age){
  results <- list(
    name = name,
    percentage = (score*100)/100,
    age = age
  )
  
}

student1 <- studentInfo("Mikasa", 95, 19)
student1

# Recursive functions
# ==========================================

# A function calling itself
#

factorial <- function(x){
  if (x < 0){stop("Input can't be negative.")}
  if (x == 0){return(1)}                  # base case
  else {return(x * factorial(x - 1))}
  
}

factorial(4)
factorial(6)

# fibanocci numbers
# 1 1 2 3 5 8 13 21

# a function to get nth fibanocci number
fibanocci <- function(x){
  if (x == 1) return(1)                    # base case
  if (x == 2) return(1)                    # base case
  else return(fibanocci(x - 1) + fibanocci(x - 2))
}

fibanocci(4)
fibanocci(7)

# Note
# {} is only required when you want to group more than one expression inside a control structure.

fibanocci(100)                            # will take a while
# there is a recursion limit


# Infinite recursion
# ==========================================

# happen when we don't take care of incorrect inputs

# a function to sum up numbers up to n
sumUp <- function(x){
  if (x < 0 ) stop("No negative values allowed")  # check to avoid incorrect inputs
  if (x == 0) return(0)                           # base case
  if (x == 1) return(1)                           # base acse
  else return(x + sumUp(x - 1))
}
sumUp(5)

sumUp <- function(x){

  if (x == 0) return(0)
  if (x == 1) return(1)
  else return(x + sumUp(x - 1))
}

sumUp(5)
sumUp(-5) # will cause infinite recursion
# with positive numbers your recursion eventually hits a base case and stops,
# while with negative numbers it never does.

# Indirect Recusrion
# ==========================================
#
# when two or more functions call each other instead of calling themselves directly.

# Function A calls Function B
funcA <- function(n) {
  if (n <= 0) {
    return("Done")
  } else {
    cat("A got", n, "\n")
    return(funcB(n - 1))   # calls B
  }
}

# Function B calls Function A
funcB <- function(n) {
  if (n <= 0) {
    return("Done")
  } else {
    cat("B got", n, "\n")
    return(funcA(n - 1))   # calls A
  }
}

funcA(5)

# Functions ... parameter
# ==========================================

# allows to pass an arbitrary number of arguments to a function

x <- c(1:20)
y <- sapply(x, sqrt)  # lapply returns a list, sapply returns a vector

myPlot <- function(x, y, ...){
  plot(x, y, ...)
}

myPlot(x, y, col = "red", pch = 20, main = "test plot")
# useful when we are not sure which arguments we may need

# Higher order Functions
# ==========================================
#
# A function with another function passed as an argument or returns a function.

myfun <- function(x, fun){
  return(fun(x))
}

myfun(c(1,2,3,4, 5), mean)
myfun(c(1,2,3,4,5), median)

class(myfun)           # function


# lapply sappla apply  are all high order functions
