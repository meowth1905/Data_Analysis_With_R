# ==========================================
# R Lecture 3
# Neethu Raj
# Based on lecture series "Data analysis using R" by Prof. Danny Arends
# ==========================================

setwd("C:\\R_Tutorial")  # Set working directory

# ==========================================
# How to read data

# data()
# read.csv()/read.table()
# readLines()
# readBin()
# ==========================================

# Sources of data:
# 1. Random numbers (runif(), rnorm(), rpois())
# 2. R packages: can load with data()

# See all available R data sets
data(package = .packages(all.available = TRUE))

# Load the dataset
data("USArrests")

# View the first few rows
head(USArrests)

# View last few rows
tail(USArrests)        # can use tail to see if data set is fully loaded or not 

# What can we do with this data?
# We can for example see if Murder and Rape are correlated or not 
# and plot the results 

# Compute the correlation between Murder and Rape
cor(USArrests$Murder, USArrests$Rape)  # computes the Pearson correlation coefficient

# Scatter plot
plot(USArrests$Murder, USArrests$Rape,
     main = "Murder vs Rape Rates by State",
     xlab = "Murder Rate",
     ylab = "Rape Rate",
     pch = 19, col = "blue")

# Add a regression line
abline(lm(Rape ~ Murder, data = USArrests), col = "red")

# ==========================================
# 3. Structured Tabular Data
# ==========================================

# read.table() and read.csv() - (read.csv() is preferred, loads faster)
?read.csv 

# Often returns a data.frame

# Important parameters:
# header
# quote
# stringsAsFactors
# na.strings
# row.names
# sep
# col.names
# check.names
# skip
# colClasses 

# Note: row names should always be unique.
# Why? Because R uses them as identifiers
# can check using:

anyDuplicated(USArrests)

# Use dim() to verify that the number of rows and columns in the data.frame matches the file

# readLines() loads text-based files line by line as a character vector
# use n to set number of lines to read
# n = -1 reads everything to the end
# readLines("file from which you want to read from", n = -1)
# Useful to process data one line at a time 

#  ==========================================
# Connections:
# specify what you are going to do: r (read), w(write), a(append)
# r+, w+, and a+ allow all three
# ==========================================
# 
# Example to Read a Text File
# 
line.n <- 1                      # track line number
file("file_name", "r")           # file() opens a connection and returns a connection object

# Store the connection in a variable
myFile <- file("file_name", "r")

line <- readLines(myFile, n = 1)  # read one line

length(line <- readLines(myFile, n = 1)) > 0  # check if length > 0 (means we read something)

# length == 0 only when we are at the end of the file
# keep reading as long as we are not at the end of the file

while (length(line <- readLines(myFile, n = 1)) > 0) {
  cat(line, "\n")                 # show the line
  line.n <- line.n + 1            # increment line tracker
}

close(myFile)                      # close the connection, otherwise R issues a warning

# R can only open a limited number of files at a time.

# ==========================================
# 4. Archived Data
# ==========================================
# Read directly from the archive
oldFile <- gzfile("file_name.gz", "r")  # open a connection

while (length(line <- readLines(myFile, n = 1)) > 0) {
  cat(line, "\n")                        # show the line
  line.n <- line.n + 1                   # increment line tracker
}

close(oldFile)                            # close the connection

# ==========================================
# 5. Read from Web
# ==========================================
# use url()
myurl <- url("https://www.google.com/")  # open a connection, can only read from a URL
lines <- readLines(myurl, n = 1)         # or use n = -1 to read everything
close(myurl)                             # close the connection

# Read the terms of service for the website that you are connected to.


# ==========================================
# 6. Read from excel
# ==========================================

# Excel auto-converts certain gene names (e.g., OCT4, MARCH1, SEPT2) into dates.
# Changing to text AFTER conversion only shows Excel's internal date number.

# Excel should not be used for biological data because it silently alters values

# No native support for xlsx in R 
# But there packages available:

# install.packages("xlsx") - need java

install.packages("openxlsx")

library("openxlsx")

# Best practice is to always export excel files as csv (save as .csv)

# ==========================================
# 7. Binary files: for eg images
# ==========================================

readBin("binary_file", n = 1)                                # read  1 line
readBin("binary_file", n = file.info("binary_file")["size"]) # read the whole file

# there is a parameter what
# which controls how the file should read
# commonly use "raw"

# BMP images
# 2D array of pixels
# But stored as one long vector of hexadecimal values

# Reading binary data from a .bmp file

my_bmp.data <- readBin("bmp_file", n = as.numeric(file.info("bmp_file"))["size"], 
        what = "raw")

# before reading a bmp file, first remove its header which is 54 bytes
my_bmp.data <- my_bmp.data[~c(1:54)]

# Select blue components in the image (RGB, we get every third value)
indices <-  seq(1, length(my_bmp.data), 3)
blues <- matrix(as.numeric(my_bmp.data[indices]), 200, 200)

# Note: 200 here is the width and height of the bmp image

image(blues) 

# ==================================================
# How to manage your data

#  %in%
# A %in% B means which elements of A are also in B.
# returns TRUE, FALSE, ... vector

A[, "ID"] # get ID column
AB_IDs <- A[, "ID"] %in% B[, "ID"]
A[AB_IDs, ]   # create a subset of A 

# Note:
# A %in% B checks elements of A in B.
# B %in% A checks elements of B in A.

# which()
# returns the indices of elements that satisfy a condition

indices <- which(A[,"Cost"] > 10)  # Returns a vector of row numbers in A where
                                   # the "Cost" column is greater than 10.
A[indices, ]                       # Use indices to subset A.

A$Cost                             # Also Get Cost column, == A[, "Cost]
sum(A$Cost[indices])               # Sum of all "Cost" values greater than 10.

# subset()
# to extract/filter entire rows based on a condition.

subset(A, Cost > 10)              # returns all rows of A where the "Cost" column
                                  # is greater than 10.

data("airquality")
head(airquality)

day5 <- subset(airquality, Month == 5, select = c(Wind, Temp) )
head(day5)

day5 <- subset(airquality, Month == 5, select = -c(Wind, Temp) )
head(day5)

# ==================================================
# How to save your data

# text file to excel

?write.table
write.table(A, file = "file_name.txt", sep = "\t",
            row.names = FALSE,
            quote = FALSE)

# cat
cat("message")                                       # print to screen
cat("your_message", file = "log.txt", append = TRUE) # add message to log file
cat("", file = "log.txt")                            # clear log file



# ==================================================
# How to get external data
# manual search
# use FTP
# use biomaRt 

# biomaRt connects to most of biological databases

# We need to specify:
# which data base you want to connect to
# what you want to retrieve
# what values you are providing means (is it a list of gene names or something else)

library(biomaRt)

listMarts()                                         # which databases are available

marts <- useMart("ENSEMBL_MART_MOUSE")            # connect to ENSEMBL_MART_MOUSE
listDatasets(marts)                               # check for available data sets

# connect to this database, use this data set
gene.db <- useMart("ENSEMBL_MART_MOUSE", dataset = "mmc57bl6nj_gene_ensembl")

# which value we want
head(listAttributes(gene.db))

attrs <- listAttributes(gene.db)
subset(attrs, grepl("gene|chromosome|start|end", name, ignore.case = TRUE))

# What you will be providing for the search
head(listFilters(gene.db))

filters <- listFilters(gene.db)
subset(filters, grepl("symbol", name, ignore.case = TRUE))

genes <- c("Trp53", "Brca1", "Egfr", "Bsn")                      # your gene list

# Query for genes
getBM(attributes = c("external_gene_name", "chromosome_name", "start_position", "end_position"),
      filters = "mgi_symbol",
      values = genes,
      mart = gene.db)
               

