#-----------------------------------------------------------------#
#-----------------------------------------------------------------#
#---------------------- R Recap - I ------------------------------#
#-----------------------------------------------------------------#
#-----------------------------------------------------------------#

# Resources used:
# - Kabacoff, R.I. (2015). R in Action (2nd ed). Manning Publishing
# - Wickham, H. and Grolemund, G. (2017), R for Data Science

# Learning objectives:
# * Recap on R's basic functionalities and data types

#  Creating vectors
# What are vectors?
# A vector is the simplest type of data structure in R. Simply put, a vector is
# a sequence of data elements of the same basic type. Members of a vector are called components.
# TODO: option + - for <-
a <- 4
a <- c(1, 2, 5, 3, 6, -2, 4) # The c() function creates any type of vector
b <- c("one", "two", "three")
c <- c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE)


# Using vector subscripts
a[3]
a[c(1, 3, 5)]
a[2:6]

a <- c("k", "j", "h", "a", "c", "m")
a[3]
a[c(1, 3, 5)]
a[2:6]


# Creating matrices
# What are matrices?
# Vectors with a dimension attribute or simply - collection of elements.
y <- matrix(1:20, nrow = 5, ncol = 4)
y

cells <- c(1, 26, 24, 68)
rnames <- c("R1", "R2")
cnames <- c("C1", "C2")
mymatrix <- matrix(cells,
    nrow = 2, ncol = 2, byrow = TRUE,
    dimnames = list(rnames, cnames)
)
mymatrix

mymatrix <- matrix(cells,
    nrow = 2, ncol = 2, byrow = FALSE,
    dimnames = list(rnames, cnames)
)
mymatrix


# Using matrix subscripts
x <- matrix(1:10, nrow = 2)
x
x[2, ]
x[, 2]
x[1, 4]
x[1, c(4, 5)]


# Creating an array
# What is an array?
# An array is a data structure that can hold multi-dimensional data.
# In R, the array is objects that can hold two or more than two-dimensional data.
dim1 <- c("A1", "A2")
dim2 <- c("B1", "B2", "B3")
dim3 <- c("C1", "C2", "C3", "C4")
z <- array(1:24, c(2, 3, 4), dimnames = list(dim1, dim2, dim3))
z


# Creating a dataframe
# # Unlike matrices, data frames can store different classes of objects in each column.
patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)
diabetes <- c("Type1", "Type2", "Type1", "Type1")
status <- c("Poor", "Improved", "Excellent", "Poor")
patientdata <- data.frame(patientID, age, diabetes, status)
patientdata


# Specifying elements of a dataframe
patientdata[1:2]
patientdata[c("diabetes", "status")]
patientdata$age


# Using factors
# Factors are variables in R which take on a limited number of different values;
# such variables are often referred to as categorical variables.
# Difference between factor and character:
# - Character/string - each element in the vector is a string of one or more characters.
# - A factor- type vector contains a set of numeric codes with character-valued levels.

patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)
diabetes <- c("Type1", "Type2", "Type1", "Type1")
status <- c("Poor", "Improved", "Excellent", "Poor")
patientdata_1 <- data.frame(patientID, age, diabetes, status)
str(patientdata_1)
summary(patientdata_1)

diabetes <- factor(diabetes)
status <- factor(status, order = TRUE, levels = c(
    "Poor",
    "Improved", "Excellent"
))
patientdata_2 <- data.frame(patientID, age, diabetes, status)
str(patientdata_2)
summary(patientdata_2)


# Creating a list
# A list is a generic vector containing other objects.
g <- "My First List"
h <- c(25, 26, 18, 39)
j <- matrix(1:10, nrow = 5)
k <- c(
    "one", "two", "three"
)
mylist <- list(g, h, j, k)
mylist


# Reading and writing datasets

# R has the ability to read and write to excel, which makes it very convenient to work on the
# same data sets as business analysts or colleagues who only know excel, meaning they can work with
# excel and hand you the files, then you work with them in R!

# To do this, we need the readxl package for R. Remember-for most of the functions you will
# need to install and call a particular library

# To read an excel file, you need the library readxl
install.packages("readxl")
install.packages("readr")
install.packages("writexl")

library(readxl)
library(readr)
library(writexl)


# Import Combined.xlsx

# Option 1 - set the work directory and read the file from the directory
# Change the pathway below
setwd("~/DataSets")
# or go to the Session --> Set working directory --> Choose directory --> Find the directory in your computer

getwd()


# Once the directory is set, read the file with the following line:
data <- read_excel("Combined.xlsx")

rm(list = ls())

# Option 2 - read directly the file from a specific pathway
data <- read_excel("~/Library/CloudStorage/OneDrive-SharedLibraries-BernerFachhochschule/bfh-bfhinternal - General/SBD3/Week 1/Scripts/Combined.xlsx")

rm(list = ls())

# Option 3
# Another way to import a data set is by clicking "Import Dataset" (from the Environment section),
# click "From Excel", then "Browse", then find the file in your computer and
# click "Import". Once you do that, the data set will be visible in the environment section!
# In summary, click Import Dataset --> From Excel --> Browse --> Find file --> Import


# Export or write excel files
# Once the working directory has been set, you can write an excel file from R.
# Let's create a random date.frame

# Definition of vectors
name <- c("Nick", "Sandra", "Monica")
age <- c(18, 25, 23)
sex <- c("Male", "Female", "Female")

data1 <- data.frame(name, age, sex)


# Export data1.xlsx to a directory
write_xlsx(data1, "data1.xlsx")

#---------------------------------- CSV FILES -------------------------------------#
# CSV stands for comma separated variable and its one of the most common format used.
# The basic format of a csv file is the first line indicating the column names and the
# rest of the rows/lines being data points separated by commas. One of the most basic
# ways to read in csv files in R is to use read.csv().

# Similarly as above, when using read.csv() you'll need to either pass in the entire path
# of the file or have the file be in the same directory as your R script.

rm(list = ls())


# Option 1
data <- read.csv("Combined.csv")


rm(list = ls())


# Option 2
data <- read.csv("~/Library/CloudStorage/OneDrive-SharedLibraries-BernerFachhochschule/bfh-bfhinternal - General/SBD3/Week 1/Scripts/Combined.csv")

# Option 3 --> import through the environment


rm(list = ls())


# Export or write csv files
# Once the working directory has been set, you can write a csv file from R.
# Let's create a random date.frame

# Definition of vectors
name <- c("Nick", "Sandra", "Monica", "Anna")
age <- c(18, 25, 23, 34)
sex <- c("Male", "Female", "Female", "Female")

data2 <- data.frame(name, age, sex)


# Export data2.csv to a directory
write.csv(data2, "data2.csv")

# Arithmetic operators
# These operators perform arithmetic on numeric or complex vectors
# (or objects which can be coerced to them).
x <- 1:4
y <- 2:5
x
y
+x
-x
x + y
x - y
x * y
x / y
x^y

# NA - Not Available
y <- c(1, 2, 3, NA)
is.na(y)
sum(y)
sum(y, na.rm = TRUE)