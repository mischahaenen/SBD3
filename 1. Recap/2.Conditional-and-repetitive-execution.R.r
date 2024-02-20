#-----------------------------------------------------------------#
#-----------------------------------------------------------------#
#---------------------- R Recap - II -----------------------------#
#-----------------------------------------------------------------#
#-----------------------------------------------------------------#

# Resources used:
# - Kabacoff, R.I. (2015). R in Action (2nd ed). Manning Publishing
# - Wickham, H. and Grolemund, G. (2017), R for Data Sceince

# Learning objectives:
# * Recap on R's functionalities concerning conditional and repetitive execution

# The environment
# Clear the environment
rm(list = ls())

# Set your environment
setwd("~/Library/CloudStorage/OneDrive-SharedLibraries-BernerFachhochschule/bfh-bfhinternal - General/SBD3/Week 1/Scripts")

# Installing packages
# install.packages("reshape2")  # if the package reshape2 is not installed on
# your computer, you should uncomment and run line 22 (install.packages("reshape2"))

# Calling necessary libraries
# Remember: every time you need to use functions that are part of a certain
# library, you need to call the library before use
library(reshape2)


# For loops
# When do we use for loops?
# The for() loops allows us to repeat a certain operation a fixed
# number of times.
# We are going to look at:
# - loops over vectors
# - loops over lists
# - loops over matrices


# Loops over a vector

# How does it work?
# for (name in vector) {commands}

# for the name (the index variable), typically i is used but it can be
# whatever you want

# for (var in seq) {
#   statement
# }

for (i in 1:10) {
    print(i^2)
}

# When do we use it?
# Imagine the following simple data frame:
df <- data.frame(
    a = rnorm(10),
    b = rnorm(10),
    c = rnorm(10),
    d = rnorm(10)
)

# We want to compute the median of each column. You could do it with copy-and-paste:
median(df$a)
median(df$b)
median(df$c)
median(df$d)


# Rule of thumb: never copy and paste more than twice!
# So the above action, breaks our rule of thumb
# Instead, we could use a for loop:
new_data <- vector("double", ncol(df)) # 1. output
for (i in seq_along(df)) { # 2. sequence The seq_along() in R is a built-in function that creates a vector that contains a sequence of numbers from 1 to the object’s length.
    new_data[[i]] <- median(df[[i]]) # 3. body
}
new_data



# If statements
# When do we use if statements?
# An if statement allows you to conditionally execute code. It looks like this:

# if (condition) {
# code executed when condition is TRUE
# } else {
# code executed when condition is FALSE
# }


# Let's try an example. Once you run line 98, you would need to enter your age
# in the console and press enter.

age <- as.numeric(readline("Input your age: "))

if (age >= 18) {
    print("You are an adult!")
} else {
    print("You are an teenager!")
}


# Another example
feelings <- c("sad", "afraid")
if (feelings == "sad") {
    print("Cheer up")
} else {
    print("I am glad you are happy")
}

# What happens?

# We need to use a for loop
for (i in feelings) {
    print(
        switch(i,
            happy  = "I am glad you are happy",
            afraid = "There is nothing to fear",
            sad    = "Cheer up",
            angry  = "Calm down now"
        )
    )
}

# One sample example on real data frame: mtcars
# You can check the basic information of a data.frame:
data(mtcars)
help(mtcars)
?mtcars
colnames(mtcars)
dim(mtcars)
str(mtcars)
summary(mtcars)

# Conditional execution
# Print rows for which the value of a certain column is larger then x

for (i in 1:nrow(mtcars)) {
    if (mtcars[i, "cyl"] > 6) print(mtcars[i, ])
}