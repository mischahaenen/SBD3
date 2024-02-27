#-----------------------------------------------------------------#
#-----------------------------------------------------------------#
#---------------------- R Recap - III ----------------------------#
#-----------------------------------------------------------------#
#-----------------------------------------------------------------#

# Resources used:
# - Kabacoff, R.I. (2015). R in Action (2nd ed). Manning Publishing
# - Wickham, H. and Grolemund, G. (2017), R for Data Sceince

# Learning objectives:
# * Learn how to write functions in R

# The environment
# Clear the environment
rm(list = ls())

# Set your environment
setwd("~/Library/CloudStorage/OneDrive-SharedLibraries-BernerFachhochschule/bfh-bfhinternal - General/SBD3/Week 1/Scripts")

# When should you write a function?
# In order to have reproducible results, we need to set the seed.
set.seed(10)
df <- data.frame(
    a = rnorm(10),
    b = rnorm(10),
    c = rnorm(10),
    d = rnorm(10)
)

# Imagine that for each column, you want to calculate something. For example,
# you might want to scale the data using the mix-max scaler (x-x_min/(x_max - x_min))
# To do this, you would need to copy-paste (and adjust) a block of code
# multiple times.

df$a <- (df$a - min(df$a, na.rm = TRUE)) /
    (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$b <- (df$b - min(df$b, na.rm = TRUE)) /
    (max(df$b, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$c <- (df$c - min(df$c, na.rm = TRUE)) /
    (max(df$c, na.rm = TRUE) - min(df$c, na.rm = TRUE))
df$d <- (df$d - min(df$d, na.rm = TRUE)) /
    (max(df$d, na.rm = TRUE) - min(df$d, na.rm = TRUE))

# What do you notice? Is there an error in the above block of code?

# For situations like this - it is better to write a function.
# Let's recreate the data
df <- data.frame(
    a = rnorm(10),
    b = rnorm(10),
    c = rnorm(10),
    d = rnorm(10)
)

range(df$a)
range(df$a)[1]
range(df$a)[2]
summary(df$a) # range gives us the min and max value of a column

# Let's make a function using range
rescale_new <- function(x) {
    range_new <- range(x, na.rm = TRUE)
    (x - range_new[1]) / (range_new[2] - range_new[1])
}

df_new <- data.frame(matrix(NA, nrow = 10, ncol = 4))

for (i in 1:ncol(df)) {
    df_new[, i] <- rescale_new(df[, i])
}

df
df_new

# There are three key steps to creating a new function:

# ** You need to pick a name for the function.

# ** You list the inputs, or arguments. Here we have just one argument. If we had more, the code would be: function(x, y, z).

# ** You place the code you have developed in body of the function, a block that immediately follows function(...).



# Some hints (also check, Chapter 19 - Kabacoff, R.I. (2015). R in Action (2nd ed). Manning Publishing):
# The name of a function should not be:
# 1) Too short
f()

# 2) Not descriptive
my_awesome_function()

# Example of a function names that are clear:
impute_missing()
collapse_years()


# Good for a family of functions
input_select()
input_checkbox()
input_text()

# Not so good
select_input()
checkbox_input()
text_input()


# Function arguments
# The arguments to a function typically fall into two broad sets:
# * one set supplies the data to compute on, and
# * the other supplies arguments that control the details of the computation

# Compute confidence interval around mean using normal approximation
mean_ci <- function(x, conf = 0.95) {
    se <- sd(x) / sqrt(length(x))
    alpha <- 1 - conf
    mean(x) + se * qnorm(c(alpha / 2, 1 - alpha / 2)) # qnorm - z score for the specified confidence interval
}

x <- runif(100)
mean_ci(x)
mean_ci(x, conf = 0.99)