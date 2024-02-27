#-----------------------------------------------------------------#
#-----------------------------------------------------------------#
#--------------------- R Recap - V -------------------------------#
#-----------------------------------------------------------------#
#-----------------------------------------------------------------#

# Resources used:
# - Kabacoff, R.I. (2015). R in Action (2nd ed). Manning Publishing
# - Wickham, H. and Grolemund, G. (2017), R for Data Sceince

# Learning objectives:
# * Recap on simple modelling in R (correlations & linear regression)

# The environment
# Clear the environment
rm(list = ls())

# Set your environment
setwd("~/Library/CloudStorage/OneDrive-SharedLibraries-BernerFachhochschule/bfh-bfhinternal - General/SBD3/Week 1/Scripts")

# Install and call the necessary libraries
# install.packages("corrplot")
# install.packages("car")
library(corrplot)
library(car)

# Correlation
# Let's use the states data
?state.x77
state.x77
dim(state.x77)
str(state.x77)
summary(state.x77)

# Lets create an array called states that contains only the first 6 columns
states <- state.x77[, 1:6]

# Correlations
cor(states)

# Correlations of a subset of features
x <- states[, c("Population", "Income", "Illiteracy", "HS Grad")]
y <- states[, c("Life Exp", "Murder")]
cor(x, y)

# Visualization methods - full matrix
?corrplot
cor_matrix <- cor(states)
corrplot(cor_matrix) # default method is the circle
corrplot(cor_matrix, method = "pie") # we can also change the method
corrplot(cor_matrix, method = "color")
corrplot(cor_matrix, method = "number")

# Enhanced visualization
scatterplotMatrix(states,
    spread = FALSE, smoother.args = list(lty = 2),
    main = "Scatter Plot Matrix"
)

# Linear model
# Lets call a new dataset
data(women)
dim(women)
str(women)
summary(women)

# Simple linear regression
# We fit a linear model where we check whether women's height determines their
# weight
fit <- lm(weight ~ height, data = women)
summary(fit)

# Let's visualize the relationship
plot(women$height, women$weight,
    main = "Women Age 30-39",
    xlab = "Height (in inches)",
    ylab = "Weight (in pounds)"
)
# add the line of best fit
abline(fit)

# Multiple linear regression
states <- as.data.frame(state.x77[, c(
    "Murder", "Population",
    "Illiteracy", "Income", "Frost"
)])
fit_2 <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = states)
summary(fit_2)