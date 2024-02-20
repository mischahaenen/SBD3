#-----------------------------------------------------------------#
#-----------------------------------------------------------------#
#---------------------- R Recap - IV -----------------------------#
#-----------------------------------------------------------------#
#-----------------------------------------------------------------#

# Resources used:
# - Kabacoff, R.I. (2015). R in Action (2nd ed). Manning Publishing
# - Wickham, H. and Grolemund, G. (2017), R for Data Sceince

# Learning objectives:
# * Recap on R graphics (ggplot2)

# The environment
# Clear the environment
rm(list = ls())

# Set your environment
setwd("~/Library/CloudStorage/OneDrive-SharedLibraries-BernerFachhochschule/bfh-bfhinternal - General/SBD3/Week 1/Scripts")

# Install and call the necessary libraries
# Remember: you only install packages once, but you call the libraries
# every time you need to use a certain function.

# install.packages("ggplot2")
# install.packages("tidyverse")

library(ggplot2)
library(tidyverse)

# Recap: ggplot2
# ggplot2 -- one of the core library for data visualization in R
# For our practical, we will be working with an integrated data set - mpg

# Call the data set "mpg"
data(mpg)
?mpg
head(mpg)
str(mpg)
summary(mpg)


# First task for us is to build a graph using ggplot2.
# The function below specifies the data frame containing the data to be plotted,
# the mapping of the variables and the geometric objects
ggplot(
    data = mpg,
    mapping = aes(
        x = displ,
        y = cty
    )
) +
    geom_point()


# Next, we can include different options in the geom_point() argument. Let's test the options
# in the following line. geom_point() understands the following aesthetics:
# **alpha
# **colour
# **group
# **shape
# **size
?geom_point()
vignette("ggplot2-specs")

# Let's play around with some of the aesthetics.
ggplot(
    data = mpg,
    mapping = aes(
        x = displ,
        y = cty
    )
) +
    geom_point(
        size = 2.5,
        shape = "diamond",
        colour = "cornflowerblue"
    )


# Let's continue adding options. We proceed by setting the alpha value as well.
ggplot(
    data = mpg,
    mapping = aes(
        x = displ,
        y = cty
    )
) +
    geom_point(
        size = 4,
        shape = "diamond",
        colour = "cornflowerblue",
        alpha = 0.5
    ) # alpha controls the transparency

# We can also use alpha to add another dimension in the visual representation
# Specifically, we can link alpha to a certain feature
ggplot(
    data = mpg,
    mapping = aes(
        x = displ,
        y = cty,
        alpha = year
    )
) +
    geom_point(
        size = 4,
        shape = "diamond",
        colour = "cornflowerblue"
    ) # alpha now presents the year

# Traditionally in data analysis, we want to infer something about the relationship between variables.
# Looking at out plot, what can we notice about the relationship between the two variables?

# Let's confirm our answer with a plot.
# Adding a line that best fits the data
ggplot(
    data = mpg,
    mapping = aes(
        x = displ,
        y = cty
    )
) +
    geom_point(
        size = 2.5,
        shape = "diamond",
        colour = "cornflowerblue"
    ) +
    geom_smooth(
        method = "lm", # Smoothed conditional means
        se = T
    ) # Display confidence interval around smooth
?geom_smooth
# lm -  linear regression model
# glm - the generalized linear model (GLM) is a flexible generalization of ordinary linear regression
#       that allows for response variables that have error distribution models other than a normal distribution.
# gam - a generalized additive model (GAM) is a generalized linear model in which the response
#       variable depends linearly on unknown smooth functions of some predictor variables
# loess - non-parametric strategy for fitting a smooth curve to data points


# Groups: We can also observe the scatter plot per groups, by including an additional argument in the mapping.
ggplot(
    data = mpg,
    mapping = aes(
        x = displ,
        y = cty,
        color = manufacturer
    )
) +
    geom_point(
        size = 2.5,
        shape = "diamond"
    ) +
    geom_smooth(
        method = "lm",
        se = F
    )


# Let's try plotting the same variables including a smooth function but this time,
# we want to observed separate plots per the individual manufacturers. For this purpose
# we use the facet_wrap argument
ggplot(
    data = mpg,
    mapping = aes(
        x = displ,
        y = cty,
        color = manufacturer
    )
) +
    geom_point(
        size = 2.5,
        shape = "diamond"
    ) +
    geom_smooth(method = "lm", se = F) +
    facet_wrap(~manufacturer)

?facet_wrap

# Labels - we discussed that the main objective of data visualization is to create clear and efficient plots
# Labels play a key role in this context hence in the next step we add the labs() function to the plot
ggplot(
    data = mpg,
    mapping = aes(
        x = displ,
        y = cty,
        color = manufacturer
    )
) +
    geom_point(
        size = 2.5,
        shape = "diamond"
    ) +
    geom_smooth(method = "lm", se = F) +
    facet_wrap(~manufacturer) +
    labs(
        title = "Relationship between the number of cylinders and engine displacement",
        subtitle = "Fuel economy data from 1999-2008, 38 models of cars",
        x = "Engine displacement, in liters",
        y = "Number of cylinders"
    )


# Additional geometric objects (bar plot & boxplots)
# Let's check a new dataset
data("diamonds")
head(diamonds)
tail(diamonds)
str(diamonds)
summary(diamonds)

# Geom_bar - barplot
# Barplots present categorical data with bars which heights proportional to
# the count. The bars can be horizontal or vertical.
ggplot(data = diamonds, mapping = aes(x = cut)) +
    geom_bar()

ggplot(data = diamonds, mapping = aes(x = cut)) +
    geom_bar() +
    coord_flip()

# Colour and fill
ggplot(data = diamonds, mapping = aes(x = cut, colour = cut)) +
    geom_bar()

ggplot(data = diamonds, mapping = aes(x = cut, fill = cut)) +
    geom_bar()

# Stacked barplot
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) +
    geom_bar() # the fill can be a different variable

# Segmented bar plot can also be used. Here each bar represents the 100-percent
# of the class variable
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) +
    geom_bar(position = "fill")

# Grouped bar plot: 'Dodge' is another option for representing two categorical
# variables in the same graph
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) +
    geom_bar(position = "dodge")

# Boxplot
# A box plot is a method for graphically depicting groups of numerical data through
# their quartiles.
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
    geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
    geom_boxplot() +
    coord_flip()