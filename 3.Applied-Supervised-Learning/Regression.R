# -------------------------------------------------------------------------- #
# -------------------------------------------------------------------------- #
# --------------------------- REGRESSION IN R -------------------------- #
# -------------------------------------------------------------------------- #
# -------------------------------------------------------------------------- #

# In this script, we learn how we can use R to train and test different models for a regression problem.
# Specifically, we will learn how to apply the following algorithms:
#  - Linear Regression
#  - Regression Tree
#  - Random Forest Regression


# We call all the libraries that we are going to use
library(readr)
library(dlookr)
library(dplyr) # for data manipulation
library(caret) # machine learning workflow
library(rsample) # for re-sampling
library(rpart) # performing regression trees
library(rpart.plot) # plotting regression trees
library(randomForest) # implementation of the random forest algorithm
library(finalfit)

install.packages("dlookr")

options(scipen = 999) # scientific notation: off


#################################################
###### Step 1: Data – Import and Data Check ######
#################################################
## Import data
# We set the working directory and we call the data that we are going to use. Please import the csv file "barcelona_listings_cleaned.csv".

rm(list = ls())

place <- "home"

{
  # working directory
  if (place == "laptop") {
    setwd("C:/Users/zangg/OneDrive - Universitaet Bern/BFH/Code & Data/3 Supervised Learning 1")
  } else if (place == "work") {
    setwd("C:/Users/cz21o522/OneDrive - Universitaet Bern/BFH/Code & Data/3 Supervised Learning 1")
  } else if (place == "powerserver") {
    setwd("H:/Berechnungen Powerserver/")
  } else if (place == "home") {
    setwd("E:/OneDrive - Universitaet Bern/BFH/Code & Data/3 Supervised Learning 1")
  }
}

getwd()
setwd("./3.Applied-Supervised-Learning")
airbnb <- read_csv("barcelona_listings_cleaned.csv")
data <- airbnb # We make a copy from the original dataset and work on the copy

# Checking dimensions and structure of the data
dim(data)
str(data)
head(data)

# We check the summary of all variables included. Here might be able to identify some data quality issues.
summary(data)

# Examine if there are missing values
diagnose(data)
missing_pattern(data) # As a (visual) alternative
# as a comparison:
missing_pattern(airquality)


hist(data$price) # check the distribution of the target variable
#--> Right skewed: Any ideas on that?!


###############################################################
######## Step 2: Split data into train and test set  ##########
###############################################################

## Split the data into training and testing sets. Here, we use 70% of the observations for training and 30% for testing.
set.seed(7) # Set random seed to make results reproducible
split <- initial_split(data, prop = 0.7)
data_train <- training(split)
data_test <- testing(split)

# roughly the same as in base R:
train.indices <- sample(nrow(data), 0.7 * nrow(data), replace = FALSE)
data.train.2 <- data[train.indices, ]
data.test.2 <- data[-train.indices, ]

hist(data_train$price) # check the distribution of the target variable in train data
hist(data_test$price) # check the distribution of the target variable in test data


####################################################################
## Step 3: Train and evaluate your model(s) [BASIC IMPLEMENTAION]##
##################################################################

##################################
### Linear Regression ############
##################################

# Train a model explaining price (i.e. Y), specify independent variables (i.e. Xs)
regression_model <- lm(
  price ~ accommodates +
    bathrooms +
    minimum_nights +
    instant_bookable,
  data = data_train
)

# Print results of the model
summary(regression_model)

# Make predictions on the test data to evaluate your model's performance
predictions <- regression_model %>% predict(data_test)

# Compute the prediction error, RMSE
cat("RMSE (Linear Regression):", RMSE(predictions, data_test$price))

##################################
### Regression trees ############
##################################

### Train a model explaining price (i.e. Y), specify independent variables (i.e. Xs)
## Set parameters for tree growth:
# maxdepth= maximum tree depth; cp= complexity parameter (stoping parameter)
tree_model <- rpart(
  price ~ accommodates +
    bathrooms +
    minimum_nights +
    instant_bookable,
  data = data_train,
  method = "anova",
  control = rpart.control(cp = 0, maxdepth = 3)
)


# Plot the decision tree
rpart.plot(tree_model, type = 5)

# Make predictions on the test data to evaluate your model's performance
predictions <- tree_model %>% predict(data_test)

# Compute the prediction error, RMSE
cat("RMSE (Regression tree):", RMSE(predictions, data_test$price))


##################################
### Random Forest Regression  ###
##################################

### Train a model explaining price (i.e. Y), specify independent variables (i.e. Xs)
## Set parameters specific to random forest:
# ntree = number of trees to be generated; mtry = number of features used in the construction of each tree. Default value = sqrt(number of features)
randomForest_model <- randomForest(
  price ~ accommodates +
    bathrooms +
    bedrooms +
    cleaning_fee +
    minimum_nights +
    instant_bookable,
  data = data_train,
  ntree = 50, mtry = 3,
  importance = TRUE,
  cp = 0
)

# Plot variable importance
varImpPlot(randomForest_model)

# Make predictions on the test data to evaluate your model's performance
predictions <- randomForest_model %>% predict(data_test)

# Compute the prediction error, RMSE
cat("RMSE (Random Forest Regression):", RMSE(predictions, data_test$price))



###############################################
#### Your turn: Can you improve these models?
###############################################

# Your turn: Can you improve these models?
