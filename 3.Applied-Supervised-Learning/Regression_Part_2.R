# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# ----------------------------- REGRESSION IN R ------------------------------- #
# --------------------------------- PART II------------------------------------ #
# ----------------------------------------------------------------------------- #

# In this script, we learn how we can use R to train and test different models
# for a regression task. Specifically, we will learn how to apply the following
# algorithms:
#  - XGBoost
#  - Support Vector Machine
#  - Neural Networks

# Within the script, we will also see all the data preparation necessary to run
# the different models and the different ways in which we can tune the algorithms.

# Libraries
library(readr)
library(ggplot2)
library(caret)
library(dlookr)
library(dplyr)
library(tidyverse)
library(mltools)
library(reshape2)
library(data.table)
library(pracma)
library(rsample)
library(PRROC)
library(e1071)
library(pROC)
library(ROCR)
library(nnet)
library(xgboost)
library(neuralnet)


# Load dataset: We will be working with the same dataset as in Week 4.
# Set working directory to the folder that contains the "barcelona_listings_cleaned.csv" file.
setwd("~/Library/CloudStorage/OneDrive-SharedLibraries-BernerFachhochschule/bfh-bfhinternal - General/SBD3/Week 5")
airbnb <- read_csv("barcelona_listings_cleaned.csv")
data <- airbnb # We make a copy from the original dataset and work on the copy


# Let's run basic check on the dataset
summary(data)
str(data)


# Let's check our dependent variable
summary(data$price)
ggplot(data, aes(x = price)) +
    geom_histogram(color = "blue") +
    labs(title = "Histograme of the Airbnb Prices")


##################################################
########### XGBoost Training & Testing ###########
##################################################


#-------------- Data preparation ---------------#

data <- data[, c(6:15)]

# One-hot encoding

# XGBoost manages only numeric vectors.
# Since our dataset contains categorical features as well, we need to run
# one-hot encoding.

# With one-hot, we convert each categorical value into a new numeric
# column and assign a binary value of 1 or 0 to those columns.
# Each integer value is represented as a binary vector.


# Define one-hot encoding function
dummy <- dummyVars(" ~ .", data = data)


# Perform one-hot encoding on the data
data_encoded <- data.frame(predict(dummy, newdata = data))


# Let's check the structure and diagnoze our data to make sure that the preprocessing
# was successful.
str(data_encoded)
diagnose(data_encoded)
summary(data_encoded)


# Train and test split

# Remember: we set the seed so to make results reproducible.
set.seed(7)
train_idx <- sample(nrow(data_encoded), 0.7 * nrow(data))
train <- data_encoded[train_idx, ]
test <- data_encoded[-train_idx, ]


# Train XGBoost model
model_xgb <- xgboost(
    data = as.matrix(train[, -17]), # excluding the target variable
    label = train$price,
    objective = "reg:squarederror",
    nrounds = 100
)


# Make predictions
predictions_xgb <- predict(model_xgb, as.matrix(test[, -17]))


# Evaluate model
mse_xgb <- mean((predictions_xgb - test$price)^2)
rmse_xgb <- sqrt(mse_xgb)
mae_xgb <- caret::MAE(test$price, predictions_xgb)


# Print RMSE
cat("RMSE of the XGB model:", round(rmse_xgb, 2))


# RMSE via the caret package
rmse_caret <- caret::RMSE(test$price, predictions_xgb)
rmse_caret


# R squared
r_squared_xgb <- cor(predictions_xgb, test$price)^2
r_squared_xgb


# Plotting
x <- 1:length(test$price[1:100])
plot(x, test$price[1:100], col = "red", type = "l")
lines(x, predictions_xgb[1:100], col = "blue", type = "l")



#################################################
############## SVM Training & Testing ############
#################################################


#-------------- Data preparation ---------------#


# Scaling is necessary for training an SVM model because SVM is based on the
# concept of maximizing the margin between the decision boundary and the support vectors.
# The margin is influenced by the scale of the input features. If the features
# have different scales, then the margin can be dominated by features with larger scales,
# leading to poor performance of the model.


# No additional data preparation needed.


# Train a model - no tuning
model_svm <- svm(price ~ .,
    data = train,
    kernel = "linear", # Linear kernel
    cost = 1
) # c = 1


# Make predictions on test data set
predictions_svm <- predict(model_svm, newdata = test)


# Evaluate model
mse_svm <- mean((predictions_svm - test$price)^2)
rmse_svm <- sqrt(mse_svm)
mae_svm <- caret::MAE(test$price, predictions_svm)
r_squared_svm <- cor(predictions_svm, test$price)^2
r_squared_svm

# Print RMSE
cat("RMSE for the basic SVM model:", round(rmse_svm, 2))



# Let's try the different implementation of the SVM where we will also tune the parameters


# First, we setup for cross validation


# Define parameter grid for tuning
tune_grid <- expand.grid(
    C = c(0.1, 1, 10),
    sigma = c(0.01, 0.1, 1)
)

# Train SVM model with cross-validation (takes some minutes)
model_svm_tune <- train(price ~ .,
    data = train, method = "svmRadial",
    trControl = trainControl(method = "cv", number = 5),
    tuneGrid = tune_grid
)


# Make predictions on testing data
predictions_svm_tune <- predict(model_svm_tune, newdata = test)


# Evaluate model performance
mse_svm_tune <- mean((predictions_svm_tune - test$price)^2)
rmse_svm_tune <- sqrt(mse_svm_tune)
r_squared_svm_tune <- cor(predictions_svm_tune, test$price)^2


# Print results
cat("RMSE of the tuned SVM model:", rmse_svm_tune, "\n")
cat("R-squared of the tuned SVM model:", r_squared_svm_tune, "\n")



#################################################
########### NN Training and Testing  ############
#################################################

data <- airbnb # We make a copy from the original dataset and work on the copy
data <- data[, c(6:15)]

# Select only the numeric variables and scale them
data_scaled <- as.data.frame(sapply(data[, sapply(data, is.numeric)], scale))

# Combine the scaled numeric variables with the non-numeric variables
data_scaled <- cbind(data[, !sapply(data, is.numeric)], data_scaled)
data <- data_scaled

# Define one-hot encoding function
dummy <- dummyVars(" ~ .", data = data)

# Perform one-hot encoding on the data
data_encoded <- data.frame(predict(dummy, newdata = data))

# Train and test split
set.seed(7)
train_idx <- sample(nrow(data_encoded), 0.7 * nrow(data))
train <- data_encoded[train_idx, ]
test <- data_encoded[-train_idx, ]

# Let's start by specifying the grid search so to tune hyperparameters for a neural network model
# In this case, the hyperparameters being searched are decay and size.
# The decay parameter controls the amount of weight decay regularization applied
# to the neural network, and the size parameter controls the number of nodes in
# the hidden layer of the neural network.
grid <- expand.grid(.decay = c(0.5, 0.1), .size = c(5, 6, 7))


# Having specified the grid search, let's train a neural network model (this might
# take some minutes)
model_nn <- train(price ~ .,
    data = train,
    method = "nnet",
    maxit = 100,
    tuneGrid = grid,
    trace = F,
    linout = 1
)


# Let's obtain the predictions
predictions_nn <- predict(model_nn, newdata = test)


# Evaluate model performance
mse_nn <- mean((predictions_nn - test$price)^2)
rmse_nn <- sqrt(mse_nn)
r_squared_nn <- cor(predictions_nn, test$price)^2


# Print results
cat("RMSE of the tuned NN model:", rmse_nn, "\n")
cat("R-squared of the tuned NN model:", r_squared_nn, "\n")


# Let's compare the performance across all different models.
# Comparison for the RMSE values
cat("RMSE of the XGBoost:", rmse_xgb, "\n")
cat("RMSE of the SVM:", rmse_svm, "\n")
cat("RMSE of the tuned SVM:", rmse_svm_tune, "\n")
cat("RMSE of the NN:", rmse_svm_tune, "\n")


# Comparison of the R-squared
cat("R-squared of the XGBoost:", r_squared_xgb, "\n")
cat("R-squared of the SVM:", r_squared_svm, "\n")
cat("R-squared of the tuned SVM:", r_squared_svm_tune, "\n")
cat("R-squared of the NN:", r_squared_nn, "\n")
