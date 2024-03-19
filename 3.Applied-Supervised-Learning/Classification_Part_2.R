# ----------------------------------------------------------------------------- #
# ----------------------------------------------------------------------------- #
# --------------------------- CLASSIFICATION IN R ----------------------------- #
# --------------------------------- PART II------------------------------------ #
# ----------------------------------------------------------------------------- #

# In this script, we learn how we can use R to train and test different models
# for a classification task. Specifically, we will learn how to apply the
# following algorithms:
#  - XGBoost
#  - Support Vector Machine
#  - Neural Networks

# Within the script, we will also see all the data preparation necessary to run
# the different models and the different ways in which we can tune the algorithms.

# We start by installing the necessary libraries. Make sure to uncomment and run
# only the libraries which you haven't installed already.
install.packages("DescTools")
install.packages("xgboost")
install.packages("caret")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("mltools")
install.packages("reshape2")
install.packages("data.table")
install.packages("pracma")
install.packages("PRROC")
install.packages("e1071")
install.packages("dlookr")
install.packages("pROC")
install.packages("ROCR")
install.packages("nnet")
install.packages("rsample")

# Next, we call the required libraries.
library(DescTools)
library(xgboost)
library(caret)
library(dplyr)
library(tidyverse)
library(mltools)
library(reshape2)
library(data.table)
library(pracma)
library(rsample)
library(PRROC)
library(e1071)
library(dlookr)
library(pROC)
library(ROCR)
library(nnet)


# Load dataset: We will be working with the same dataset as in Week 4.
# Set working directory to the folder that contains the "Employee-Attrition_cleaned.csv" file.
setwd("~/Library/CloudStorage/OneDrive-SharedLibraries-BernerFachhochschule/bfh-bfhinternal - General/SBD3/Week 5")
hr_data <- read_csv("./3.Applied-Supervised-Learning/Employee-Attrition_cleaned.csv")
data <- hr_data # We make a copy from the original dataset and work on the copy


# Let's run basic check on the dataset
summary(data)
str(data)
table(data$Attrition)
PercTable(data$Attrition)


##################################################
########### XGBoost Training & Testing ###########
##################################################


#-------------- Data preparation ---------------#


# Step 1: Scaling of numeric variables

# For most ML algorithms, scaling is useful if not necessary. Namely, if the data
# contains features that are measured in different units or have very different ranges,
# this can affect either the outcomes of the model or the duration of training.

# For XGBoost, scaling is not generally necessary but still recommended as
# it might enable for faster convergence.

# Select only the numeric variables and scale them
data_scaled <- as.data.frame(sapply(data[, sapply(data, is.numeric)], scale))

# Combine the scaled numeric variables with the non-numeric variables
data_scaled <- cbind(data[, !sapply(data, is.numeric)], data_scaled)
data <- data_scaled

# Step 2: One-hot encoding

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

# Let's remove our dependent variable and binarize it.
data_encoded <- data_encoded[, -c(1:2)]
data_encoded$Attrition <- as.numeric(factor(data$Attrition, levels = c("No", "Yes"))) - 1

# Let's check the structure and diagnoze our data to make sure that the preprocessing
# was successful.
str(data_encoded)
diagnose(data_encoded)

# Step 3: Train and test split

# Remember: we set the seed so to make results reproducible.
set.seed(7)
split <- initial_split(data_encoded, prop = 0.7) # 70% for training and 30% for testing
data.train <- training(split)
data.test <- testing(split)

# Set the dependent variable and independent variables
y <- data.train$Attrition
x <- data.train[, !names(data.train) %in% "Attrition"] # We keep all other variables as explantory features

# Extract the independent variables from the test dataset (without out dependent variable)
x_test <- data.test[, !names(data.test) %in% "Attrition"]

# Train the XGBoost model
# Note on the parameters:
# ** nrounds [default=100] - It controls the maximum number of iterations.
#                            For classification, it is similar to the number of trees to grow.
# ** eta [default=0.3][range: (0,1)] - It controls the learning rate, i.e., the
#                                      rate at which our model learns patterns in data.
#                                      Lower eta leads to slower computation.
#                                      It must be supported by increase in nrounds.
#                                      Typically, it lies between 0.01 - 0.3
# ** max_depth[default=6][range: (0,Inf)] - It controls the depth of the tree.
#                                           Larger the depth, more complex the model;
#                                           higher chances of overfitting.
#                                           There is no standard value for max_depth.
#                                           Larger data sets require deep trees to
#                                           learn the rules from data.
# ** subsample[default=1][range: (0,1)] - It controls the number of samples (observations) supplied to a tree.
#                                         Typically, its values lie between (0.5-0.8)
# ** colsample_bytree[default=1][range: (0,1)] - It control the number of features (variables) supplied to a tree.
#                                                Typically, its values lie between (0.5,0.9)
# This is not an exclusive list: you can tune more parameters.

model_xgb <- xgboost(
    data = as.matrix(x),
    label = y,
    objective = "binary:logistic",
    nrounds = 100,
    eta = 0.1,
    max_depth = 6,
    subsample = 0.8,
    colsample_bytree = 0.8
)


# Predict the dependent variable using the trained model and the test dataset
predictions_xgb <- predict(model_xgb, as.matrix(x_test))


# Create confusion matrix
conf_mat_xgb <- confusionMatrix(as.factor(round(predictions_xgb)), as.factor(data.test$Attrition))
conf_mat_xgb


# Create ROC curve and calculate AUC value
roc_obj_xgb <- roc(data.test$Attrition, predictions_xgb)


# Plot ROC curve
plot(roc_obj_xgb)


# Slightly nicer visualization
plot(roc_obj_xgb,
    print.thres = c(0.2, 0.7, 0.9),
    grid = TRUE,
    grid.col = "lightgray",
    lwd = 2,
    col = "blue",
    legacy.axes = TRUE,
    main = "ROC Curve for the XGBoost Model",
    xlab = "False Positive Rate",
    ylab = "True Positive Rate"
)


# Obtain the AUC value
auc(roc_obj_xgb)


##################################################
############## SVM Training & Testing ############
##################################################


#-------------- Data preparation ---------------#


# Scaling is necessary for training an SVM model because SVM is based on the
# concept of maximizing the margin between the decision boundary and the support vectors.
# The margin is influenced by the scale of the input features. If the features
# have different scales, then the margin can be dominated by features with larger scales,
# leading to poor performance of the model.


# No additional data preparation needed.


# Train a model - no tuning
model_svm <- svm(Attrition ~ .,
    data = data.train,
    kernel = "linear", # Linear kernel
    cost = 1
) # c = 1


# Make predictions on test data set
predictions_svm <- predict(model_svm, newdata = x_test)


# Evaluate the model using confusion matrix
conf_mat_svm <- confusionMatrix(as.factor(round(predictions_svm)), as.factor(data.test$Attrition))
conf_mat_svm


# Create ROC curve and calculate AUC value
roc_obj_svm <- roc(data.test$Attrition, predictions_svm)


# Plot ROC curve
plot(roc_obj_svm)


# Slightly nicer visualization
plot(roc_obj_svm,
    print.thres = c(0.2, 0.7, 0.9),
    grid = TRUE,
    grid.col = "lightgray",
    lwd = 2,
    col = "blue",
    legacy.axes = TRUE,
    main = "ROC Curve for the XGBoost Model",
    xlab = "False Positive Rate",
    ylab = "True Positive Rate"
)


# Obtain the AUC value
auc(roc_obj_svm)


# Let's try a different implementation of the SVM where we will also tune the parameters


# First, we setup for cross validation
# Cross-validation is a technique for evaluating ML models by training several
# ML models on subsets of the available input data and evaluating them on the
# complementary subset of the data.

# Below we specify the cross validation that we will carry out.
# As a method, we select repeatedcv which stands for 10-fold cross validation.
# With this method we divide our data randomly into 10 parts. We use 9 of those
# parts for training and reserve one tenth for testing.
# Furthermore, we specify repeats = 5. This means we repeat this procedure 5 times
# each time reserving a different tenth for testing.

ctrl <- trainControl(
    method = "repeatedcv", # 10-fold cross validation
    repeats = 5, # do 5 repetitions of cv
    summaryFunction = twoClassSummary, # Use AUC to pick the best model
    classProbs = TRUE
)


# For us to use the "svmRadial" model from the caret package, the y needs to be
# a factor class. Hence, in the next lines, we adjust for this in our y and data.test objects.
y_1 <- factor(y, levels = c(0, 1), labels = c("0", "1"))
levels(y_1) <- c("No", "Yes")

data.test_1 <- data.test
data.test_1$Attrition <- as.factor(data.test_1$Attrition)
levels(data.test_1$Attrition) <- c("No", "Yes")


# Next, we train and tune the SVM. The next line takes a bit of time to run.
model_svm.tune <- train(
    x = x,
    y = y_1,
    method = "svmRadial", # Radial kernel
    metric = "ROC",
    trControl = ctrl
)


# Let's see the model
model_svm.tune


# Test the tuned SVM model, i.e. lets make predictions on test dataset.
predictions_svm.tune <- predict(model_svm.tune, newdata = x_test)


# Next we obtain the confusion matrix
conf_mat_svm.tune <- confusionMatrix(predictions_svm.tune, data.test_1$Attrition)
conf_mat_svm.tune

# Let's compare
conf_mat_svm
conf_mat_svm.tune


#################################################
########### NN Training and Testing  ############
#################################################


#-------------- Data preparation ---------------#

# Similarly as for the SVM model, the nnet function requires a y that has a
# factor class, so we ajudst for this in the following lines.
data.test_2 <- data.test
data.train_2 <- data.train
data.test_2$Attrition <- as.factor(data.test_2$Attrition)
data.train_2$Attrition <- as.factor(data.train_2$Attrition)

data.train_2$Attrition <- factor(data.train_2$Attrition, labels = c("No", "Yes"))
data.test_2$Attrition <- factor(data.test_2$Attrition, labels = c("No", "Yes"))

# Train the NN
# When you train a neural network (nnet) we need to specify two hyper-parameters:
# size and decay. Size is the number of units in hidden layer (nnet fit a single
# hidden layer neural network) and decay is the regularization parameter to avoid over-fitting.
model_nn <- nnet::nnet(Attrition ~ .,
    data = data.train_2,
    size = 10,
    decay = 5e-4,
    maxit = 1000,
    trace = FALSE
)

# Make predictions on the testing set
predictions_nn <- predict(model_nn, data.test_2)
predictions_nn_class <- round(predictions_nn)

# Get the confusion matrix
conf_mat_nn <- table(predictions_nn_class, data.test_2$Attrition)
conf_mat_nn


# Obtain the ROC curve
# In case you get any error on the following line -- it might be because of
# you try to use the “prediction” function in R while “neuralnet” library is
# attached. Fix:Simply detach the neural net library using command
# detach(package:neuralnet)
pred_nn <- prediction(predictions_nn, data.test_2$Attrition)
perf_nn <- performance(pred_nn, "tpr", "fpr")
plot(perf_nn,
    avg = "threshold",
    colorize = TRUE,
    lwd = 3,
    main = "ROC curve for the NN model"
)

# Obtain the AUC value
auc_nn <- performance(pred_nn, measure = "auc")
auc_nn@y.values[[1]]



###################################################
########### Additional implementation  ############
###################################################


# Let's try a different implementation where we can also do a bit of grid search
ctrl_nn <- trainControl(
    method = "repeatedcv",
    repeats = 5,
    classProbs = TRUE,
    summaryFunction = twoClassSummary
)

nnetGrid <- expand.grid(
    size = seq(from = 1, to = 10, by = 1),
    decay = seq(from = 0.1, to = 0.5, by = 0.1)
)

# This takes ±10 min to run.
model_nn_tune <- train(Attrition ~ .,
    data = data.train_2,
    method = "nnet",
    metric = "ROC",
    trControl = ctrl_nn,
    tuneGrid = nnetGrid,
    verbose = FALSE
)

# Make predictions on the testing set
predictions_nn_tune <- predict(model_nn_tune, data.test_2, type = "prob")

# Get the confusion matrix
conf_mat_nn_tune <- table(as.factor(round(predictions_nn_tune[[2]])), data.test_2$Attrition)
conf_mat_nn_tune

# Obtain the ROC curve
prediction_nn_tune_correct <- as.data.frame(predictions_nn_tune[[2]])
colnames(prediction_nn_tune_correct) <- "Prediction"
pred_nn_tune <- prediction(prediction_nn_tune_correct, data.test_2$Attrition)
perf_nn_tune <- performance(pred_nn_tune, "tpr", "fpr")
plot(perf_nn_tune,
    avg = "threshold",
    colorize = TRUE,
    lwd = 3,
    main = "ROC curve for the NN model"
)

# Obtain the AUC value
auc_nn_tune <- performance(pred_nn_tune, measure = "auc")
auc_nn_tune@y.values[[1]]
