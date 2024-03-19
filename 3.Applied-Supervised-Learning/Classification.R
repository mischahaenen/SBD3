# -------------------------------------------------------------------------- #
# -------------------------------------------------------------------------- #
# --------------------------- CLASSIFICATION IN R -------------------------- #
# -------------------------------------------------------------------------- #
# -------------------------------------------------------------------------- #

# In this script, we learn how we can use R to train and test different models for a classification task.
# Specifically, we will learn how to apply the following algorithms:
#  - Logistic Regression
#  - Simple Decision Tree
#  - Random Forest

# We start by calling all the libraries that we are going to use.
library(readr)
library(dplyr)
library(caret)
library(DescTools)
library(rsample)
install.packages("DescTools")
library(pROC)
library(ROCR)
install.packages("ROCR")
library(randomForest)
library(rpart)
library(rpart.plot)
library(dlookr)
library(finalfit)

options(scipen = 999) # scientific notation: off



#################################################
##### Step 1: Data – Import and Data Check ######
#################################################
## Import data
# We set the working directory and we call the data that we are going to use. Please import the csv file "Employee-Attrition.csv".

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

# Import the data
hr_data <- read_csv("Employee-Attrition_cleaned.csv")
data <- hr_data # We make a copy from the original dataset and work on the copy

# Checking dimensions and structure of the data
dim(data)
str(data)
head(data)

# We check the summary of all variables included. Here might be able to identify some data quality issues.
summary(data)

# Examine if there are missing values
diagnose(data)
missing_pattern(data)



# Y: Check how represented each class is
PercTable(data$Attrition)
table(data$Attrition) / nrow(data) * 100 # don't always need new packages...




###############################################################
######## Step 2: Split data into train and test set  ##########
###############################################################

## Split the data into training and testing sets. Here, we use 70% of the observations for training and 30% for testing.
# To ensure that the target variable is represented equally in both data sets, we apply a stratified sampling approach.
set.seed(7) # Set random seed to make results reproducible
data$Attrition <- as.factor(data$Attrition)
split <- initial_split(data, prop = 0.7)
data.train <- training(split)
data.test <- testing(split)



PercTable(data.train$Attrition) # check the distribution of the target variable in train data
PercTable(data.test$Attrition) # check the distribution of the target variable in test data

####################################################################
## Step 3: Train and evaluate your model(s) [BASIC IMPLEMENTATION]##
##################################################################

##################################
### Logistic Regression ##########
##################################

# Train a model explaining Attrition (i.e. Y), specify independent variables (i.e. Xs)
logistic_model <- glm(
  Attrition ~ Age +
    DistanceFromHome +
    MonthlyIncome +
    JobSatisfaction,
  data = data.train,
  family = binomial()
)

# Print results of the model
summary(logistic_model)


# Make predictions on the test data
data.test$pred_score_lg <- predict(logistic_model, type = "response", data.test)

# Predict churn if probability is greater than 50%
data.test$pred_lg <- ifelse(data.test$pred_score_lg > 0.5, "Yes", "No")
data.test$pred_lg <- as.factor(data.test$pred_lg)

# Examine the confusion matrix
table(data.test$pred_lg, data.test$Attrition)

# Compute the accuracy on the test dataset
mean(data.test$pred_lg == data.test$Attrition) # How can we tell if this is good? What's the H0?




##################################
### Simple Classification Tree ###
##################################

## Train a model explaining Attrition (i.e. Y), specify independent variables (i.e. Xs)
# Specify: maximum tree depth and the complexity parameter (stopping parameter)
tree_model <- rpart(
  Attrition ~ Age +
    DistanceFromHome +
    MonthlyIncome +
    JobSatisfaction,
  data = data.train,
  method = "class",
  control = rpart.control(cp = 0.001, maxdepth = 4)
)

# Plot the decision tree
rpart.plot(tree_model, type = 5)

# Make predictions on the test data
data.test$pred_DT <- predict(tree_model, type = "class", data.test)

# Examine the confusion matrix
table(data.test$pred_DT, data.test$Attrition)

# Compute the accuracy on the test dataset
mean(data.test$pred_DT == data.test$Attrition)



##################################
### Random Forest ################
##################################

### Train a model explaining Attrition (i.e. Y), specify independent variables (i.e. Xs)
## ntree defines the number of trees to be generated
# mtry is the number of features used in the construction of each tree. Default value = sqrt(number of features)
RF_model <- randomForest(
  Attrition ~ Age +
    DistanceFromHome +
    MonthlyIncome +
    BusinessTravel +
    Gender +
    JobSatisfaction,
  data = data.train, ntree = 20, mtry = 4, importance = TRUE
)

# Make predictions on the test data
data.test$pred_RF <- predict(RF_model, type = "class", data.test)

# Examine the confusion matrix
table(data.test$pred_RF, data.test$Attrition)

# Compute the accuracy on the test dataset
mean(data.test$pred_RF == data.test$Attrition)

# Plot variable importance
varImpPlot(RF_model)

Yes





###############################################
#### Your turn: Can you improve these models?
###############################################
