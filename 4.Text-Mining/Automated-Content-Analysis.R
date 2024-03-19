###################################################################################################
### AUTOMATED CONTENT & SENTIMENT ANALYSIS
### (c) Patrick Cichy, Berner Fachhochschule BFH
###################################################################################################

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### STEP 1: LOAD PACKAGES AND SET WORKING DIRECTORY
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
install.packages("syuzhet")
library(syuzhet)
library(stringr)
library(tidyverse)
library(ggplot2)
library(scales)
options(scipen = 999)

# working directory
setwd("E:/OneDrive - Universitaet Bern/BFH/Code & Data/5 Text Mining I/")
getwd()


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### STEP 2: LOAD AND SELECT DATA (E.G. AMAZON REVIEWS)
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# load data
load("4.Text-Mining/Activity_trackers.rda")

# OPTIONAL: Specify minimum text length (number of characters)
reviews <- subset(reviews, reviews$text_length > 100)

# OPTIONAL: Further selection (select variable to filter)
reviews <- subset(reviews, reviews$product == "Fitbit Charge 2")

# number of reviews over time
plot_data <- reviews %>%
    group_by(reviewDate_month) %>%
    count()

ggplot(plot_data, aes(x = reviewDate_month, y = n)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    ggtitle("Number of reviews over time (per month)")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### STEP 3: PERFORM AUTOMATED CONTENT ANALYSIS
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Select text column and create your custom dictionary
reviews$dic1 <- "NA"
reviews$dic1 <- str_count(reviews$reviewText, "design|color|black|looks")
reviews$dic1_occurence <- "NA"
# select threshold for occurence of the topic
reviews$dic1_occurence <- ifelse(reviews$dic1 >= 2, 1, 0)
# number of reviews that cover topic
sum(reviews$dic1_occurence)

## VISUALIZE RESULTS
# sum of reviews that cover topic per day
plot_data <- reviews %>%
    group_by(reviewDate_month) %>%
    summarise(n = sum(dic1_occurence))

ggplot(plot_data, aes(x = reviewDate_month, y = n)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    ggtitle("Number of reviews covering the topic (per month)")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### STEP 4: PERFORM SENTIMENT ANALYSIS
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Select text column and calculate sentiment scores. You can change the method (e.g."syuzhet", "bing", "nrc")
reviews$sentiment <- "NA"
reviews$sentiment <- get_sentiment(reviews$reviewText, method = "syuzhet", lang = "english")

## VISUALIZE RESULTS
# mean over time
plot_data <- reviews %>%
    group_by(reviewDate_month) %>%
    summarise(n = mean(sentiment))

ggplot(plot_data, aes(x = reviewDate_month, y = n)) +
    geom_line() +
    theme_minimal() +
    ggtitle("Sentiment scores over time (mean per month)")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### STEP 5: PERFORM ADDITIONAL ANALYSIS
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# correlation of sentiment with custom dictionary
cor(reviews$dic1, reviews$reviewRating, method = "pearson")







##################################################################################################
##      As an add-on:
##      1) keep all reviews with at least 100 characters (not only the fitbit charge 2 ones)
##      2) calculate a sentiment score for all of them
##      3) Which is, on average, the most/least liked product?
##      4) Which is the most ambiguous product with regard to customer feedback/reviews?
##################################################################################################
