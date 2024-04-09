library(tidyverse)
library(tidytext)
library(scales)
library(readr)
library(syuzhet)


# Load all Trump tweets from 2009 to 2021
trump_tweets <- read_csv("C:/Users/cz21o522/OneDrive - Universitaet Bern/BFH/Code & Data/6 Text Mining II/trump_tweets.csv")



#### 1. Do some preprocessing

# Select only the year prior to the election
trump_tweets$year <- as.numeric(format(trump_tweets$datetime, "%Y"))
trump_tweets <- trump_tweets %>% filter(year == 2015)

# remove links
trump_tweets$text <- gsub("http.*", "", trump_tweets$text)
trump_tweets$text <- gsub("https", "", trump_tweets$text)

# remove "rt" (when retweeted)
trump_tweets$text <- gsub("rt", "", trump_tweets$text)



# Tokenize (to lower case, remove punctuation)
trump_tweets.tokenized <- trump_tweets %>%
    unnest_tokens(word, text)

# load list of stop words from the tidytext package
data("stop_words")
# view first 10 words
head(stop_words, n = 10)

# How many words do we have right now?
nrow(trump_tweets.tokenized)

# remove stop words from your list of words
trump_tweets.cleaned <- trump_tweets.tokenized %>%
    inner_join(stop_words, join_by(word))

# check
nrow(trump_tweets.cleaned)


#### 2. Sentiment analysis

# let's get all the anger words from the nrc dictonary
nrc.anger <- get_sentiments("nrc") %>%
    filter(sentiment == "fear")

# left-join to keep all the words and add "anger"
trump_tweets.cleaned <- trump_tweets.cleaned %>%
    left_join(nrc.anger, join_by(word))

# binary indicator
trump_tweets.cleaned$sentiment.anger <- ifelse(is.na(trump_tweets.cleaned$sentiment), 0, 1)
table(trump_tweets.cleaned$sentiment.anger)

# also, just some sentiment score from last-time
trump_tweets.cleaned$sentiment.syuzhet <- get_sentiment(trump_tweets.cleaned$word,
    method = "syuzhet",
    lang = "english"
)

# Collapse data to tweets data again

# boolean to 0/1
trump_tweets.cleaned$is_retweet <- ifelse(trump_tweets.cleaned$is_retweet == TRUE, 1, 0)
trump_tweets.cleaned$is_deleted <- ifelse(trump_tweets.cleaned$is_deleted == TRUE, 1, 0)

tweets <- trump_tweets.cleaned %>%
    group_by(id) %>%
    summarise_at(vars(sentiment.anger, sentiment.syuzhet, is_retweet, is_deleted, datetime, favorites), list(mean))


#### 3. Modelling sentiment

# get hour of the tweet
tweets$hour <- format(tweets$datetime, format = "%H")
tweets$night <- ifelse(as.numeric(tweets$hour) > 6 & as.numeric(tweets$hour) < 20, 0, 1)

# split the data into training and test sets
set.seed(1727)
train.indices <- sample(nrow(tweets), 0.7 * nrow(tweets), replace = FALSE)
data.train <- data[train.indices, ]
data.test <- data[-train.indices, ]

# check distribution of target var (sentiment.anger)

hist(tweets$sentiment.anger)
hist(data.test$sentiment.anger)

# Train model
sentiment.reg <- lm(
    sentiment.anger ~ night +
        favorites,
    data = data.train
)

# Print results of the model
summary(sentiment.reg)

# Make predictions on the test data to evaluate your model's performance
predictions <- sentiment.reg %>% predict(data.test)
