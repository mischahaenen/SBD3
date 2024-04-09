# should be folder SBD3 othwerwise change path with setwd()
getwd()
setwd("./4.Text-Mining/groupwork_2/")
load("Tweets_all.rda")
# check that tweets are loaded
head(tweets)
summary(tweets)

# use packages
library(dplyr)
library(ggplot2)
library(lubridate)

# 1. How many tweets are being posted by the various Universities when? Are there any 'release' strategies visible?

# Preprocessing Step (if needed)
tweets$university <- as.character(tweets$university)

# Aggregate tweets by university and hour
tweets_by_hour <- tweets %>%
    group_by(university, tweet_hour) %>%
    summarise(count = n())

# Visualize tweet frequency by hour
ggplot(tweets_by_hour, aes(x = tweet_hour, y = count, color = university)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Tweet Frequency by Hour", x = "Hour", y = "Tweet Count")

# Additional aggregations for exploration
tweets_by_day <- tweets %>%
    mutate(weekday = weekdays(as.Date(tweet_date))) %>%
    group_by(university, weekday) %>%
    summarise(count = n())

tweets$tweet_year <- year(tweets$tweet_date)

tweets_by_year <- tweets %>%
    group_by(university, tweets$tweet_year) %>%
    summarize(count = n())

ggplot(tweets_by_hour, aes(x = tweet_hour, y = count, color = university)) +
    geom_line() +
    facet_wrap(~ tweets$tweet_year) + # Split visualization by year
    theme_minimal() +
    labs(title = "Tweet Frequency by Hour (Split by Year)", x = "Hour", y = "Tweet Count")

ggplot(tweets_by_day, aes(x = weekday, y = count, color = university)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    facet_wrap(~ tweets$tweet_year) + # Split visualization by year
    theme_minimal() +
    labs(title = "Tweet Frequency by Day of Week (Split by Year)", x = "Weekday", y = "Tweet Count")


# 2. What are the tweets about and how do other Twitter users react to them (likes, etc.)?

# Load necessary libraries
library(tidyverse)
library(tidytext)
library(wordcloud)
library(lubridate) # For working with dates and time

# Load the dataset
load("./Groupwork_Textmining/Tweets_all.rda")

# -------------------------------------------------------------------
# Question 2: Content and Reaction Analysis
# -------------------------------------------------------------------

# Top words per university
tweets %>%
    unnest_tokens(word, full_text) %>%
    anti_join(stop_words) %>%
    count(university, word, sort = TRUE)

# Word clouds per university
generate_wordcloud <- function(data) {
    wordcloud(data$word, data$freq, max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Set2"))
}

tweets %>%
    unnest_tokens(word, full_text) %>%
    anti_join(stop_words) %>%
    group_by(university) %>%
    nest() %>%
    mutate(wordcloud = map(data, generate_wordcloud))

# Hashtag analysis (if hashtags are present)
tweets %>%
    filter(str_detect(full_text, "#")) %>%
    mutate(hashtags = str_extract_all(full_text, "#\\w+")) %>%
    unnest(hashtags) %>%
    count(university, hashtags, sort = TRUE)

# Engagement analysis
engagement_summary <- tweets %>%
    group_by(university) %>%
    summarize(
        avg_likes = mean(favorite_count),
        avg_retweets = mean(retweet_count),
        total_tweets = n()
    )

# Time-based engagement analysis
tweets %>%
    group_by(university, timeofday_hour) %>%
    summarize(
        avg_likes = mean(favorite_count),
        avg_retweets = mean(retweet_count)
    )


# 3. How do the university tweets differ in terms of content, style, emotions, etc?

# 4. What specific advice can you give us as communication department of BFH based on your analysis? How can we integrate the analysis of tweets in our internal processes, can you think of any data products that would be of value for us?
