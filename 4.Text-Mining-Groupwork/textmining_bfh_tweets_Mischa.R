# -------------------------------------------------------------------
# Question 1: How many tweets are being posted by the various Universities when? Are there any 'release' strategies visible?
# -------------------------------------------------------------------

# use packages
library(tidyverse)
library(lubridate)

# should be folder SBD3 othwerwise change path with setwd()
getwd()
setwd("../data/")
load("Tweets_all.rda")

# check that tweets are loaded
head(tweets)
summary(tweets)

# Preprocessing Step
tweets$created_at <- as.POSIXct(tweets$created_at, format = "%Y-%m-%d %H:%M:%S")
tweets$date <- as.Date(tweets$created_at)
tweets$year <- year(tweets$created_at)
tweets$university <- as.character(tweets$university)

# Calculate time intervals in days between tweets
tweets <- tweets %>%
    arrange(university, year, date, created_at) %>%
    group_by(university, year, date) %>%
    mutate(time_interval = as.numeric(difftime(created_at, lag(created_at), units = "hours")))

# View time intervals
tweets$time_interval

ggplot(tweets, aes(x = tweet_hour, y = university)) +
    geom_line()

# Plotting the data
# Plotting the data
tweets %>%
    filter(!is.na(time_interval)) %>%
    ggplot(aes(x = time_interval)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black") + # Binwidth of 1 hour
    facet_wrap(~ university + year, scales = "free_y") +
    labs(
        title = "Tweet Time Intervals by University and Year",
        x = "Time Interval (hours)",
        y = "Frequency"
    ) +
    theme_minimal()

# Calculate interval summary statistics
interval_summary <- tweets %>%
    group_by(university, year) %>%
    summarise(
        average_interval = if (all(is.na(time_interval))) NA_real_ else mean(time_interval, na.rm = TRUE),
        median_interval = if (all(is.na(time_interval))) NA_real_ else median(time_interval, na.rm = TRUE),
        min_interval = if (all(is.na(time_interval))) NA_real_ else min(time_interval, na.rm = TRUE),
        max_interval = if (all(is.na(time_interval))) NA_real_ else max(time_interval, na.rm = TRUE),
        sd_interval = if (all(is.na(time_interval))) NA_real_ else sd(time_interval, na.rm = TRUE)
    )


# -------------------------------------------------------------------
# Question 2: Content and Reaction Analysis
# -------------------------------------------------------------------


# -------------------------------------------------------------------
# Question 3: How do the university tweets differ in terms of content, style, emotions, etc?
# -------------------------------------------------------------------


# -------------------------------------------------------------------
# Question 4: What specific advice can you give us as communication department of BFH based on your analysis? How can we integrate the analysis of tweets in our internal processes, can you think of any data products that would be of value for us?
# -------------------------------------------------------------------
