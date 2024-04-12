# should be folder SBD3 othwerwise change path with setwd()
getwd()
setwd("./4.Text-Mining/groupwork_2/")
load("Tweets_all.rda")
# check that tweets are loaded
head(tweets)
summary(tweets)

# -------------------------------------------------------------------
# Question 1: How many tweets are being posted by the various Universities when? Are there any 'release' strategies visible?
# -------------------------------------------------------------------

# use packages
library(tidyverse)
library(lubridate)

# Preprocessing Step
tweets$university <- as.character(tweets$university)
tweets$created_at <- as.POSIXct(tweets$created_at, format = "%Y-%m-%d %H:%M:%S")
tweets$date <- as.Date(tweets$created_at)
tweets$year <- year(tweets$created_at)

# Analyze tweet frequency by university, year, and day
tweet_frequency <- tweets %>%
    group_by(university, year, date) %>%
    summarise(daily_tweets = n(), .groups = "drop")

# Split the analysis for each year for a detailed view
plot_list <- list()
years <- unique(tweet_frequency$year)

for (yr in years) {
    yearly_data <- filter(tweet_frequency, year == yr)
    p <- ggplot(yearly_data, aes(x = date, y = daily_tweets, color = university)) +
        geom_line() +
        labs(
            title = paste("Daily Tweet Frequency in", yr),
            x = "Date",
            y = "Number of Tweets"
        ) +
        theme_minimal() +
        theme(legend.position = "bottom")

    plot_list[[as.character(yr)]] <- p
}

#
print(plot_list[["2022"]])

# -------------------------------------------------------------------
# Question 2: Content and Reaction Analysis
# -------------------------------------------------------------------


# -------------------------------------------------------------------
# Question 3: How do the university tweets differ in terms of content, style, emotions, etc?
# -------------------------------------------------------------------


# -------------------------------------------------------------------
# Question 4: What specific advice can you give us as communication department of BFH based on your analysis? How can we integrate the analysis of tweets in our internal processes, can you think of any data products that would be of value for us?
# -------------------------------------------------------------------
