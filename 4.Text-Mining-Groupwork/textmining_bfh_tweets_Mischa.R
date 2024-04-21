# -------------------------------------------------------------------
# Question 1: How many tweets are being posted by the various Universities when? Are there any 'release' strategies visible? Explain me and show in R how I can generate diagrams and R do explain the interval of the postings.
# -------------------------------------------------------------------

# Use packages
library(tidyverse)
library(lubridate)

# Set working directory
setwd("../data/")

# Load data
load("Tweets_all.rda")

# Check that tweets are loaded
head(tweets)
summary(tweets)

# Preprocessing Step: Convert date and time to POSIXct and format according to date, year and university
tweets$created_at <- as.POSIXct(tweets$created_at, format = "%Y-%m-%d %H:%M:%S")
tweets$date <- as.Date(tweets$created_at)
tweets$year <- year(tweets$created_at)
tweets$university <- as.character(tweets$university)

# Count each tweet by university and hour of the day
tweet_counts_by_hour_of_day <- tweets %>%
    group_by(university, timeofday_hour) %>%
    count() %>%
    arrange(university, timeofday_hour)

# Plot the number of tweets by university and hour of the day
ggplot(tweet_counts_by_hour_of_day, aes(x = timeofday_hour, y = n, color = university)) +
    geom_point() +
    facet_wrap(~university) +
    labs(title = "Number of tweets by university and hour", x = "Hour of day", y = "Number of tweets")

# Show most active hours for each university
hours_with_most_tweets_by_uni <- tweet_counts_by_hour_of_day %>%
    group_by(university, timeofday_hour) %>%
    summarize(total_tweets = sum(n)) %>%
    group_by(university) %>%
    slice_max(n = 1, order_by = total_tweets)

print(hours_with_most_tweets_by_uni)

# Show most active hour overall
hour_with_most_tweets <- tweet_counts_by_hour_of_day %>%
    group_by(timeofday_hour) %>%
    summarize(total_tweets = sum(n)) %>%
    arrange(desc(total_tweets)) %>%
    slice(1)

print(hour_with_most_tweets)

# Calculate time intervals between tweets
tweets <- tweets %>%
    arrange(university, created_at) %>% # Arrange by university and timestamp
    group_by(university) %>% # Group by university
    mutate(time_interval = as.numeric(difftime(created_at, lag(created_at), units = "hours")))

# Descriptive statistics of time intervals
summary(tweets$time_interval)

unique_universities <- tweets$university %>% unique()

for (uni in unique_universities) {
    # Filter data for the specific university
    filtered_data <- tweets %>%
        filter(university == uni)

    # Create the plot
    ggplot(filtered_data, aes(x = time_interval)) +
        geom_histogram(fill = "lightblue") +
        labs(title = "Distribution of time intervals between tweets", x = "Time interval (hours)")

    # Save the plot (optional)
    # ggsave(filename = paste0(uni, "_tweet_plot.png"))
}

# Visualize distribution of time intervals





# -------------------------------------------------------------------
# Question 2: Content and Reaction Analysis
# -------------------------------------------------------------------


# -------------------------------------------------------------------
# Question 3: How do the university tweets differ in terms of content, style, emotions, etc?
# -------------------------------------------------------------------


# -------------------------------------------------------------------
# Question 4: What specific advice can you give us as communication department of BFH based on your analysis? How can we integrate the analysis of tweets in our internal processes, can you think of any data products that would be of value for us?
# -------------------------------------------------------------------
