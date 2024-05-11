# -------------------------------------------------------------------
# Question 1: How many tweets are being posted by the various Universities when? Are there any 'release' strategies visible? Explain me and show in R how I can generate diagrams and R do explain the interval of the postings.
# -------------------------------------------------------------------

# Use packages
library(tidyverse)
library(lubridate)

# Set working directory
getwd()
setwd("../../data/")

# Load data
load("Tweets_all.rda")

# Check that tweets are loaded
head(tweets)
summary(tweets)

# Preprocessing Step: Convert date and time to POSIXct and format according to date, year and university
tweets$created_at <- as.POSIXct(tweets$created_at, format = "%Y-%m-%d %H:%M:%S")
tweets$date <- as.Date(tweets$created_at)
tweets$day <- weekdays(tweets$created_at)
tweets$day <- factor(tweets$day, levels = c(
  "Monday",
  "Tuesday",
  "Wednesday",
  "Thursday",
  "Friday",
  "Saturday",
  "Sunday"
))
tweets$year <- year(tweets$created_at)
tweets$university <- as.character(tweets$university)
View(tweets)

# Count each tweet by university and hour of the day
tweet_counts_by_hour_of_day <- tweets %>%
  group_by(university, timeofday_hour) %>%
  count() %>%
  arrange(university, timeofday_hour)

# Plot the number of tweets by university and hour of the day
ggplot(
  tweet_counts_by_hour_of_day,
  aes(
    x = timeofday_hour, y = n,
    color = university, group = university
  )
) +
  geom_line() +
  facet_wrap(~university) +
  labs(
    title = "Number of tweets by university and hour",
    x = "Hour of day",
    y = "Number of tweets"
  )

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

# Count each tweet by university and weekday
tweet_counts_by_week_day <- tweets %>%
  group_by(university, day) %>%
  count() %>%
  arrange(university, day)

# Plot the number of tweets by university and day of the week
ggplot(tweet_counts_by_week_day, aes(
  x = day,
  y = n, color = university,
  group = university
)) +
  geom_line() +
  facet_wrap(~university) +
  labs(
    title = "Number of tweets by university and day of the week",
    x = "Day of the week", y = "Number of tweets"
  )

# Show most active days for each university
days_with_most_tweets_by_uni <- tweet_counts_by_week_day %>%
  group_by(university, day) %>%
  summarize(total_tweets = sum(n)) %>%
  group_by(university) %>%
  slice_max(n = 1, order_by = total_tweets)

print(days_with_most_tweets_by_uni)

# Calculate time intervals between tweets
# Function to calculate mode -> https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode
find_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

tweets <- tweets %>%
  arrange(university, created_at) %>%
  group_by(university) %>%
  mutate(time_interval = as.numeric(
    difftime(created_at, lag(created_at), units = "mins")
  ))

# Descriptive statistics of time intervals
summary(tweets$time_interval)
View(tweets)

setwd("../4.Text-Mining-Groupwork/plots")
unique_years <- tweets$year %>% unique()
# Plot distribution of time intervals between tweets for each year
for (curr_year in unique_years) {
  # Filter data for the specific year
  filtered_data <- tweets %>%
    filter(year(created_at) == curr_year)

  ggplot(filtered_data, aes(x = time_interval)) +
    geom_histogram(fill = "lightblue") +
    facet_wrap(~university) +
    labs(
      title = paste0(
        "Distribution of time intervals between tweets - ", curr_year
      ),
      x = "Time interval (minutes)",
      y = "Frequency"
    )

  ggsave(filename = paste0("time_interval_plot_", curr_year, ".png"))
  # Save plot if needed

  universities <- filtered_data$university %>% unique()
  for (uni in universities) {
    # Filter data for the specific university
    filtered_data <- tweets %>%
      filter(university == uni & year(created_at) == curr_year)
    # Calculate mode (most common interval) in hours
    most_common_interval_minutes <- find_mode(filtered_data$time_interval)
    # Convert to days for better readability
    most_common_interval_hours <- most_common_interval_hours / 60
    print(paste0(
      "Most common time interval for ", uni,
      " in ",
      year,
      " is ", most_common_interval_minutes, " minutes"
    ))
  }
}




# -------------------------------------------------------------------
# Question 2: Content and Reaction Analysis
# -------------------------------------------------------------------


# -------------------------------------------------------------------
# Question 3: How do the university tweets differ in terms of content, style, emotions, etc?
# -------------------------------------------------------------------
# Data Preprocessing
library(tidyverse)
library(textclean)
library(lubridate)
library(wordcloud2)
library(sentimentr)
library(lexicon)
library(topicmodels)
library(tidytext)
library(tidyverse)
library(quanteda)
library(stopwords)
library(topicmodels)
library(tidytext)

getwd()
setwd("../../data/")
load("Tweets_all.rda")

# Removes: URLS, Emojis, Punctuation, Numbers
tokens <- tokens(tweets$full_text,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_numbers = TRUE,
  remove_url = TRUE,
  remove_separators = TRUE
)
# TODO: Add
extended_stopwords <- c()
# remove stopwords
tokens <- tokens_select(tokens,
  pattern = c(stopwords("english"), extended_stopwords), selection = "remove"
)

# transform to lowercase
tokens <- tokens_tolower(tokens)
# Stem all words
tokens <- tokens_wordstem(tokens)
# Create n-grams of any length
tokens <- tokens_ngrams(tokens, n = 1:2)
# Create Document-feature-matrix
doc_matrix <- dfm(tokens)

# ----------------------------
# Content Analysis
# ----------------------------
# Word Frequencies
word_freqs <- doc_matrix %>%
  colSums() %>%
  sort(decreasing = TRUE)

# Top 20 words
head(word_freqs, 20)

# Wordcloud
wordcloud2(doc_matrix, size = 0.6)


# N-grams wirh "design"
ngrams_with_design <- grep("design", names(word_freqs), value = TRUE)
word_freqs[ngrams_with_design]

# Custom Dictionary
dict_design <- c("design", "color", "look", "style", "appearance")
tweets$full_text <-
  str_count(tweets$full_text, paste(dict_design, collapse = "|"))
tweets$design_occurence <- ifelse(tweets$full_text > 0, 1, 0)

head(word_freqs, 15) %>%
  as.data.frame() %>%
  ggplot(aes(x = reorder(names(.), .), y = .)) +
  geom_col() +
  coord_flip() +
  labs(x = "N-grams", y = "Frequency")

# Bigram and Trigram Analysis
# TODO

# Plot Design Topic over Time
plot_data <- tweets %>%
  group_by(reviewDate_month) %>%
  summarize(n = sum(design_occurence))
ggplot(plot_data, aes(x = reviewDate_month, y = n)) +
  geom_bar(stat = "identity") +
  labs(title = "Design Topic Mentions per Month")

# Topic Modeling (LDA)
tweet_corpus <- Corpus(VectorSource(tweets$full_text))
tweet_dfm <- dfm(tweet_corpus)

lda_model <- LDA(tweet_dfm, k = 5, control = list(seed = 1234))
topic_terms <- tidy(lda_model, matrix = "beta")

topic_terms %>%
  group_by(topic) %>%
  top_n(8, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Beta") +
  facet_wrap(~topic, scales = "free") +
  coord_flip()


lda_model <- LDA(doc_matrix, k = 5, control = list(seed = 1234))

topic_terms <- tidy(lda_model, matrix = "beta")

topic_terms %>%
  group_by(topic) %>%
  top_n(8, beta) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()
# Style Analysis
# TODO: Context: Basic sentiment lexicons might miss sarcasm or irony in tweets. Explore pre-trained models or domain-specific lexicons for improved accuracy.
# TODO: Negation Handling: Consider handling negation words (e.g., "not good") that reverse sentiment.
# Tweet Length
tweets %>%
  mutate(tweet_length = nchar(tweet_text)) %>%
  ggplot(aes(x = tweet_length)) +
  geom_histogram() +
  labs(title = "Distribution of Tweet Lengths")

# Emoji Analysis (requires some emoji extraction technique)
tweets %>%
  filter(str_detect(tweet_text, "emoji")) %>%
  select(tweet_text, emojis) %>%
  unnest_tokens(emoji, emojis) %>%
  count(emoji, sort = TRUE)

# ----------------------------
# Sentiment Analysis
# ----------------------------

# Using AFINN Lexicon
tweets <- tweets %>%
  mutate(sentiment = textdata(full_text) %>%
    hash_lookup(key = afinn))

# Sentiment over Time
plot_data <- tweets %>%
  group_by(reviewDate_month) %>%
  summarize(mean_sentiment = mean(sentiment))

ggplot(plot_data, aes(x = reviewDate_month, y = mean_sentiment)) +
  geom_line() +
  labs(title = "Mean Sentiment over Time")

# Advanced: Handling Negation
tweets$negated_sentiment <- tweets$sentiment
tweets$negated_sentiment[grepl("not\\s", tweets$full_text)] <- -1 * tweets$negated_sentiment

# Sentiment Scores
tweets <- tweets %>%
  mutate(sentiment = get_sentiment(tweet_text))

# Visualize Sentiment Distribution
tweets %>%
  ggplot(aes(x = sentiment)) +
  geom_histogram() +
  labs(title = "Distribution of Tweet Sentiment")

# Comparison and Visualization
# Sentiment by University
tweets %>%
  ggplot(aes(x = university, y = sentiment, fill = university)) +
  geom_boxplot() +
  labs(title = "Sentiment Comparison Across Universities")

# ----------------------------
# Additional Analysis
# ----------------------------

cor(tweets$design_occurence, tweets$sentiment)

# Product Ambiguity
product_sentiment <- tweets %>%
  group_by(product) %>%
  summarize(sentiment_var = var(sentiment))
product_sentiment[order(product_sentiment$sentiment_var, decreasing = TRUE), ]

# ----------------------------
# Add On Analysis
# ----------------------------
all_products <- subset(reviews, text_length > 100)

all_products <- all_products %>%
  mutate(sentiment = textdata(full_text) %>%
    hash_lookup(key = afinn))

best_worst_products <- all_products %>%
  group_by(product) %>%
  summarize(mean_sentiment = mean(sentiment)) %>%
  arrange(desc(mean_sentiment))

best_worst_products
# -------------------------------------------------------------------
# Question 4: What specific advice can you give us as communication department of BFH based on your analysis? How can we integrate the analysis of tweets in our internal processes, can you think of any data products that would be of value for us?
# -------------------------------------------------------------------
