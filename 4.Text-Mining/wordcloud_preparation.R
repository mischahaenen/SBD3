#############################################
####    Visualizing Trump Tweets with     ###
####      a negative sentiment from       ###
####             2016 to 2020             ###
#############################################



library(tidyverse)
library(tidytext)
library(readr)
library(syuzhet)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)


rm(list = ls())

# Load all Trump tweets from 2009 to 2021
trump_tweets <- read_csv("C:/Users/cz21o522/OneDrive - Universitaet Bern/BFH/Code & Data/6 Text Mining II/trump_tweets.csv")

# Only keep data from 2019 & 2020


# Only keep the tweet text


# remove links

# remove "empty" tweets (that contained only a link before)
tweets <- tweets %>% filter(grepl("[a-zA-Z]", text))


# Tokenize (to lower case, remove punctuation; use unnest_tokens() )


tweets.tokenized %>% count(word, sort = TRUE)
# So there are all the stop words and "rt", let's remove them


# remove stopwords  (using stop_words from tidytext and then an anti_join)
# view first 10 stop words
head(stop_words, n = 10)


# remove stop words from list of words


# check most frequent words again:
# still the retweets...


# remove "rt"


tweets.cleaned %>% count(word, sort = TRUE) # now we're good to go

# sentiment (syuzhet) of all the remaining words
tweets.cleaned$sentiment.syuzhet <- get_sentiment(tweets.cleaned$word,
    method = "syuzhet",
    lang = "english"
)

# only keep those with a negative sentiment

# Term Document Matrix
term.matrix <- TermDocumentMatrix(tweets.cleaned$word)
term.matrix <- as.matrix(term.matrix)
v <- sort(rowSums(term.matrix), decreasing = TRUE)
data <- data.frame(word = names(v), freq = v)
head(data, 10) # so the most frequent word here is actually "trump"...


# Make the word cloud
set.seed(17)
wordcloud(
    words = data$word, freq = data$freq, min.freq = 10,
    max.words = 200, random.order = FALSE, rot.per = 0.35,
    colors = brewer.pal(8, "Dark2")
)
