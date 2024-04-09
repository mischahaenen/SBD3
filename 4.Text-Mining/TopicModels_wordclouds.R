###################################################################################################
### TOPIC MODELING
### (c) Patrick Cichy, Berner Fachhochschule BFH; Modifications Christoph Zangger
###################################################################################################

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### STEP 1: LOAD PACKAGES AND SET WORKING DIRECTORY
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
library(tidyverse)
library(quanteda)
library(stopwords)
library(topicmodels)
library(tidytext)

options(scipen = 999)

# Set working directory
setwd("E:/OneDrive - Universitaet Bern/BFH/Code & Data/6 Text Mining II/")
getwd()


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### STEP 2: LOAD AND SELECT DATA (E.G. AMAZON REVIEWS)
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# load data
load("Activity_trackers.rda")

# OPTIONAL: Specify minimum text length (number of characters)
reviews <- subset(reviews, reviews$text_length > 100)

# OPTIONAL: Further selection (select variable to filter)
reviews <- subset(reviews, reviews$product == "Fitbit Charge 2")

# OPTIONAL: Create random sample
reviews <- sample_n(reviews, 1000)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### STEP 3: PREPARE TEXT DATA (TEXT PRE-PROCESSING)
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Define additional stopwords
extended_stopwords <- c("fitbit", "charge", "love", "one", "like", "get", "use", "can")

# Transform words into tokens, select basic text preprocessing steps
tokens <- tokens(reviews$reviewText,
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_numbers = TRUE,
    remove_url = TRUE,
    remove_separators = TRUE
)

# remove stopwords
tokens <- tokens_select(tokens, pattern = c(stopwords("english"), extended_stopwords), selection = "remove")

# transform to lowercase
tokens <- tokens_tolower(tokens)

# Stem all words
tokens <- tokens_wordstem(tokens)

# Create n-grams of any length
tokens <- tokens_ngrams(tokens, n = 1:2)

# Create Document-feature-matrix
myDfm <- dfm(tokens)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### STEP 4: ANALYZE TEXT
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create LDA model (specify number of topics)
reviews_lda <- LDA(myDfm, k = 3, control = list(seed = 1111))
topics <- as.data.frame(terms(reviews_lda, 50)) # First fifty words per topic

# Convert into tidy-format to visualize results
reviews_lda_td <- tidy(reviews_lda)
reviews_lda_td

# Extract top-terms per topic
top_terms <- reviews_lda_td %>%
    group_by(topic) %>%
    top_n(8, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)

# Visualize top-terms and their loadings (can you assign topic labels based on this information?)
top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
    facet_wrap(~topic, scales = "free") +
    coord_flip()

# Most different words among topics (using log ratios)
diff <- reviews_lda_td %>%
    mutate(topic = paste0("topic", topic)) %>%
    spread(topic, beta) %>%
    filter(topic1 > 0.001 | topic2 > 0.001 | topic3 > 0.001) %>%
    mutate(
        logratio_t1t2 = log2(topic2 / topic3),
        logratio_t1t3 = log2(topic3 / topic1),
        logratio_t2t3 = log2(topic3 / topic2)
    )
diff



# Link results to metadata
tmResult <- posterior(reviews_lda) # terms: p(term|topic); topics: p(document|topic)
theta <- tmResult$topics
lda_results <- cbind(reviews, theta) # add to org. reviews
rm(theta, reviews_lda, reviews_lda_td, tmResult, top_terms, tokens)


# Per-document-per-topic probabilities (gamma)
reviews.documents <- tidy(reviews_lda, matrix = "gamma")
reviews.documents <- reviews.documents[order(reviews.documents$document), ]
reviews.documents

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### STEP 5: GO BACK TO STEP 3 AND 4 TO RECONSIDER PREPROCESSING, STOPWORDS AND THE NUMBER OF TOPICS.
###         ITERATE MULTIPLE TIMES AND OBSERVE HOW RESULTS CHANGE. GOOD LUCK!
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### OPTIONAL: CHOOSE THE BEST NUMBER OF TOPICS BASED ON A METRIC
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

library(ldatuning)
library(parallel)
### Calculate different metrics to estimate the most preferable number of topics for LDA model
## Be aware: The procedure is computation intensive
# ldatuning uses parallelism, specify the correct number of CPU cores in mc.core parameter to archive best performance

detectCores()


# Calculate selected metrics
result <- FindTopicsNumber(
    myDfm,
    topics = seq(from = 2, to = 10, by = 1),
    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
    method = "Gibbs",
    control = list(seed = 77),
    mc.cores = 12L,
    verbose = TRUE
)

# plot results
FindTopicsNumber_plot(result)
