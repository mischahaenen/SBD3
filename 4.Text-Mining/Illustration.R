#################################################
### Tokenize "I have a dream" and wordcloud   ###
#################################################



# Install
# install.packages("tm")  # for text mining
# install.packages("SnowballC") # for text stemming
# install.packages("wordcloud") # word-cloud generator
# install.packages("RColorBrewer") # color palettes
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

# Read the text, line by line
speech <- readLines("E:/OneDrive - Universitaet Bern/BFH/Code & Data/5 Text Mining I/I have a dream.txt")

# Load the data as a corpus
corpus <- Corpus(VectorSource(speech))

# Replace special characters with blank space, if there are any using gsub
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus <- tm_map(corpus, toSpace, "/")
corpus <- tm_map(corpus, toSpace, "@")
corpus <- tm_map(corpus, toSpace, "\\|")


# Convert the text to lower case
corpus <- tm_map(corpus, content_transformer(tolower))
# Remove numbers
corpus <- tm_map(corpus, removeNumbers)
# Remove english common stopwords
corpus <- tm_map(corpus, removeWords, stopwords("english"))
# Remove punctuations
corpus <- tm_map(corpus, removePunctuation)
# Eliminate extra white spaces
corpus <- tm_map(corpus, stripWhitespace)
# Text stemming (remove suffixes, etc.; only root form of the word)
# corpus <- tm_map(corpus, stemDocument) #doesn't work that nicely

# Make a term matrix (i.e., frequency of words table)
term.matrix <- TermDocumentMatrix(corpus)
term.matrix <- as.matrix(term.matrix)
v <- sort(rowSums(term.matrix), decreasing = TRUE)
data <- data.frame(word = names(v), freq = v)
head(data, 10)


# Make word clodu
set.seed(17)
wordcloud(
    words = data$word, freq = data$freq, min.freq = 1,
    max.words = 200, random.order = FALSE, rot.per = 0.35,
    colors = brewer.pal(8, "Dark2")
)
