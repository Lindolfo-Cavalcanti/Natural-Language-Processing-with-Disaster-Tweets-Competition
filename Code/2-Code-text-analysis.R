# Load the required libraries
library(quanteda)  # For text analysis
library(tidyverse) # For data manipulation
library(tm)        # For text processing
library(caret)     # For model training
library(quanteda.textmodels) # For text modeling
library(quanteda.textplots) # For text visualization
library(RColorBrewer) # For color schemes

# Load the data
train.original = read_csv("Data/train.csv")  # Load the training data
test.original = read_csv("Data/test.csv")    # Load the test data

# Calculate the proportion of each target category
table(train.original$target) %>% prop.table()

# Convert the target column to a factor
train.original$target = as.factor(train.original$target)

# Convert the target column to factors
train.original$target[train.original$target == "1"] = "true"
train.original$target[train.original$target == "0"] = "false"

# Initialize the test target column
test.original$target = NA

# Convert the text column to lowercase
train.original$text = train.original$text %>% tolower()
test.original$text = test.original$text %>% tolower()

# Remove punctuation from the text columns
train.original$text = train.original$text %>% removePunctuation()
test.original$text = test.original$text %>% removePunctuation()

# Remove numbers from the text columns
train.original$text = train.original$text %>% removeNumbers()
test.original$text = test.original$text %>% removeNumbers()

# Remove stopwords from the text columns
train.original$text = train.original$text %>% removeWords(stopwords("en"))
test.original$text = test.original$text %>% removeWords(stopwords("en"))

# Remove extra whitespace from the text columns
train.original$text = train.original$text %>% stripWhitespace()
test.original$text = test.original$text %>% stripWhitespace()  

# Create document variables for the training and test data
docvars.train = data.frame(train.original$keyword, train.original$target)
docvars.test = data.frame(test.original$keyword, test.original$target)

# Create a corpus object for the training and test data
train.corpus = corpus(train.original$text, docvars = docvars.train)
test.corpus = corpus(test.original$text, docvars = docvars.test)

# Assign document IDs to the training and test corpus objects
doc.id = paste(train.original$id)
docnames(train.corpus) = doc.id

doc.id.test = paste(test.original$id)
docnames(test.corpus) = doc.id.test

# Tokenize the training and test corpus objects
train.token = tokens(train.corpus)
test.token = tokens(test.corpus)

# Perform stemming on the training and test token objects
train.token = train.token %>% quanteda::tokens_wordstem(language = "english")
test.token = test.token %>% quanteda::tokens_wordstem(language = "english")

# Remove emojis from the training and test token objects
emojis <- c("\U0001F600-\U0001F64F", "\U0001F300-\U0001F5FF", "\U0001F680-\U0001F6FF", "\U0001F1E0-\U0001F1FF", "\U00002702-\U000027B0", "\U000024C2-\U0001F251")
train.token = train.token %>% tokens_remove(pattern = emojis)
test.token = test.token %>% tokens_remove(pattern = emojis)

# Replace contractions in the training and test token objects
contractions = c("im",
                 "dont",
                 "û",
                 "ain't",
                 "aren't",
                 "can't",
                 "can't've",
                 "'cause",
                 "could've",
                 "couldn't",
                 "couldn't've",
                 "didn't",
                 "doesn't",
                 "don't",
                 "hadn't",
                 "hadn't've",
                 "hasn't",
                 "haven't",
                 "he'd",
                 "he'd've",
                 "he'll",
                 "he'll've",
                 "he's",
                 "how'd",
                 "how'd'y",
                 "how'll",
                 "how's",
                 "I'd",
                 "I'd've",
                 "I'll",
                 "I'll've",
                 "I'm",
                 "I've",
                 "isn't",
                 "it'd",
                 "it'd've",
                 "it'll",
                 "it'll've",
                 "it's",
                 "let's",
                 "ma'am",
                 "mayn't",
                 "might've",
                 "mightn't",
                 "mightn't've",
                 "must've",
                 "mustn't",
                 "mustn't've",
                 "needn't",
                 "needn't've",
                 "o'clock",
                 "oughtn't",
                 "oughtn't've",
                 "shan't",
                 "sha'n't",
                 "shan't've",
                 "she'd",
                 "she'd've",
                 "she'll",
                 "she'll've",
                 "she's",
                 "should've",
                 "shouldn't",
                 "shouldn't've",
                 "so've",
                 "so's",
                 "that'd",
                 "that'd've",
                 "that's",
                 "there'd",
                 "there'd've",
                 "there's",
                 "they'd",
                 "they'd've",
                 "they'll",
                 "they'll've",
                 "they're",
                 "they've",
                 "to've",
                 "wasn't",
                 "we'd",
                 "we'd've",
                 "we'll",
                 "we'll've",
                 "we're",
                 "we've",
                 "weren't",
                 "what'll",
                 "what'll've",
                 "what're",
                 "what's",
                 "what've",
                 "when's",
                 "when've",
                 "where'd",
                 "where's",
                 "where've",
                 "who'll",
                 "who'll've",
                 "who's",
                 "who've",
                 "why's",
                 "why've",
                 "will've",
                 "won't",
                 "won't've",
                 "would've",
                 "wouldn't",
                 "wouldn't've",
                 "y'all",
                 "y'all'd",
                 "y'all'd've",
                 "y'all're",
                 "y'all've",
                 "you'd",
                 "you'd've",
                 "you'll",
                 "you'll've",
                 "you're",
                 "you've"
)
contractions.replacement = c("i am",
                             "do not",
                             "you",
                            "am not",
                             "are not",
                             "cannot",
                             "cannot have",
                             "because",
                             "could have",
                             "could not",
                             "could not have",
                             "did not",
                             "does not",
                             "do not",
                             "had not",
                             "had not have",
                             "has not",
                             "have not",
                             "he would",
                             "he would have",
                             "he will",
                             "he will have",
                             "he is",
                             "how did",
                             "how do you",
                             "how will",
                             "how is",
                             "I would",
                             "I would have",
                             "I will",
                             "I will have",
                             "I am",
                             "I have",
                             "is not",
                             "it would",
                             "it would have",
                             "it will",
                             "it will have",
                             "it is",
                             "let us",
                             "madam",
                             "may not",
                             "might have",
                             "might not",
                             "might not have",
                             "must have",
                             "must not",
                             "must not have",
                             "need not",
                             "need not have",
                             "of the clock",
                             "ought not",
                             "ought not have",
                             "shall not",
                             "shall not",
                             "shall not have",
                             "she would",
                             "she would have",
                             "she will",
                             "she will have",
                             "she is",
                             "should have",
                             "should not",
                             "should not have",
                             "so have",
                             "so is",
                             "that would",
                             "that would have",
                             "that is",
                             "there would",
                             "there would have",
                             "there is",
                             "they would",
                             "they would have",
                             "they will",
                             "they will have",
                             "they are",
                             "they have",
                             "to have",
                             "was not",
                             "we would",
                             "we would have",
                             "we will",
                             "we will have",
                             "we are",
                             "we have",
                             "were not",
                             "what will",
                             "what will have",
                             "what are",
                             "what is",
                             "what have",
                             "when is",
                             "when have",
                             "where did",
                             "where is",
                             "where have",
                             "who will",
                             "who will have",
                             "who is",
                             "who have",
                             "why is",
                             "why have",
                             "will have",
                             "will not",
                             "will not have",
                             "would have",
                             "would not",
                             "would not have",
                             "you all",
                             "you all would",
                             "you all would have",
                             "you all are",
                             "you all have",
                             "you would",
                             "you would have",
                             "you will",
                             "you will have",
                             "you are",
                             "you have")

train.token = train.token %>% tokens_replace(pattern = contractions, replace = contractions.replacement)
test.token = test.token %>% tokens_replace(pattern = contractions, replace = contractions.replacement)

# Remove additional stopwords from the training token object

train.token = train.token %>% tokens_select(pattern = stopwords("en"), select = "remove")
test.token = test.token %>% tokens_select(pattern = stopwords("en"), select = "remove")

additional.stopwords = c("get", "just", "i am", "like", "lol", "u", "will", "will", "can", "got", "us", "do not", "may", "ûªs")
train.token =  train.token %>% tokens_remove(additional.stopwords)

# Create a document-term matrix for the training and test token objects
train.dfm = train.token %>% dfm()
test.dfm =  test.token %>% dfm()

# Create a color palette for the word cloud
pal = brewer.pal(5, "Dark2")

# Visualize the word cloud for the training data
textplot_wordcloud(train.dfm, min_count = 50, max_words = 200, color = pal)

# Train a Naive Bayes model on the training data
naive_bayes = textmodel_nb(x = train.dfm, y = train.original$target)

# Print the model summary
summary(naive_bayes)

# Match the test data with the training data
matched.dfm = dfm_match(test.dfm, features = featnames(train.dfm))

# Predict the target categories for the test data
predict.clas = predict(naive_bayes, newdata = matched.dfm)

# Update the test data with the predicted target categories
test.original$target = predict.clas
test.original$target = as.character(test.original$target)

# Prepare the test submission data
test.submission = test.original
test.submission$target[test.submission$target == "true"] = "1"
test.submission$target[test.submission$target == "false"] = "0"

# View the test submission data
View(test.submission)

# Remove unnecessary columns from the test submission data
test.submission$keyword = NULL
test.submission$location = NULL
test.submission$text = NULL

# Print the structure of the test submission data
glimpse(test.submission)

# Write the test submission data to a CSV file
write_csv(test.submission, "submission.csv")

