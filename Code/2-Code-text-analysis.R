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
contractions = c(...)
contractions.replacement = c(...)
train.token = train.token %>% tokens_replace(pattern = contractions, replace = contractions.replacement)
test.token = test.token %>% tokens_replace(pattern = contractions, replace = contractions.replacement)

# Remove additional stopwords from the training token object
additional.stopwords = c(...)
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

