library(tidyverse)
library(quanteda)
library(caret)
library(DT)
library(quanteda.textstats)

train_original = read_csv("Data/train.csv")

test_original = read_csv("Data/test.csv")

# Clean data

  # Tokenize
    
    train_token = train_original$text|> tokens(what = "word",
      remove_punct = TRUE,
      remove_symbols = TRUE,
      remove_numbers = TRUE,
      remove_hyphens = TRUE
    )

  # To lower

    train_token = train_token |> tokens_tolower()

  # Remove stopwords

    train_token = train_token |> tokens_remove(stopwords("en"))

  # Remove emotes

    emotes_patern = c("u0089û", "\U0001F600-\U0001F64F", "\U0001F300-\U0001F5FF", "\U0001F680-\U0001F6FF", "\U0001F1E0-\U0001F1FF", "\U00002702-\U000027B0", "\U000024C2-\U0001F251")
    
    train_token = train_token |> tokens_remove(emotes_patern)

  # Replace contractions

    contraction <- c("ain't",
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
    "you've")

    contraction_replacement <- c("am not",
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

    train_token = train_token |> tokens_replace(pattern = contraction, 
      replacement = contraction_replacement, valuetype = "fixed")


  # Stemming

      train_token = train_token |> tokens_wordstem(language = "english")

  # Remove URLs

      url_pattern = "http"
  
      train_token = train_token |> tokens_remove(pattern = url_pattern)

  # Remove whitespace

      train_token = train_token |> tokens_remove(pattern = "\\s+")

  # Remove u0089û_

      train_token = train_token |> tokens_remove(pattern = "u0089û_")

  # Remove NA's

      train_token = train_token[!is.na(train_token)]

  
# View token

      train_dfm = train_token |> dfm(tolower = FALSE)

      train_dfm <- train_dfm |> dfm_trim(min_termfreq = 4)

train_matrix = train_dfm |> as.matrix()

View(train_matrix)

train_token_df = cbind(
  target = train_original$target, convert(train_dfm, to = "data.frame"))

View(train_token_df)

# Cleanup colnames

 names(train_token_df) = make.names(names(train_token_df))

# Create Model

  # Cross Validation with caret

    cv_folds = train_original$target |> caret::createMultiFolds(k = 10, times = 3 )
    cv_cntrl = caret::trainControl(method = "repeatedcv", number = 10,
      repeats = 3, index = cv_folds)


  # doSNOW
    library(doSNOW)

    cl = makeCluster(4, type = "SOCK")
    registerDoSNOW(cl)