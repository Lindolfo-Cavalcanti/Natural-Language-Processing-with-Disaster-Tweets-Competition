# 1. Configuração do Ambiente

  library(quanteda)
  library(tidyverse)
  library(tm)
  library(caret)

# 2. Carregar e Preparar os Dados
  
  train.original = read_csv("Data/train.csv")
  test.original = read_csv("Data/test.csv")

# 3. Pré-processamento dos Dados
  
  train.original$text = train.original$text %>% tolower()
  test.original$text = test.original$text %>% tolower()
  
  train.original$text = train.original$text %>% removePunctuation()
  test.original$text = test.original$text %>% removePunctuation()
  
  train.original$text = train.original$text %>% removeNumbers()
  test.original$text = test.original$text %>% removeNumbers()
  
  train.original$text = train.original$text %>% removeWords(stopwords("en"))
  test.original$text = test.original$text %>% removeWords(stopwords("en"))
  
  train.original$text = train.original$text %>% stripWhitespace()
  test.original$text = test.original$text %>% stripWhitespace()  
  

# 4. Tokenização e Criação do Corpus
  
  train.corpus = corpus(train.original$text)
  test.corpus = corpus(test.original$text)
  
  train.token = tokens(train.corpus)
  test.token = tokens(test.corpus)
  
  train.token = train.token %>% quanteda::tokens_wordstem(language = "english")
  test.token = test.token %>% quanteda::tokens_wordstem(language = "english")

  emojis <- c("\U0001F600-\U0001F64F", "\U0001F300-\U0001F5FF", "\U0001F680-\U0001F6FF", "\U0001F1E0-\U0001F1FF", "\U00002702-\U000027B0", "\U000024C2-\U0001F251")

  train.token = train.token %>% tokens_remove(pattern = emojis)
  test.token = test.token %>% tokens_remove(pattern = emojis)
  
  contractions = c( 
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
  
  contractions.replacement = c("am not",
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
  
# 5. Dividir os Dados em Conjuntos de Treinamento e Teste

# 6. Treinamento do Modelo

# 7. Avaliação do Modelo

# 8. Ajuste e Melhoria do Modelo

# 9. Exportar e Utilizar o Modelo
