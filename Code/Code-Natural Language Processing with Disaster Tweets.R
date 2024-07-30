library(tidyverse)
library(quanteda)
library(caret)
library(DT)


test_original = read_csv("Data/test.csv")

train_original = read_csv("Data/train.csv")

# Explore Data

str(train_original)

train_original %>% is.na() %>% colSums()

unique(train_original$keyword)

unique(train_original$location)

DT::datatable(train_original)

# Clean Data

  # Create a Corpus

    corpus_train = train_original %>% quanteda::corpus(text_field = "text")
    
  # Lower cases
 
  # Remove Emotes
    
  # Remove Contractions
    
  # Remove punctuation 
    
  # Stemming 
    
  # Remove URLs
    
  # Remove stop words
    
  
      
    