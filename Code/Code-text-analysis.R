


  clean_column_names <- function(colnames) {
    colnames <- gsub("[^[:alnum:]_]", "", colnames) 
    colnames <- make.names(colnames, unique = TRUE) 
    return(colnames)
  }
  
  names(data.frame.train.tfidf) <- clean_column_names(names(data.frame.train.tfidf))
  names(data.frame.test.reduced) <- clean_column_names(names(data.frame.test.reduced))
  
  
    cv.folds = createMultiFolds(train.original$target, k = 10, times = 3)
    cv.control = trainControl(method = "repeatedcv", number = 10, repeats = 3,
                              index = cv.folds)
    
    model.rpart = caret::train(target ~ ., data = data.frame.train.tfidf, 
                               method = "rpart", trControl = cv.control, tuneLength = 7)
