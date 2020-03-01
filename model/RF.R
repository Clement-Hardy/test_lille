library(randomForest)


RF_fit_predict <- function(train, test, only_future=TRUE){
  train <- train %>% subset(select=-Date)
  train$y <- log(train$y)
  model <- randomForest(y ~ ., data = train, ntree = 500)
  pred <- predict(model, newdata=test)
  pred <- data.frame(ds=1:length(pred)[1], yhat=exp(pred))
  return (pred)
}


#data <- prepare_data()
#train <- data %>% slice(1:120)
#test <- data %>% slice(121:144)