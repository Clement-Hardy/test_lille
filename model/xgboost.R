

xgboost_fit_predict <- function(train, test, only_future=TRUE){
  train <- train %>% subset(select=-Date)
  test <- test %>% subset(select=-Date)
  X_train <- xgboost::xgb.DMatrix(as.matrix(train %>% select(-y)), label=log(train$y))
  X_test <- xgboost::xgb.DMatrix(as.matrix(test %>% select(-y)))
  model <- xgboost::xgboost(data = X_train,
                            nrounds = 400,
                            verbose=0)
  if (only_future){
    pred <- predict(model, newdata=X_test)
  }
  else{
    pred <- c(predict(model, newdata=X_train), predict(model, newdata=X_test))
  }
  pred <- data.frame(ds=1:length(pred)[1], yhat=exp(pred))
  return(pred)
}

