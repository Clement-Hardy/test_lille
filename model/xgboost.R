library(xgboost)


xgboost_fit_predict <- function(train, test){
  X_train <- xgb.DMatrix(as.matrix(train %>% select(-y)), label=log(train$y))
  X_test <- xgb.DMatrix(as.matrix(test %>% select(-y)))
  xgb <- xgboost(data = X_train,
                 nrounds = 300)
  pred <- c(predict(xgb, newdata=X_train), predict(xgb, newdata=X_test))
  pred <- data.frame(ds=1:length(pred)[1], yhat=exp(pred))
  return(pred)
}

