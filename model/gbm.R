library(gbm)


gbm_fit_predict <- function(train, test, only_future=TRUE, optimized=TRUE){
  train$y <- log(train$y)
  train <- train %>% subset(select=-Date)
  train <- train %>% subset(select=-Temperature)
  test <- test %>% subset(select=-Date)
  if (optimized){
    model <- gbm(formula = y ~ .,
                 data = train,
                 n.trees = 400,
                 interaction.depth = 1,
                 n.minobsinnode =9,
                 shrinkage=0.05,
                 distribution = "laplace",
                 verbose=FALSE)
  }else{
    model <- gbm(formula = y~.,
                 data=train,
                 verbose=FALSE)
  }
  if (only_future){
    pred <- predict(model, newdata=test, n.trees=400)
  }
  else{
    pred <- c(predict(model, newdata=X_train), predict(model, newdata=X_test))
  }
  pred <- data.frame(ds=1:length(pred)[1], yhat=exp(pred))

  return(pred)
}
