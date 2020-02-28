library(gbm)


gbm_fit_predict <- function(train, test, only_future=TRUE){
  train$y <- log(train$y)
  train <- train %>% subset(select=-Date)
  test <- test %>% subset(select=-Date)
  model <- gbm(formula = y ~ .,
                data = train,
                n.trees = 5000,
                interaction.depth = 1,
                n.minobsinnode =9,
                shrinkage=0.005,
                distribution = "laplace",
                verbose=FALSE)
  if (only_future){
    pred <- predict(model, newdata=test, n.trees=5000)
  }
  else{
    pred <- c(predict(model, newdata=X_train), predict(model, newdata=X_test))
  }
  pred <- data.frame(ds=1:length(pred)[1], yhat=exp(pred))

  return(pred)
}