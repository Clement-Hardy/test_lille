library(nnfor)



mlp_fit_predict <- function(train, period=12, only_future=TRUE){
  train <- train %>% subset(select=-Date)
  X_train <- train %>% select(-y)
  y_train <- log(train$y) %>% ts()
  
  model <- mlp(y_train)
  
  pred <- forecast(model,
                   h=period) 
  
  fitted_value <- pred$fitted %>% as.numeric()
  pred <- pred$mean %>% as.numeric()
  pred <- c(fitted_value, pred)
  pred <- data.frame(ds=1:length(pred)[1], yhat=exp(pred))
  if (only_future){
    pred <- pred%>%top_n(period, wt=ds)
  }
  return(pred)
}

