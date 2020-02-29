library(nnfor)



mlp_fit_predict <- function(train, test, only_future=TRUE){
  train <- train %>% subset(select=-Date)
  train <- train %>% subset(select=-MarkDown1)
  train <- train %>% subset(select=-MarkDown3)
  train <- train %>% subset(select=-MarkDown4)
  train <- train %>% subset(select=-MarkDown5)
  train <- train %>% subset(select=-Size)
  train <- train %>% subset(select=-Type)
  test <- test %>% subset(select=-Date)
  X_train <- train %>% select(-y)
  y_train <- log(train$y) %>% ts()
  X_test <- test %>% select(-y)
  
  model <- mlp(y_train,
               xreg=X_train)
  
  pred <- forecast(model, xreg=X_test) 
  
  fitted_value <- pred$fitted %>% as.numeric()
  pred <- pred$mean %>% as.numeric()
  pred <- c(fitted_value, pred)
  pred <- data.frame(ds=1:length(pred)[1], yhat=pred)
  if (only_future){
    pred <- pred%>%top_n(period, wt=ds)
  }
  return(pred)
}

