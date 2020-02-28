library(forecast)



tbats_fit_predict <- function(train, period=12){
  model <- tbats(train$y)
  pred <- forecast(model, h=period) 
  fitted_value <- pred$fitted %>% as.numeric()
  pred <- pred$mean %>% as.numeric()
  pred <- c(fitted_value, pred)
  pred <- data.frame(ds=1:length(pred)[1], yhat=pred)
  return(pred)
}
