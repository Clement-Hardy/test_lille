library(forecast)



autoarima_fit_predict <- function(train, period=12, only_future=TRUE){
  model <- auto.arima(train$y)
  pred <- forecast(model, h=period) 
  fitted_value <- pred$fitted %>% as.numeric()
  pred <- pred$mean %>% as.numeric()
  pred <- c(fitted_value, pred)
  pred <- data.frame(ds=1:length(pred)[1], yhat=pred)
  
  if (only_future){
    pred <- pred%>%top_n(period, wt=ds)
  }
  return(pred)
}
