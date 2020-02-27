prophet_fit_predict <- function(train, period=12){
  data <- data.frame(ds=train$Date, y=train$y)
  model <- prophet(daily.seasonality = FALSE,
                   weekly.seasonality = FALSE)
  
  model <- add_seasonality(model, name='monthly',
                           period=30.5, fourier.order=1)
  model <- fit.prophet(model, data)
  future <- make_future_dataframe(model, periods = period, freq="week")
  pred <- predict(model, future) %>% select(ds, yhat)
  return(pred)
}