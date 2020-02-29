library(prophet)


prophet_make_future <- function(train, period=12){
  
  ## Temperature
  data <- data.frame(ds=train$ds, y=train$Temperature)
  model <- prophet(daily.seasonality = FALSE,
                   weekly.seasonality = FALSE)
  
  model <- fit.prophet(model, data)
  future <- make_future_dataframe(model, periods = period,
                                  freq="week")
  pred <- predict(model, future) %>% select(ds, yhat)
  pred$yhat <- pred$yhat
  names(pred)[2] <- "Temperature"
  pred <- (pred%>%top_n(period, wt=ds))
  
  
  ## CPI
  #data <- data.frame(ds=train$ds, y=train$CPI)
  #model <- prophet(daily.seasonality = FALSE,
  #                 weekly.seasonality = FALSE)
  
  #model <- fit.prophet(model, data)
  #future <- make_future_dataframe(model, periods = period,
  #                                freq="week")
  #pred_cpi <- predict(model, future) %>% select(ds, yhat)
  #pred_cpi$yhat <- pred_cpi$yhat
  #names(pred_cpi)[2] <- "CPI"
  #pred_cpi <- (pred_cpi%>%top_n(period, wt=ds))
  
  
  
  #pred <- pred %>% merge(pred_cpi)
  return(pred)
}



prophet_fit_predict <- function(train, period=12, add_reg=TRUE){
  data <- data.frame(ds=train$Date, y=log(train$y),
                     Temperature=train$Temperature)
  model <- prophet(daily.seasonality = FALSE,
                   weekly.seasonality = FALSE)
  
  model <- add_seasonality(model, name='monthly',
                           period=30.5, fourier.order=1)
  if (add_reg){
    model <- add_regressor(model, "Temperature",
                            mode='additive')
  }

  model <- fit.prophet(model, data)
  if (add_reg){
    future <- prophet_make_future(train=data)
  }
  else{
    future <- make_future_dataframe(model, periods = period,
                                    freq="week")
  }
  pred <- predict(model, future) %>% select(ds, yhat)
  pred$yhat <- exp(pred$yhat)
  return(pred)
}


