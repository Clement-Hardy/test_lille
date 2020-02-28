fit_predict <- function(data, model, period=12, only_future=TRUE){
  if (model=="prophet"){
    pred <- prophet_fit_predict(train = data,
                                period = period)
    if (only_future){
      return (pred%>%top_n(period, wt=ds))
    }
    return (pred)
    
  }
  
  if (model=="tbats"){
    pred <- tbats_fit_predict(train = data,
                              period = period)
    if (only_future){
      return (pred%>%top_n(period, wt=ds))
    }
    return (pred)
  }
  
  if (model=="RF"){
    pred <- tbats_fit_predict(train = data,
                              period = period)
    if (only_future){
      return (pred%>%top_n(period, wt=ds))
    }
    return (pred)
  } 
}

cross_val_store <- function(model, data, nb_samples=3, period=12){
  metrics <- list(rmse=c(), rmspe=c())
  for (i in 1:nb_samples){
    nb_index <- dim(data)[1] - i*period
    data_temp <- data %>% top_n(-nb_index, wt=Date)
    pred <- fit_predict(data=data_temp,
                        model=model,
                        period=period,
                        only_future = TRUE)
    data_pred <- (data%>%slice((nb_index+1):(nb_index+12)))
    metrics$rmse <- c(metrics$rmse, RMSE(pred$yhat, data_pred$y))
    metrics$rmspe <- c(metrics$rmspe, RMSPE(pred$yhat, data_pred$y))
    #plot(data_pred$y,type='l')
    #lines(pred$yhat,col="red")
  }
  metrics$mean_rmse <- mean(metrics$rmse)
  metrics$mean_rmspe <- mean(metrics$rmspe)
  return (metrics)
}


cross_val_all_store <- function(model, data, nb_samples=3, period=12){
  metrics <- list(rmse=c(), mean_rmse=c(), rmspe=c())
  
  pb <- progress_bar$new(
    format = "  Cross validating [:bar] :percent eta: :eta",
    total = 100, clear = FALSE, width= 60)
  for (i in unique(data$Store)){
    pb$tick()
    metric <- cross_val_store(model=model,
                              data=filter(data,Store==i),
                              nb_samples = nb_samples,
                              period = period)
    
    metrics$rmse <- c(metrics$rmse, metric$rmse)
    metrics$rmspe <- c(metrics$rmspe, metric$rmspe)
  }
  metrics$mean_rmse <- mean(metrics$rmse)
  metrics$mean_rmspe <- mean(metrics$rmspe)
  return (metrics)
}

