fit_predict <- function(data, model, period=12, only_future=TRUE, data_test=NULL){
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
    pred <- RF_fit_predict(train = data,
                          test=data_test,
                          only_future=only_future)
    return (pred)
  } 
  if (model=="xgboost"){
    pred <- xgboost_fit_predict(train = data,
                                test=data_test,
                                only_future=only_future)
    return (pred)
  } 
  if (model=="gbm"){
    pred <- gbm_fit_predict(train = data,
                            test=data_test,
                            only_future=only_future)
    return (pred)
  }
}



build_train_val_one_store <- function(data, period, num_sample=1){
  nb_index <- dim(data)[1] - num_sample*period
  data_train <- data %>% top_n(-nb_index, wt=Date)
  data_val <- (data%>%slice((nb_index+1):(nb_index+12)))
  return (list(data_train, data_val))
}



build_train_val <- function(data, period, num_sample=1){
  if (length(unique(data$Store))==1){
    temp <- build_train_val_one_store(data=data,
                                      period=period,
                                      num_sample = num_sample)
    data_train <- temp[[1]]
    data_val <- temp[[2]]
  }
  else{
    data_train <- data.frame()
    data_val <- data.frame()
    
    for (i in unique(data$Store)){
      temp <- build_train_val_one_store(data=filter(data, Store==i),
                                        period=period,
                                        num_sample = num_sample)
      data_train <- rbind(data_train, temp[[1]])
      data_val <- rbind(data_val, temp[[2]])
    }
  }
  return (list(data_train, data_val))
}



cross_val_store <- function(model, data, nb_samples=3, period=12){
  metrics <- list(rmse=c(), rmspe=c())
  for (i in 1:nb_samples){
    #nb_index <- dim(data)[1] - i*period
    #data_temp <- data %>% top_n(-nb_index, wt=Date)
    temp <- build_train_val(data=data,
                            period=period,
                            num_sample = i)
    data_train <- temp[[1]]
    data_val <- temp[[2]]
    pred <- fit_predict(data=data_train,
                        model=model,
                        period=period,
                        only_future = TRUE,
                        data_test = data_val)
    #data_pred <- (data%>%slice((nb_index+1):(nb_index+12)))
    metrics$rmse <- c(metrics$rmse, RMSE(pred$yhat, data_val$y))
    metrics$rmspe <- c(metrics$rmspe, RMSPE(pred$yhat, data_val$y))
    #plot(data_val$y,type='l')
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
    total = 45, clear = FALSE, width= 60)
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

