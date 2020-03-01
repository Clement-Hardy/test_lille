fit_predict <- function(data, model, period=12, only_future=TRUE, data_test=NULL, add_reg=FALSE, optimized=TRUE){
  if (model=="prophet"){
    pred <- prophet_fit_predict(train = data,
                                period = period,
                                add_reg = add_reg)
    if (only_future){
      return (pred%>%top_n(period, wt=ds))
    }
    
  }
  
  if (model=="tbats"){
    pred <- tbats_fit_predict(train = data,
                              period = period)
    if (only_future){
      return (pred%>%top_n(period, wt=ds))
    }
  }
  
  if (model=="RF"){
    pred <- RF_fit_predict(train = data,
                           test=data_test,
                           only_future=only_future)
  } 
  if (model=="xgboost"){
    pred <- xgboost_fit_predict(train = data,
                                test=data_test,
                                only_future=only_future)
  } 
  if (model=="gbm"){
    pred <- gbm_fit_predict(train = data,
                            test=data_test,
                            only_future=only_future,
                            optimized=optimized)
  }
  if (model=="elm"){
    pred <- elm_fit_predict(train = data,
                            period=period,
                            only_future=only_future)
    
  }
  if (model=="mlp"){
    pred <- elm_fit_predict(train = data,
                            period=period,
                            only_future=only_future)
    
  }
  if (model=="autoarima"){
    pred <- autoarima_fit_predict(train = data,
                                  period=period,
                                  only_future=only_future)
  }
  if (model=="nnetar"){
    pred <- nnetar_fit_predict(train = data,
                               period=period,
                              only_future=only_future)
  }
  return (pred)
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



cross_val_store <- function(model, data, nb_samples=3, period=12, by_dept=FALSE, add_reg=FALSE, optimized=TRUE){
  metrics <- list(rmse=c(), rmspe=c())
  for (i in 1:nb_samples){
    if (by_dept==FALSE){
      temp <- build_train_val(data=data,
                              period=period,
                              num_sample = i)
      data_train <- temp[[1]]
      data_val <- temp[[2]]
      pred <- fit_predict(data=data_train,
                          model=model,
                          period=period,
                          only_future = TRUE,
                          data_test = data_val,
                          add_reg=add_reg,
                          optimized = optimized)
      metrics$rmse <- c(metrics$rmse, RMSE(pred$yhat, data_val$y))
      metrics$rmspe <- c(metrics$rmspe, RMSPE(pred$yhat, data_val$y))
    }
    else{
      sum_pred <- replicate(period, 0)
      sum_y_test <- replicate(period, 0)
      for (j in unique(data$Dept)){
        temp_data <- data %>% filter(Dept==j)
        if (dim(temp_data)[1]>100){
          temp <-  build_train_val(data = temp_data,
                                   period=period,
                                   num_sample = i)
        
          data_train <- temp[[1]]
          data_val <- temp[[2]]
          pred <- fit_predict(data=data_train,
                              model=model,
                              period=period,
                              only_future = TRUE,
                              data_test = data_val,
                              add_reg=add_reg,
                              optimized=optimized)
 
          if(any(is.na(pred$yhat))==FALSE & any(is.na(data_val$y))==FALSE){
           sum_pred <- sum_pred + pred$yhat
           sum_y_test <- sum_y_test + data_val$y
          }
        }
      }
      metrics$rmse <- c(metrics$rmse, RMSE(sum_pred, sum_y_test))
      metrics$rmspe <- c(metrics$rmspe, RMSPE(sum_pred, sum_y_test))
    }
  }
  metrics$mean_rmse <- mean(metrics$rmse)
  metrics$mean_rmspe <- mean(metrics$rmspe)
  return (metrics)
}




cross_val_all_store <- function(model, data, nb_samples=3, period=12, by_dept=FALSE, add_reg=FALSE, optimized=TRUE){
  metrics <- list(rmse=c(), mean_rmse=c(), rmspe=c())
  
  pb <- progress_bar$new(
    format = ":model cross validating [:bar] :percent eta: :eta",
    total = length(unique(data$Store)), clear = FALSE, width= 60)
  for (i in unique(data$Store)){
    pb$tick(tokens=list(model=model))
    metric <- cross_val_store(model=model,
                              data=filter(data,Store==i),
                              nb_samples = nb_samples,
                              period = period,
                              by_dept=by_dept,
                              add_reg=add_reg,
                              optimized=optimized)
    
    metrics$rmse <- c(metrics$rmse, metric$rmse)
    metrics$rmspe <- c(metrics$rmspe, metric$rmspe)
    
  }
  metrics$mean_rmse <- mean(metrics$rmse)
  metrics$mean_rmspe <- mean(metrics$rmspe)
  return (metrics)
}

