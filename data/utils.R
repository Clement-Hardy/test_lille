library(dplyr)
#library(prophet)

read_data <- function(type_data="train"){
  
  if (type_data=="train"){
    return(read.csv("data/train.csv"))
  }
  if (type_data=="test"){
    return(read.csv("data/test.csv"))
  }
  if (type_data=="stores"){
    return(read.csv("data/stores.csv"))
  }
  if (type_data=="features"){
    return(read.csv("data/features.csv"))
  }
}



prepare_data <- function(type_data="train"){
  data <- read_data(type_data = type_data)
  stores <- read_data(type_data = "stores")
  features <- read_data(type_data = "features")
  
  if (type_data=="train"){
    data <- data_to_timeseries(data = data)
  }
  data <- merge(data, stores) %>% merge(features)
  data[is.na(data)] <- 0
  
  return(data)
}



data_to_timeseries <- function(data){
  data <- group_by(data, Store, Date)  %>% summarise(y=sum(Weekly_Sales))
  return(data)
}


prophet_fit_predict <- function(train, period=12){
  data <- data.frame(ds=train$Date, y=train$y)
  model <- prophet(data)
  future <- make_future_dataframe(model, periods = period)
  return(future)
}