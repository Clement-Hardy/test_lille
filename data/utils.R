library(dplyr)
library(prophet)
library(MLmetrics)
library(progress)
library(lubridate)


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


generate_delete_variables <- function(data){
  data$month <- month(as.Date(data$Date))
  data$week <- week(as.Date(data$Date))
  data$year <- year(as.Date(data$Date))
  data$Type <- as.numeric(data$Type)
  data$IsHoliday <- as.numeric(data$IsHoliday)
  final_data <- data.frame()
  for (i in unique(data$Store)){
    data_temp <- data %>% filter(Store==i)
    temp <- data_temp$CPI
    data_temp$CPI[13:length(temp)] <- temp[1:(length(temp)-12)]
    temp <- data_temp$Unemployment
    data_temp$Unemployment[13:length(temp)] <- temp[1:(length(temp)-12)]
    final_data <- rbind(final_data, data_temp)
  }
  
  
  #final_data <- final_data %>% subset(select=-Date)
  final_data <- final_data %>% subset(select=-Fuel_Price)
  final_data <- final_data %>% subset(select=-Temperature)
  return (final_data)
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
  data <- generate_delete_variables(data=data)
  return(data)
}



data_to_timeseries <- function(data){
  data <- group_by(data, Store, Date)  %>% summarise(y=sum(Weekly_Sales))
  return(data)
}

