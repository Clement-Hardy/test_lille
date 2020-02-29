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
    
    temp <- data_temp$Temperature
    #data_temp$Temperature[13:length(temp)] <- temp[1:(length(temp)-13)]
    #data_temp$Temperature1[12:length(temp)] <- temp[2:(length(temp)-12)]
    
    temp <- data_temp$Fuel_Price
    #data_temp$Fuel_Price[12:length(temp)] <- temp[1:(length(temp)-12)]
    
    
    temp <- data_temp$Unemployment
    data_temp$Unemployment[13:length(temp)] <- temp[1:(length(temp)-12)]
    
    temp <- data_temp$y 
    data_temp <- data_temp %>% slice(14:(dim(data_temp)[1]))
    data_temp$y1 <- temp[2:(length(temp)-12)] - temp[1:(length(temp)-13)]
    data_temp$y2 <- temp[1:(length(temp)-13)]
    #data_temp$y3 <- temp[3:(length(temp)-14)]
    #data_temp$y4 <- temp[2:(length(temp)-15)]
    #data_temp$y5 <- temp[1:(length(temp)-16)]
    final_data <- rbind(final_data, data_temp)
  }
  
  
  final_data <- final_data %>% subset(select=-Fuel_Price)
  final_data <- final_data %>% subset(select=-Temperature)
  final_data <- final_data %>% subset(select=-year)
  final_data <- final_data %>% subset(select=-IsHoliday)
  final_data <- final_data %>% subset(select=-Unemployment)
  final_data <- final_data %>% subset(select=-MarkDown2)
  return (final_data)
}




prepare_data <- function(type_data="train", add_variable=TRUE){
  data <- read_data(type_data = type_data)
  stores <- read_data(type_data = "stores")
  features <- read_data(type_data = "features")
  
  if (type_data=="train"){
    data <- data_to_timeseries(data = data)
  }
  data <- merge(data, stores) %>% merge(features)
  data[is.na(data)] <- 0
  if (add_variable){
    data <- generate_delete_variables(data=data)
  }
  return(data)
}



data_to_timeseries <- function(data){
  data <- data %>% group_by(Store, Date)  %>% summarise(y=sum(Weekly_Sales))
  return(data)
}

