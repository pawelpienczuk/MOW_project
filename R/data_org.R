#data_org.R
# data collecting and organization

# training_data <- read.csv("training.csv")

#' Title
#'
#' @param data data to be organized
#' @param target "Appliances" or "lights"
#' @param testDataLength length of output subset (from 1 do testDataLength)
#'
#' @return organised data set (or subset, if testDataLength specified), 
#'
dataOrganization <- function(data,target,testDataLength=nrow(data)){
    
  #day_mon = day + 30 * month
  #min_hour = minutes + 60 * hours
  if (!is.data.frame(data)){
    message("data is no data.frame type")
    stop()  
  }
  if ((target!="Appliances")&(target!="lights"))
  {
    message("Target is not \"Appliances\" neither \"lights\" ")
    stop()
  }
  if (testDataLength<2|testDataLength>nrow(data)){
    message("testDataLength must be between 2 and length(data)")
    stop()
  }
  
  
  if (target == "Appliances"){
    out_data <- data.frame(
      Appliances = data$Appliances,
      month = as.numeric(substring(data$date,6,7)),
      day = as.numeric(substring(data$date,9,10)),
      hours = as.numeric(substring(data$date,12,13)),
      minutes = as.numeric(substring(data$date,15,16)),
      # day_mon = day_mon,
      # min_hour = min_hour,
      T1 = data$T1,
      T2 = data$T2,
      T3 = data$T3,
      T4 = data$T4,
      T5 = data$T5,
      T6 = data$T6,
      T7 = data$T7,
      T8 = data$T8,
      T9 = data$T9,
      RH_1 = data$RH_1,
      RH_2 = data$RH_2,
      RH_3 = data$RH_3,
      RH_4 = data$RH_4,
      RH_5 = data$RH_5,
      RH_6 = data$RH_6,
      RH_7 = data$RH_7,
      RH_8 = data$RH_8,
      RH_9 = data$RH_9,
      T_out = data$T_out,
      RH_out = data$RH_out,
      Press_mm_hg = data$Press_mm_hg,
      Windspeed = data$Windspeed,
      Visibility = data$Visibility,
      Tdewpoint = data$Tdewpoint,
      rv1 = data$rv1,
      rv2 = data$rv2
    )  
  } else if(target == "lights"){
    out_data <- data.frame(
      lights = data$lights,
      month = as.numeric(substring(data$date,6,7)),
      day = as.numeric(substring(data$date,9,10)),
      hours = as.numeric(substring(data$date,12,13)),
      minutes = as.numeric(substring(data$date,15,16)),
      # day_mon = day_mon,
      # min_hour = min_hour,
      T1 = data$T1,
      T2 = data$T2,
      T3 = data$T3,
      T4 = data$T4,
      T5 = data$T5,
      T6 = data$T6,
      T7 = data$T7,
      T8 = data$T8,
      T9 = data$T9,
      RH_1 = data$RH_1,
      RH_2 = data$RH_2,
      RH_3 = data$RH_3,
      RH_4 = data$RH_4,
      RH_5 = data$RH_5,
      RH_6 = data$RH_6,
      RH_7 = data$RH_7,
      RH_8 = data$RH_8,
      RH_9 = data$RH_9,
      T_out = data$T_out,
      RH_out = data$RH_out,
      Press_mm_hg = data$Press_mm_hg,
      Windspeed = data$Windspeed,
      Visibility = data$Visibility,
      Tdewpoint = data$Tdewpoint,
      rv1 = data$rv1,
      rv2 = data$rv2
    )  
  } else {
    print("Target can be only lights or Appliances")
    stop()
  }
  
    # checking scirpts on smaller dataset, comment to use full  
    out_data <- out_data[1:testDataLength,]
    
    # unused data to forget
}