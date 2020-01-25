#data_org.R
# data collecting and organization

# training_data <- read.csv("training.csv")
complete_data <- read.csv("energydata_complete.csv")

month = as.numeric(substring(complete_data$date,6,7))
day = as.numeric(substring(complete_data$date,9,10))
hours = as.numeric(substring(complete_data$date,12,13))
minutes = as.numeric(substring(complete_data$date,15,16))
day_mon = day + 30 * month
min_hour = minutes + 60 * hours

if (target_t == "Appliances"){
  test_data <- data.frame(
    Appliances = complete_data$Appliances,
    month = as.numeric(substring(complete_data$date,6,7)),
    day = as.numeric(substring(complete_data$date,9,10)),
    hours = as.numeric(substring(complete_data$date,12,13)),
    minutes = as.numeric(substring(complete_data$date,15,16)),
    # day_mon = day_mon,
    # min_hour = min_hour,
    T1 = complete_data$T1,
    T2 = complete_data$T2,
    T3 = complete_data$T3,
    T4 = complete_data$T4,
    T5 = complete_data$T5,
    T6 = complete_data$T6,
    T7 = complete_data$T7,
    T8 = complete_data$T8,
    T9 = complete_data$T9,
    RH_1 = complete_data$RH_1,
    RH_2 = complete_data$RH_2,
    RH_3 = complete_data$RH_3,
    RH_4 = complete_data$RH_4,
    RH_5 = complete_data$RH_5,
    RH_6 = complete_data$RH_6,
    RH_7 = complete_data$RH_7,
    RH_8 = complete_data$RH_8,
    RH_9 = complete_data$RH_9,
    T_out = complete_data$T_out,
    RH_out = complete_data$RH_out,
    Press_mm_hg = complete_data$Press_mm_hg,
    Windspeed = complete_data$Windspeed,
    Visibility = complete_data$Visibility,
    Tdewpoint = complete_data$Tdewpoint,
    rv1 = complete_data$rv1,
    rv2 = complete_data$rv2
  )  
} else if(target_t == "lights"){
  test_data <- data.frame(
    lights = complete_data$lights,
    month = as.numeric(substring(complete_data$date,6,7)),
    day = as.numeric(substring(complete_data$date,9,10)),
    hours = as.numeric(substring(complete_data$date,12,13)),
    minutes = as.numeric(substring(complete_data$date,15,16)),
    day_mon = day_mon,
    min_hour = min_hour,
    T1 = complete_data$T1,
    T2 = complete_data$T2,
    T3 = complete_data$T3,
    T4 = complete_data$T4,
    T5 = complete_data$T5,
    T6 = complete_data$T6,
    T7 = complete_data$T7,
    T8 = complete_data$T8,
    T9 = complete_data$T9,
    RH_1 = complete_data$RH_1,
    RH_2 = complete_data$RH_2,
    RH_3 = complete_data$RH_3,
    RH_4 = complete_data$RH_4,
    RH_5 = complete_data$RH_5,
    RH_6 = complete_data$RH_6,
    RH_7 = complete_data$RH_7,
    RH_8 = complete_data$RH_8,
    RH_9 = complete_data$RH_9,
    T_out = complete_data$T_out,
    RH_out = complete_data$RH_out,
    Press_mm_hg = complete_data$Press_mm_hg,
    Windspeed = complete_data$Windspeed,
    Visibility = complete_data$Visibility,
    Tdewpoint = complete_data$Tdewpoint,
    rv1 = complete_data$rv1,
    rv2 = complete_data$rv2
  )  
} else {
  print("Target can be only lights or Appliances")
  stop()
}

# checking scirpts on smaller dataset, comment to use full  
test_data <- test_data[1:testDataLength,]

# unused data to forget

rm(complete_data, day_mon, day,min_hour,minutes,month,hours)
