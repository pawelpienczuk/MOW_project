# main v1.0
# main functionality

rm(list = ls())

training_data <- read.csv("training.csv")
model_appliances <- lm(Appliances~T1+T2+T3+T4+T5+T6+T7+T8+T9+RH_1+RH_2+RH_3+RH_4+RH_5+RH_6+RH_7+RH_8+RH_9+T_out+Press_mm_hg+Windspeed+Visibility+Tdewpoint+rv1+rv2, data = training_data)
model_lights <- lm(Appliances~T1+T2+T3+T4+T5+T6+T7+T8+T9+RH_1+RH_2+RH_3+RH_4+RH_5+RH_6+RH_7+RH_8+RH_9+T_out+Press_mm_hg+Windspeed+Visibility+Tdewpoint+rv1+rv2, data = training_data)

print(model_appliances)
print(model_lights)
<<<<<<< Updated upstream

complete_data <- read.csv("energydata_complete.csv")
vars <- data.frame(
  date = substring(complete_data$date,1,10),
  time = substring(complete_data$date,12,20),
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
  Press_mm_hg = complete_data$Press_mm_hg,
  Windspeed = complete_data$Windspeed,
  Visibility = complete_data$Visibility,
  Tdewpoint = complete_data$Tdewpoint,
  rv1 = complete_data$rv1,
  rv2 = complete_data$rv2
)

write.csv(vars,"vars.csv", row.names = FALSE)
=======
>>>>>>> Stashed changes
