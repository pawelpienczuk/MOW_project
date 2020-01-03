# main v1.0
# main functionality

library(rsample)     # data splitting 
library(dplyr)       # data wrangling
library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees

rm(list = ls())

training_data <- read.csv("training.csv")

# creating model
model_appliances <- lm(Appliances~T1+T2+T3+T4+T5+T6+T7+T8+T9+RH_1+RH_2+RH_3+RH_4+RH_5+RH_6+RH_7+RH_8+RH_9+T_out+Press_mm_hg+Windspeed+Visibility+Tdewpoint+rv1+rv2, data = training_data)
model_lights <- lm(lights~T_out+Press_mm_hg+Windspeed+Visibility, data = training_data)

# printing model's coefs 
print(model_appliances)
print(model_lights)

# REDUNDANT - data organization
complete_data <- read.csv("energydata_complete.csv")
# vars <- data.frame(
#   month = substring(complete_data$date,6,7),
#   day = substring(complete_data$date,9,10),
#   hours = substring(complete_data$date,12,13),
#   minutes = substring(complete_data$date,15,16),
#   T1 = complete_data$T1,
#   T2 = complete_data$T2,
#   T3 = complete_data$T3,
#   T4 = complete_data$T4,
#   T5 = complete_data$T5,
#   T6 = complete_data$T6,
#   T7 = complete_data$T7,
#   T8 = complete_data$T8,
#   T9 = complete_data$T9,
#   RH_1 = complete_data$RH_1,
#   RH_2 = complete_data$RH_2,
#   RH_3 = complete_data$RH_3,
#   RH_4 = complete_data$RH_4,
#   RH_5 = complete_data$RH_5,
#   RH_6 = complete_data$RH_6,
#   RH_7 = complete_data$RH_7,
#   RH_8 = complete_data$RH_8,
#   RH_9 = complete_data$RH_9,
#   T_out = complete_data$T_out,
#   Press_mm_hg = complete_data$Press_mm_hg,
#   Windspeed = complete_data$Windspeed,
#   Visibility = complete_data$Visibility,
#   Tdewpoint = complete_data$Tdewpoint,
#   rv1 = complete_data$rv1,
#   rv2 = complete_data$rv2
# )

vars_train <- data.frame(
  Appliances = training_data$Appliances,
  month = substring(training_data$date,6,7),
  day = substring(training_data$date,9,10),
  hours = substring(training_data$date,12,13),
  minutes = substring(training_data$date,15,16), 
  T1 = training_data$T1,
  T2 = training_data$T2,
  T3 = training_data$T3,
  T4 = training_data$T4,
  T5 = training_data$T5,
  T6 = training_data$T6,
  T7 = training_data$T7,
  T8 = training_data$T8,
  T9 = training_data$T9,
  RH_1 = training_data$RH_1,
  RH_2 = training_data$RH_2,
  RH_3 = training_data$RH_3,
  RH_4 = training_data$RH_4,
  RH_5 = training_data$RH_5,
  RH_6 = training_data$RH_6,
  RH_7 = training_data$RH_7,
  RH_8 = training_data$RH_8,
  RH_9 = training_data$RH_9,
  T_out = training_data$T_out,
  Press_mm_hg = training_data$Press_mm_hg,
  Windspeed = training_data$Windspeed,
  Visibility = training_data$Visibility,
  Tdewpoint = training_data$Tdewpoint,
  rv1 = training_data$rv1,
  rv2 = training_data$rv2
)
month = vars_train$month

vars_train2 <- data.frame(
  Appliances = vars_train$Appliances,
  month = vars_train$month,
  day = vars_train$day,
  hours = vars_train$hours,
  minutes = vars_train$minutes,
  # month + days
  #mon_day = 
  # hours + minutes
  #hor_min = 
  T1 = vars_train$T1,
  T2 = vars_train$T2,
  T3 = vars_train$T3,
  T4 = vars_train$T4,
  T5 = vars_train$T5,
  T6 = vars_train$T6,
  T7 = vars_train$T7,
  T8 = vars_train$T8,
  T9 = vars_train$T9,
  RH_1 = vars_train$RH_1,
  RH_2 = vars_train$RH_2,
  RH_3 = vars_train$RH_3,
  RH_4 = vars_train$RH_4,
  RH_5 = vars_train$RH_5,
  RH_6 = vars_train$RH_6,
  RH_7 = vars_train$RH_7,
  RH_8 = vars_train$RH_8,
  RH_9 = vars_train$RH_9,
  T_out = vars_train$T_out,
  Press_mm_hg = vars_train$Press_mm_hg,
  Windspeed = vars_train$Windspeed,
  Visibility = vars_train$Visibility,
  Tdewpoint = vars_train$Tdewpoint,
  rv1 = vars_train$rv1,
  rv2 = vars_train$rv2
)

regtree_appliances2 <- rpart(
  formula = Appliances~month+day+hours+minutes+T1+T2+T3+T4+T5+T6+T7+T8+T9+RH_1+RH_2+RH_3+RH_4+RH_5+RH_6+RH_7+RH_8+RH_9+T_out+Press_mm_hg+Windspeed+Visibility+Tdewpoint+rv1+rv2,
  data = vars_train,
  method = "anova",
  control = list(maxdepth=5)
)

regtree_appliances3<- rpart(
  formula = Appliances~month+day+hours+minutes+T1+T2+T3+T4+T5+T6+T7+T8+T9+RH_1+RH_2+RH_3+RH_4+RH_5+RH_6+RH_7+RH_8+RH_9+T_out+Press_mm_hg+Windspeed+Visibility+Tdewpoint+rv1+rv2,
  data = vars_train2,
  method = "anova",
  control = list(maxdepth=5)
)

rpart.plot(regtree_appliances2)
plotcp(regtree_appliances2)

rpart.plot(regtree_appliances3)
plotcp(regtree_appliances3)

# # converting into matrix
# inputs <- data.matrix(vars[,21:24])
# 
# coefs <- t(coef(model_lights)[2:5])
# a <- coef(model_lights)[1]
# 
# results <- matrix(0,19735,1)
# 
# for (i in 1:19735){
#   results[i] <- a + inputs[i,] * t(coefs) 
# }
# # write.csv(vars,"vars.csv", row.names = FALSE)