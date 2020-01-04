# main v1.1
# linear and reg_trees
#test data -> no min_hour on 91-102 rows, why ?!?!?

#hours[91:102] are N/A, in complete_data[91:102] and test_data[91:102] are OK, why ?!?!?

#strtoi read '08' and '09' as 'N/A'


library(rsample)     # data splitting 
library(dplyr)       # data wrangling
library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees
library(ipred)       # bagging
library(caret)       # bagging
library(Metrics)     # RMSE

rm(list = ls())

training_data <- read.csv("training.csv")

# REDUNDANT - data organization
complete_data <- read.csv("energydata_complete.csv")

month = as.numeric(substring(complete_data$date,6,7))
day = as.numeric(substring(complete_data$date,9,10))
hours = as.numeric(substring(complete_data$date,12,13))
minutes = as.numeric(substring(complete_data$date,15,16))
day_mon = day + 30 * month
min_hour = minutes + 60 * hours

test_data <- data.frame(
  month = substring(complete_data$date,6,7),
  day = substring(complete_data$date,9,10),
  hours = substring(complete_data$date,12,13),
  minutes = substring(complete_data$date,15,16),
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
  Press_mm_hg = complete_data$Press_mm_hg,
  Windspeed = complete_data$Windspeed,
  Visibility = complete_data$Visibility,
  Tdewpoint = complete_data$Tdewpoint,
  rv1 = complete_data$rv1,
  rv2 = complete_data$rv2
)

month = strtoi(as.vector(substring(training_data$date,6,7)))
day = strtoi(as.vector(substring(training_data$date,9,10)))
hours = strtoi(as.vector(substring(training_data$date,12,13)))
minutes = strtoi(as.vector(substring(training_data$date,15,16)))

day_mon = day + 30 * month
min_hour = minutes + 60 * hours

vars_train <- data.frame(
  Appliances = training_data$Appliances,
  month = substring(training_data$date,6,7),
  day = substring(training_data$date,9,10),
  hours = substring(training_data$date,12,13),
  minutes = substring(training_data$date,15,16),
  day_mon = day_mon,
  min_hour = min_hour,
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

# creating linear model
model_linear_app <- lm(
  formula = Appliances~day_mon+min_hour+T_out+Press_mm_hg+Windspeed+Visibility,
  data = vars_train
)

# printing linear model's coefs 
print(model_linear_app)

# regression trees
m_nobagging <- rpart(
  formula = Appliances~day_mon+min_hour+T1+T2+T3+T4+T5+T6+T7+T8+T9+RH_1+RH_2+RH_3+RH_4+RH_5+RH_6+RH_7+RH_8+RH_9+T_out+Press_mm_hg+Windspeed+Visibility+Tdewpoint+rv1+rv2,
  data = vars_train,
  method = "anova",
  control = list( xval=5)
)

# BAGGING

set.seed(123)

# train bagged model
bagged_m1 <- bagging(
  formula = Appliances~day_mon+min_hour+T1+T2+T3+T4+T5+T6+T7+T8+T9+RH_1+RH_2+RH_3+RH_4+RH_5+RH_6+RH_7+RH_8+RH_9+T_out+Press_mm_hg+Windspeed+Visibility+Tdewpoint+rv1+rv2,
  data = vars_train,
  coob = TRUE
)

# predicting from test data
pred_linear <- predict(model_linear_app,test_data)
pred_nobagging <- predict(m_nobagging,test_data)
pred_bagged <- predict(bagged_m1,test_data)

#evaluating RMSE for each model
rmse_linear <- rmse(actual=complete_data$Appliances,predicted=pred_linear)
rmse_nobagging <- rmse(actual=complete_data$Appliances,predicted=pred_nobagging)
rmse_bagged <- rmse(actual=complete_data$Appliances,predicted=pred_bagged)