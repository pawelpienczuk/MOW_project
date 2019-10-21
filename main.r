# main v1.0
# main functionality

rm(list = ls())

training_data <- read.csv("training.csv")

# creating model
model_appliances <- lm(Appliances~T1+T2+T3+T4+T5+T6+T7+T8+T9+RH_1+RH_2+RH_3+RH_4+RH_5+RH_6+RH_7+RH_8+RH_9+T_out+Press_mm_hg+Windspeed+Visibility+Tdewpoint+rv1+rv2, data = training_data)
model_lights <- lm(Appliances~T1+T2+T3+T4+T5+T6+T7+T8+T9+RH_1+RH_2+RH_3+RH_4+RH_5+RH_6+RH_7+RH_8+RH_9+T_out+Press_mm_hg+Windspeed+Visibility+Tdewpoint+rv1+rv2, data = training_data)

# printing model's coefs 
print(model_appliances)
print(model_lights)
