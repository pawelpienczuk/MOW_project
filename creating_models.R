#creating models

# creating linear model
model_linear_app <- lm(
  formula = Appliances~day_mon+min_hour+T1+T2+T3+T4+T5+T6+T7+T8+T9+RH_1+RH_2+RH_3+RH_4+RH_5+RH_6+RH_7+RH_8+RH_9+RH_out+T_out+Press_mm_hg+Windspeed+Visibility+Tdewpoint+rv1+rv2,
  data = vars_train
)

# printing linear model's coefs 
print(model_linear_app)

# regression trees
m_nobagging <- rpart(
  formula = Appliances~day_mon+min_hour+T1+T2+T3+T4+T5+T6+T7+T8+T9+RH_1+RH_2+RH_3+RH_4+RH_5+RH_6+RH_7+RH_8+RH_9+RH_out+T_out+Press_mm_hg+Windspeed+Visibility+Tdewpoint+rv1+rv2,
  data = vars_train,
  method = "anova",
  control = list( xval=5)
)

# BAGGING

set.seed(123)

# train bagged model
bagged_m1 <- bagging(
  formula = Appliances~day_mon+min_hour+T1+T2+T3+T4+T5+T6+T7+T8+T9+RH_1+RH_2+RH_3+RH_4+RH_5+RH_6+RH_7+RH_8+RH_9+RH_out+T_out+Press_mm_hg+Windspeed+Visibility+Tdewpoint+rv1+rv2,
  data = vars_train,
  coob = TRUE,
  nbagg=20
)

# BAGGING caret

ctrl <- trainControl(method = "cv", number = 10)      # change from 10

bagged_caret_m1 <- train(
  Appliances~day_mon+min_hour+T1+T2+T3+T4+T5+T6+T7+T8+T9+RH_1+RH_2+RH_3+RH_4+RH_5+RH_6+RH_7+RH_8+RH_9+RH_out+T_out+Press_mm_hg+Windspeed+Visibility+Tdewpoint+rv1+rv2,
  data = vars_train,
  method = "treebag",
  trControl = ctrl,
  importance = TRUE
)

# piecewise linear regression, dmr page 284
plr_tree <- grow.modtree(
  formula = Appliances~day_mon+min_hour+T1+T2+T3+T4+T5+T6+T7+T8+T9+RH_1+RH_2+RH_3+RH_4+RH_5+RH_6+RH_7+RH_8+RH_9+RH_out+T_out+Press_mm_hg+Windspeed+Visibility+Tdewpoint+rv1+rv2, 
  data = vars_train,
  maxdepth=8
)