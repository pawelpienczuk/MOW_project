#creating models

# creating linear model
model_linear_app <- lm(
  formula = as.formula(x),
  data = test_data
)

# printing linear model's coefs 
print(model_linear_app)

# regression trees
m_nobagging <- rpart(
  formula = as.formula(x),
  data = test_data,
  method = "anova",
  control = list( xval=5)
)

# BAGGING

set.seed(123)

# train bagged model
bagged_m1 <- bagging(
  formula = as.formula(x),
  data = test_data,
  coob = TRUE,
  nbagg=20
)

# BAGGING caret

ctrl <- trainControl(method = "cv", number = 10)      # change from 10

bagged_caret_m1 <- train(
  form = as.formula(x),
  data = test_data,
  method = "treebag",
  trControl = ctrl,
  importance = TRUE
)

# piecewise linear regression, dmr page 284
plr_tree <- grow.modtree(
  formula = as.formula(x),
  data = test_data,
  maxdepth=8
)