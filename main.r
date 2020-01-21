# main v1.1
# linear and reg_trees
# large RMSE, possible to reduce??'
# poor piecewise linear regression
setwd("E:/Documents/Studies/MOW/MOW_project")
#setwd("Z:/Offtop/MOW/Project/MOW_project")

library(rsample)     # data splitting 
library(dplyr)       # data wrangling
library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees
library(ipred)       # bagging
library(caret)       # bagging
library(Metrics)     # RMSE
library(dmr.regtree)  

rm(list = ls())

# DATA COLLECTION AND ORGANIZATION GOES HERE

source('data_org.R', echo=TRUE)

# FEATURE SELECTION GOES HERE

source('feature_selection.R', echo=TRUE)

# CREATING MODELS GOES HERE

# source('creating_models.R', echo=TRUE)

# predicting from test data
# pred_linear <- predict(model_linear_app,test_data)
# pred_nobagging <- predict(m_nobagging,test_data)
# pred_bagged <- predict(bagged_m1,test_data)
# pred_caret <- predict(bagged_caret_m1,test_data)
# pred_plr <- predict(plr_tree,test_data,5)

#evaluting procedures goes here

#source('evaluating_models.R', echo=TRUE)

source("model_eval.R")

tempdataframe <- model_eval(test_data = test_data,
                            fun = rpart,
                            attr = attr_part,
                            crossval_number = 10
                            )