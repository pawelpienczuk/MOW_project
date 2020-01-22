# main v1.1
# linear and reg_trees
# large RMSE, possible to reduce??'
# poor piecewise linear regression
#setwd("E:/Documents/Studies/MOW/MOW_project")
setwd("Z:/Offtop/MOW/Project/MOW_project")

library(rsample)     # data splitting 
library(dplyr)       # data wrangling
library(ipred)       # bagging
library(caret)       # bagging
library(dmr.regtree)  

rm(list = ls())

# DATA COLLECTION AND ORGANIZATION GOES HERE

source('R/data_org.R', echo=TRUE)

# FEATURE SELECTION GOES HERE

source('R/feature_selection.R')

res <- feature_selection(test_data = test_data,
                               type = "bootstrap",
                               part = 0.5,
                               trees_num=10)

# CREATING MODELS GOES HERE

# source('creating_models.R', echo=TRUE)

# predicting from test data
# pred_linear <- predict(model_linear_app,test_data)
# pred_nobagging <- predict(m_nobagging,test_data)
# pred_bagged <- predict(bagged_m1,test_data)
# pred_caret <- predict(bagged_caret_m1,test_data)
# pred_plr <- predict(plr_tree,test_data,5)

#evaluation procedures goes here
source("R/model_eval.R")

tempdataframe <- model_eval(test_data = test_data,
                            fun = rpart,
                            attr = as.character(res$attr_part[1]),
                            crossval_number = 10
                            )
tempdataframe2 <- model_eval(test_data = test_data,
                             fun = lm,
                             attr = as.character(res$attr_part[1]),
                             crossval_number = 10
                            )
tempdataframe3 <- model_eval(test_data = test_data,
                             fun = bagging,
                             attr = as.character(res$attr_part[1]),
                             crossval_number = 10
)