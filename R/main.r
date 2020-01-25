# main v1.1
#setwd("E:/Documents/Studies/MOW/MOW_project")
#setwd("Z:/Offtop/MOW/Project/MOW_project")
setwd(".")

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

res <- feature_selection(formula_full = Appliances~.,
                          target = "Appliances",
                          test_data = test_data,
                          type = "relief",
                          part = 0.5,
                          trees_num=100
                          )

formula <- as.vector(res$attr_part[1])
# CREATING MODELS GOES HERE

# EVALUATION PROCEDURES GOES HERE

source("R/model_eval.R")

ctrl <- trainControl(method = "cv", number = 10)
args_t <- c(method="treebag",trControl=ctrl)

tempdataframe <- model_eval(test_data = test_data,
                            fun = rpart,
                            formula = formula,
                            crossval_number = 20
                            )
tempdataframe2 <- model_eval(test_data = test_data,
                             fun = lm,
                             formula = formula,
                             crossval_number = 20
                            )
tempdataframe3 <- model_eval(test_data = test_data,
                             fun = bagging,
                             formula = formula,
                             crossval_number = 20
)
# tempdataframe4 <- model_eval(test_data = test_data,
#                              fun = train,
#                              formula = formula,
#                              crossval_number = 20,
#                              args=args_t
# )