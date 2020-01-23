# main v1.1
#setwd("E:/Documents/Studies/MOW/MOW_project")
#setwd("Z:/Offtop/MOW/Project/MOW_project")
setwd("F:/GitHub/MOW_project")
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
source('R/simple.filter.R')

res <- feature_selection(test_data = test_data,
                               type = "rf",
                               part = 0.25,
                               trees_num=20)

# CREATING MODELS GOES HERE

# EVALUATION PROCEDURES GOES HERE

source("R/model_eval.R")

ctrl <- trainControl(method = "cv", number = 10)
args_t <- c(method="treebag",trControl=ctrl)

tempdataframe <- model_eval(test_data = test_data,
                            fun = rpart,
                            formula = as.character(res$attr_part[1]),
                            crossval_number = 10
                            )
tempdataframe2 <- model_eval(test_data = test_data,
                             fun = lm,
                             formula = as.character(res$attr_part[1]),
                             crossval_number = 10
                            )
tempdataframe3 <- model_eval(test_data = test_data,
                             fun = bagging,
                             formula = as.character(res$attr_part[1]),
                             crossval_number = 10
)
tempdataframe4 <- model_eval(test_data = test_data,
                             fun = train,
                             formula = as.character(res$attr_part[1]),
                             crossval_number = 10,
                             args=args_t
)