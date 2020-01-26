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


target_t <- "lights"

# DATA COLLECTION AND ORGANIZATION GOES HERE
# 10 to try function, 1000 to test try algorithm, 14803 to full
source('R/data_org.R')

testDataLength <- 1000 
complete_data <- read.csv("energydata_complete.csv")

test_data <- dataOrganization(complete_data, target_t,testDataLength)

# FEATURE SELECTION GOES HERE

source('R/feature_selection.R')

res <- feature_selection(formula_full = as.formula(paste(target_t, "~.")),
                          target = target_t,
                          test_data = test_data,
                          type = "simple",
                          part = 0.5,
                          trees_num=20
                          )

formula <- as.vector(res$attr_part[1])
# CREATING MODELS GOES HERE

# EVALUATION PROCEDURES GOES HERE

source("R/model_eval.R")

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
plr_args <- list(minsplit=2, maxdepth=8)

tempdataframe4 <- model_eval(test_data=test_data,
                             fun = grow.modtree,
                             formula = formula,
                             crossval_number = 20,
                             args = plr_args
                             )

ctrl <- trainControl(method = "cv", number = 10)
args_t <- c(method="treebag",trControl=ctrl)

tempdataframe5 <- model_eval(test_data = test_data,
                             fun = train,
                             formula = formula,
                             crossval_number = 20,
                             args=args_t
)