# main v1.1
#setwd("E:/Documents/Studies/MOW/MOW_project")
#setwd("Z:/Offtop/MOW/Project/MOW_project")
#setwd("F:/GitHub/MOW_project")
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

testDataLength <- 2000
complete_data <- read.csv("energydata_complete.csv")

test_data <- dataOrganization(complete_data, target_t,testDataLength)

# FEATURE SELECTION GOES HERE

source('R/feature_selection.R')
source("R/model_eval.R")

featSelTypes <- list("rf", "simple","relief")
corMeasures <- NULL
formula <- NULL
# for every type of feature selection method
for (k in 1:length(featSelTypes)){
  res <- feature_selection(formula_full = as.formula(paste(target_t, "~.")),
                           target = target_t,
                           test_data = test_data,
                           type = featSelTypes[k],
                           part = 0.5,
                           trees_num=500
  )
  
  formula <- c(formula,as.vector(res$attr_part[1]))
  
  # create and cross-validate rpart model based on particular feature sel. method
  crossval.rpart <- model_eval(test_data = test_data,
                               fun = rpart,
                               formula = formula[k],
                               crossval_number = 20,
                               args = list(method="anova")
  )
  corMeasures <- c(corMeasures,crossval.rpart$COR)
}

# with all attributes
crossval.rpart <- model_eval(test_data = test_data,
                             fun = rpart,
                             formula = paste(target_t, "~."),
                             args = list(method ="anova"),
                             crossval_number = 20
)
corMeasures <- c(corMeasures,crossval.rpart$COR)
formula <- c(formula,(paste(target_t, "~.")))

#selection of best formula
k <- which.max(corMeasures)
formula <- formula[k]

# EVALUATION PROCEDURES GOES HERE

crossval.lm <- model_eval(test_data = test_data,
                             fun = lm,
                             formula = formula,
                             crossval_number = 20
)
crossval.rpart <- model_eval(test_data = test_data,
                            fun = rpart,
                            formula = formula,
                            crossval_number = 20
                            )

crossval.bagging <- model_eval(test_data = test_data,
                             fun = bagging,
                             formula = formula,
                             crossval_number = 20
)
plr_args <- list(minsplit=2, maxdepth=8)

crossval.plr <- model_eval(test_data=test_data,
                             fun = grow.modtree,
                             formula = formula,
                             crossval_number = 20,
                             args = plr_args
                             )

ctrl <- trainControl(method = "cv", number = 10)
args_t <- c(method="treebag",trControl=ctrl)

crossval.caret <- model_eval(test_data = test_data,
                             fun = train,
                             formula = formula,
                             crossval_number = 20,
                             args=args_t
)

crossval.all <- rbind(crossval.lm,crossval.rpart,crossval.bagging,crossval.plr,crossval.caret)
