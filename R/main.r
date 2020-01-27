#setwd("E:/Documents/Studies/MOW/MOW_project")
#setwd("Z:/Offtop/MOW/Project/MOW_project")
#setwd("F:/GitHub/MOW_project")
setwd(".")

# Main script 
# Krzysztof Belewicz
# Pawel Pienczuk
# MOW - Metody odkrywania wiedzy
# FEIT WUT Class

library(rsample)     # data splitting 
library(dplyr)       # data wrangling
library(ipred)       # bagging
library(caret)       # bagging
library(dmr.regtree)  

rm(list = ls())

# to make the final comparision change line 122 "test_data$..."
target_t <- "Appliances"

# DATA COLLECTION AND ORGANIZATION GOES HERE
# 10 to try function, 2000 to test try algorithm, 19735 to full
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
                               crossval_number = 10,
                               args = list(method="anova")
  )
  corMeasures <- c(corMeasures,crossval.rpart$COR)
}

# with all attributes
crossval.rpart <- model_eval(test_data = test_data,
                             fun = rpart,
                             formula = paste(target_t, "~."),
                             args = list(method ="anova"),
                             crossval_number = 10
)
corMeasures <- c(corMeasures,crossval.rpart$COR)
formula <- c(formula,paste(target_t, "~."))

#selection of best formula
k <- which.max(corMeasures)
formula <- formula[k]

# EVALUATION PROCEDURES GOES HERE

crossval.lm <- model_eval(test_data = test_data,
                             fun = lm,
                             formula = formula,
                             crossval_number = 10
)
crossval.rpart <- model_eval(test_data = test_data,
                            fun = rpart,
                            formula = formula,
                            crossval_number = 10
                            )

crossval.bagging <- model_eval(test_data = test_data,
                             fun = bagging,
                             formula = formula,
                             crossval_number = 10
)
plr_args <- list(minsplit=2, maxdepth=8)

crossval.plr <- model_eval(test_data=test_data,
                             fun = grow.modtree,
                             formula = formula,
                             crossval_number = 10,
                             args = plr_args
                             )

ctrl <- trainControl(method = "cv", number = 10)
args_t <- c(method="treebag",trControl=ctrl)

crossval.caret <- model_eval(test_data = test_data,
                             fun = train,
                             formula = formula,
                             crossval_number = 10,
                             args=args_t
)

crossval.all <- rbind(crossval.lm,crossval.rpart,crossval.bagging,crossval.plr,crossval.caret)

bestModelInd <- which.max(crossval.all$COR)

# bagging was the best model, so there is a creation
bagmodel <- bagging(as.formula(formula), test_data)

pred = predict(bagmodel, test_data)

comparison = data.frame(k=1:nrow(test_data), pred=pred, true=test_data$Appliances)

ggplot(comparison[1:100,],x="time&date", y=target_t) +
  geom_point(aes(x=k, y=pred), color = "darkred") +
  geom_point(aes(x=k, y=true), color = "blue") + 
  labs(title="Scatterplot") + 
  xlab("time/date") + 
  ylab(target_t)