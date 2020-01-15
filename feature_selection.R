# feature_selection.R

library(randomForest)
print("FEATURE SELECTION")

full_model_RF <- randomForest(formula = Appliances ~ .,
                      data = test_data,
                      importance = TRUE,
                      ntree=100)
importance_RF <- data.frame(importance(full_model_RF, type=1), "k"=1:(ncol(test_data)-1))
importance_RF <- data.frame("importance"=importance_RF[order(importance_RF[,1],decreasing = TRUE),1],
                            "k"=importance_RF$k[order(importance_RF[,1],decreasing = TRUE)], 
                            "attr"=rownames(importance_RF)[order(importance_RF[,1], decreasing=TRUE)])

# plotting most important parameters
varImpPlot(x=full_model_RF,
           n.var=16,
           type=1,
           main="Variable importance")