#' evaluating_models.R

library(dmr.claseval)

##KCV EVALUATION

#EVALUATION FOR RPART

app.rpart <- crossval(rpart, 
                    as.formula(x), 
                    test_data, 
                    k=10)
rankMSE.rpart <- mse(app.rpart$pred, app.rpart$true)
rankRRSE.rpart <- rrse(app.rpart$pred, app.rpart$true)
rankMAE.rpart <- mae(app.rpart$pred, app.rpart$true)
rankRMSE.rpart <- rmse(app.rpart$pred, app.rpart$true)
rankRAE.rpart <- rae(app.rpart$pred, app.rpart$true)
rankCOR.rpart <- cor(app.rpart$pred, app.rpart$true, method='pearson')
rankRSE.rpart <- rse(app.rpart$pred, app.rpart$true)

#EVALUATION FOR lm

app.lm <- crossval(lm, 
                    as.formula(x), 
                    test_data, 
                    k=10)
rankMSE.lm <- mse(app.lm$pred, app.lm$true)
rankRRSE.lm <- rrse(app.lm$pred, app.lm$true)
rankMAE.lm <- mae(app.lm$pred, app.lm$true)
rankRMSE.lm <- rmse(app.lm$pred, app.lm$true)
rankRAE.lm <- rae(app.lm$pred, app.lm$true)
rankCOR.lm <- cor(app.lm$pred, app.lm$true, method='pearson')
rankRSE.lm <- rse(app.lm$pred, app.lm$true)

#EVALUTING FOR BAGGING

app.bag <- crossval(bagging, 
                      as.formula(x), 
                      test_data, 
                      k=10)
rankMSE.bag <- mse(app.bag$pred, app.bag$true)
rankRRSE.bag <- rrse(app.bag$pred, app.bag$true)
rankMAE.bag <- mae(app.bag$pred, app.bag$true)
rankRMSE.bag <- rmse(app.bag$pred, app.bag$true)
rankRAE.bag <- rae(app.bag$pred, app.bag$true)
rankCOR.bag <- cor(app.bag$pred, app.bag$true, method='pearson')
rankRSE.bag <- rse(app.bag$pred, app.bag$true)

# using new feature selection 

app.rpart2 <- crossval(rpart, 
                      as.formula(attr_part), 
                      test_data, 
                      k=10)
rankMSE.rpart2 <- mse(app.rpart2$pred, app.rpart2$true)
rankRRSE.rpart2 <- rrse(app.rpart2$pred, app.rpart2$true)
rankMAE.rpart2 <- mae(app.rpart2$pred, app.rpart2$true)
rankRMSE.rpart2 <- rmse(app.rpart2$pred, app.rpart2$true)
rankRAE.rpart2 <- rae(app.rpart2$pred, app.rpart2$true)
rankCOR.rpart2 <- cor(app.rpart2$pred, app.rpart2$true, method='pearson')
rankRSE.rpart2 <- rse(app.rpart2$pred, app.rpart2$true)

#EVALUATION FOR lm

app.lm2 <- crossval(lm, 
                    as.formula(attr_part), 
                   test_data, 
                   k=10)
rankMSE.lm2 <- mse(app.lm2$pred, app.lm2$true)
rankRRSE.lm2 <- rrse(app.lm2$pred, app.lm2$true)
rankMAE.lm2 <- mae(app.lm2$pred, app.lm2$true)
rankRMSE.lm2 <- rmse(app.lm2$pred, app.lm2$true)
rankRAE.lm2 <- rae(app.lm2$pred, app.lm2$true)
rankCOR.lm2 <- cor(app.lm2$pred, app.lm2$true, method='pearson')
rankRSE.lm2 <- rse(app.lm2$pred, app.lm2$true)

#EVALUTING FOR BAGGING

app.bag2 <- crossval(bagging, 
                     as.formula(attr_part), 
                    test_data, 
                    k=10)
rankMSE.bag2 <- mse(app.bag2$pred, app.bag2$true)
rankRRSE.bag2 <- rrse(app.bag2$pred, app.bag2$true)
rankMAE.bag2 <- mae(app.bag2$pred, app.bag2$true)
rankRMSE.bag2 <- rmse(app.bag2$pred, app.bag2$true)
rankRAE.bag2 <- rae(app.bag2$pred, app.bag2$true)
rankCOR.bag2 <- cor(app.bag2$pred, app.bag2$true, method='pearson')
rankRSE.bag2 <- rse(app.bag2$pred, app.bag2$true)
