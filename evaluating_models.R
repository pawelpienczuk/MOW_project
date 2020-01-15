# evaluating_models.R

#evaluating RMSE for each model
rmse_linear <- rmse(actual=complete_data$Appliances,predicted=pred_linear)
rmse_nobagging <- rmse(actual=complete_data$Appliances,predicted=pred_nobagging)
rmse_bagged <- rmse(actual=complete_data$Appliances,predicted=pred_bagged)
rmse_caret <- rmse(actual=complete_data$Appliances,predicted=pred_caret)
rmse_plr <- rmse(actual=complete_data$Appliances,predicted=pred_plr)

mse_linear <- mse(actual=complete_data$Appliances,predicted=pred_linear)
mse_nobagging <- mse(actual=complete_data$Appliances,predicted=pred_nobagging)
mse_bagged <- mse(actual=complete_data$Appliances,predicted=pred_bagged)
mse_caret <- mse(actual=complete_data$Appliances,predicted=pred_caret)
mse_plr <- mse(actual=complete_data$Appliances,predicted=pred_plr)

mae_linear <- mae(actual=complete_data$Appliances,predicted=pred_linear)
mae_nobagging <- mae(actual=complete_data$Appliances,predicted=pred_nobagging)
mae_bagged <- mae(actual=complete_data$Appliances,predicted=pred_bagged)
mae_caret <- mae(actual=complete_data$Appliances,predicted=pred_caret)
mae_plr <- mae(actual=complete_data$Appliances,predicted=pred_plr)

rse_linear <- rse(actual=complete_data$Appliances,predicted=pred_linear)
rse_nobagging <- rse(actual=complete_data$Appliances,predicted=pred_nobagging)
rse_bagged <- rse(actual=complete_data$Appliances,predicted=pred_bagged)
rse_caret <- rse(actual=complete_data$Appliances,predicted=pred_caret)
rse_plr <- rse(actual=complete_data$Appliances,predicted=pred_plr)

rrse_linear <- rrse(actual=complete_data$Appliances,predicted=pred_linear)
rrse_nobagging <- rrse(actual=complete_data$Appliances,predicted=pred_nobagging)
rrse_bagged <- rrse(actual=complete_data$Appliances,predicted=pred_bagged)
rrse_caret <- rrse(actual=complete_data$Appliances,predicted=pred_caret)
rrse_plr <- rrse(actual=complete_data$Appliances,predicted=pred_plr)

rae_linear <- rae(actual=complete_data$Appliances,predicted=pred_linear)
rae_nobagging <- rae(actual=complete_data$Appliances,predicted=pred_nobagging)
rae_bagged <- rae(actual=complete_data$Appliances,predicted=pred_bagged)
rae_caret <- rae(actual=complete_data$Appliances,predicted=pred_caret)
rae_plr <- rae(actual=complete_data$Appliances,predicted=pred_plr)

cor_linear <- cor(complete_data$Appliances,pred_linear,method='pearson')
cor_nobagging <- cor(complete_data$Appliances,pred_nobagging,method='pearson')
cor_bagged <- cor(complete_data$Appliances,pred_bagged,method='pearson')
cor_caret <- cor(complete_data$Appliances,pred_caret,method='pearson')
cor_plr <- cor(complete_data$Appliances,pred_plr,method='pearson')
