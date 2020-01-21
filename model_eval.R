# model evaluation

library(dmr.claseval)

#' Model Evaluation
#'
#' @param test_data 
#' @param fun 
#' @param attr 
#' @param crossval_number 
#'
#' @return number
#' @export 
#'
#' @examples
model_eval <- function(test_data,fun,attr,crossval_number){
  temp_model <- crossval(fun, 
                        as.formula(attr), 
                        test_data, 
                        k=crossval_number)
  MSE <- mse(temp_model$pred, temp_model$true)
  RRSE <- rrse(temp_model$pred, temp_model$true)
  MAE <- mae(temp_model$pred, temp_model$true)
  RMSE <- rmse(temp_model$pred, temp_model$true)
  RAE <- rae(temp_model$pred, temp_model$true)
  COR <- cor(temp_model$pred, temp_model$true, method="pearson")
  RSE <- rse(temp_model$pred, temp_model$true)
  
  out <- data.frame(MSE,RRSE,MAE,RMSE,RAE,COR,RSE)
  return(out) 
}