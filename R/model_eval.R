# model evaluation

library(dmr.claseval)

#' @title Model Evaluation
#'
#' @param test_data - data to perform cross-validation
#' @param fun - regression algorithm (e.g. train,bagging,lm) default=rpart
#' @param formula - regression formula
#' @param crossval_number - number of cross-validations, default=10
#' @param args - passed arguments into regression algorithm, default=NULL
#'
#' @return data frame with measures in following order: MSE, RRSE, MAE, RMSE, RAE, COR, RSE
#' 
#'
model_eval <<- function(test_data,fun=rpart,formula,crossval_number=10,args=NULL){
  
  if ( !is.data.frame(test_data) ){
    message("Pass data frame format")
    stop()
  }
  if ( !is.character(formula)) {
    message("Pass string into attr parameter")
    stop()
  }
  if (crossval_number<1){
    message("Cross validations number must be greater than 0")
    stop()
  }
  if (crossval_number==1){
    warning("No cross-validation")
  }
  temp_model <- crossval(fun, 
                        as.formula(formula), 
                        test_data, 
                        k=crossval_number,
                        args = args)
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