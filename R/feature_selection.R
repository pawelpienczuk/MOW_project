# feature_selection.R

library(randomForest)
library(rpart)
library(rpart.plot)  # plotting regression trees
library(Metrics)     # RMSE
library(dmr.disc)
library(dmr.stats)
library(dmr.attrsel)

#' @title Feature Selection
#'
#' @param test_data data to perform feature ranking
#' @param formula_full  full formula, with targets and all attributes
#' @param target target (character type) 
#' @param type  "rf" - randomForest IMPORTANCE-based ranking (default),
#' "relief" - RELIEF algorithm 
#' "simple" - simple filter algorithm
#' "wrapper" - wrapper algorithm
#' @param part part of attributes returned by feature selection, 0 < part <=1 (default=0.5)
#' @param trees_num numbers of trees (randomForest) or bootstrap sets (bootstrap)
#'
#' @return formula with selected attributes
#' 
#'
#' 
feature_selection <<- function(formula_full,target,test_data,type="rf",part=1,trees_num=10){
  
  if ( !is.data.frame(test_data) ){
    message("Pass data frame format")
    stop()
  }
  if (trees_num<1) {
    message("Trees number should be >= 1")
    stop()
  }
  if (part>1|part<=0){
    message("Parts of atrributes must be >0 and <=1")
    stop()  
  }
  
  print("FEATURE SELECTION")
  count <- ceiling((ncol(test_data)-1)*part)
  
  if (type=="rf"){
    full_model_RF <- randomForest(formula = formula_full,
                        data = test_data,
                        importance = TRUE,
                        ntree=trees_num)
    importance_RF <- data.frame(importance(full_model_RF, type=1), "k"=1:(ncol(test_data)-1))
    wyniki <- data.frame("importance"=importance_RF[order(importance_RF[,1],decreasing = TRUE),1],
                              "k"=importance_RF$k[order(importance_RF[,1],decreasing = TRUE)], 
                              "attr"=rownames(importance_RF)[order(importance_RF[,1], decreasing=TRUE)])
    
    # plotting most important parameters
    varImpPlot(x=full_model_RF,
             n.var=count,
             type=1,
             main="Variable importance")
    
    # passing most important attributes from feature selection
    count <- ceiling((ncol(test_data)-1)*part)
    attr_part <- paste(wyniki$attr[2:count], collapse = "+")
    attr_part <- paste(target, "~", attr_part)
    out <- data.frame(attr_part,wyniki)
    return(out)
  }
  else if(type=="relief"){
    
    res <- rrelief.filter(formula = formula_full,
                         data = test_data,
                         k=3,
                         K=trees_num)
    
    names_t <- names(res)
    
    # plotting most important parameters
    barplot(as.vector(res)[1:count],main="Most important variables",names.arg = names_t[1:count])
      
    attr_part <- paste(names_t[1:count], collapse = "+")
    attr_part <- paste(target, "~", attr_part)
    out <- data.frame(attr_part,res)
    return(out)
  }
  
  else if (type=="simple"){
    
    res <- simple.filter(formula = formula_full,
                         data = discnm.eqfreq(~.,test_data,50),
                         dd=symunc)
    
    names_t <- names(res)
    
    # plotting most important parameters
    barplot(as.vector(res)[1:count],main="Most important variables",names.arg = names_t[1:count])
    
    attr_part <- paste(names_t[1:count], collapse = "+")
    attr_part <- paste(target, "~", attr_part)
    out <- data.frame(attr_part,res)
    return(out)
  }
  
  else if (type=="wrapper"){
    
    res <- wrapper.filter.select(formula = formula_full,
                      data = test_data,
                      utils = simple.filter(formula_full,test_data),
                      alg = rpart,
                      args = list(minsplit=2)
    )
                      # initf = asel.init.none,
                      # nextf = asel.next.forward
                      
    names_t <- res$subset
    
    # plotting most important parameters
    barplot(res$eval[1:count],main="Most important variables",names.arg = names_t[1:count])
    
    attr_part <- paste(names_t, collapse = "+")
    attr_part <- paste(target, "~", attr_part)
    out <- data.frame(attr_part,res)
    return(out)
  }
  else{
    message("Wrong type of feature selection: \"relief\" \"rf\" or \"simple\" ")
    stop()
  }
}