# feature_selection.R

library(randomForest)
library(rpart)
library(rpart.plot)  # plotting regression trees
library(Metrics)     # RMSE
library(dmr.disc)
library(dmr.stats)

#' @title Feature Selection
#'
#' @param test_data  - data to perform feature ranking
#' @param type - "rf" - randomForest IMPORTANCE-based ranking (MSE)(default),
#' "bootstrap" - bootstraped R2-based (coef. of determination) ranking
#' "simple" - simple filter based algorithms
#' @param part - part of attributes returned by feature selection, 0 < part <=1 (default=0.5)
#' @param trees_num - numbers of trees (randomForest) or bootstrap sets (bootstrap)
#'
#' @return formula with selected attributes
#' 
#'
#' 
feature_selection <<- function(test_data,type="rf",part=1,trees_num=10){
  
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
    full_model_RF <- randomForest(formula = Appliances ~ .,
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
    attr_part <- paste("Appliances", "~", attr_part)
    out <- data.frame(attr_part,wyniki)
    return(out)
  }
  else if(type=="bootstrap"){
    
    r2 <- function(pred.y, true.y)
    { 1 - length(true.y)*mse(pred.y = pred.y,true.y=true.y)/((length(true.y)-1)*var(true.y)) }
    
    N=trees_num
    
    wyniki <- data.frame(matrix(0,ncol=3,nrow=(ncol(test_data)-2)))
    
    # bootstrap
    for (i in 1:N)
    {
      wyniki_tmp <- data.frame("quality"=double(), "quality_perm"=double(), "quality_diff"=double())
      
      n <- sample(nrow(test_data), nrow(test_data), replace=TRUE)
      data_train <- test_data[n,]
      data_test <- test_data[-n,]
      
      # tworzenie modelu na wszystkich atrybutach
      model <- rpart(formula=Appliances~.,
                     data=data_train,
                     method='anova'
      )
      pred_full <- predict(model, data_test)
      
      quality <- r2(pred.y = pred_full, true.y = data_test$Appliances)
      
      # badanie wplywu permutacji 
      for (k in 2:(ncol(test_data)-1))
      {
        # permutacja k-tego atrybutu
        data_test_perm <- data_test
        data_test_perm[,k] <- sample(data_test_perm[,k])
        
        # predykcja dla atrybutow o spermutowanych wartosciach
        pred_perm <- predict(model, data_test_perm)
        
        quality_perm <- r2(pred.y = pred_perm, true.y = data_test_perm$Appliances)
        
        quality_diff <- quality - quality_perm
        wyniki_tmp <- rbind(wyniki_tmp, c(quality, quality_perm, quality_diff))
      }
      # dodanie wynikow z i-tej iteracji
      wyniki <- wyniki + wyniki_tmp
    }
    
    wyniki <- wyniki/N
    
    wyniki <- data.frame(2:k, colnames(test_data)[2:k], wyniki[,3])
    wyniki <- wyniki[order(wyniki[,3], decreasing = TRUE),]
    colnames(wyniki) <- c("k", "attr", "R2_diff")
    
    count <- ceiling((ncol(test_data)-1)*part)
    message('Najwa?aniejsze ', part*100, '% atrybut?w to:\n', paste(wyniki$attr[1:count], ' '), '\n')
    attr_part <- paste(wyniki$attr[2:count], collapse = "+")
    attr_part <- paste("Appliances", "~", attr_part)
    
    out <- data.frame(attr_part,wyniki)
    return(out)
  }
  
  else if (type=="simple"){
    # Symmetric uncertainty - symunc
    # discnm.eqfreq - equal-frequency discretization 
    
    res <- simple.filter(formula = Appliances~.,
                         data = discnm.eqfreq(~.,test_data,50),
                         dd=symunc)
    
    names_t <- names(res)
    attr_part <- paste(names_t[1:count], collapse = "+")
    attr_part <- paste("Appliances", "~", attr_part)
    out <- data.frame(attr_part,res)
    return(out)
  }
  else{
    message("Wrong type of feature selection: \"bootstrap\" \"rf\" or \"simple\" ")
    stop()
  }
}