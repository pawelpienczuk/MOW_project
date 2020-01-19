# feature_selection.R

library(randomForest)
regression <- TRUE

print("FEATURE SELECTION")

full_model_RF <- randomForest(formula = Appliances ~ .,
                      data = test_data,
                      importance = TRUE,
                      ntree=500)
importance_RF <- data.frame(importance(full_model_RF, type=1), "k"=1:(ncol(test_data)-1))
importance_RF <- data.frame("importance"=importance_RF[order(importance_RF[,1],decreasing = TRUE),1],
                            "k"=importance_RF$k[order(importance_RF[,1],decreasing = TRUE)], 
                            "attr"=rownames(importance_RF)[order(importance_RF[,1], decreasing=TRUE)])

# plotting most important parameters
varImpPlot(x=full_model_RF,
           n.var=16,
           type=1,
           main="Variable importance")

# passing most important attributes from feature selection
x <- paste(importance_RF$attr[1:16], collapse = "+")
x <- paste("Appliances", "~", x)

r2 <- function(pred.y, true.y)
{ 1 - length(true.y)*mse(pred.y, true.y)/((length(true.y)-1)*var(true.y)) }

N=10

wyniki <- data.frame(matrix(0,ncol=3,nrow=(ncol(test_data)-1)))

# bootstrap
for (i in 1:N)
{
  wyniki_tmp <- data.frame("quality"=double(), "quality_perm"=double(), "quality_diff"=double())
  
  n <- sample(nrow(test_data), nrow(test_data), replace=TRUE)
  data_train <- test_data[n,]
  data_test <- test_data[-n,]
  
  # tworzenie modelu na wszystkich atrybutach
  model <- rpart(formula=Appliances~.,
                 data=data_test,
                 method='anova'
                 )
  pred_full <- predict(model, data_test)
  
  quality <- r2(pred.y = pred_full, true.y = test_data$Appliances)
  
  # badanie wplywu permutacji 
  for (k in 1:(ncol(test_data)-1))
  {
    # permutacja k-tego atrybutu
    data_test_perm <- data_test
    data_test_perm[,k] <- sample(data_test_perm[,k])
    
    # predykcja dla atrybut?w o spermutowanych warto?ciach
    pred_perm <- predict(model, data_test_perm)
    if (class(pred_perm) == "matrix") 
    {
      pred_perm_tmp <- vector()
      for (m in 1:nrow(pred_perm)) 
        pred_perm_tmp[m]=levels(data$Y)[which.max(pred_perm[m,])]
      pred_perm <- factor(pred_perm_tmp, levels <- levels(data_test$Y))
    }
    
    quality_perm <- r2(pred.y = pred_perm, true.y = data_test_perm$Appliances)

    quality_diff <- quality - quality_perm
    wyniki_tmp <- rbind(wyniki_tmp, c(quality, quality_perm, quality_diff))
  }
  # dodanie wynik?w z i-tej iteracji
  wyniki <- wyniki + wyniki_tmp
}

wyniki <- wyniki/N

wyniki <- data.frame(1:k, colnames(test_data)[1:k], wyniki[,3])
wyniki <- wyniki[order(wyniki[,3], decreasing = TRUE),]
ifelse(regression, colnames(wyniki) <- c("k", "attr", "R2_diff"), colnames(wyniki) <- c("k", "attr", "acc_diff"))

part <- 0.5

count <- ceiling((ncol(test_data)-1)*part)
message('Najwa?aniejsze ', part*100, '% atrybut?w to:\n', paste(wyniki$attr[2:count], ' '), '\n')
attr_part <- paste(wyniki$attr[2:count], collapse = "+")
attr_part <- paste("Appliances", "~", attr_part)
