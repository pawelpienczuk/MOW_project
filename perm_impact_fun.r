# data <- con2
# N=1
# model_fun=rpart
# regression=FALSE

perm_impact_fun <- function(data, N, model_fun, regression, ...) {
  
  # do³¹czenie bibliotek
  library(caret)
  library(ModelMetrics)
  
  # zapewnienie prawid³owej nazwy kolumny wynikowej (uproszczenie) i prawid³owego typu danych 
  colnames(data)[ncol(data)] <- "Y"
  if (regression & class(data$Y)!="numeric") 
    data$Y <- as.numeric(data$Y)
  if (!regression & class(data$Y)!="factor") 
    data$Y <- as.factor(data$Y)

  wyniki <- data.frame(matrix(0,ncol=3,nrow=(ncol(data)-1)))

  for (i in 1:N)
  {
    wyniki_tmp <- data.frame("quality"=double(), "quality_perm"=double(), "quality_diff"=double())
    
    # proba bootstrapowa
    n <- sample(nrow(data), nrow(data), replace=TRUE)
    data_train <- data[n,]
    data_test <- data[-n,]
    
    # tworzenie modelu
    model <- model_fun(Y~., data=data_train, ...)
    pred <- predict(model, data_test)
    
    # zabezpieczenie
    if (class(pred) == "matrix")
    {
      pred_tmp <- vector()
      for (m in 1:nrow(pred)) 
        pred_tmp[m]=levels(data$Y)[which.max(pred[m,])]
      pred <- factor(pred_tmp, levels <- levels(data_test$Y))
    }
    
    # wyznaczenie odpowiedniej miary jakoœci 
    if (regression)
      quality <- 1-mse(data_test$Y, pred)/var(data_test$Y)
    else 
    {
      xtab <- table(pred, data_test$Y)
      good <- 0
      for (j in 1:nlevels(data_test$Y)) 
        good <- good + xtab[j,j]
      quality  <- good/sum(xtab)
    }
    
    # badanie wplywu permutacji 
    for (k in 1:(ncol(data)-1))
    {
      # permutacja k-tego atrybutu
      data_test_perm <- data_test
      data_test_perm[,k] <- sample(data_test_perm[,k])
    
      # predykcja dla atrybutów o spermutowanych wartoœciach
      pred_perm <- predict(model, data_test_perm)
      if (class(pred_perm) == "matrix") 
      {
        pred_perm_tmp <- vector()
        for (m in 1:nrow(pred_perm)) 
          pred_perm_tmp[m]=levels(data$Y)[which.max(pred_perm[m,])]
        pred_perm <- factor(pred_perm_tmp, levels <- levels(data_test$Y))
      }
      
      # wyznaczenie odpowiedniej miary jakoœci 
      if (regression)
        quality_perm <- mse(data_test_perm$Y, pred_perm)/var(data_test_perm$Y)
      else 
      {
        xtab <- table(pred_perm, data_test_perm$Y)
        good <- 0
        for (j in 1:nlevels(data_test$Y)) 
          good <- good + xtab[j,j]
        quality_perm  <- good/sum(xtab)
      }
      quality_diff <- quality - quality_perm
      wyniki_tmp <- rbind(wyniki_tmp, c(quality, quality_perm, quality_diff))
    }
    # dodanie wyników z i-tej iteracji
    wyniki <- wyniki + wyniki_tmp
  }
  
  # uœrednienie wyników
  wyniki <- wyniki/N
  if (regression)
    message("Wspó³czynnik determinacji R2 modelu dla danych oryginalnych to: ", quality)
  else
    message("Dok³adnoœæ modelu dla danych oryginalnych to: ", quality)
  
  # przygotowanie danych wynikowych
  wyniki <- data.frame(1:k, colnames(data)[1:k], wyniki[,3])
  wyniki <- wyniki[order(wyniki[,3], decreasing = TRUE),]
  ifelse(regression, colnames(wyniki) <- c("k", "attr", "R2_diff"), colnames(wyniki) <- c("k", "attr", "acc_diff"))
  
  return(wyniki)
  
}