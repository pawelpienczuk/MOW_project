part_model <- function(data, attr, part, N, model_fun, regression, ...) {

  # do³¹czenie bibliotek
  library(caret)
  library(ModelMetrics)
  
  # zapewnienie prawid³owej nazwy kolumny wynikowej (uproszczenie) i prawid³owego typu danych 
  colnames(data)[ncol(data)] <- "Y"
  if (regression & class(data$Y)!="numeric") 
    data$Y <- as.numeric(data$Y)
  if (!regression & class(data$Y)!="factor") 
    data$Y <- as.factor(data$Y)
  
  # wybór czêœci najwa¿niejszych atrybutów
  count <- ceiling((ncol(data)-1)*part)
  message('Najwa¿aniejsze ', part*100, '% atrybutów to:\n', paste(attr[1:count], ' '), '\n')
  attr_part <- paste(attr[1:count], collapse = "+")
  attr_part <- paste("Y", "~", attr_part)
  
  # zerowanie zmiennych
  quality_full <- 0
  quality_part <- 0
  
  for (i in 1:N)
  {
    # proba bootstrapowa
    n <- sample(nrow(data), nrow(data), replace=TRUE)
    data_train <- data[n,]
    data_test <- data[-n,]
    
    # tworzenie modelu na wszystkich atrybutach
    model_full <- model_fun(Y~., data=data_train, ...)
    pred_full <- predict(model_full, data_test)
    
    # zabezpieczenie
    if (class(pred_full) == "matrix")
    {
      pred_tmp <- vector()
      for (m in 1:nrow(pred_full))
      {
        pred_tmp[m]=levels(data$Y)[which.max(pred_full[m,])]
      }
      pred_full <- factor(pred_tmp, levels <- levels(data_test$Y))
    }
  
    # wyznaczenie odpowiedniej miary jakoœci 
    if (regression)
      quality_full_tmp <- 1-mse(data_test$Y, pred_full)/var(data_test$Y)
    else 
    {
      xtab <- table(pred_full, data_test$Y)
      good <- 0
      for (j in 1:nlevels(data_test$Y)) 
        good <- good + xtab[j,j]
      quality_full_tmp  <- good/sum(xtab)
    }
    
    # tworzenie modelu na podstawie czêsci najwa¿niejszych atrybutów
    model_part <- model_fun(as.formula(attr_part), data = data_train, ...)
    pred_part <- predict(model_part, data_test)
    
    # zabezpieczenie
    if (class(pred_part) == "matrix")
    {
      pred_tmp <- vector()
      for (m in 1:nrow(pred_part))
      {
        pred_tmp[m]=levels(data$Y)[which.max(pred_part[m,])]
      }
      pred_part <- factor(pred_tmp, levels <- levels(data_test$Y))
    }
    
    # wyznaczenie odpowiedniej miary jakoœci 
    if (regression)
      quality_part_tmp <- 1-mse(data_test$Y, pred_part)/var(data_test$Y)
    else 
    {
      xtab <- table(pred_part, data_test$Y)
      good <- 0
      for (j in 1:nlevels(data_test$Y)) 
        good <- good + xtab[j,j]
      quality_part_tmp  <- good/sum(xtab)
    }
    
    quality_full <- quality_full + quality_full_tmp
    quality_part <- quality_part + quality_part_tmp
  }
  
  quality_full <- quality_full/N
  quality_part <- quality_part/N
  out <- data.frame(part, quality_full, quality_part)

  if (regression)
    colnames(out) <- c("part", "R2_full", "R2_part")
  else
    colnames(out) <- c("part", "acc_full", "acc_part")
  
  return(out)
  
}