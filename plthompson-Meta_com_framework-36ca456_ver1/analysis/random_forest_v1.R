# ordinal random forest
# 


RF <- function(dat){
  library(ordinalForest)
  library(purrr)
  library(tidyr)
  # Split into training dataset and test dataset:
  trainind <- sort(sample(1:nrow(dat), size = floor(nrow(dat)*(2/3))))
  testind <- setdiff(1:nrow(dat), trainind)
  dat_train <- dat[trainind, c(-1,-2)]
  dat_test <- dat[testind, c(-1,-2)]
  
  # separate the variables
  dat_train_k <- dat_train[ ,-c(2,3)]
  dat_train_k$k <- as.factor(dat_train$k)
  dat_train_i <- dat_train[ ,-c(1,3)]
  dat_train_i$i <- as.factor(dat_train$i)
  dat_train_j <- dat_train[ ,-c(1,2)]
  dat_train_j$j <- as.factor(dat_train$j)
  
  # ordinal random forests
  ordforres_k <- ordfor(depvar = "k", data = dat_train_k, nsets = 1000, ntreeperdiv = 100, ntreefinal = 5000, perffunction = "equal")
  ordforres_i <- ordfor(depvar = "i", data = dat_train_i, nsets = 1000, ntreeperdiv = 100, ntreefinal = 5000, perffunction = "equal")
  ordforres_j <- ordfor(depvar = "j", data = dat_train_j, nsets = 1000, ntreeperdiv = 100, ntreefinal = 5000, perffunction = "equal")
  
  # Study variable importance values:
  importance_k <- sort(ordforres_k$varimp, decreasing = TRUE)
  importance_i <- sort(ordforres_i$varimp, decreasing = TRUE)
  importance_j <- sort(ordforres_j$varimp, decreasing = TRUE)
  
  # Predict values of the ordinal target variable in the test dataset:
  preds_k <- predict(ordforres_k, newdata = dat_test)
  preds_i <- predict(ordforres_i, newdata = dat_test)
  preds_j <- predict(ordforres_j, newdata = dat_test)
  
  # Compare predicted values with true values:
  prediction_k <- table(data.frame(true_values = dat_test$k, predictions = preds_k$ypred))
  prediction_i <- table(data.frame(true_values = dat_test$i, predictions = preds_i$ypred))
  prediction_j <- table(data.frame(true_values = dat_test$j, predictions = preds_j$ypred))
  ## End(Not run)
  
  # performance
  performance_k <- sum(diag(prediction_k)) / sum(prediction_k) # 47%
  performance_i <- sum(diag(prediction_i))/ sum(prediction_i) # 40%
  performance_j <- sum(diag(prediction_j))/ sum(prediction_j) # 77%
  
  # reture results
  return(list(model = list(i = ordforres_i, 
                           j = ordforres_j, 
                           k = ordforres_k), 
              importance = list(i = importance_i, 
                                j = importance_j, 
                                k = importance_k),
              prediction = list(i = prediction_i,
                                j = prediction_j,
                                k = prediction_k),
              performance = list(i = performance_i,
                                 j = performance_j, 
                                 k = performance_k)))
}

setwd("C:\\Users\\andy\\Downloads\\analysis")
load()
RF_VP <- RF(dat_VP)
RF_DNCI <- RF(dat_DNCI)
# save(RF_VP, RF_DNCI, file = ".\\data\\RF.rdata")

rbind(RF_VP$performance)
rbind(RF_DNCI$performance)
