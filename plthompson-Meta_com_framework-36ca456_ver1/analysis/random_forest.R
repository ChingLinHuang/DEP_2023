# ordinal random forest
# 
library(ordinalForest)
library(purrr)
library(tidyr)

# Split into training dataset and test dataset:
set.seed(123)
trainind <- sort(sample(1:nrow(dat_VP), size=floor(nrow(dat_VP)*(2/3))))
testind <- setdiff(1:nrow(dat_VP), trainind)
dat_VP_train <- dat_VP[trainind, c(-1,-2)]
dat_VP_test <- dat_VP[testind, c(-1,-2)]

# separate the variables
dat_VP_train_k <- dat_VP_train[ ,c(1,4,5,6)]
dat_VP_train_k$k <- as.factor(dat_VP_train$k)
dat_VP_train_i <- dat_VP_train[ ,c(2,4,5,6)]
dat_VP_train_i$i <- as.factor(dat_VP_train$i)
dat_VP_train_j <- dat_VP_train[ ,c(3,4,5,6)]
dat_VP_train_j$j <- as.factor(dat_VP_train$j)

# ordinal random forests
ordforres_k <- ordfor(depvar = "k", data = dat_VP_train_k, nsets = 1000, ntreeperdiv = 100, ntreefinal = 5000, perffunction = "equal")
ordforres_i <- ordfor(depvar = "i", data = dat_VP_train_i, nsets = 1000, ntreeperdiv = 100, ntreefinal = 5000, perffunction = "equal")
ordforres_j <- ordfor(depvar = "j", data = dat_VP_train_j, nsets = 1000, ntreeperdiv = 100, ntreefinal = 5000, perffunction = "equal")

# Study variable importance values:
sort(ordforres_k$varimp, decreasing = TRUE)
sort(ordforres_i$varimp, decreasing = TRUE)
sort(ordforres_j$varimp, decreasing = TRUE)

# Predict values of the ordinal target variable in the test dataset:
preds_k <- predict(ordforres_k, newdata = dat_VP_test)
preds_i <- predict(ordforres_i, newdata = dat_VP_test)
preds_j <- predict(ordforres_j, newdata = dat_VP_test)

# Compare predicted values with true values:
prediction_k <- table(data.frame(true_values = dat_VP_test$k, predictions = preds_k$ypred))
prediction_i <- table(data.frame(true_values = dat_VP_test$i, predictions = preds_i$ypred))
prediction_j <- table(data.frame(true_values = dat_VP_test$j, predictions = preds_j$ypred))
## End(Not run)

# performance
perf_k <- sum(diag(prediction_k)) / sum(prediction_k) # 47%
perf_i <- (sum(diag(prediction_i)) + sum(prediction_i[row(prediction_i) == col(prediction_i) + 1]) + sum(prediction_i[row(prediction_i) == col(prediction_i) - 1])) / sum(prediction_i) # 40%
perf_j <- (sum(diag(prediction_j)) + sum(prediction_j[row(prediction_j) == col(prediction_j) + 1]) + sum(prediction_j[row(prediction_j) == col(prediction_j) - 1])) / sum(prediction_j) # 77%

setwd("C:\\Users\\andy\\Downloads\\analysis")
save(ordforres_i, ordforres_j, ordforres_k, file = "RF.rdata")
