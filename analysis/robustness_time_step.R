# Robustness testing -- time step
# 
library(tidyverse)
library(ordinalForest)
library(randomForest)
library(purrr)

##############
##############  Time step robustness
##############
##########

### load data
#####
setwd("C:\\Users\\andy\\Downloads\\analysis\\res")
load(".\\RF\\RF_t4_all.rdata")
load(".\\RF\\dat.rdata")
n_rep <- length(unique(dat$rep))
rep_train <- 1:floor(n_rep*(2/3)) # first 2/3 replicate as training data
rep_test <- setdiff(1:n_rep, rep_train) # last 1/3 replicate as testing data

dat_test <- dat %>% 
  filter (rep %in% rep_test) %>% 
  select(-rep) %>%
  mutate(k = as.factor(k), i = as.factor(i), j = as.factor(j)) %>%
  drop_na()

#RF <- function(dat_train, dat_test, selvar){
  # subset data by selvar
  dat_train_k <- dat_train %>% select(k, selvar)
  dat_train_i <- dat_train %>% select(i, selvar)
  dat_train_j <- dat_train %>% select(j, selvar)
  dat_test_k <- dat_test %>% select(k, selvar)
  dat_test_i <- dat_test %>% select(i, selvar)
  dat_test_j <- dat_test %>% select(j, selvar)
  
  # ordinal random forests
  forres_k <- randomForest(k ~ ., data = dat_train_k)
  ordforres_i <- ordfor(depvar = "i", data = dat_train_i)
  ordforres_j <- ordfor(depvar = "j", data = dat_train_j)
  
  # Study variable importance values:
  importance_k <- as.vector(t(forres_k$importance))
  names(importance_k) <- selvar
  importance_k <- importance_k
  importance_i <- ordforres_i$varimp
  importance_j <- ordforres_j$varimp
  importance <- rbind(importance_k, importance_i, importance_j)
  
  # Predict values of the ordinal target variable in the test dataset:
  preds_k <- predict(forres_k, newdata = dat_test)
  preds_i <- predict(ordforres_i, newdata = dat_test)
  preds_j <- predict(ordforres_j, newdata = dat_test)
  
  # Compare predicted values with true values:
  prediction_k <- table(data.frame(true_values = dat_test$k, predictions = preds_k))
  prediction_i <- table(data.frame(true_values = dat_test$i, predictions = preds_i$ypred))
  prediction_j <- table(data.frame(true_values = dat_test$j, predictions = preds_j$ypred))
  ## End(Not run)
  
  # performance
  performance_k <- sum(diag(prediction_k)) / sum(prediction_k)
  performance_i <- sum(diag(prediction_i))/ sum(prediction_i)
  performance_j <- sum(diag(prediction_j))/ sum(prediction_j) 
  
  # reture results
  return(list(model = list(i = ordforres_i, 
                           j = ordforres_j, 
                           k = forres_k), 
              importance = importance,
              prediction = list(i = prediction_i,
                                j = prediction_j,
                                k = prediction_k),
              performance = data.frame(i = performance_i,
                                       j = performance_j, 
                                       k = performance_k)))
  
}

#####


### Randomly choosing four time steps
#####
## Initial data setting
# Saving accuracy in estimating parameters 
Accuracy <- data.frame(i = 0, j = 0, k = 0)
# Saving the true value
PredMat_i <- matrix(ncol = 101, nrow = nrow(dat_test))
PredMat_i[ ,1] <- dat_test$i # true value
PredMat_j <- matrix(ncol = 101, nrow = nrow(dat_test))
PredMat_j[ ,1] <- dat_test$j
PredMat_k <- matrix(ncol = 101, nrow = nrow(dat_test))
PredMat_k[ ,1] <- dat_test$k

# Save accuracy of the RF with correct time steps
Accuracy[1, ] <- RF_t4_all$performance

## Saving estimated results: correct time steps
n_rand <- 99
PredMat_i[ ,2] <- predict(RF_t4_all$model$i, newdata = dat_test)$ypred # estimated value by correct time steps
PredMat_j[ ,2] <- predict(RF_t4_all$model$j, newdata = dat_test)$ypred
PredMat_k[ ,2] <- predict(RF_t4_all$model$k, newdata = dat_test)

statistics_name <- c("Selection", "DispLimit", "HomoDisp", "Drift", "Env", "EnvSpatial", "Spatial", "Resid", "DNCI", "CI.DNCI")

## Saving estimated results: reshuffled time steps
for(i in 3:(n_rand+2)){
  print(paste0("i = ", i))
  time_step <- sort(sample(41:60, size = 4), decreasing = T)
  # randomly select 4 time steps as the testing data
  dat_test_1 <- dat_test %>%
    select(paste0(rep(statistics_name, each = 4), time_step))
  colnames(dat_test_1) <- RF_t4_all$model$i$forestfinal$forest$independent.variable.names
  
  # prediction
  PredMat_i[ ,i] <- predict(RF_t4_all$model$i, newdata = dat_test)$ypred
  PredMat_j[ ,i] <- predict(RF_t4_all$model$j, newdata = dat_test)$ypred
  PredMat_k[ ,i] <- predict(RF_t4_all$model$k, newdata = dat_test)
  
  prediction_i <- table(data.frame(true_values = dat_test$i, 
                                   predictions = PredMat_i[ ,i]))
  prediction_j <- table(data.frame(true_values = dat_test$j, 
                                   predictions = PredMat_j[ ,i]))
  prediction_k <- table(data.frame(true_values = dat_test$k, 
                                   predictions = PredMat_k[ ,i]))
  # Accuracy
  Accuracy[i-1, ] <- c(sum(diag(prediction_i))/ sum(prediction_i), sum(diag(prediction_j))/ sum(prediction_j), sum(diag(prediction_k)) / sum(prediction_k))

}
PredMat_i <- as.data.frame(PredMat_i)
colnames(PredMat_i)[1] <- "true" 
PredMat_j <- as.data.frame(PredMat_j)
colnames(PredMat_j)[1] <- "true" 
PredMat_k <- as.data.frame(PredMat_k)
colnames(PredMat_k)[1] <- "true" 

#save(Accuracy, PredMat_i, PredMat_j, PredMat_k, file = ".//RF//robustness_time_step.rdata")
#####


### Plot
#####
setwd("C:\\Users\\andy\\Downloads\\analysis\\")
load(".\\res\\RF\\robustness_time_step.rdata")

# Accuracy distribution < 0.03%
sd(Accuracy$i) # 0.0002932
sd(Accuracy$j) # 0.0002927
sd(Accuracy$k) # 0.0002840

colnames(Accuracy) <- c("Dispersal", "Niche width", "Competition")
Accuracy <- Accuracy[, c(2,3,1)]
Accuracy %>%
  gather(key = "Process", value = "Accuracy", `Niche width`:Dispersal) %>%
  ggplot(aes(x = Process, y = Accuracy)) + 
  geom_boxplot() +
  theme(legend.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))
ggsave(file = ".\\figures\\Robustness_time_step_Accuracy_distribution.pdf", width = 15, height = 15, units = "cm")
ggsave(file = ".\\figures\\Robustness_time_step_Accuracy_distribution.jpg", width = 15, height = 15, units = "cm")

# # prediction error
# # boxplot
# PredMat_i %>%
#   gather(key = "Train_set", value = "Prediction", -true) %>%
#   ggplot(aes(x = as.factor(true), y = Prediction)) +
#   geom_boxplot() +
#   ggtitle("Prediction of dispersal ability")
# ggsave(file = ".\\figures\\Robustness_time_step_Prediction_boxplot_i.pdf", width = 15, height = 15, units = "cm")
# ggsave(file = ".\\figures\\Robustness_time_step_Prediction_boxplot_i.jpg", width = 15, height = 15, units = "cm")
# 
# PredMat_j %>%
#   gather(key = "Train_set", value = "Prediction", -true) %>%
#   ggplot(aes(x = as.factor(true), y = Prediction)) +
#   geom_boxplot() +
#   ggtitle("Prediction of Abiotic response")
# ggsave(file = ".\\figures\\Robustness_time_step_Prediction_boxplot_j.pdf", width = 15, height = 15, units = "cm")
# ggsave(file = ".\\figures\\Robustness_time_step_Prediction_boxplot_j.jpg", width = 15, height = 15, units = "cm")
# 
# PredMat_k %>%
#   gather(key = "Train_set", value = "Prediction", -true) %>%
#   ggplot(aes(x = as.factor(true), y = Prediction)) +
#   geom_boxplot() +
#   ggtitle("Prediction of Bioric interaction")
# ggsave(file = ".\\figures\\Robustness_time_step_Prediction_boxplot_k.pdf", width = 15, height = 15, units = "cm")
# ggsave(file = ".\\figures\\Robustness_time_step_Prediction_boxplot_k.jpg", width = 15, height = 15, units = "cm")
# 
# # Heatmap
# PredMat_i %>%
#   gather(key = "Train_set", value = "Prediction", -true) %>%
#   select(true, Prediction) %>%
#   group_by(true, Prediction) %>%
#   count() %>%
#   group_by(true) %>%
#   group_modify(~ {.x %>% mutate(prop = n / sum(.x$n))}) %>%
#   ggplot(aes(x = as.factor(true), y = as.factor(Prediction), fill = prop)) + 
#   geom_tile() + 
#   scale_fill_gradientn(colours = terrain.colors(50)) + 
#   ggtitle("Prediction of dispersal ability") +
#   xlab("True value") +
#   ylab("Prediction")
# ggsave(file = ".\\figures\\Robustness_time_step_Prediction_tile_i.pdf", width = 15, height = 15, units = "cm")
# ggsave(file = ".\\figures\\Robustness_time_step_Prediction_tile_i.jpg", width = 15, height = 15, units = "cm")
# 
# PredMat_j %>%
#   gather(key = "Train_set", value = "Prediction", -true) %>%
#   select(true, Prediction) %>%
#   group_by(true, Prediction) %>%
#   count() %>%
#   group_by(true) %>%
#   group_modify(~ {.x %>% mutate(prop = n / sum(.x$n))}) %>%
#   ggplot(aes(x = as.factor(true), y = as.factor(Prediction), fill = prop)) + 
#   geom_tile() + 
#   scale_fill_gradientn(colours = terrain.colors(50)) + 
#   ggtitle("Prediction of abiotic response") +
#   xlab("True value") +
#   ylab("Prediction")
# ggsave(file = ".\\figures\\Robustness_time_step_Prediction_tile_j.pdf", width = 15, height = 15, units = "cm")
# ggsave(file = ".\\figures\\Robustness_time_step_Prediction_tile_j.jpg", width = 15, height = 15, units = "cm")
# 
# PredMat_k %>%
#   gather(key = "Train_set", value = "Prediction", -true) %>%
#   select(true, Prediction) %>%
#   group_by(true, Prediction) %>%
#   count() %>%
#   group_by(true) %>%
#   group_modify(~ {.x %>% mutate(prop = n / sum(.x$n))}) %>%
#   ggplot(aes(x = as.factor(true), y = as.factor(Prediction), fill = prop)) + 
#   geom_tile() + 
#   scale_fill_gradientn(colours = terrain.colors(50)) + 
#   ggtitle("Prediction of biotic interaction") +
#   xlab("True value") +
#   ylab("Prediction")
# ggsave(file = ".\\figures\\Robustness_time_step_Prediction_tile_k.pdf", width = 15, height = 15, units = "cm")
# ggsave(file = ".\\figures\\Robustness_time_step_Prediction_tile_k.jpg", width = 15, height = 15, units = "cm")

#####

##########