# Random forest

library(tidyverse)
library(ordinalForest)
library(randomForest)
library(purrr)

RF <- function(dat_train, dat_test, selvar){
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

### load data
#####
setwd("C:\\Users\\andy\\Downloads\\analysis\\res")
load(".\\VP\\res_VP.rdata")
load(".\\DNCI\\res_DNCI.rdata")
load(".\\Stegen\\res_Stegen.rdata")

### Data filtering
dat_raw <- dat_VP %>% 
  full_join(dat_Stegen) %>% 
  full_join(dat_DNCI) 
sum(dat_raw$DNCI == "too sparse") # 8874
sum(dat_raw$DNCI == "no turnover") # 440
sum(dat_raw$DNCI == "NaN") # 9119

# dat_DNCI_remove <- dat_DNCI %>%
#   filter(DNCI == "too sparse" | 
#            DNCI == "no turnover" | 
#            DNCI == "NaN") %>%
#   select(-t, -rep) %>%
#   unique

dat_raw <- dat_raw %>% 
  filter(DNCI != "too sparse" & 
           DNCI != "no turnover" & 
           DNCI != "NaN") %>%
  mutate(DNCI = as.numeric(DNCI), CI.DNCI = as.numeric(CI.DNCI))
colnames(dat_raw)[colnames(dat_raw) == "Env and Spatial"] <- "EnvSpatial"
rm(dat_VP, dat_DNCI, dat_Stegen)


statistics_name <- c("Selection", "DispLimit", "HomoDisp", "Drift", "Env", "EnvSpatial", "Spatial", "Resid", "DNCI", "CI.DNCI")

for(ind in 1:length(statistics_name)){
  dat_spread <- dat_raw %>%
    select(rep, t, k ,i, j, statistics_name[ind]) %>%
    spread(key = t, value = statistics_name[ind])
  colnames(dat_spread)[5:24] <- paste0(statistics_name[ind], colnames(dat_spread)[5:24])
  
  if (ind == 1){
    dat <- dat_spread
  }else{
    dat <- dat %>% left_join(dat_spread)
  }
}
nrow(dat) # 6486
dat <- dat[complete.cases(dat), ]
nrow(dat) # 5150

save(dat, file = ".\\RF\\dat.rdata")
#rm(dat)

#####

### partition training and testing data
set.seed(123)

setwd("C:\\Users\\andy\\Downloads\\analysis\\res\\RF")
load("dat.rdata")
n_rep <- length(unique(dat$rep))
rep_train <- 1:floor(n_rep*(2/3)) # first 2/3 replicate as training data
rep_test <- setdiff(1:n_rep, rep_train) # last 1/3 replicate as testing data
dat_train <- dat %>% 
  filter (rep %in% rep_train) %>% 
  select(-rep) %>%
  mutate(k = as.factor(k), i = as.factor(i), j = as.factor(j))

dat_test <- dat %>% 
  filter (rep %in% rep_test) %>% 
  select(-rep) %>%
  mutate(k = as.factor(k), i = as.factor(i), j = as.factor(j))


setwd("C:\\Users\\andy\\Downloads\\analysis\\res\\RF")

### Combination: one time step with Stegen
RF_t1_Stegen <- RF(dat_train = dat_train, 
                   dat_test = dat_test, 
                   selvar = c("Selection60", "DispLimit60", "HomoDisp60", "Drift60"))
save(RF_t1_Stegen, file = "RF_t1_Stegen.rdata")

### Combination: 4 time step with Stegen
RF_t4_Stegen <- RF(dat_train = dat_train, 
                   dat_test = dat_test, 
                   selvar = paste0(rep(c("Selection", "DispLimit", "HomoDisp", "Drift"), each = 4), c(60, 56, 52, 48)))
save(RF_t4_Stegen, file = "RF_t4_Stegen.rdata")

### Combination: 20 time step with Stegen
RF_t20_Stegen <- RF(dat_train = dat_train, 
                    dat_test = dat_test, 
                    selvar = paste0(rep(c("Selection", "DispLimit", "HomoDisp", "Drift"), each = 20), 60:41))
save(RF_t20_Stegen, file = "RF_t20_Stegen.rdata")

### Combination: 1 time step with VP
RF_t1_VP <- RF(dat_train = dat_train, 
               dat_test = dat_test, 
               selvar = paste0(rep(c("Env", "EnvSpatial", "Spatial", "Resid"), each = 1), 60))
save(RF_t1_VP, file = "RF_t1_VP.rdata")

### Combination: 4 time step with VP
RF_t4_VP <- RF(dat_train = dat_train, 
               dat_test = dat_test, 
               selvar = paste0(rep(c("Env", "EnvSpatial", "Spatial", "Resid"), each = 4), c(60, 56, 52, 48)))
save(RF_t4_VP, file = "RF_t4_VP.rdata")


### Combination: 20 time step with VP
RF_t20_VP <- RF(dat_train = dat_train, 
                dat_test = dat_test, 
                selvar = paste0(rep(c("Env", "EnvSpatial", "Spatial", "Resid"), each = 20), 60:41))
save(RF_t20_VP, file = "RF_t20_VP.rdata")


### Combination: 1 time step with DNCI
RF_t1_DNCI <- RF(dat_train = dat_train, 
                 dat_test = dat_test, 
                 selvar = paste0(rep(c("DNCI", "CI.DNCI"), each = 1), 60))
save(RF_t1_DNCI, file = "RF_t1_DNCI.rdata")

### Combination: 4 time step with DNCI
RF_t4_DNCI <- RF(dat_train = dat_train, 
                 dat_test = dat_test, 
                 selvar = paste0(rep(c("DNCI", "CI.DNCI"), each = 4), c(60, 56, 52, 48)))
save(RF_t4_DNCI, file = "RF_t4_DNCI.rdata")

### Combination: 20 time step with DNCI
RF_t20_DNCI <- RF(dat_train = dat_train, 
                  dat_test = dat_test, 
                  selvar = paste0(rep(c("DNCI", "CI.DNCI"), each = 20), 41:60))
save(RF_t20_DNCI, file = "RF_t20_DNCI.rdata")

### Combination: 1 time step with VP, Stegn, DNCI
RF_t1_all <- RF(dat_train = dat_train, 
                dat_test = dat_test, 
                selvar = paste0(rep(c("Selection", "DispLimit", "HomoDisp", "Drift", "Env", "EnvSpatial", "Spatial", "Resid", "DNCI", "CI.DNCI"), each = 1), 60))
save(RF_t1_all, file = "RF_t1_all.rdata")

### Combination: 4 time step with VP, Stegn, DNCI
RF_t4_all <- RF(dat_train = dat_train, 
                dat_test = dat_test, 
                selvar = paste0(rep(c("Selection", "DispLimit", "HomoDisp", "Drift", "Env", "EnvSpatial", "Spatial", "Resid", "DNCI", "CI.DNCI"), each = 4), c(60, 56, 52, 48)))
save(RF_t4_all, file = "RF_t4_all.rdata")

### Combination: 4 time step with VP, Stegen, DNCI
RF_t20_all <- RF(dat_train = dat_train, 
                 dat_test = dat_test, 
                 selvar = paste0(rep(c("Selection", "DispLimit", "HomoDisp", "Drift", "Env", "EnvSpatial", "Spatial", "Resid", "DNCI", "CI.DNCI"), each = 20), 60:41))
save(RF_t20_all, file = "RF_t20_all.rdata")

