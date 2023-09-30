##
##

library(tidyverse)
library(ordinalForest)
library(randomForest)

source("C:\\Users\\andy\\Downloads\\analysis\\variation partitioning\\code\\functions_prepare_VP.R")
source("C:\\Users\\andy\\Downloads\\analysis\\DNCI\\code\\functions_prepare_DNCI.R")

# random sample 1000 times (with 4 time periods)
# set.seed(123)
# for (n_sample in 1:100){
#   ind_sample <- sort(sample(1:625, round(625*0.9))) # 562 subplots
#   if (n_sample == 1)
#     rand_sample <- ind_sample
#   else
#     rand_sample <- rbind(rand_sample, ind_sample)
# }
# write.csv(rand_sample, file = "C:\\Users\\andy\\Downloads\\analysis\\empirical\\Fushan\\res\\rand_sample.csv")


rand_sample <- read.csv("C:\\Users\\andy\\Downloads\\analysis\\empirical\\Fushan\\res\\rand_sample.csv")
rand_sample <- rand_sample[,-1]

res <- data.frame(n_sample = 0, Time = 0, Env = 0, EnvSpatial = 0, Spatial = 0, Resid = 0, DNCI = 0, CI.DNCI = 0)

setwd("C:\\Users\\andy\\Downloads\\analysis\\empirical\\Fushan\\data")
env <- read.csv("env.csv", row.names = 1)
dist.mat <- read.csv("dist.csv", row.names = 1)

iter <- 1
for (n_sample in 1:25){
  ind_sample <- rand_sample[n_sample, ] %>% as.matrix %>% c
  for(t in 1:4){
    print(date())
    print(paste0("iter = ", iter, "; t = ", t))
    res[iter, 1] <- n_sample
    res[iter, 2] <- t
    ### load data
    spe <- read.csv(paste0("spe_", t, ".csv"), row.names = 1)
    spe_s <- spe[ind_sample, ]
    spe_s <- spe_s[, colSums(spe_s) > 0]
    env_s <- env[ind_sample, ]
    dist.mat_s <- dist.mat[ind_sample, ind_sample]
    
    ### Calculate summary statistics
    # variation partitioning
    res.VP1 <- VP(spe = spe_s, env = env_s, dist.mat = dist.mat_s)
    res[iter, 3:6] <- res.VP1$part$part$indfract$Adj.R.squared
    
    # DNCI
    res.DNCI <- DNCIndex(spe = spe_s, Nperm = 100, dataType = "prab", time_sparse = 1000)
    res[iter, 7:8] <- res.DNCI$res[4:5]
    
    iter <- iter + 1
  }
}
save(res, file = "C:\\Users\\andy\\Downloads\\analysis\\empirical\\Fushan\\res\\res_CI_1.rdata")



### load data
setwd("C:\\Users\\andy\\Downloads\\analysis\\empirical\\Fushan\\res")

# VP and DNCI
first <- 1
for (i in 1:5){
  load(paste0("res_CI_", i, ".rdata"))
  if(first == 1){
    res_all_VP <- res
    first <- first + 1
  }
  else
    res_all_VP <- rbind(res_all_VP, res)
}

# Stegen
first <- 1
for (i in 1:5){
  res_Stegen <- read.csv(paste0("res_stegen_CI_", i, ".csv"))
  if(first == 1){
    res_all_Stegen <- res_Stegen
    first <- first + 1
  }
  else
    res_all_Stegen <- rbind(res_all_Stegen, res_Stegen)
}


res_all <- res_all_Stegen %>% left_join(res_all_VP)
#save(res_all, file = "C:\\Users\\andy\\Downloads\\analysis\\empirical\\Fushan\\res\\res_all_CI.rdata")

### prediction of random forest 
load("C:\\Users\\andy\\Downloads\\analysis\\empirical\\Fushan\\res\\res_all_CI.rdata")
load("C:\\Users\\andy\\Downloads\\analysis\\res\\RF\\RF_t4_all.rdata")

# inputs
colnames(RF_t4_all$importance)
for (n_s in 1:100){
  if(n_s == 1){
    inputs_1 <- res_all[(n_s*4):(n_s*4-3), 3:12] %>% 
      as.matrix %>% 
      as.numeric
    inputs <- as.data.frame(matrix(inputs_1, nrow = 1))
    colnames(inputs) <- colnames(RF_t4_all$importance)
  }
  else{
    inputs_1 <- res_all[(n_s*4):(n_s*4-3), 3:12] %>% 
      as.matrix %>% 
      as.numeric
    inputs[n_s, ] <- inputs_1
  }
}

# Prediction
pred <- data.frame(n_sample = 0, i = 0, j = 0, k = 0)
for (n_s in 1:100){
  pred[n_s, 1] <- n_s
  preds_k <- predict(RF_t4_all$model$k, newdata = inputs[n_s, ])
  preds_i <- predict(RF_t4_all$model$i, newdata = inputs[n_s, ])
  preds_j <- predict(RF_t4_all$model$j, newdata = inputs[n_s, ])
  pred[n_s, 2:4] <- c(preds_i$ypred, preds_j$ypred, preds_k)
}
#save(pred, file = "pred_CI.rdata")
table(pred$k)
table(pred$i)
table(pred$j)


# ## complete data
# preds_k # 2 competition-colonization trade-off
# preds_i$ypred # 8 intermediate dispersal
# preds_j$ypred # 10 intermediate environmental filtering





