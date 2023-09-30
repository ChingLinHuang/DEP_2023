library(tidyverse)
library(ordinalForest)
library(randomForest)

### Load functions for calculating VP and DNCI
source("C:\\Users\\andy\\Downloads\\analysis\\variation partitioning\\code\\functions_prepare_VP.R")
source("C:\\Users\\andy\\Downloads\\analysis\\DNCI\\code\\functions_prepare_DNCI.R")

res <- data.frame(Time = 1:4, Env = 0, EnvSpatial = 0, Spatial = 0, Resid = 0, DNCI = 0, CI.DNCI = 0)

setwd("C:\\Users\\andy\\Downloads\\analysis\\empirical\\Fushan\\data")
env <- read.csv("env.csv", row.names = 1)


dist.mat <- read.csv("dist.csv", row.names = 1)
for(t in 1:4){
### load data
spe <- read.csv(paste0("spe_", t, ".csv"), row.names = 1)

### Calculate summary statistics
# variation partitioning
res.VP1 <- VP(spe = spe, env = env, dist.mat = dist.mat)
res[t, 2:5] <- res.VP1$part$part$indfract$Adj.R.squared

# DNCI
res.DNCI <- DNCIndex(spe = spe, Nperm = 100, dataType = "prab", time_sparse = 1000)
res[t, 6:7] <- res.DNCI$res[4:5]
}

# Stegen
res.Stegen <- read.csv("C:\\Users\\andy\\Downloads\\analysis\\empirical\\Fushan\\res\\res_stegen.csv")

res <- res.Stegen %>% left_join(res, by = "Time")
#save(res, file = "C:\\Users\\andy\\Downloads\\analysis\\empirical\\Fushan\\res\\res.rdata")

### prediction of random forest 
load("C:\\Users\\andy\\Downloads\\analysis\\empirical\\Fushan\\res\\res.rdata")
load("C:\\Users\\andy\\Downloads\\analysis\\res\\RF\\RF_t4_all.rdata")

# inputs
colnames(RF_t4_all$importance)
inputs <- as.numeric(as.matrix(res[4:1, -1]))
inputs <- as.data.frame(matrix(inputs, nrow = 1))
colnames(inputs) <- colnames(RF_t4_all$importance)

# Prediction
preds_k <- predict(RF_t4_all$model$k, newdata = inputs)
preds_i <- predict(RF_t4_all$model$i, newdata = inputs)
preds_j <- predict(RF_t4_all$model$j, newdata = inputs)

preds_k # 2 competition-colonization trade-off
preds_i$ypred # 8 intermediate dispersal
preds_j$ypred # 10 intermediate environmental filtering

