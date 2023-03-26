##
##

library(tidyr)
library(vegan)

source("C:\\Users\\andy\\Downloads\\analysis\\variation partitioning\\code\\functions_prepare_VP.R")

# load data
setwd("C:\\Users\\andy\\Downloads\\analysis")
load("RF.rdata")

spe <- read.csv(".\\empirical\\Lalashan\\spe.csv", row.names = 1)
env <- read.csv(".\\empirical\\Lalashan\\env.csv", row.names = 1)
# soil <- read.csv(".\\empirical\\soil.csv")

position <- data.frame(x = rep(0:9, each = 10), 
                       y = rep(0:9, times = 10))
dist.mat <- dist(position)

# variation partitioning
VP_LFDP <- VP(spe = spe, env = env, dist.mat = dist.mat)
res_VP_LFDP <- VP_LFDP$part$part$indfract$Adj.R.squared %>%
  t() %>%
  data.frame()
colnames(res_VP_LFDP) <- c("Env", "Env and Spatial", "Spatial", "Resid")

# random forest
pred_VP_LFDP_j <- predict(RF_VP$model$j, newdata = res_VP_LFDP)
pred_VP_LFDP_j$ypred # j = 5 # niche width
pred_VP_LFDP_i <- predict(RF_VP$model$i, newdata = res_VP_LFDP)
pred_VP_LFDP_i$ypred # i = 15 # dispersal ability
pred_VP_LFDP_k <- predict(RF_VP$model$k, newdata = res_VP_LFDP)
pred_VP_LFDP_k$ypred # k = 1 # competition
