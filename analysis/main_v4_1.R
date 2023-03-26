# multigroup DNCI

library(adespatial)
library(tidyverse)
library(som.nn)
library(vegan)
library(purrr)

source("C:\\Users\\andy\\Downloads\\analysis\\variation partitioning\\code\\functions_prepare_VP.R")
source("C:\\Users\\andy\\Downloads\\analysis\\DNCI\\code\\functions_prepare_DNCI_v1.R")

setwd("C:\\Users\\andy\\Downloads\\analysis\\data")

## results for variation paritioning
# variables descriptions:
# E: env
# S: spatial
# ES: union of env and spatial
# E_S: env with no spatial
# E.S: intersection of env and spatial
# S_E: spatial with no env
# resid: residual
# 
  parameter_space <- cross_df(list(j = 1:13, i = 1:15, k = 1:5, rep = 1:6, t = c(60, 56, 52, 48))) %>% as.data.frame
  
  res <- data.frame(rep = 0, t = 0, k = 0, i = 0, j = 0)
  
  SaveDatInd <- 1
for(ind in 1:2000){
    rep <- parameter_space[ind, "rep"]
    t <- parameter_space[ind, "t"]
    k <- parameter_space[ind, "k"]
    i <- parameter_space[ind, "i"]
    j <- parameter_space[ind, "j"]
    
    print(paste0("ind = ", ind, ", SaveDatInd = ", SaveDatInd))
    print(paste0("rep = ", rep, ", t = ", t, ", k = ", k, ", i = ", i, ", j = ", j))
    res[SaveDatInd, 1:5] <- c(rep, t, k, i, j)
    
    setwd("C:\\Users\\andy\\Downloads\\analysis\\data")
    ## load data
    # species composition # dat_spe
    load(paste0(".\\spe_4_1\\spe_rep", rep, "k", k, "i", i, "j", j, "t", t
                , ".rdata"))
    # environment # dat_env
    load(paste0(".\\env_4_1\\env_rep", rep, "k", k, "i", i, "j", j, "t", t
                , ".rdata"))
    # trait # dat_trait
    load(paste0(".\\trait_4_1\\trait_rep", rep, "k", k, "i", i, "j", j, "t", t, ".rdata"))
    # distance matrix # dist.mat
    load(paste0(".\\dist_4_1\\dist_rep", rep, "k", k, "i", i, "j", j, "t", t, ".rdata"))
    
    # filter scenarios with low number of occurrences, abundance or richness
    if(sum(dat_spe > 0) < 200 | sum(dat_spe) < 1000 | ncol(dat_spe) < 3) next
    
    spe <- dat_spe
    env <- data.frame(env = dat_env[rownames(spe), 1])
    trait <- data.frame(z = dat_trait[colnames(dat_spe), 1])
    dist.mat <- dist.mat[rownames(spe), rownames(spe)]
    
    # DNCI
    res.DNCI <- DNCIndex_multiGroup(spe = spe, Nperm = 99, dataType = "count")
    res$DNCI[SaveDatInd] <- res.DNCI
    print(paste0("DNCI did"))
    
    
    SaveDatInd <- SaveDatInd + 1
    print(date())
}
  
colnames(res[1:5]) <- c("rep", "t", "k", "i", "j")
  
setwd("C:\\Users\\andy\\Downloads\\analysis\\res")
save(res, file = paste0("res_multi_1.rdata"))  
  






# load data
setwd("C:\\Users\\andy\\Downloads\\analysis\\res\\raw")
dat <- data.frame()
for (i in 1:6){
  load(paste0("res_", i, ".rdata"))
  dat <- rbind(dat, res)
  for (j in 1:4){
    load(paste0("res_", i, "_1_", j,".rdata"))
    dat <- rbind(dat, res)
  }
}


# VP
ind_VP <- !do.call(rbind, lapply(dat$VP, function(x) is.null(x)))
dat_VP <- dat[ind_VP, 1:5]
dat_VP <- cbind(dat_VP, do.call(rbind, lapply(dat$VP[ind_VP], function(x) x$part$indfract$Adj.R.squared)))
colnames(dat_VP)[6:9] <- c("Env", "Env and Spatial", "Spatial", "Resid")

setwd("C:\\Users\\andy\\Downloads\\analysis\\res\\VP_4")
save(dat_VP, file = "res_VP.rdata")

# DNCI
# remove NA
ind_DNCI <- !do.call(rbind, lapply(dat$DNCI, function(x) is.null(x)))
dat_DNCI <- dat[ind_DNCI,1:5]
dat_DNCI <- cbind(dat_DNCI, do.call(rbind, lapply(dat$DNCI[ind_DNCI], function(x) {
  if (identical(x, "too sparse")) return("too sparse")
  else return(x[4:5])
})))
colnames(dat_DNCI)[6:7] <- c("DNCI","CI.DNCI")

setwd("C:\\Users\\andy\\Downloads\\analysis\\res\\DNCI_4")
save(dat_DNCI, file = "res_DNCI.rdata")


# load data
setwd("C:\\Users\\andy\\Downloads\\analysis\\res\\raw")
dat <- data.frame()
for (i in 1:6){
  res <- read.csv(paste0("outputfile_rep", i, ".csv"))
  dat <- rbind(dat, res)
  for (j in 1:4){
    res <- read.csv(paste0("outputfile_rep", i, "_1_", j,".csv"))
    dat <- rbind(dat, res)
  }
}

dat_Stegen <- dat[rowSums(dat[6:9]) != 0, ]
colnames(dat_Stegen)[5] <- "t" 

setwd("C:\\Users\\andy\\Downloads\\analysis\\res\\Stegen_4")
save(dat_Stegen, file = "res_Stegen.rdata")
