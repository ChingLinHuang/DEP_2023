library(adespatial)
library(tidyverse)
library(som.nn)
library(vegan)
library(purrr)

source("C:\\Users\\andy\\Downloads\\analysis\\variation partitioning\\code\\functions_prepare_VP.R")
source("C:\\Users\\andy\\Downloads\\analysis\\DNCI\\code\\functions_prepare_DNCI.R")

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

parameter_space <- cross_df(list(j = 1:13, i = 1:15, k = 1:4, env = 2, rep = 1:30))

res <- data.frame(rep = 0, env = 0, k = 0, i = 0, j = 0)

iinndd <- 1
for(ind in 1:4000){
  tryCatch({
    rep <- parameter_space$rep[ind]
    env <- parameter_space$env[ind]
    k <- parameter_space$k[ind]
    i <- parameter_space$i[ind]
    j <- parameter_space$j[ind]
    
    print(paste0("ind = ", ind, ", iinndd = ", iinndd))
    print(paste0("rep = ", rep, ", env = ", env, ", k = ", k, ", i = ", i, ", j = ", j))
    res[iinndd, 1:5] <- c(rep, env, k, i, j)
    
    setwd("C:\\Users\\andy\\Downloads\\analysis\\data")
    ## load data
    # species composition # dat_spe
    load(paste0(".\\spe_3\\spe_rep", rep, "env", env, "k", k, "i", i, "j", j
                , ".rdata"))
    # environment # dat_env
    load(paste0(".\\env_3\\env_rep", rep, "env", env, "k", k, "i", i, "j", j
                , ".rdata"))
    # trait # dat_trait
    load(paste0(".\\trait_3\\trait_rep", rep, "env", env, "k", k, "i", i, "j", j, ".rdata"))
    # distance matrix # dist.mat
    load(paste0(".\\dist_3\\dist_rep", rep, "env", env, "k", k, "i", i, "j", j, ".rdata"))
    
    # filter scenarios with low number of occurrences, abundance or richness
    if(sum(dat_spe > 0) < 200 | sum(dat_spe) < 1000 | ncol(dat_spe) < 3) next
    
    
    # variation partitioning
    res.VP1 <- VP(spe = dat_spe, env = dat_env, dist.mat = dist.mat)
    res$VP[iinndd] <- res.VP1
    print(paste0("rep = ", rep, ", VP did"))
    
    
    # DNCI
    res.DNCI <- DNCI(spe = dat_spe)
    res$DNCI[iinndd] <- res.DNCI
    print(paste0("rep = ", rep, ", DNCI did"))
    
    
    iinndd <- iinndd + 1
  }, error=function(e){print(e)})
}


colnames(res[1:5]) <- c("rep", "env", "k", "i", "j")

setwd("C:\\Users\\andy\\Downloads\\analysis\\data")
# save(res, file = "res_1.rdata")


# load data
setwd("C:\\Users\\andy\\Downloads\\analysis\\data")
dat <- data.frame()
for (i in 2:12){
  load(paste0("res_", i, ".rdata"))
  dat <- rbind(dat, res)
}

# VP
ind_VP <- !do.call(rbind, lapply(dat$VP, function(x) is.null(x)))
dat_VP <- dat[ind_VP, 1:5]
dat_VP <- cbind(dat_VP, do.call(rbind, lapply(dat$VP[ind_VP], function(x) x$part$indfract$Adj.R.squared)))
colnames(dat_VP)[6:9] <- c("Env", "Env and Spatial", "Spatial", "Resid")

setwd("C:\\Users\\andy\\Downloads\\analysis\\data")
save(dat_VP, file = "res_VP.rdata")

# DNCI
# remove NA
ind_DNCI <- !do.call(rbind, lapply(dat$DNCI, function(x) is.null(x)))
dat_DNCI <- dat[ind_DNCI,1:5]
dat_DNCI <- cbind(dat_DNCI, do.call(rbind, lapply(dat$DNCI[ind_DNCI], function(x) x[4:5]))) %>%
  drop_na()
colnames(dat_DNCI)[6:7] <- c("DNCI","CI.DNCI")

setwd("C:\\Users\\andy\\Downloads\\analysis\\data")
save(dat_DNCI, file = "res_DNCI.rdata")
