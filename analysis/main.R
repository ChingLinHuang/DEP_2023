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

parameter_space <- cross_df(list(m = c(1,2,4,5,7,8), l = c(1,4), j = c(1,2,3,6,7,8), i = c(1,3,5), k = 4:5, env = 2, rep = 2:10))

res <- list(parameter = data.frame(rep = 0, env = 0, k = 0, i = 0,j = 0,l = 0,m = 0))

iinndd <- 1
for(rep in 2:10){
  for(env in 2){
    for (k in 4:5){
      for (i in c(1,3,5)){
        for (j in c(1,2,3,6,7,8)){
          for (l in c(1,4)){
            for (m in c(1,2,4,5,7,8)){
              tryCatch({
              print(iinndd)
              print(paste0("rep = ", rep, ", env = ", env, ", k = ", k, ", i = ", i, ", j = ", j, ", l = ", l, ", m = ", m))
              res$parameter[iinndd, ] <- c(rep, env, k, i, j, l, m)
              
              ## load data
              # species composition # dat_spe
              load(paste0(".\\spe\\spe_rep", rep, "env", env, "k", k, "i", i, "j", j, "l", l, "m", m, ".rdata"))
              # environment # dat_env
              load(paste0(".\\env\\env_rep", rep, "env", env, "k", k, "i", i, "j", j, "l", l, "m", m, ".rdata"))
              # trait # dat_trait
              load(paste0(".\\trait\\trait_rep", rep, "env", env, "k", k, "i", i, "j", j, "l", l, "m", m, ".rdata"))
              # distance matrix # dist.mat
              load(paste0(".\\dist\\dist_rep", rep, "env", env, "k", k, "i", i, "j", j, "l", l, "m", m, ".rdata"))
              
              # filter scenarios with low number of occurrences, abundance or richness
              if(sum(dat_spe > 0) < 200 | sum(dat_spe) < 1000 | ncol(dat_spe) < 3) break
              
              
              # variation partitioning
              list.VP1 <- VP(spe = dat_spe, env = dat_env, dist.mat = dist.mat)
              res$VP[iinndd] <- list.VP1
              print(paste0("rep = ", rep, ", VP did"))
              
              
              # DNCI
              list.DNCI <- DNCI(spe = dat_spe)
              res$DNCI[iinndd] <- list.DNCI
              print(paste0("rep = ", rep, ", DNCI did"))
              
              
              iinndd <- iinndd + 1
              }, error=function(e){print(e)})
            }
          }
        }
      }
    }
  }
}

colnames(res[1:7]) <- c("rep", "env", "k", "i", "j", "l", "m")
save(res, file = "res.rdata")



