library(adespatial)
library(tidyverse)
library(som.nn)
library(vegan)
library(purrr)

source("C:\\Users\\andy\\Downloads\\analysis\\variation partitioning\\code\\functions_prepare_VP.R")
source("C:\\Users\\andy\\Downloads\\analysis\\DNCI\\code\\functions_prepare_DNCI.R")

# ## Check number of site in each sample
# #####
# check_list <- cross_df(list(rep = 13:18, archetype = c("SS", "ND", "ME", "PD"), t = c(60, 56, 52, 48))) %>% as.data.frame()
# 
# for(ind in 1:nrow(check_list)){
# 
#   rep <- check_list$rep[ind]
#   t <- check_list$t[ind]
#   archetype <- check_list$archetype[ind]
# 
#   if(archetype == "SS"){
#     k <- 2; i <- 8; j <- 6
#   } else if(archetype == "ND"){
#     k <- 4; i <- 9; j <- 13
#   } else if(archetype == "ME"){
#     k <- 2; i <- 15; j <- 4
#   } else{
#     k <- 5; i <- 13; j <- 6
#   }
# 
#   setwd("C:\\Users\\andy\\Downloads\\analysis\\data")
#   ## load data
#   # species composition # dat_spe
#   load(paste0(".\\spe_4_3\\spe_rep", rep, "k", k, "i", i, "j", j, "t", t
#               , ".rdata"))
#   check_list$n_site[ind] <- nrow(dat_spe)
# 
# }
# # most of the sample has 100 sites, only 8 samples has 99.


#####


### Create random sampling
#####
robust_list <- cross_df(list(n_sample = 1:15, effort = 9:1, rep = 13:18, archetype = c("SS", "ND", "ME", "PD"))) %>% as.data.frame()

for(ind in 1:nrow(robust_list)){
  n_sample <- robust_list$n_sample[ind]
  effort  <- robust_list$effort [ind]
  rep <- robust_list$rep[ind]
  t <- robust_list$t[ind]
  archetype <- robust_list$archetype[ind]
  
  site_sample <- sample(1:99, size = 10*effort)
  
  
  if(archetype == "SS"){
    k <- 2; i <- 8; j <- 6
  } else if(archetype == "ND"){
    k <- 4; i <- 9; j <- 13
  } else if(archetype == "ME"){
    k <- 2; i <- 15; j <- 4
  } else{
    k <- 5; i <- 13; j <- 6
  }
  
  setwd("C:\\Users\\andy\\Downloads\\analysis\\data")
  for(t in c(60, 56, 52, 48)){
    ## load data
    # species composition # dat_spe
    load(paste0(".\\spe_4_3\\spe_rep", rep, "k", k, "i", i, "j", j, "t", t
                , ".rdata"))
    # environment # dat_env
    load(paste0(".\\env_4_3\\env_rep", rep, "k", k, "i", i, "j", j, "t", t
                , ".rdata"))
    # trait # dat_trait
    load(paste0(".\\trait_4_3\\trait_rep", rep, "k", k, "i", i, "j", j, "t", t, ".rdata"))
    # distance matrix # dist.mat
    load(paste0(".\\dist_4_3\\dist_rep", rep, "k", k, "i", i, "j", j, "t", t, ".rdata"))
    
    dat_spe <- dat_spe[site_sample, ]
    ind_sp <- colSums(dat_spe) > 0 
    dat_spe <- dat_spe[ ,ind_sp]
    
    dat_env <- data.frame(env = dat_env[site_sample, ])
    dat_trait <- data.frame(z = dat_trait[ind_sp, ])
    dist.mat <- dist.mat[site_sample, site_sample]
    
    # rdata
    save(dat_spe, file = paste0(".\\spe_4_3_robust\\rep", rep, "_", archetype, "_", "effort", effort, "n", n_sample, "t", t, ".rdata"))
    save(dat_env, file = paste0(".\\env_4_3_robust\\rep", rep, "_", archetype, "_", "effort", effort, "n", n_sample, "t", t, ".rdata"))
    save(dat_trait, file = paste0(".\\trait_4_3_robust\\rep", rep, "_", archetype, "_", "effort", effort, "n", n_sample, "t", t, ".rdata"))
    save(dist.mat, file = paste0(".\\dist_4_3_robust\\rep", rep, "_", archetype, "_", "effort", effort, "n", n_sample, "t", t, ".rdata"))
    
    # csv
    write.csv(dat_spe, file = paste0(".\\spe_4_3_robust\\rep", rep, "_", archetype, "_", "effort", effort, "n", n_sample, "t", t, ".csv"))
    write.csv(dat_env, file = paste0(".\\env_4_3_robust\\rep", rep, "_", archetype, "_", "effort", effort, "n", n_sample, "t", t, ".csv"))
    write.csv(dat_trait, file = paste0(".\\trait_4_3_robust\\rep", rep, "_", archetype, "_", "effort", effort, "n", n_sample, "t", t, ".csv"))
    write.csv(dist.mat, file = paste0(".\\dist_4_3_robust\\rep", rep, "_", archetype, "_", "effort", effort, "n", n_sample, "t", t, ".csv"))
  }
}

#####



### Calculate VP and DNCI
######
setwd("C:\\Users\\andy\\Downloads\\analysis\\data")
parameter_space <- cross_df(list(n_sample = 1:15, effort = 9:1, rep = 13:18, archetype = c("SS", "ND", "ME", "PD"), t = c(60, 56, 52, 48)))%>% as.data.frame

res <- data.frame(rep = 0, t = 0, archetype = 0, effort = 0, n_sample = 0)

SaveDatInd <- 1
for(ind in 1:12960){
  rep <- parameter_space[ind, "rep"]
  t <- parameter_space[ind, "t"]
  archetype <- parameter_space[ind, "archetype"]
  effort <- parameter_space[ind, "effort"]
  n_sample <- parameter_space[ind, "n_sample"]
  
  print(paste0("ind = ", ind, ", SaveDatInd = ", SaveDatInd))
  res[SaveDatInd, 1:5] <- c(rep, t, archetype, effort, n_sample)
  
  setwd("C:\\Users\\andy\\Downloads\\analysis\\data")
  ## load data
  # species composition # dat_spe
  load(paste0(".\\spe_4_3_robust\\rep", rep, "_", archetype, "_", "effort", effort, "n", n_sample, "t", t, ".rdata"))
  # environment # dat_env
  load(paste0(".\\env_4_3_robust\\rep", rep, "_", archetype, "_", "effort", effort, "n", n_sample, "t", t, ".rdata"))
  # distance matrix # dist.mat
  load(paste0(".\\dist_4_3_robust\\rep", rep, "_", archetype, "_", "effort", effort, "n", n_sample, "t", t, ".rdata"))
  
  spe <- dat_spe
  env <- dat_env
  
  # variation partitioning
  res.VP1 <- VP(spe = spe, env = env, dist.mat = dist.mat)
  res$VP[SaveDatInd] <- res.VP1
  print(paste0("VP did"))
  
  
  # DNCI
  res.DNCI <- DNCIndex(spe = spe, Nperm = 100, dataType = "prab")
  res$DNCI[SaveDatInd] <- res.DNCI
  print(paste0("DNCI did"))
  
  
  SaveDatInd <- SaveDatInd + 1
  print(date())
}

setwd("C:\\Users\\andy\\Downloads\\analysis\\res")
save(res, file = paste0("res_robust_1.rdata"))

######







# load data
setwd("C:\\Users\\andy\\Downloads\\analysis\\res\\robust")
dat <- data.frame()
for (i in 1:6){
  load(paste0("res_robust_", i, ".rdata"))
  dat <- rbind(dat, res)
}

dat[ ,c("rep", "t", "effort", "n_sample")] <- apply(dat[ ,c("rep", "t", "effort", "n_sample")], 2, function(x) as.numeric(x))

# VP
ind_VP <- !do.call(rbind, lapply(dat$VP, function(x) is.null(x)))
dat_VP <- dat[ind_VP, 1:5]
dat_VP <- cbind(dat_VP, do.call(rbind, lapply(dat$VP[ind_VP], function(x) x$part$indfract$Adj.R.squared)))
colnames(dat_VP)[6:9] <- c("Env", "Env and Spatial", "Spatial", "Resid")

setwd("C:\\Users\\andy\\Downloads\\analysis\\res\\robust")
save(dat_VP, file = "res_robust_VP.rdata")

# DNCI
# remove NA
ind_DNCI <- !do.call(rbind, lapply(dat$DNCI, function(x) is.null(x)))
dat_DNCI <- dat[ind_DNCI,1:5]
dat_DNCI <- cbind(dat_DNCI, do.call(rbind, lapply(dat$DNCI[ind_DNCI], function(x) {
  ifelse(identical(x, "too sparse"), return("too sparse"),
         ifelse(identical(x, "no turnover"), return("no turnover"),
                return(x[4:5])))
})))
colnames(dat_DNCI)[6:7] <- c("DNCI","CI.DNCI")

setwd("C:\\Users\\andy\\Downloads\\analysis\\res\\robust")
save(dat_DNCI, file = "res_robust_DNCI.rdata")


# load data
setwd("C:\\Users\\andy\\Downloads\\analysis\\res\\robust")
dat <- data.frame()
for (i in 1:4){
  res <- read.csv(paste0("outputfile_robust_", i, ".csv"))
  dat <- rbind(dat, res)
}
# save(dat, file = "dat_raw_Stegen.rdata")

dat_Stegen <- dat
setwd("C:\\Users\\andy\\Downloads\\analysis\\res\\robust")
save(dat_Stegen, file = "res_robust_Stegen.rdata")
