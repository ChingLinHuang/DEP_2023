
library(tidyverse)
library(vegan)


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
parameter_space <- cross_df(list(j = 1:13, i = 1:15, k = 1:5)) %>% as.data.frame
res <- data.frame(k = 0, i = 0, j = 0)
for(ind in 1:975){
  print(paste0("ind = ", ind))
  k <- parameter_space[ind, "k"]
  i <- parameter_space[ind, "i"]
  j <- parameter_space[ind, "j"]
  res[ind, 1:3] <- c(k, i, j)
  
  load(paste0(".\\spe_rep", 1, "k", k, "i", i, "j", j, ".rdata"))
  spe <- dat_spe %>% mutate(rep = 1, Patch = 1:100)
  
  for(rep in 2:30){
    load(paste0(".\\spe_rep", rep, "k", k, "i", i, "j", j, ".rdata"))
    spe_1 <- dat_spe %>% mutate(rep = rep, Patch = 1:100)
    spe <- rbind(spe, spe_1)
  }
  
  # Dissiliarity among replicates for each patch
  D_vec <- 0
  D_w <- 0
  for(p in 1:100){
    diss <- spe %>% filter(Patch == p) %>%
      select(-rep, -Patch) %>%
      vegdist %>% mean
    D_vec[p] <- diss
    
    diss_gamma <- spe %>% filter(Patch == p) %>%
      select(-rep, -Patch) %>%
      colSums() 
    diss_gamma <- sum(diss_gamma > 0)
    
    diss_alpha <- spe %>% filter(Patch == p) %>%
      select(-rep, -Patch)
    diss_alpha <- mean(rowSums(diss_alpha > 0))
    
    D_w[p] <- diss_alpha/diss_gamma
      
  }
  res$D_mean[ind] = mean(D_vec)
  res$D_sd[ind] = sd(D_vec)
  res$D_mean_w[ind] = mean(D_w)
  res$D_sd_w[ind] = sd(D_w)
}


# plotting
res %>% 
  filter(!is.na(D_mean_w)) %>%
  filter(k == 1) %>%
  ggplot(aes(x = as.factor(i), y = as.factor(j), fill = D_mean_w)) + 
  geom_tile()
