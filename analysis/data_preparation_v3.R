#
# 20220314
# Extract some scenarios from the raw data for progress report

library(dplyr)
library(tidyr)
library(som.nn)
# Load data
setwd("C:\\Users\\andy\\Downloads\\plthompson-Meta_com_framework-36ca456_ver1\\outputs\\outputfile_v3")

# same for loop with "main.r"
for(rep in 1:6){
  print(paste0("rep = ", rep))
  landscape<- read.csv(paste0("C:\\Users\\andy\\Downloads\\plthompson-Meta_com_framework-36ca456_ver1\\data\\landscape_data\\landscape_", rep,".csv"), row.names = 1)
  for(env in 2){
    for (k in 1:4){
      for (i in 1:15){
        for (j in 1:13){
          #tryCatch({
          # read raw data
          dat <- read.csv(paste0("outputfile_rep", rep, "env", env, "k", k, "i", i, "j", j,".csv")) %>%
            filter(Time == 4) # filter final fate
          
          PATCH <- sort(unique(dat$Patch))
          # species composition
          dat_spe <- dat %>% 
            select(N, Species, Patch) %>% 
            spread(key = Species, value = N, fill = 0) %>%
            select(-Patch)
          save(dat_spe, file = paste0("C:\\Users\\andy\\Downloads\\analysis\\data\\spe_3\\", "spe_rep", rep, "env", env, "k", k, "i", i, "j", j, ".rdata"))
          
          # environmental data
          dat_env <- dat %>%
            select(Patch, env) %>%
            unique() %>%
            arrange(Patch) %>%
            select(-Patch)
          save(dat_env, file = paste0("C:\\Users\\andy\\Downloads\\analysis\\data\\env_3\\", "env_rep", rep, "env", env, "k", k, "i", i, "j", j, ".rdata"))
          
          # trait data
          dat_trait <- dat %>%
            select(Species, z) %>%
            unique() %>%
            arrange(Species) %>%
            select(-Species)
          save(dat_trait, file = paste0("C:\\Users\\andy\\Downloads\\analysis\\data\\trait_3\\", "trait_rep", rep, "env", env, "k", k, "i", i, "j", j, ".rdata"))
          
          # distance matrix
          dist.mat <- as.matrix(dist.torus(coors = landscape[PATCH, ]))
          save(dist.mat, file = paste0("C:\\Users\\andy\\Downloads\\analysis\\data\\dist_3\\", "dist_rep", rep, "env", env, "k", k, "i", i, "j", j, ".rdata"))
          
          #}, error=function(e){})  
        }
      }
    }
  }
}

