library(dplyr)
library(tidyr)
library(som.nn)

### Load simulated data
setwd("C:\\Users\\andy\\Downloads\\plthompson-Meta_com_framework-36ca456_ver1\\outputs\\outputfile_v4")

### Transform big matrix data to species composition, distance matrix, environmental data and traits data
for(rep in 1:18){
  print(paste0("rep = ", rep, " ", date()))
  landscape <- read.csv(paste0("C:\\Users\\andy\\Downloads\\plthompson-Meta_com_framework-36ca456_ver1\\data\\landscape_data\\landscape_", rep,".csv"), row.names = 1)
  for (k in 1:5){
    for (i in 1:15){
      for (j in 1:13){
        for (t in 1:60){
          
          # read raw data
          dat <- read.csv(paste0("outputfile_rep", rep, "k", k, "i", i, "j", j,".csv")) %>% filter(Time == t)
          
          PATCH <- sort(unique(dat$Patch))
          #SD <- seq(from = 0.02, to = 0.4, by = 30)
          
          # environmental data
          dat_env <- dat %>%
            select(Patch, env) %>%
            unique() %>%
            arrange(Patch) %>%
            select(-Patch)
          
          save(dat_env, file = paste0("C:\\Users\\andy\\Downloads\\analysis\\data\\env_4_1\\", "env_rep", rep, "k", k, "i", i, "j", j, "t", t,".rdata"))
          #write.csv(dat_env, file = paste0("C:\\Users\\andy\\Downloads\\analysis\\data\\env_4_1\\", "env_rep", rep, "k", k, "i", i, "j", j, "t", t,".csv"))
          
          # trait data
          dat_trait <- dat %>%
            select(Species, z) %>%
            unique() %>%
            arrange(Species) %>%
            select(-Species)
          
          save(dat_trait, file = paste0("C:\\Users\\andy\\Downloads\\analysis\\data\\trait_4_1\\", "trait_rep", rep, "k", k, "i", i, "j", j, "t", t,".rdata"))
          write.csv(dat_trait, file = paste0("C:\\Users\\andy\\Downloads\\analysis\\data\\trait_4_1\\", "trait_rep", rep, "k", k, "i", i, "j", j, "t", t,".csv"))
          
          # distance matrix
          dist.mat <- as.matrix(dist.torus(coors = landscape[PATCH, ]))
          save(dist.mat, file = paste0("C:\\Users\\andy\\Downloads\\analysis\\data\\dist_4_1\\", "dist_rep", rep, "k", k, "i", i, "j", j, "t", t,".rdata"))
          #write.csv(dist.mat, file = paste0("C:\\Users\\andy\\Downloads\\analysis\\data\\dist_4_1\\", "dist_rep", rep, "k", k, "i", i, "j", j, "t", t,".csv"))
          
          # species composition
          dat_spe <- dat %>% 
            select(N, Species, Patch) %>% 
            spread(key = Species, value = N, fill = 0) %>%
            select(-Patch)
          
          save(dat_spe, file = paste0("C:\\Users\\andy\\Downloads\\analysis\\data\\spe_4_1\\", "spe_rep", rep, "k", k, "i", i, "j", j, "t", t,".rdata"))
          write.csv(dat_spe, file = paste0("C:\\Users\\andy\\Downloads\\analysis\\data\\spe_4_1\\", "spe_rep", rep, "k", k, "i", i, "j", j, "t", t,".csv"))
        }
      }
    }
  }
}

