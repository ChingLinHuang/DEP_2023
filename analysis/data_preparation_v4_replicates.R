
library(tidyverse)
library(som.nn)
# Load data
setwd("C:\\Users\\andy\\Downloads\\plthompson-Meta_com_framework-36ca456_ver1\\outputs\\replicates")


for(rep in 1:30){
  print(paste0("rep = ", rep, " ", date()))
  for (k in 1:5){
    for (i in 1:15){
      for (j in 1:13){
          t = 61
          
          # read raw data
          dat <- read.csv(paste0("outputfile_rep", rep, "k", k, "i", i, "j", j,".csv")) %>% filter(Time == t)
          
          # empty species composition
          empty <- data.frame(N = rep(0, 50*100), Species = rep(1:50, each = 100), Patch = rep(1:100, times = 50))
          
          # species composition
          dat_spe <- dat %>% 
            select(N, Species, Patch) %>% 
            rbind(empty) %>%
            group_by(Species, Patch) %>%
            summarise(N = sum(N)) %>%
            spread(key = Species, value = N, fill = 0) %>%
            select(-Patch)
          
          save(dat_spe, file = paste0("C:\\Users\\andy\\Downloads\\analysis\\data\\replicates\\", "spe_rep", rep, "k", k, "i", i, "j", j,".rdata"))
        
      }
    }
  }
}
