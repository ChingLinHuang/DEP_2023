library(tidyverse)

### Analysis procedure
#
# 1. Simulate data with multiple scenarios (5 competition types, 15 dispersal ability and 13 niche width) and store in ".\plthompson-Meta_com_framework-36ca456_ver1\outputs\outputfile_v4"
#
# 2. Transform the raw data into species composition, trait, environment and distance matrix to the folder ".\analysis\data"
#
# 3. Calculate VP, DNCI and Stegen statistics and filter data for random forest
#####
setwd("C:\\Users\\andy\\Downloads\\analysis\\res")
load(".\\VP_4\\res_VP.rdata")
load(".\\DNCI_4\\res_DNCI.rdata")
load(".\\Stegen_4\\res_Stegen.rdata")

dat_raw <- dat_VP %>% 
  full_join(dat_Stegen) %>% 
  full_join(dat_DNCI) 

# sample: rep (1~18), k (1~5), i (1~15), j (1~13), t (41~60)
18*5*15*13  # Total sample = 17500

# scenario: k (1~5), i (1~15), j (1~13)
5*15*13  # Total scenario = 975

## filtered not (sum(dat_spe > 0) < 200 and sum(dat_spe) < 1000 and ncol(dat_spe) < 3) 
dat_raw %>% select(rep, k, i, j) %>% unique() %>% nrow() # Remained sample = 6966
dat_raw %>% select(k, i, j) %>% unique() %>% nrow() # Remained scenario = 453

## filter not "too sparse" in DNCI
dat <- dat_raw %>% filter(DNCI != "too sparse")
dat %>% select(rep, k, i, j) %>% unique() %>% nrow() # remained sample = 6492
dat %>% select(k, i, j) %>% unique() %>% nrow() # remained scenarios 430
dat <- dat %>% 
  mutate(DNCI = as.numeric(DNCI), CI.DNCI = as.numeric(CI.DNCI)) %>% 
  mutate(DNCI_N = (DNCI - CI.DNCI > 0), DNCI_D = (DNCI + CI.DNCI < 0))


## filter complete statistics across time 41 to 60
setwd("C:\\Users\\andy\\Downloads\\analysis\\res\\RF")
load("dat.rdata")
nrow(dat) # remained 5983 samples
dat %>% select(k, i, j) %>% unique() %>% nrow() # remained 399 scenarios

## range of the statistics
summary_statistics <- dat %>% 
  left_join(dat_raw) %>% 
  select(Selection, DispLimit, HomoDisp, Drift, Env, `Env and Spatial`, Spatial, DNCI, CI.DNCI) %>% 
  mutate(DNCI = as.numeric(DNCI), CI.DNCI = as.numeric(CI.DNCI)) %>%
  apply(2, function(x) c(range(x), mean(x), sd(x)))
row.names(summary_statistics) <- c("Min", "Max", "Mean", "Sd")

## 

#####

# 4. Construct 12 random forest (combination of 3 time step set (t = 60, t = 60, 56, 52, 48 or t = 60~41) and 4 statistics set (only VP, only DNCI, only Stegen or all statistics)) and calculate the accuracy and performance and save in ".\analysis\res\RF"

# 5. Choose RF_t4_all and test its robustness on time steps choice
# 
# 6. Choose RF_t4_all and test its robustness on sampling effect
#

# Discuss statistics
# Stegen: Dispersal limitation
test <- dat_res %>% 
  filter(t == 60, Stegen == "DispLimit") %>%
  select(k, i, j, Selection, DispLimit, HomoDisp, Drift)

