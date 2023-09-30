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
load(".\\VP\\res_VP.rdata")
load(".\\DNCI\\res_DNCI.rdata")
load(".\\Stegen\\res_Stegen.rdata")

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

## filter not "too sparse" and "no turnover" in DNCI
sum(dat_raw$DNCI == "too sparse") # 8874
sum(dat_raw$DNCI == "no turnover") # 440
sum(dat_raw$DNCI == "NaN") # 9119
dat <- dat_raw %>% filter(DNCI != "too sparse" & 
                          DNCI != "no turnover" & 
                          DNCI != "NaN")
dat %>% select(rep, k, i, j) %>% unique() %>% nrow() # remained sample = 6486
dat %>% select(k, i, j) %>% unique() %>% nrow() # remained scenarios = 429
dat <- dat %>% 
  mutate(DNCI = as.numeric(DNCI), CI.DNCI = as.numeric(CI.DNCI))
colnames(dat)[colnames(dat) == "Env and Spatial"] <- "EnvSpatial"


## filter complete statistics across time 41 to 60
setwd("C:\\Users\\andy\\Downloads\\analysis\\res\\RF")
load("dat.rdata")
nrow(dat) # remained 5150 samples
dat %>% select(k, i, j) %>% unique() %>% nrow() # remained 349 scenarios
dat %>% select(k) %>% table() # remained scenarios/replicates in each competition types 

## range of the statistics
summary_statistics <- dat %>% 
  left_join(dat_raw) %>% 
  select(Selection, DispLimit, HomoDisp, Drift, Env, EnvSpatial = `Env and Spatial`, Spatial, Resid, DNCI, CI.DNCI) %>% 
  mutate(DNCI = as.numeric(DNCI), CI.DNCI = as.numeric(CI.DNCI)) %>%
  mutate(sd.DNCI = CI.DNCI/2) %>%
  select(-CI.DNCI) %>%
  apply(2, function(x) c(range(x), mean(x), sd(x)))
row.names(summary_statistics) <- c("Min", "Max", "Mean", "Sd")
#save(summary_statistics, file = "C:\\Users\\andy\\Downloads\\analysis\\tables\\summary_statistics.rdata")


## range of the statistics -- plotting
summary_statistics <- dat %>% 
  left_join(dat_raw) %>% 
  select(Selection, DispLimit, HomoDisp, Drift, Env, EnvSpatial = `Env and Spatial`, Spatial, Resid, DNCI, CI.DNCI) %>% 
  mutate(DNCI = as.numeric(DNCI), CI.DNCI = as.numeric(CI.DNCI)) %>%
  mutate(sd.DNCI = CI.DNCI/2) %>%
  select(-CI.DNCI)

load("C:\\Users\\andy\\Downloads\\analysis\\empirical\\Fushan\\res\\res.rdata")
res <- res[ ,-1]
res[ ,10] <- res[ ,10]/2
colnames(res)[10] <- "sd.DNCI"

NAME <- colnames(res)

windows()
par(mfrow = c(3,4))
for(i in 1:10){
  hist(summary_statistics[ ,i], ann = F, cex.axis = 1.5)
  abline(v = res[1:4,i], lty = 2)
  title(title = NULL, xlab = NAME[i], cex.lab = 2)
}
plot(1:10, ann = F, axe = F, type = "n")
plot(1:10, ann = F, axe = F, type = "n")
# save as "distribution_statistics.pdf"
## 

#####

# 4. Construct 12 random forest (combination of 3 time step set (t = 60, t = 60, 56, 52, 48 or t = 60~41) and 4 statistics set (only VP, only DNCI, only Stegen or all statistics)) and calculate the accuracy and performance and save in ".\analysis\res\RF"

# 5. Choose RF_t4_all and test its robustness on time steps choice
# 
# 6. Choose RF_t4_all and test its robustness on sampling effect
#


