# Robustness testing -- sampling effect
# 
library(tidyverse)
library(ordinalForest)
library(randomForest)
library(purrr)

##############
##############  Sampling effort robustness
##############
##########

### load data
#####
setwd("C:\\Users\\andy\\Downloads\\analysis\\res")
load(".\\robust\\res_robust_VP.rdata")
load(".\\robust\\res_robust_DNCI.rdata")
load(".\\robust\\res_robust_Stegen.rdata")

dat_raw <- dat_VP %>% 
  full_join(dat_Stegen) %>% 
  full_join(dat_DNCI) 
sum(dat_raw$DNCI == "too sparse") # 43
sum(dat_raw$DNCI == "no turnover") # 0
sum(dat_raw$DNCI == "NaN") # 0
dat_raw <- dat_raw %>% 
  filter(DNCI != "too sparse") %>%
  mutate(DNCI = as.numeric(DNCI), CI.DNCI = as.numeric(CI.DNCI))
colnames(dat_raw)[colnames(dat_raw) == "Env and Spatial"] <- "EnvSpatial"
rm(dat_VP, dat_DNCI, dat_Stegen)


statistics_name <- c("Selection", "DispLimit", "HomoDisp", "Drift", "Env", "EnvSpatial", "Spatial", "Resid", "DNCI", "CI.DNCI")

for(ind in 1:length(statistics_name)){
  dat_spread <- dat_raw %>%
    select(rep, t, archetype ,effort, n_sample, statistics_name[ind]) %>%
    spread(key = t, value = statistics_name[ind])
  colnames(dat_spread)[5:8] <- paste0(statistics_name[ind], colnames(dat_spread)[5:8])
  
  if (ind == 1){
    dat <- dat_spread
  }else{
    dat <- dat %>% left_join(dat_spread)
  }
}
nrow(dat) # 3240
dat <- dat[complete.cases(dat), ]
nrow(dat) # 3212

Arch_parm <- data.frame(archetype = c("SS", "ND", "ME", "PD"),
                        k = c(2, 1, 2, 5), 
                        i = c(8, 8, 15, 9),
                        j = c(6, 13, 4, 8))
dat <- dat %>% left_join(Arch_parm)

save(dat, file = ".\\RF\\dat_robust.rdata")
#rm(dat)

#####


### Robustness to sampling effort
setwd("C:\\Users\\andy\\Downloads\\analysis\\res")
load(".\\RF\\RF_t4_all.rdata")
load(".\\RF\\dat_robust.rdata")

# prediction
dat_pred <- dat %>% 
  select(-rep, -archetype) %>%
  mutate(i_pred = predict(RF_t4_all$model$i, newdata = .)$ypred) %>%
  mutate(j_pred = predict(RF_t4_all$model$j, newdata = .)$ypred) %>%
  mutate(k_pred = predict(RF_t4_all$model$k, newdata = .)) %>%
  select(effort, k ,i, j, k_pred, i_pred, j_pred)

table(dat_pred$effort)

prediction_i <- table(data.frame(true_values = dat_pred$i, 
                                 predictions = dat_pred$i_pred))
prediction_j <- table(data.frame(true_values = dat_pred$j, 
                                 predictions = dat_pred$j_pred))
prediction_k <- table(data.frame(true_values = dat_pred$k, 
                                 predictions = dat_pred$k_pred))

Accuracy <- dat_pred %>% 
  mutate(i_accu = (i_pred == i), 
         j_accu = (j_pred == j), 
         k_accu = (k_pred == k)) %>%
  group_by(effort) %>%
  summarise(i_accu = mean(i_accu),
            j_accu = mean(j_accu),
            k_accu = mean(k_accu),
            i_accu = mean(i_accu),
            j_accu = mean(j_accu),
            k_accu = mean(k_accu))

save(dat_pred, Accuracy, file = ".//robust//robustness_sampling_effort.rdata")


##### Plotting
setwd("C:\\Users\\andy\\Downloads\\analysis\\")
load(".\\res\\robust\\robustness_sampling_effort.rdata")
Accuracy$effort <- Accuracy$effort * 10
colnames(Accuracy)[2:4] <- c("Dispersal ability", "Niche width", "Competition")
Accuracy <- Accuracy[ ,c(1,3,4,2)]
Accuracy_1 <- Accuracy %>% 
  gather(key = "Parameter", value = "Accuracy", -effort)
Accuracy_1$Parameter <- factor(Accuracy_1$Parameter, c("Niche width", "Competition", "Dispersal ability"))
Accuracy_1 %>%
  ggplot(aes(x = effort, y = Accuracy, color = Parameter)) +
  geom_line(aes(linetype = Parameter), size = 1.2) +
  theme(legend.position = c(.95, .4),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.key.size = unit(1, 'cm')) +
  xlab("Subsampling size")
ggsave(file = ".\\figures\\Robustness_sampling_effort_Accuracy.pdf", width = 15, height = 15, units = "cm")
ggsave(file = ".\\figures\\Robustness_sampling_effort_Accuracy.jpg", width = 15, height = 15, units = "cm")
