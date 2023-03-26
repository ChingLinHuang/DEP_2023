# Plotting
#
#
library(ggplot2)
library(gg.layers)
library(dplyr)
library(tidyr)

# load data
setwd("C:\\Users\\andy\\Downloads\\analysis\\data")
dat <- data.frame()
for (i in 1:6){
  load(paste0("res_", i, ".rdata"))
  dat <- rbind(dat, res)
}




### VP ###
ind_VP <- !do.call(rbind, lapply(dat$VP, function(x) is.null(x)))
dat_VP <- dat[ind_VP, 1:7]
dat_VP <- cbind(dat_VP, do.call(rbind, lapply(dat$VP[ind_VP], function(x) x$part$indfract$Adj.R.squared)))
colnames(dat_VP)[8:11] <- c("Env", "Env and Spatial", "Spatial", "Resid")
  
dat_VP_long <- dat_VP %>%
  gather(key = "fract", value = "prop", Env:Resid)


# set k = 4 (equal), env = 2 (fixed), l = 1 (low growth rate)
dat_VP_1 <- dat_VP_long %>% 
  filter(k == 4 & env == 2 & l == 1) %>%
  filter(fract != "Resid") %>%
  arrange(fract) %>%
  select(-rep) %>%
  group_by(env, k, i, j, l, m, fract) %>%
  summarize_all(mean)

ggplot(data = dat_VP_1, aes(x = m, y = prop, fill = fract)) +
  geom_bar(stat="identity") +
  facet_grid(j ~ i) +
  labs(title = "VP     equal competition + fixed environment + low growth rate",
       x = "strength of dispersal limitation") +
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Niche width", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Expected number of emigrants", breaks = NULL, labels = NULL))

# set k = 5 (equal), env = 2 (fixed), l = 1 (low growth rate)
dat_VP_2 <- dat_VP_long %>% 
  filter(k == 5 & env == 2 & l == 1) %>%
  filter(fract != "Resid") %>%
  arrange(fract) %>%
  select(-rep) %>%
  group_by(env, k, i, j, l, m, fract) %>%
  summarize_all(mean)

ggplot(data = dat_VP_2, aes(x = m, y = prop, fill = fract)) +
  geom_bar(stat="identity") +
  facet_grid(j ~ i) +
  labs(title = "CC trade-off + fixed environment + low growth rate",
       x = "strength of dispersal limitation") +
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Niche width", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Expected number of emigrants", breaks = NULL, labels = NULL)) 


### DNCI ###
# remove NA
ind_DNCI <- !do.call(rbind, lapply(dat$DNCI, function(x) is.null(x)))
dat_DNCI <- dat[ind_DNCI,1:7]
dat_DNCI <- cbind(dat_DNCI, do.call(rbind, lapply(dat$DNCI[ind_DNCI], function(x) x$DNCI)))
colnames(dat_DNCI)[8] <- "DNCI"

# set k = 4 (equal), env = 2 (fixed), l = 1 (low growth rate)
dat_DNCI_1 <- dat_DNCI %>%
  filter(k == 4 & env == 2 & l == 1) %>%
  select(-rep) %>%
  filter(j %in% c(3,5,7)) %>%
  filter(i %in% c(1,3,5))

# remove outliers
ggplot(data = dat_DNCI_1, aes(x = m, y = DNCI, group = m, fill = as.factor(m))) +
  geom_boxplot2() +
  theme(legend.position="none") + 
  facet_grid(j ~ i, scales = "free") +
  labs(title = "DNCI     equal competition + fixed environment + low growth rate",
       x = "strength of dispersal limitation") +
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Niche width", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Expected number of emigrants", breaks = NULL, labels = NULL))



