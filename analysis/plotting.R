# Plotting
#
#
library(ggplot2)
library(dplyr)

# load data
load(file = paste0("C:\\Users\\andy\\Downloads\\analysis\\variation partitioning\\data\\dat.Rdata"))


colnames(dat)
summary(dat)
length(unique(dat$Time))

dat_long <- dat %>%
  filter(sig_niche < 2) %>% 
  mutate(sig_niche = round(sig_niche, digits = 3)) %>%
  select(E, E.S, S, dispersal, sig_niche, alpha, Time, rep) %>%
  gather(key = "partition", value = "percent", E:S) %>%
  arrange(dispersal) %>%
  mutate(dispersal = rank(dispersal))

# replace dispersal to factors
for(ii in 1:length(unique(dat_long$dispersal))){ 
  ind <- dat_long$dispersal == unique(dat_long$dispersal)[ii]
  dat_long$dispersal[ind] <- ii
}
dat_long$dispersal <- as.factor(dat_long$dispersal)

#

# dat_long <- dat %>%
#   select(E, E.S, S, resid, dispersal, sig_niche, alpha, Time, rep) %>%
#   gather(key = "partition", value = "percent", E:resid) 

  

setwd("C:\\Users\\andy\\Downloads\\analysis\\variation partitioning\\figures")
# Figure1-1: Fix Time == 7, alpha == "no"
dat1_1 <- dat_long %>% 
  filter(Time == 7) %>% # final state
  filter(alpha == "no")
length(unique(dat1_1$dispersal))# 9
dat1_1 <- dat1_1[dat1_1$dispersal %in% unique(dat1_1$dispersal)[c(1,4,7,9)], ]

ggplot(data = dat1_1, aes(x = as.factor(dispersal), y = percent)) +
  geom_boxplot(aes(fill =  partition)) +
  facet_grid(sig_niche ~ alpha)
ggsave("fig1_1.png", width = 11, height = 5)

ggplot(data = dat1_1, aes(x = as.factor(sig_niche), y = percent)) +
  geom_boxplot(aes(fill =  partition)) +
  facet_grid(dispersal ~ alpha)

# Figure1-2: Fix Time == 7, alpha == "stable"
dat1_2 <- dat_long %>% 
  filter(Time == 7) %>% # final state
  filter(alpha == "stable")
length(unique(dat1_2$dispersal))# 9
dat1_2 <- dat1_2[dat1_2$dispersal %in% unique(dat1_2$dispersal)[c(1,4,7,9)], ]
ggplot(data = dat1_2, aes(x = as.factor(dispersal), y = percent)) +
  geom_boxplot(aes(fill =  partition)) +
  facet_grid(sig_niche ~ alpha)
ggsave("fig1_2.png", width = 11, height = 5)

# Figure1-3: Fix Time == 7, alpha == "mixed"
dat1_3 <- dat_long %>% 
  filter(Time == 7) %>% # final state
  filter(alpha == "mixed")
length(unique(dat1_3$dispersal))# 9
dat1_3 <- dat1_3[dat1_3$dispersal %in% unique(dat1_3$dispersal)[c(1,4,7,9)], ]
ggplot(data = dat1_3, aes(x = as.factor(dispersal), y = percent)) +
  geom_boxplot(aes(fill =  partition)) +
  facet_grid(sig_niche ~ alpha)
ggsave("fig1_3.png", width = 11, height = 5)

# Figure1-4: Fix Time == 7, alpha == "equal"
dat1_4 <- dat_long %>% 
  filter(Time == 7) %>% # final state
  filter(alpha == "equal")
length(unique(dat1_4$dispersal))# 9
dat1_4 <- dat1_4[dat1_4$dispersal %in% unique(dat1_4$dispersal)[c(1,4,7,9)], ]
ggplot(data = dat1_4, aes(x = as.factor(dispersal), y = percent)) +
  geom_boxplot(aes(fill =  partition)) +
  facet_grid(sig_niche ~ alpha)
ggsave("fig1_4.png", width = 11, height = 5)

# Figure1-5: Fix Time == 7, alpha == "patch"
dat1_5 <- dat_long %>% 
  filter(Time == 7) %>% # final state
  filter(alpha == "patch")
length(unique(dat1_5$dispersal))# 9
dat1_5 <- dat1_5[dat1_5$dispersal %in% unique(dat1_5$dispersal)[c(1,4,5,7)], ]
ggplot(data = dat1_5, aes(x = as.factor(dispersal), y = percent)) +
  geom_boxplot(aes(fill =  partition)) +
  facet_grid(sig_niche ~ alpha)
ggsave("fig1_5.png", width = 11, height = 5)

