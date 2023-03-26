# Plotting
# 
library(tidyverse)
library(reshape2)
library(vegan)
library(gganimate)
library(gifski)


##########  
##########  Simulation model
##########  


##### Parametric space
##### 
setwd("C:\\Users\\andy\\Downloads\\analysis\\res")
load(".\\VP\\res_VP.rdata")
load(".\\DNCI\\res_DNCI.rdata")
load(".\\Stegen\\res_Stegen.rdata")

dat <- dat_VP %>% 
  full_join(dat_Stegen) %>% 
  full_join(dat_DNCI) 
sum(dat$DNCI == "too sparse") # 8874
sum(dat$DNCI == "no turnover") # 440
sum(dat$DNCI == "NaN") # 9119
dat <- dat %>% 
  filter(DNCI != "too sparse" & 
           DNCI != "no turnover" & 
           DNCI != "NaN") %>%
  mutate(DNCI = as.numeric(DNCI), CI.DNCI = as.numeric(CI.DNCI)) %>%
  mutate(sd.DNCI = CI.DNCI/2) %>% 
  select(-CI.DNCI)
colnames(dat)[colnames(dat) == "Env and Spatial"] <- "EnvSpatial"

setwd("C:\\Users\\andy\\Downloads\\analysis\\figures")

# parameter space for four archetypes
dat %>% 
  filter(t == 60) %>% 
  select(i, j) %>%
  group_by(i, j) %>%
  summarise(n_rep = n()) %>%
  ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) + 
  geom_tile() + 
  annotate("text", x = 8, y = 4, label= "SS", color = "white") + 
  annotate("text", x = 9, y = 11, label= "ND", color = "white") + 
  annotate("text", x = 15, y = 2, label= "ME", color = "white") + 
  annotate("text", x = 13, y = 4, label= "PD", color = "white") + 
  annotate("text", x = 8, y = 8, label= "FFDP", color = "white", size = 3) + 
  labs(y = "Niche width", x = "Dispersal ability") +
  theme(legend.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))
ggsave("Parameter_space_overlook_archetypes.pdf", width = 15, height = 15, units = "cm")
ggsave("Parameter_space_overlook_archetypes.jpg", width = 15, height = 15, units = "cm")

# parameter space only for empirical 
dat %>% 
  filter(t == 60) %>% 
  select(i, j) %>%
  group_by(i, j) %>%
  summarise(n_rep = n()) %>%
  ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) + 
  geom_tile() + 
  annotate("text", x = 8, y = 8, label= "FFDP", color = "white", size = 3)  + 
  labs(y = "Niche width", x = "Dispersal ability") +
  theme(legend.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))
ggsave("Parameter_space_overlook_empirical.pdf", width = 15, height = 15, units = "cm")
ggsave("Parameter_space_overlook_empirical.jpg", width = 15, height = 15, units = "cm")

# parameter space for no competition
dat %>%
  filter(k == 1, t == 60) %>% 
  select(i, j) %>%
  group_by(i, j) %>%
  summarise(n_rep = n()) %>%
  ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) + 
  geom_tile() + 
  labs(y = "Niche width", x = "Dispersal ability") +
  scale_fill_distiller(palette = "YlGnBu") +
  theme(legend.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))

ggsave("Parameter_space_overlook_no_competition.pdf", width = 15, height = 15, units = "cm")
ggsave("Parameter_space_overlook_no_competition.jpg", width = 15, height = 15, units = "cm")

# parameter space for stable competition
dat %>%
  filter(k == 2, t == 60) %>% 
  select(i, j) %>%
  group_by(i, j) %>%
  summarise(n_rep = n()) %>%
  ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) + 
  geom_tile() + 
  annotate("text", x = 5, y = 4, label= "SS", color = "black") + 
  annotate("text", x = 12, y = 2, label= "ME", color = "black") + 
  annotate("text", x = 5, y = 8, label= "FFDP", color = "black", size = 3) + 
  labs(y = "Niche width", x = "Dispersal ability") +
  scale_fill_distiller(palette = "YlGnBu") +
  theme(legend.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))

ggsave("Parameter_space_overlook_stable_competition.pdf", width = 15, height = 15, units = "cm")
ggsave("Parameter_space_overlook_stable_competition.jpg", width = 15, height = 15, units = "cm")

# parameter space for mixed competition
dat %>%
  filter(k == 3, t == 60) %>% 
  select(i, j) %>%
  group_by(i, j) %>%
  summarise(n_rep = n()) %>%
  ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) + 
  geom_tile() + 
  labs(y = "Niche width", x = "Dispersal ability") +
  scale_fill_distiller(palette = "YlGnBu") +
  theme(legend.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))

ggsave("Parameter_space_overlook_mixed_competition.pdf", width = 15, height = 15, units = "cm")
ggsave("Parameter_space_overlook_mixed_competition.jpg", width = 15, height = 15, units = "cm")

# parameter space for equal competition
dat %>%
  filter(k == 4, t == 60) %>% 
  select(i, j) %>%
  group_by(i, j) %>%
  summarise(n_rep = n()) %>%
  ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) + 
  geom_tile() + 
  annotate("text", x = 1, y = 11, label= "ND", color = "black") + 
  labs(y = "Niche width", x = "Dispersal ability") +
  scale_fill_distiller(palette = "YlGnBu") +
  theme(legend.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))

ggsave("Parameter_space_overlook_equal_competition.pdf", width = 15, height = 15, units = "cm")
ggsave("Parameter_space_overlook_equal_competition.jpg", width = 15, height = 15, units = "cm")

# parameter space for CC
dat %>%
  filter(k == 5, t == 60) %>% 
  select(i, j) %>%
  group_by(i, j) %>%
  summarise(n_rep = n()) %>%
  ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) + 
  geom_tile() + 
  annotate("text", x = 9, y = 4, label= "PD", color = "black")  + 
  labs(y = "Niche width", x = "Dispersal ability") +
  scale_fill_distiller(palette = "YlGnBu") +
  theme(legend.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))

ggsave("Parameter_space_overlook_CC_tradeoff.pdf", width = 15, height = 15, units = "cm")
ggsave("Parameter_space_overlook_CC_tradeoff.jpg", width = 15, height = 15, units = "cm")

#####



##########  
##########  Statistics
##########  

# ### data preparation
# #####
# setwd("C:\\Users\\andy\\Downloads\\analysis\\res")
# load(".\\VP_4\\res_VP.rdata")
# load(".\\DNCI_4\\res_DNCI.rdata")
# load(".\\Stegen_4\\res_Stegen.rdata")
# 
# dat <- dat_VP %>% 
#   left_join(dat_Stegen) %>% 
#   left_join(dat_DNCI)
# sum(dat$DNCI == "too sparse") # 3194
# dat <- dat %>% 
#   filter(DNCI != "too sparse") %>%
#   mutate(DNCI = as.numeric(DNCI), CI.DNCI = as.numeric(CI.DNCI))
# 
# dat_res_Stegen_1 <- dat %>%
#   select(Selection, DispLimit, HomoDisp, Drift) %>%
#   apply(., 1, function (x) sum(x == max(x)))
# table(dat_res_Stegen_1) # only 12 samples has the two same maximum
# 
# dat_1 <- dat[dat_res_Stegen_1 == 1, ]
# 
# dat_res <- dat_1%>%
#   mutate(VP = dat_1 %>% 
#            select(Env, `Env and Spatial`, Spatial) %>%
#            apply(., 1, function (x) colnames(.)[x == max(x)])) %>%
#   mutate(Stegen = dat_1 %>% 
#            select(Selection, DispLimit, HomoDisp, Drift) %>%
#            apply(., 1, function (x) colnames(.)[x == max(x)])) %>%
#   mutate(D = dat_1 %>% 
#            select(DNCI, CI.DNCI) %>%
#            apply(., 1, function (x) {
#              if(x[1] > 0 && x[1] - x[2] > 0) 
#                return("Niche")
#              else if(x[1] < 0 && x[1] + x[2] < 0) 
#                return("Dispersal")
#              else 
#                return("Random")
#            })) 
# table(dat_res$VP)  
# table(dat_res$Stegen)  
# table(dat_res$D)  
# 
# setwd("C:\\Users\\andy\\Downloads\\analysis\\res")
# # save(dat_res, file = "dat_res.rdata")
# #####

setwd("C:\\Users\\andy\\Downloads\\analysis\\figures")

#####Archetypes
##### 
SS <- dat %>%
  filter(k == 2, i == 8, j == 6)
ND <- dat %>%
  filter(k == 4, i == 9, j == 13)
ME <- dat %>%
  filter(k == 2, i == 15, j == 4)
PD <- dat %>%
  filter(k == 5, i == 13, j == 6)
#####


##### Compare different archetypes
##### 
SS_1 <- SS %>% mutate(Arch = "SS") %>% select(-i, -j, -k)
ME_1 <- ME %>% mutate(Arch = "ME") %>% select(-i, -j, -k)
ND_1 <- ND %>% mutate(Arch = "ND") %>% select(-i, -j, -k)
PD_1 <- PD %>% mutate(Arch = "PD") %>% select(-i, -j, -k)
dat_arch <- rbind(SS_1, ME_1, ND_1, PD_1)


# # single replicate
# dat_arch %>%
#   filter(rep == 3) %>%
#   select(-rep) %>%
#   melt(id.vars = c("Arch", "t")) %>%
#   ggplot(aes(x = t, y = value, col = Arch, group = Arch)) +
#   geom_line() +
#   facet_wrap(vars(variable), scales = "free", nrow = 3) +
#   ggtitle("rep = 1")
# ggsave("Compare_archetypes_single_rep.pdf", width = 20, height = 15, units = "cm")
# ggsave("Compare_archetypes_single_rep.jpg", width = 20, height = 15, units = "cm")

# six replicate
dat_arch_1 <- dat_arch
#dat_arch_1$DNCI_log <- sign(dat_arch$DNCI) * sqrt(abs(dat_arch$DNCI))

dat_arch_1 %>%
  filter(rep < 7) %>%
  melt(id.vars = c("Arch", "t", "rep")) %>%
  ggplot(aes(x = t, y = value, col = Arch, group = interaction(Arch, rep))) +
  geom_line() +
  facet_wrap(vars(variable), scales = "free", nrow = 3)
ggsave("Compare_archetypes_six_rep.pdf", width = 20, height = 15, units = "cm")
ggsave("Compare_archetypes_six_rep.jpg", width = 20, height = 15, units = "cm")

# # all replicate
# dat_arch_1 <- dat_arch
# dat_arch_1$DNCI_log <- sign(dat_arch$DNCI) * sqrt(abs(dat_arch$DNCI))
# 
# dat_arch_1 %>%
#   melt(id.vars = c("Arch", "t", "rep")) %>%
#   ggplot(aes(x = t, y = value, col = Arch, group = interaction(Arch, rep))) +
#   geom_line() +
#   facet_wrap(vars(variable), scales = "free", nrow = 3) +
#   ggtitle("Compare different archetypes")
# ggsave("Compare_archetypes_all_rep.pdf", width = 20, height = 15, units = "cm")
# ggsave("Compare_archetypes_all_rep.jpg", width = 20, height = 15, units = "cm")
##### 



# setwd("C:\\Users\\andy\\Downloads\\analysis\\res")
# load("dat_res.rdata")
# 
# ##### Inference of analytical method on parametric space
# ##### 
# setwd("C:\\Users\\andy\\Downloads\\analysis\\figures")
# # VP and k = 1
# dat_res %>% 
#   filter(t == 60, k == 1) %>%
#   select(i, j, VP) %>%
#   group_by(i, j, VP) %>%
#   summarise(n_rep = n()) %>%
#   ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) +
#   geom_tile() + 
#   labs(title = "Inference of processes by VP in no competition", y = "Niche width", x = "Dispersal ability") +
#   scale_fill_distiller(palette = "YlGnBu") + 
#   facet_wrap(vars(VP), nrow = 1)
# ggsave("Parameter_space_VP_no_comp.pdf", width = 20, height = 15, units = "cm")
# ggsave("Parameter_space_VP_no_comp.jpg", width = 20, height = 15, units = "cm")
# 
# # VP and k = 2
# dat_res %>% 
#   filter(t == 60, k == 2) %>%
#   select(i, j, VP) %>%
#   group_by(i, j, VP) %>%
#   summarise(n_rep = n()) %>%
#   ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) +
#   geom_tile() + 
#   labs(title = "Inference of processes by VP in stable competition", y = "Niche width", x = "Dispersal ability") +
#   scale_fill_distiller(palette = "YlGnBu") + 
#   facet_wrap(vars(VP), nrow = 1)
# ggsave("Parameter_space_VP_stable_comp.pdf", width = 20, height = 15, units = "cm")
# ggsave("Parameter_space_VP_stable_comp.jpg", width = 20, height = 15, units = "cm")
# 
# # VP and k = 3
# dat_res %>% 
#   filter(t == 60, k == 3) %>%
#   select(i, j, VP) %>%
#   group_by(i, j, VP) %>%
#   summarise(n_rep = n()) %>%
#   ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) +
#   geom_tile() + 
#   labs(title = "Inference of processes by VP in mixed competition", y = "Niche width", x = "Dispersal ability") +
#   scale_fill_distiller(palette = "YlGnBu") + 
#   facet_wrap(vars(VP), nrow = 1)
# ggsave("Parameter_space_VP_mix_comp.pdf", width = 20, height = 15, units = "cm")
# ggsave("Parameter_space_VP_mix_comp.jpg", width = 20, height = 15, units = "cm")
# 
# # VP and k = 4
# dat_res %>% 
#   filter(t == 60, k == 4) %>%
#   select(i, j, VP) %>%
#   group_by(i, j, VP) %>%
#   summarise(n_rep = n()) %>%
#   ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) +
#   geom_tile() + 
#   labs(title = "Inference of processes by VP in equal competition", y = "Niche width", x = "Dispersal ability") +
#   scale_fill_distiller(palette = "YlGnBu") + 
#   facet_wrap(vars(VP), nrow = 1)
# ggsave("Parameter_space_VP_eq_comp.pdf", width = 20, height = 15, units = "cm")
# ggsave("Parameter_space_VP_eq_comp.jpg", width = 20, height = 15, units = "cm")
# 
# # VP and k = 5
# dat_res %>% 
#   filter(t == 60, k == 5) %>%
#   select(i, j, VP) %>%
#   group_by(i, j, VP) %>%
#   summarise(n_rep = n()) %>%
#   ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) +
#   geom_tile() + 
#   labs(title = "Inference of processes by VP in CC trade-off", y = "Niche width", x = "Dispersal ability") +
#   scale_fill_distiller(palette = "YlGnBu") + 
#   facet_wrap(vars(VP), nrow = 1)
# ggsave("Parameter_space_VP_CC_trade.pdf", width = 20, height = 15, units = "cm")
# ggsave("Parameter_space_VP_CC_trade.jpg", width = 20, height = 15, units = "cm")
# 
# # Stegen and k = 1
# dat_res %>% 
#   filter(t == 60, k == 1) %>%
#   select(i, j, Stegen) %>%
#   group_by(i, j, Stegen) %>%
#   summarise(n_rep = n()) %>%
#   ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) +
#   geom_tile() + 
#   labs(title = "Inference of processes by VP in no competition", y = "Niche width", x = "Dispersal ability") +
#   scale_fill_distiller(palette = "YlGnBu") + 
#   facet_wrap(vars(Stegen), nrow = 1)
# ggsave("Parameter_space_Stegen_no_comp.pdf", width = 20, height = 15, units = "cm")
# ggsave("Parameter_space_Stegen_no_comp.jpg", width = 20, height = 15, units = "cm")
# 
# # Stegen and k = 2
# dat_res %>% 
#   filter(t == 60, k == 2) %>%
#   select(i, j, Stegen) %>%
#   group_by(i, j, Stegen) %>%
#   summarise(n_rep = n()) %>%
#   ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) +
#   geom_tile() + 
#   labs(title = "Inference of processes by Stegen in stable competition", y = "Niche width", x = "Dispersal ability") +
#   scale_fill_distiller(palette = "YlGnBu") + 
#   facet_wrap(vars(Stegen), nrow = 1)
# ggsave("Parameter_space_Stegen_stable_comp.pdf", width = 20, height = 15, units = "cm")
# ggsave("Parameter_space_Stegen_stable_comp.jpg", width = 20, height = 15, units = "cm")
# 
# # Stegen and k = 3
# dat_res %>% 
#   filter(t == 60, k == 3) %>%
#   select(i, j, Stegen) %>%
#   group_by(i, j, Stegen) %>%
#   summarise(n_rep = n()) %>%
#   ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) +
#   geom_tile() + 
#   labs(title = "Inference of processes by Stegen in mixed competition", y = "Niche width", x = "Dispersal ability") +
#   scale_fill_distiller(palette = "YlGnBu") + 
#   facet_wrap(vars(Stegen), nrow = 1)
# ggsave("Parameter_space_Stegen_mix_comp.pdf", width = 20, height = 15, units = "cm")
# ggsave("Parameter_space_Stegen_mix_comp.jpg", width = 20, height = 15, units = "cm")
# 
# # Stegen and k = 4
# dat_res %>% 
#   filter(t == 60, k == 4) %>%
#   select(i, j, Stegen) %>%
#   group_by(i, j, Stegen) %>%
#   summarise(n_rep = n()) %>%
#   ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) +
#   geom_tile() + 
#   labs(title = "Inference of processes by Stegen in equal competition", y = "Niche width", x = "Dispersal ability") +
#   scale_fill_distiller(palette = "YlGnBu") + 
#   facet_wrap(vars(Stegen), nrow = 1)
# ggsave("Parameter_space_Stegen_eq_comp.pdf", width = 20, height = 15, units = "cm")
# ggsave("Parameter_space_Stegen_eq_comp.jpg", width = 20, height = 15, units = "cm")
# 
# # Stegen and k = 5
# dat_res %>% 
#   filter(t == 60, k == 5) %>%
#   select(i, j, Stegen) %>%
#   group_by(i, j, Stegen) %>%
#   summarise(n_rep = n()) %>%
#   ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) +
#   geom_tile() + 
#   labs(title = "Inference of processes by Stegen in CC trade-off", y = "Niche width", x = "Dispersal ability") +
#   scale_fill_distiller(palette = "YlGnBu") + 
#   facet_wrap(vars(Stegen), nrow = 1)
# ggsave("Parameter_space_Stegen_CC_trade.pdf", width = 20, height = 15, units = "cm")
# ggsave("Parameter_space_Stegen_CC_trade.jpg", width = 20, height = 15, units = "cm")
# 
# # DNCI and k = 1
# dat_res %>% 
#   filter(t == 60, k == 1) %>%
#   select(i, j, D) %>%
#   group_by(i, j, D) %>%
#   summarise(n_rep = n()) %>%
#   ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) +
#   geom_tile() + 
#   labs(title = "Inference of processes by DNCI in no competition", y = "Niche width", x = "Dispersal ability") +
#   scale_fill_distiller(palette = "YlGnBu") + 
#   facet_wrap(vars(D), nrow = 1)
# ggsave("Parameter_space_DNCI_no_comp.pdf", width = 20, height = 15, units = "cm")
# ggsave("Parameter_space_DNCI_no_comp.jpg", width = 20, height = 15, units = "cm")
# 
# # DNCI and k = 2
# dat_res %>% 
#   filter(t == 60, k == 2) %>%
#   select(i, j, D) %>%
#   group_by(i, j, D) %>%
#   summarise(n_rep = n()) %>%
#   ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) +
#   geom_tile() + 
#   labs(title = "Inference of processes by DNCI in stable competition", y = "Niche width", x = "Dispersal ability") +
#   scale_fill_distiller(palette = "YlGnBu") + 
#   facet_wrap(vars(D), nrow = 1)
# ggsave("Parameter_space_DNCI_stable_comp.pdf", width = 20, height = 15, units = "cm")
# ggsave("Parameter_space_DNCI_stable_comp.jpg", width = 20, height = 15, units = "cm")
# 
# # DNCI and k = 3
# dat_res %>% 
#   filter(t == 60, k == 3) %>%
#   select(i, j, D) %>%
#   group_by(i, j, D) %>%
#   summarise(n_rep = n()) %>%
#   ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) +
#   geom_tile() + 
#   labs(title = "Inference of processes by DNCI in mixed competition", y = "Niche width", x = "Dispersal ability") +
#   scale_fill_distiller(palette = "YlGnBu") + 
#   facet_wrap(vars(D), nrow = 1)
# ggsave("Parameter_space_DNCI_mix_comp.pdf", width = 20, height = 15, units = "cm")
# ggsave("Parameter_space_DNCI_mix_comp.jpg", width = 20, height = 15, units = "cm")
# 
# # DNCI and k = 4
# dat_res %>% 
#   filter(t == 60, k == 4) %>%
#   select(i, j, D) %>%
#   group_by(i, j, D) %>%
#   summarise(n_rep = n()) %>%
#   ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) +
#   geom_tile() + 
#   labs(title = "Inference of processes by DNCI in equal competition", y = "Niche width", x = "Dispersal ability") +
#   scale_fill_distiller(palette = "YlGnBu") + 
#   facet_wrap(vars(D), nrow = 1)
# ggsave("Parameter_space_DNCI_eq_comp.pdf", width = 20, height = 15, units = "cm")
# ggsave("Parameter_space_DNCI_eq_comp.jpg", width = 20, height = 15, units = "cm")
# 
# # DNCI and k = 5
# dat_res %>% 
#   filter(t == 60, k == 5) %>%
#   select(i, j, D) %>%
#   group_by(i, j, D) %>%
#   summarise(n_rep = n()) %>%
#   ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) +
#   geom_tile() + 
#   labs(title = "Inference of processes by DNCI in CC trade-off", y = "Niche width", x = "Dispersal ability") +
#   scale_fill_distiller(palette = "YlGnBu") + 
#   facet_wrap(vars(D), nrow = 1)
# ggsave("Parameter_space_DNCI_CC_trade.pdf", width = 20, height = 15, units = "cm")
# ggsave("Parameter_space_DNCI_CC_trade.jpg", width = 20, height = 15, units = "cm")
# 
# 
# #####




