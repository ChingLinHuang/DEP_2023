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

# Archetypes dynamics
#####
setwd("C:\\Users\\andy\\Downloads\\analysis\\data\\spe_4_1")

for(t in 1:60){
  # SS: k = 2, i = 8, j = 6
  load(paste0("spe_rep1k2i8j6t", t, ".rdata"))
  CCA_SS <- cca(dat_spe)
  plot_SS <- ordiplot(CCA_SS, display = "site")$default %>% 
    as.data.frame() %>%
    mutate(time = t, archetype = "SS")
  
  # ND: k = 4, i = 9, j = 13 
  load(paste0("spe_rep1k4i9j13t", t, ".rdata"))
  CCA_ND <- cca(dat_spe)
  plot_ND <- ordiplot(CCA_ND, display = "site")$default %>% 
    as.data.frame() %>%
    mutate(time = t, archetype = "ND")
  
  # ME: k = 2, i = 15, j = 4
  load(paste0("spe_rep1k2i15j4t", t, ".rdata"))
  CCA_ME <- cca(dat_spe)
  plot_ME <- ordiplot(CCA_ME, display = "site")$default %>%
    as.data.frame() %>%
    mutate(time = t, archetype = "ME")
  
  # PD: k = 5, i = 15, j= 6
  load(paste0("spe_rep1k5i15j6t", t, ".rdata"))
  CCA_PD <- cca(dat_spe)
  plot_PD <- ordiplot(CCA_PD, display = "site")$default %>% 
    as.data.frame() %>%
    mutate(time = t, archetype = "PD")
  
  if(t == 1){
  dat <- rbind(plot_SS, plot_ND, plot_ME, plot_PD)
  } else{
    dat <- rbind(dat, plot_SS, plot_ND, plot_ME, plot_PD)
  }
}


setwd("C:\\Users\\andy\\Downloads\\analysis\\figures")
p_SS <- dat %>%
  filter(time > 10, archetype == "SS") %>%
  ggplot(aes(x = CA1, y = CA2, frame = time)) +
  geom_point() + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +  
  labs(title = "Species sorting", subtitle = 'Time: {frame_time}', x = "CA1", y = "CA2") +
  transition_time(time) 

ggsave(p_SS)
mygif <- animate(p_SS, duration = 15, renderer = gifski_renderer())  
anim_save(filename = "Simulation_NMDS_dynamics_SS.gif", mygif)

p_ND <- dat %>%
  filter(time > 10, archetype == "ND") %>%
  ggplot(aes(x = CA1, y = CA2, frame = time)) +
  geom_point() + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +  
  labs(title = "Neutral dynamics", subtitle = 'Time: {frame_time}', x = "CA1", y = "CA2") +
  transition_time(time)
mygif <- animate(p_ND, duration = 15, renderer = gifski_renderer())  
anim_save(filename = "Simulation_NMDS_dynamics_ND.gif", mygif)

p_ME <- dat %>%
  filter(time > 10, archetype == "ME") %>%
  ggplot(aes(x = CA1, y = CA2, frame = time)) +
  geom_point() + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +  
  labs(title = "Mass effect", subtitle = 'Time: {frame_time}', x = "CA1", y = "CA2") +
  transition_time(time)
mygif <- animate(p_ME, duration = 15, renderer = gifski_renderer())  
anim_save(filename = "Simulation_NMDS_dynamics_ME.gif", mygif)

p_PD <- dat %>%
  filter(time > 10, archetype == "PD") %>%
  ggplot(aes(x = CA1, y = CA2, frame = time)) +
  geom_point() + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +  
  labs(title = "Patch dynamics", subtitle = 'Time: {frame_time}', x = "CA1", y = "CA2") +
  transition_time(time)
mygif <- animate(p_PD, duration = 15, renderer = gifski_renderer())  
anim_save(filename = "Simulation_NMDS_dynamics_PD.gif", mygif)

#####

##### Parametric space
##### 
setwd("C:\\Users\\andy\\Downloads\\analysis\\res")
load(".\\VP_4\\res_VP.rdata")
load(".\\DNCI_4\\res_DNCI.rdata")
load(".\\Stegen_4\\res_Stegen.rdata")

dat <- dat_VP %>% 
  left_join(dat_Stegen) %>% 
  left_join(dat_DNCI)
sum(dat$DNCI == "too sparse") # 3194
dat <- dat %>% 
  filter(DNCI != "too sparse") %>%
  mutate(DNCI = as.numeric(DNCI), CI.DNCI = as.numeric(CI.DNCI))

setwd("C:\\Users\\andy\\Downloads\\analysis\\figures")

range(dat$i) # 1 to 15
range(dat$j) # 3 to 13
unique(dat$k) # 1 to 5

# parameter space for empirical
dat %>% 
  filter(t == 60) %>% 
  select(i, j) %>%
  group_by(i, j) %>%
  summarise(n_rep = n()) %>%
  ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) + 
  geom_tile() + 
  annotate("text", x = 15, y = 8, label= "FFDP", color = "white")
ggsave("Parameter_space_overlook_empirical.pdf", width = 15, height = 15, units = "cm")
ggsave("Parameter_space_overlook_empirical.jpg", width = 15, height = 15, units = "cm")

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
  annotate("text", x = 15, y = 4, label= "PD", color = "white") +
  annotate("text", x = 15, y = 11, label= "HD", color = "white") +
  annotate("text", x = 2, y = 11, label= "DL", color = "white") 
ggsave("Parameter_space_overlook_archetypes.pdf", width = 15, height = 15, units = "cm")
ggsave("Parameter_space_overlook_archetypes.jpg", width = 15, height = 15, units = "cm")

# parameter space for no competition
dat %>%
  filter(k == 1, t == 60) %>% 
  select(i, j) %>%
  group_by(i, j) %>%
  summarise(n_rep = n()) %>%
  ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) + 
  geom_tile() + 
  labs(title = "no competition", y = "Niche width", x = "Dispersal ability") +
  scale_fill_distiller(palette = "YlGnBu")

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
  labs(title = "Stable competition", y = "Niche width", x = "Dispersal ability") +
  scale_fill_distiller(palette = "YlGnBu")

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
  labs(title = "Mixed competition", y = "Niche width", x = "Dispersal ability") +
  scale_fill_distiller(palette = "YlGnBu")

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
  labs(title = "Equal competition", y = "Niche width", x = "Dispersal ability") +
  scale_fill_distiller(palette = "YlGnBu")

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
  labs(title = "Competition-colonization trade-off", y = "Niche width", x = "Dispersal ability") +
  scale_fill_distiller(palette = "YlGnBu")

ggsave("Parameter_space_overlook_CC_tradeoff.pdf", width = 15, height = 15, units = "cm")
ggsave("Parameter_space_overlook_CC_tradeoff.jpg", width = 15, height = 15, units = "cm")

#####



##########  
##########  Statistics
##########  

setwd("C:\\Users\\andy\\Downloads\\analysis\\res")
load(".\\VP_4\\res_VP.rdata")
load(".\\DNCI_4\\res_DNCI.rdata")
load(".\\Stegen_4\\res_Stegen.rdata")

dat <- dat_VP %>% 
  left_join(dat_Stegen) %>% 
  left_join(dat_DNCI)
sum(dat$DNCI == "too sparse") # 3194
dat <- dat %>% 
  filter(DNCI != "too sparse") %>%
  mutate(DNCI = as.numeric(DNCI), CI.DNCI = as.numeric(CI.DNCI))

setwd("C:\\Users\\andy\\Downloads\\analysis\\figures")


##### Inference and parametric space
##### 
dat_res_Stegen_1 <- dat %>%
  select(Selection, DispLimit, HomoDisp, Drift) %>%
  apply(., 1, function (x) sum(x == max(x)))
table(dat_res_Stegen_1) # only 12 samples has the two same maximum

dat_1 <- dat[dat_res_Stegen_1 == 1, ]

dat_res <- dat_1%>%
  mutate(VP = dat_1 %>% 
           select(Env, `Env and Spatial`, Spatial) %>%
           apply(., 1, function (x) colnames(.)[x == max(x)])) %>%
  mutate(Stegen = dat_1 %>% 
           select(Selection, DispLimit, HomoDisp, Drift) %>%
           apply(., 1, function (x) colnames(.)[x == max(x)])) %>%
  mutate(D = dat_1 %>% 
           select(DNCI, CI.DNCI) %>%
           apply(., 1, function (x) {
             if(x[1] > 0 && x[1] - x[2] > 0) 
               return("Niche")
             else if(x[1] < 0 && x[1] + x[2] < 0) 
               return("Dispersal")
             else 
               return("Random")
           })) 
table(dat_res$VP)  
table(dat_res$Stegen)  
table(dat_res$D)  

setwd("C:\\Users\\andy\\Downloads\\analysis\\res")
# save(dat_res, file = "dat_res.rdata")
load("dat_res.rdata")

setwd("C:\\Users\\andy\\Downloads\\analysis\\figures")
# VP and k = 1
dat_res %>% 
  filter(t == 60, k == 1) %>%
  select(i, j, VP) %>%
  group_by(i, j, VP) %>%
  summarise(n_rep = n()) %>%
  ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) +
  geom_tile() + 
  labs(title = "Inference of processes by VP in no competition", y = "Niche width", x = "Dispersal ability") +
  scale_fill_distiller(palette = "YlGnBu") + 
  facet_wrap(vars(VP), nrow = 1)
ggsave("Parameter_space_VP_no_comp.pdf", width = 20, height = 15, units = "cm")
ggsave("Parameter_space_VP_no_comp.jpg", width = 20, height = 15, units = "cm")

# VP and k = 2
dat_res %>% 
  filter(t == 60, k == 2) %>%
  select(i, j, VP) %>%
  group_by(i, j, VP) %>%
  summarise(n_rep = n()) %>%
  ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) +
  geom_tile() + 
  labs(title = "Inference of processes by VP in stable competition", y = "Niche width", x = "Dispersal ability") +
  scale_fill_distiller(palette = "YlGnBu") + 
  facet_wrap(vars(VP), nrow = 1)
ggsave("Parameter_space_VP_stable_comp.pdf", width = 20, height = 15, units = "cm")
ggsave("Parameter_space_VP_stable_comp.jpg", width = 20, height = 15, units = "cm")
  
# VP and k = 3
dat_res %>% 
  filter(t == 60, k == 3) %>%
  select(i, j, VP) %>%
  group_by(i, j, VP) %>%
  summarise(n_rep = n()) %>%
  ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) +
  geom_tile() + 
  labs(title = "Inference of processes by VP in mixed competition", y = "Niche width", x = "Dispersal ability") +
  scale_fill_distiller(palette = "YlGnBu") + 
  facet_wrap(vars(VP), nrow = 1)
ggsave("Parameter_space_VP_mix_comp.pdf", width = 20, height = 15, units = "cm")
ggsave("Parameter_space_VP_mix_comp.jpg", width = 20, height = 15, units = "cm")

# VP and k = 4
dat_res %>% 
  filter(t == 60, k == 4) %>%
  select(i, j, VP) %>%
  group_by(i, j, VP) %>%
  summarise(n_rep = n()) %>%
  ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) +
  geom_tile() + 
  labs(title = "Inference of processes by VP in equal competition", y = "Niche width", x = "Dispersal ability") +
  scale_fill_distiller(palette = "YlGnBu") + 
  facet_wrap(vars(VP), nrow = 1)
ggsave("Parameter_space_VP_eq_comp.pdf", width = 20, height = 15, units = "cm")
ggsave("Parameter_space_VP_eq_comp.jpg", width = 20, height = 15, units = "cm")

# VP and k = 5
dat_res %>% 
  filter(t == 60, k == 5) %>%
  select(i, j, VP) %>%
  group_by(i, j, VP) %>%
  summarise(n_rep = n()) %>%
  ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) +
  geom_tile() + 
  labs(title = "Inference of processes by VP in CC trade-off", y = "Niche width", x = "Dispersal ability") +
  scale_fill_distiller(palette = "YlGnBu") + 
  facet_wrap(vars(VP), nrow = 1)
ggsave("Parameter_space_VP_CC_trade.pdf", width = 20, height = 15, units = "cm")
ggsave("Parameter_space_VP_CC_trade.jpg", width = 20, height = 15, units = "cm")

# Stegen and k = 1
dat_res %>% 
  filter(t == 60, k == 1) %>%
  select(i, j, Stegen) %>%
  group_by(i, j, Stegen) %>%
  summarise(n_rep = n()) %>%
  ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) +
  geom_tile() + 
  labs(title = "Inference of processes by VP in no competition", y = "Niche width", x = "Dispersal ability") +
  scale_fill_distiller(palette = "YlGnBu") + 
  facet_wrap(vars(Stegen), nrow = 1)
ggsave("Parameter_space_Stegen_no_comp.pdf", width = 20, height = 15, units = "cm")
ggsave("Parameter_space_Stegen_no_comp.jpg", width = 20, height = 15, units = "cm")

# Stegen and k = 2
dat_res %>% 
  filter(t == 60, k == 2) %>%
  select(i, j, Stegen) %>%
  group_by(i, j, Stegen) %>%
  summarise(n_rep = n()) %>%
  ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) +
  geom_tile() + 
  labs(title = "Inference of processes by Stegen in stable competition", y = "Niche width", x = "Dispersal ability") +
  scale_fill_distiller(palette = "YlGnBu") + 
  facet_wrap(vars(Stegen), nrow = 1)
ggsave("Parameter_space_Stegen_stable_comp.pdf", width = 20, height = 15, units = "cm")
ggsave("Parameter_space_Stegen_stable_comp.jpg", width = 20, height = 15, units = "cm")

# Stegen and k = 3
dat_res %>% 
  filter(t == 60, k == 3) %>%
  select(i, j, Stegen) %>%
  group_by(i, j, Stegen) %>%
  summarise(n_rep = n()) %>%
  ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) +
  geom_tile() + 
  labs(title = "Inference of processes by Stegen in mixed competition", y = "Niche width", x = "Dispersal ability") +
  scale_fill_distiller(palette = "YlGnBu") + 
  facet_wrap(vars(Stegen), nrow = 1)
ggsave("Parameter_space_Stegen_mix_comp.pdf", width = 20, height = 15, units = "cm")
ggsave("Parameter_space_Stegen_mix_comp.jpg", width = 20, height = 15, units = "cm")

# Stegen and k = 4
dat_res %>% 
  filter(t == 60, k == 4) %>%
  select(i, j, Stegen) %>%
  group_by(i, j, Stegen) %>%
  summarise(n_rep = n()) %>%
  ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) +
  geom_tile() + 
  labs(title = "Inference of processes by Stegen in equal competition", y = "Niche width", x = "Dispersal ability") +
  scale_fill_distiller(palette = "YlGnBu") + 
  facet_wrap(vars(Stegen), nrow = 1)
ggsave("Parameter_space_Stegen_eq_comp.pdf", width = 20, height = 15, units = "cm")
ggsave("Parameter_space_Stegen_eq_comp.jpg", width = 20, height = 15, units = "cm")

# Stegen and k = 5
dat_res %>% 
  filter(t == 60, k == 5) %>%
  select(i, j, Stegen) %>%
  group_by(i, j, Stegen) %>%
  summarise(n_rep = n()) %>%
  ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) +
  geom_tile() + 
  labs(title = "Inference of processes by Stegen in CC trade-off", y = "Niche width", x = "Dispersal ability") +
  scale_fill_distiller(palette = "YlGnBu") + 
  facet_wrap(vars(Stegen), nrow = 1)
ggsave("Parameter_space_Stegen_CC_trade.pdf", width = 20, height = 15, units = "cm")
ggsave("Parameter_space_Stegen_CC_trade.jpg", width = 20, height = 15, units = "cm")

# DNCI and k = 1
dat_res %>% 
  filter(t == 60, k == 1) %>%
  select(i, j, D) %>%
  group_by(i, j, D) %>%
  summarise(n_rep = n()) %>%
  ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) +
  geom_tile() + 
  labs(title = "Inference of processes by DNCI in no competition", y = "Niche width", x = "Dispersal ability") +
  scale_fill_distiller(palette = "YlGnBu") + 
  facet_wrap(vars(D), nrow = 1)
ggsave("Parameter_space_DNCI_no_comp.pdf", width = 20, height = 15, units = "cm")
ggsave("Parameter_space_DNCI_no_comp.jpg", width = 20, height = 15, units = "cm")

# DNCI and k = 2
dat_res %>% 
  filter(t == 60, k == 2) %>%
  select(i, j, D) %>%
  group_by(i, j, D) %>%
  summarise(n_rep = n()) %>%
  ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) +
  geom_tile() + 
  labs(title = "Inference of processes by DNCI in stable competition", y = "Niche width", x = "Dispersal ability") +
  scale_fill_distiller(palette = "YlGnBu") + 
  facet_wrap(vars(D), nrow = 1)
ggsave("Parameter_space_DNCI_stable_comp.pdf", width = 20, height = 15, units = "cm")
ggsave("Parameter_space_DNCI_stable_comp.jpg", width = 20, height = 15, units = "cm")

# DNCI and k = 3
dat_res %>% 
  filter(t == 60, k == 3) %>%
  select(i, j, D) %>%
  group_by(i, j, D) %>%
  summarise(n_rep = n()) %>%
  ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) +
  geom_tile() + 
  labs(title = "Inference of processes by DNCI in mixed competition", y = "Niche width", x = "Dispersal ability") +
  scale_fill_distiller(palette = "YlGnBu") + 
  facet_wrap(vars(D), nrow = 1)
ggsave("Parameter_space_DNCI_mix_comp.pdf", width = 20, height = 15, units = "cm")
ggsave("Parameter_space_DNCI_mix_comp.jpg", width = 20, height = 15, units = "cm")

# DNCI and k = 4
dat_res %>% 
  filter(t == 60, k == 4) %>%
  select(i, j, D) %>%
  group_by(i, j, D) %>%
  summarise(n_rep = n()) %>%
  ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) +
  geom_tile() + 
  labs(title = "Inference of processes by DNCI in equal competition", y = "Niche width", x = "Dispersal ability") +
  scale_fill_distiller(palette = "YlGnBu") + 
  facet_wrap(vars(D), nrow = 1)
ggsave("Parameter_space_DNCI_eq_comp.pdf", width = 20, height = 15, units = "cm")
ggsave("Parameter_space_DNCI_eq_comp.jpg", width = 20, height = 15, units = "cm")

# DNCI and k = 5
dat_res %>% 
  filter(t == 60, k == 5) %>%
  select(i, j, D) %>%
  group_by(i, j, D) %>%
  summarise(n_rep = n()) %>%
  ggplot(aes(x = as.factor(i), y = as.factor(j), fill = n_rep)) +
  geom_tile() + 
  labs(title = "Inference of processes by DNCI in CC trade-off", y = "Niche width", x = "Dispersal ability") +
  scale_fill_distiller(palette = "YlGnBu") + 
  facet_wrap(vars(D), nrow = 1)
ggsave("Parameter_space_DNCI_CC_trade.pdf", width = 20, height = 15, units = "cm")
ggsave("Parameter_space_DNCI_CC_trade.jpg", width = 20, height = 15, units = "cm")


#####

setwd("C:\\Users\\andy\\Downloads\\analysis\\figures")
#####Archetypes
##### 
SS <- dat %>%
  filter(k == 2, i == 8, j == 6)
ND <- dat %>%
  filter(k == 1, i == 8, j == 13)
ME <- dat %>%
  filter(k == 2, i == 15, j == 4)
PD <- dat %>%
  filter(k == 5, i == 9, j == 8)
HD <- dat %>%
  filter(k == 1, i == 15, j == 13)
DL <- dat %>%
  filter(k == 1, i == 2, j == 13)
#####


##### Archetypes dynamics 
##### 
# Species sorting dynamics
SS %>% 
  select(rep, t, Env, `Env and Spatial`, Resid, Spatial, Selection, DispLimit, HomoDisp, Drift, DNCI, CI.DNCI) %>%
  filter(rep < 5) %>%
  melt(id.vars = c("rep", "t")) %>%
  ggplot(aes(x = t, y = value, col = as.factor(rep), group = rep)) +
  geom_line() +
  facet_wrap(vars(variable), scales = "free", nrow = 3) +
  ggtitle("Species sorting dynamics")
ggsave("Archetypes_Species_sorting_dynamics.pdf", width = 20, height = 15, units = "cm")
ggsave("Archetypes_Species_sorting_dynamics.jpg", width = 20, height = 15, units = "cm")

# Neutral dynamics
ND %>% 
  select(rep, t, Env, `Env and Spatial`, Resid, Spatial, Selection, DispLimit, HomoDisp, Drift, DNCI, CI.DNCI) %>%
  filter(rep < 5) %>%
  melt(id.vars = c("rep", "t")) %>%
  ggplot(aes(x = t, y = value, col = as.factor(rep), group = rep)) +
  geom_line() +
  facet_wrap(vars(variable), scales = "free", nrow = 3) +
  ggtitle("Neutral dynamics")
ggsave("Archetypes_Neutral_dynamics.pdf", width = 20, height = 15, units = "cm")
ggsave("Archetypes_Neutral_dynamics.jpg", width = 20, height = 15, units = "cm")

# Mass effect dynamics
ME %>% 
  select(rep, t, Env, `Env and Spatial`, Resid, Spatial, Selection, DispLimit, HomoDisp, Drift, DNCI, CI.DNCI) %>%
  filter(rep < 5) %>%
  melt(id.vars = c("rep", "t")) %>%
  ggplot(aes(x = t, y = value, col = as.factor(rep), group = rep)) +
  geom_line() +
  facet_wrap(vars(variable), scales = "free", nrow = 3) +
  ggtitle("Mass effect dynamics")
ggsave("Archetypes_Mass_effect_dynamics.pdf", width = 20, height = 15, units = "cm")
ggsave("Archetypes_Mass_effect_dynamics.jpg", width = 20, height = 15, units = "cm")

# Patch dynamics
PD %>% 
  select(rep, t, Env, `Env and Spatial`, Resid, Spatial, Selection, DispLimit, HomoDisp, Drift, DNCI, CI.DNCI) %>%
  filter(rep < 5) %>%
  melt(id.vars = c("rep", "t")) %>%
  ggplot(aes(x = t, y = value, col = as.factor(rep), group = rep)) +
  geom_line() +
  facet_wrap(vars(variable), scales = "free", nrow = 3) +
  ggtitle("Patch dynamics")
ggsave("Archetypes_Patch_dynamics.pdf", width = 20, height = 15, units = "cm")
ggsave("Archetypes_Patch_dynamics.jpg", width = 20, height = 15, units = "cm")

# Homogenizing dispersal
HD %>% 
  select(rep, t, Env, `Env and Spatial`, Resid, Spatial, Selection, DispLimit, HomoDisp, Drift, DNCI, CI.DNCI) %>%
  filter(rep < 5) %>%
  melt(id.vars = c("rep", "t")) %>%
  ggplot(aes(x = t, y = value, col = as.factor(rep), group = rep)) +
  geom_line() +
  facet_wrap(vars(variable), scales = "free", nrow = 3) +
  ggtitle("Homogenizing dispersal")
ggsave("Archetypes_Homogenizing_dispersal.pdf", width = 20, height = 15, units = "cm")
ggsave("Archetypes_Homogenizing_dispersal.jpg", width = 20, height = 15, units = "cm")

# Homogenizing dispersal
DL %>% 
  select(rep, t, Env, `Env and Spatial`, Resid, Spatial, Selection, DispLimit, HomoDisp, Drift, DNCI, CI.DNCI) %>%
  filter(rep < 5) %>%
  melt(id.vars = c("rep", "t")) %>%
  ggplot(aes(x = t, y = value, col = as.factor(rep), group = rep)) +
  geom_line() +
  facet_wrap(vars(variable), scales = "free", nrow = 3) +
  ggtitle("Dispersal limitation")
ggsave("Archetypes_Dispersal_limitation.pdf", width = 20, height = 15, units = "cm")
ggsave("Archetypes_Dispersal_limitation.jpg", width = 20, height = 15, units = "cm")
#####

##### Compare different archetypes
##### 
SS_1 <- SS %>% mutate(Arch = "SS") %>% select(-i, -j, -k)
ME_1 <- ME %>% mutate(Arch = "ME") %>% select(-i, -j, -k)
ND_1 <- ND %>% mutate(Arch = "ND") %>% select(-i, -j, -k)
PD_1 <- PD %>% mutate(Arch = "PD") %>% select(-i, -j, -k)
HD_1 <- HD %>% mutate(Arch = "HD") %>% select(-i, -j, -k)
DL_1 <- DL %>% mutate(Arch = "DL") %>% select(-i, -j, -k)
dat_arch <- rbind(SS_1, ME_1, ND_1, PD_1, HD_1, DL_1)


# single replicate
dat_arch %>%
 filter(rep == 3) %>%
  select(-rep) %>%
  melt(id.vars = c("Arch", "t")) %>%
  ggplot(aes(x = t, y = value, col = Arch, group = Arch)) +
  geom_line() +
  facet_wrap(vars(variable), scales = "free", nrow = 3) +
  ggtitle("rep = 1")
ggsave("Compare_archetypes_single_rep.pdf", width = 20, height = 15, units = "cm")
ggsave("Compare_archetypes_single_rep.jpg", width = 20, height = 15, units = "cm")

# six replicate
dat_arch_1 <- dat_arch
dat_arch_1$DNCI_log <- sign(dat_arch$DNCI) * sqrt(abs(dat_arch$DNCI))

dat_arch_1 %>%
  filter(rep < 7) %>%
  melt(id.vars = c("Arch", "t", "rep")) %>%
  ggplot(aes(x = t, y = value, col = Arch, group = interaction(Arch, rep))) +
  geom_line() +
  facet_wrap(vars(variable), scales = "free", nrow = 3) +
  ggtitle("Compare different archetypes")
ggsave("Compare_archetypes_six_rep.pdf", width = 20, height = 15, units = "cm")
ggsave("Compare_archetypes_six_rep.jpg", width = 20, height = 15, units = "cm")

# all replicate
dat_arch_1 <- dat_arch
dat_arch_1$DNCI_log <- sign(dat_arch$DNCI) * sqrt(abs(dat_arch$DNCI))

dat_arch_1 %>%
  melt(id.vars = c("Arch", "t", "rep")) %>%
  ggplot(aes(x = t, y = value, col = Arch, group = interaction(Arch, rep))) +
  geom_line() +
  facet_wrap(vars(variable), scales = "free", nrow = 3) +
  ggtitle("Compare different archetypes")
ggsave("Compare_archetypes_all_rep.pdf", width = 20, height = 15, units = "cm")
ggsave("Compare_archetypes_all_rep.jpg", width = 20, height = 15, units = "cm")
##### 






