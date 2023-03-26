# results of DNCI
# 

setwd("C:\\Users\\andy\\Downloads\\analysis\\DNCI\\data")
res <- NULL
for (i in c(1,3,5,7,11,12,14,15,19,20,26)){
  load(paste0("res_DNCI_", i, "_1.rdata"))
  res <- rbind(res, res.DNCI)
}
  

length(unique(res$dispersal)) #9
length(unique(res$sig_niche)) #7
length(unique(res$alpha)) #3

res <- res %>% 
  mutate(dispersal = round(as.numeric(dispersal), 5)) %>%
  mutate(sig_niche = round(as.numeric(dispersal), 5))

# Figure2-1: Fix Time == 7, alpha == "stable"
dat2_1 <- res %>% 
  filter(Time == 7) %>% # final state
  filter(alpha == "stable")

ggplot(data = dat2_1, aes(x = as.factor(sig_niche), y = DELTA.dn)) +
  geom_boxplot()
# ggsave("fig1_2.png", width = 11, height = 5)

