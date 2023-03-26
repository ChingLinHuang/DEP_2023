#
# 20220313
# Extract some scenarios from the raw data for progress report

library(dplyr)
# Load data
setwd("C:\\Users\\andy\\Downloads\\plthompson-Meta_com_framework-36ca456_ver1\\outputs\\outputfile_v2")
model.df.test <- NULL
for(rep in 2:9){
  print(paste0("rep = ", rep))
  for(env in 2){
    for (k in 5){
      for (i in c(3,5)){
        for (j in c(1,2,7,8)){
          for (l in 1:4){
            for (m in c(1,2,4,5,7,8)){
              tryCatch({
                dat <- read.csv(paste0("outputfile_rep", rep, "env", env, "k", k, "i", i, "j", j, "l", l, "m", m, ".csv"))
                model.df.test <- rbind(model.df.test, dat)
              }, error=function(e){})
            }
          }
        }
      }
    }
  }
}

#save(model.df.test, file = "C:\\Users\\andy\\Downloads\\analysis\\model_df_test.rdata")
# load("C:\\Users\\andy\\Downloads\\analysis\\model_df_test.rdata")


test2 <- model.df.test
NUM <- data.frame(rep = 0, env = 0, dispersal_a = 0, dispersal_b = 0, sig_niche = 0, grow = 0, alpha = 0, Time = 0, N = 0, Species = 0 , Patch = 0, size = 0)

NUM <- test2 %>%
  select(N, Species, dispersal_a, dispersal_b, sig_niche, grow, alpha, Time, env_type, rep) %>%
  group_by(dispersal_a, dispersal_b, sig_niche, grow, alpha, Time, env_type, rep) %>%
  group_modify(~ .x %>% 
                 mutate(abundance = sum(.x$N), richness = length(unique(.x$Species)), occurrence = nrow(.x)) %>%
                 select(-N, -Species) %>%
                 unique())



hist(NUM$abundance)
hist(NUM$richness)

# save(NUM, file = "C:\\Users\\andy\\Downloads\\analysis\\NUM.rdata")
load("NUM.rdata")

low_abund_richness <- NUM %>% 
  filter(Time == 4) %>%
  filter(abundance < 1000 | richness < 3) %>%
  select(dispersal_a, dispersal_b, sig_niche, grow, alpha, Time, env_type, rep)




