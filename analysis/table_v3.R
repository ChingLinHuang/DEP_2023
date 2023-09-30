library(tidyverse)
library(gt)
library(webshot2)
library(scales)
######
######  Summary of statistics
######
load("C:\\Users\\andy\\Downloads\\analysis\\tables\\summary_statistics.rdata")
tb <- summary_statistics %>%
  as.tibble %>%
  round(digits = 3) %>%
  mutate(name = c("Min", "Max", "Mean", "Sd"))

tb <- tb[ ,c(5:8,1:4,9:11)]
colnames(tb)[9] <- "value"

tg <- tb %>% gt(rowname_col = "name") %>%
  tab_spanner(
    label = "VP",
    columns = c(Env, EnvSpatial, Spatial, Resid)
  ) %>% 
  tab_spanner(
    label = "Stegen",
    columns = c(Selection, DispLimit, HomoDisp, Drift)
  )%>% 
  tab_spanner(
    label = "DNCI",
    columns = c(value, sd.DNCI)
  )

tg
setwd("C:\\Users\\andy\\Downloads\\analysis\\tables")
gtsave(tg, file = "summary_statistics.tex")
gtsave(tg, file = "summary_statistics.png")


#######
####### results of RF
#######
setwd("C:\\Users\\andy\\Downloads\\analysis\\res\\RF")
load(paste0("RF_t4_all.rdata"))

setwd("C:\\Users\\andy\\Downloads\\analysis\\tables")

### importance
#####
tb <- t(round(RF_t4_all$importance, digits = 4))
tb[ ,1] <- round(tb[ ,1], digits = 2)
tb <- tb[ ,c(3,1,2)]
colnames(tb) <- c("Niche width", "Competition type", "Dispersal ability")
name <- paste0(rep(c("Selection", "DispLimit", "HomoDisp", "Drift", "Env", "EnvSpatial", "Spatial", "Resid", "DNCI", "sd.DNCI"), each = 4), rep(c(20, 16, 12, 8), 10))
tg <- tb %>% 
  as_tibble %>% 
  mutate(name = name) %>%
  gt(rowname_col = "name") 
tg
gtsave(tg, file = "importance_t4_all.tex")
gtsave(tg, file = "importance_t4_all.png", expand = 20)
# 
# tg_1 <- tb[1:20, ] %>%
#   gt(rowname_col = "name") 
# tg_2 <- tb[21:40, ] %>%
#   gt(rowname_col = "name") 
# tg_1
# gtsave(tg_1, file = "importance_t4_all_s1.tex")
# gtsave(tg_1, file = "importance_t4_all_s1.png", expand = 20)
# gtsave(tg_2, file = "importance_t4_all_s2.tex")
# gtsave(tg_2, file = "importance_t4_all_s2.png", expand = 20)

#####

### Performance
#####
# Method <- c("Stegen", "VP", "DNCI", "all")
# Time <- c(1, 4, 20)
# ind <- 1
# tb <- c()
# for(m in Method){
#   for(t in Time){
#     setwd("C:\\Users\\andy\\Downloads\\analysis\\res\\RF")
#     load(paste0("RF_t", t, "_", m, ".rdata"))
#     #dat <- get(paste0("RF_t", t, "_", m))
#     tb <- rbind(tb, get(paste0("RF_t", t, "_", m))$performance)
#     row.names(tb)[ind] <- paste0("t", t, "_", m)
#     ind <- ind + 1
#   }
# }
# 
# colnames(tb) <- c("Dispersal ability", "Abiotic response", "Biotic response")
# name <- row.names(tb)
# tb <- tb %>%
#   apply(2, function(x) percent(x, accuracy = 0.01)) %>%
#   as_tibble %>%
#   mutate(name = name)
# save(tb, file = "table_performance.rdata")
# # 
setwd("C:\\Users\\andy\\Downloads\\analysis\\res\\RF")
load("table_performance.rdata")

tb$`Dispersal ability` <- str_sub(tb$`Dispersal ability`, 1, -2) # remove "%"
tb$`Abiotic response` <- str_sub(tb$`Abiotic response`, 1, -2)
tb$`Biotic response` <- str_sub(tb$`Biotic response`, 1, -2)

colnames(tb)[1:3] <- c("Dispersal ability", "Niche width", "Competition type")
tb_1 <- data.frame(Stegen = c("O","O","O","X","X","X","X","X","X","O","O","O"), 
                   VP = c("X","X","X","O","O","O","X","X","X","O","O","O"), 
                   DNCI = c("X","X","X","X","X","X","O","O","O","O","O","O"), 
                   Snapshots = c(1,4,20,1,4,20,1,4,20,1,4,20))

tb <- cbind(tb_1, tb) %>% select(-name)
tb <- tb[c(4,5,6,1,2,3,7:12), ]
tb <- tb[ ,c(2,1,3:7)]
tg <- tb %>% gt() %>% 
  tab_spanner(
    label = "Summary statistics",
    columns = c(VP, Stegen, DNCI)
  ) %>%
  tab_spanner(
    label = "Explanatory variables",
    columns = c(VP, Stegen, DNCI, Snapshots)
  ) %>%
  tab_spanner(
    label = "Performance of prediction (%)",
    columns = c(`Niche width`, `Competition type`, `Dispersal ability`)
  )
tg
setwd("C:\\Users\\andy\\Downloads\\analysis\\tables")
gtsave(tg, "Performance.png")
gtsave(tg, "Performance.tex")
#####


#####
##### Empirical
##### 

load("C:\\Users\\andy\\Downloads\\analysis\\empirical\\Fushan\\res\\res.rdata")

tb <- t(res) %>% as.data.frame()
tb <- tb[-1, ]
colnames(tb) <- c("Year 1", "Year 2", "Year 3", "Year 4")
tb <- round(tb, digits = 2)
tb[10, ] <- tb[10, ]/2 
row.names(tb)[10] <- "sd.DNCI"
tg <- tb %>% 
  mutate(name = row.names(tb)) %>%
  as.tibble() %>%
  gt(rowname_col = "name") 
tg  

setwd("C:\\Users\\andy\\Downloads\\analysis\\tables")
gtsave(tg, "Empirical_statistics.png")
gtsave(tg, "Empirical_statistics.tex")

#####
##### parameters table
#####
niche_width <- exp(seq(from = log(0.001), to = log(10), length.out = 13)) %>%
  formatC(format = "e", digits = 4) %>%  
  c(., rep(NA, 2))
disp_ability <- exp(seq(from = log(1e-5), to = log(1), length.out = 16)) %>%
  formatC(format = "e", digits = 4) 
disp_ability <- disp_ability[-16]

tb <- data.frame(Level = 1:15, niche_width, disp_ability)
colnames(tb)[2:3] <- c("Niche width", "Dispersal ability")  

tg <- tb %>% 
  as.tibble() %>%
  gt() 
tg  

setwd("C:\\Users\\andy\\Downloads\\analysis\\tables")
gtsave(tg, "Parameters_table.png")
gtsave(tg, "Parameters_table.tex")