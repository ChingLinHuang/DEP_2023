# 
# 
# 
# 

library(vegan)
library(tidyverse)
library (twinspanR)
library(tictoc)

source("C:\\Users\\andy\\Downloads\\analysis\\DNCI\\code\\PER-SIMPER-DNCI_Tutorial-master\\PerSIMPER_v3.R")
source("C:\\Users\\andy\\Downloads\\analysis\\DNCI\\code\\PER-SIMPER-DNCI_Tutorial-master\\DNCI_ses_v3.R")
#source("C:\\Users\\andy\\Downloads\\analysis\\DNCI\\code\\PER-SIMPER-DNCI_Tutorial-master\\DNCI_ses_overall_v1.R")
#source("C:\\Users\\andy\\Downloads\\analysis\\DNCI\\code\\PER-SIMPER-DNCI_Tutorial-master\\PerSIMPER_Overall_v1.R")

DNCIndex <- function(spe, Nperm = 1000, dataType = "prab", time_sparse = 180){
  if(dataType == "prab")
    spe <- spe > 0
  
  # filter out no turnover data
  if(sum(spe) == (nrow(spe)*ncol(spe)))
    return("no turnover")
  
  # Ward cluster
  ward <- hclust (dist (spe), method = 'ward.D2')
  grouping <- cutree (ward, 2)
  # CCA <- cca(spe)
  # ordiplot (CCA, display = 'si', type = 'n')
  # points (CCA, pch = grouping, col = grouping)
  # 
  if (length(unique(grouping)) != 2) return("only 1 group")
  # DCNI
  res_DNCI <- DNCI.ses(spe, grouping, Nperm = Nperm, dataTYPE = dataType, count = F, plotSIMPER = F, time_sparse = time_sparse)
  return(list(res = res_DNCI)) 
}

# testing
setwd("C:\\Users\\andy\\Downloads\\analysis\\data")
load(paste0(".\\spe_4_2\\spe_rep", 7, "k", 4, "i", 15, "j", 9, "t", 41
            , ".rdata"))
spe <- dat_spe > 0
DNCIndex(spe, Nperm = 100, dataType = "prab")


