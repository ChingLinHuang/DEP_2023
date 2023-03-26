# 
# 
# 
# 

library(vegan)
library(tidyverse)
library (twinspanR)
library(tictoc)
library(DNCImper)

#source("C:\\Users\\andy\\Downloads\\analysis\\DNCI\\code\\PER-SIMPER-DNCI_Tutorial-master\\PerSIMPER_v2.R")
#source("C:\\Users\\andy\\Downloads\\analysis\\DNCI\\code\\PER-SIMPER-DNCI_Tutorial-master\\DNCI_ses_v2.R")
source("C:\\Users\\andy\\Downloads\\analysis\\DNCI\\code\\PER-SIMPER-DNCI_Tutorial-master\\DNCI_ses_overall_v1.R")
source("C:\\Users\\andy\\Downloads\\analysis\\DNCI\\code\\PER-SIMPER-DNCI_Tutorial-master\\PerSIMPER_Overall_v1.R")

DNCIndex_multiGroup <- function(spe, Nperm = 99, dataType = "prab"){
  grouping <- 1:nrow(spe)
  # CCA <- cca(spe)
  # ordiplot (CCA, display = 'si', type = 'n')
  # points (CCA, pch = grouping, col = grouping)

  # DCNI
  res_DNCI <- DNCI.ses_overall(spe, grouping, Nperm = Nperm, dataTYPE = dataType, count = F, plotSIMPER = F)
  return(list(res = res_DNCI)) 
}

# load("C:\\Users\\andy\\Downloads\\analysis\\data\\spe_4_1\\spe_rep1k1i8j8t60.rdata")
# date()
# DNCI(dat_spe, Nperm = 99, dataType = "count") -> res_1
# date()
# DNCI(dat_spe, Nperm = 99, dataType = "prab") -> res_2
# date()            
#             
