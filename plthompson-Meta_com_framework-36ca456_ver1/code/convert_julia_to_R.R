############################
#---------------------------
# Summary of model.df:
# colnames:
# N: size of population of the species
# lambda: (abiotic term + biotic term)
# density: biotic term 
# env_match: abiotic term (gaussian niche)
# env: environment value in each patch at each period
# Species: species id number
# Time: time period
# Patch: patch id number
# z: species trait value (niche optima)
# dispersal: parameter for binomial distribution for generating number of emigration
# sig_niche: niche breadth (for defining abiotic term) from 0.001 to 10 (13 levels cut uniformly after log-transformed)
# alpha: competition parameters (0.5, 1.5, "equal", "patch_dynamics")



library(data.table)
library(readr)
reps <- 20


setwd("C:\\Users\\andy\\Downloads\\plthompson-Meta_com_framework-36ca456_ver1\\outputs")
for(i in 1:reps){
  print(i)
  #model.df <- read_csv("/Volumes/LaCie Mac/outputs/outputfile_1.csv")

  model.df <- read_csv(paste("./outputfile_", i, "_", 2, ".csv", sep = "")) 
  save(model.df, file = paste("./rdata/outputfile_", i, "_", 1, ".RData", sep =""))
}
                     