###############################
#---------------------
# Write out data
#---------------------
## data
# env_r.csv: env.df
# disp_mat_r.csv: disp_map
# landscape_r.csv: landscape
# time_r.csv: Tmax and burn_in
#----------------------
################################

setwd("C:\\Users\\andy\\Downloads\\plthompson-Meta_com_framework-36ca456_ver1")
source("./code/model_functions.R")

reps <- 30

for(r in 1:reps){
  print(r)
  time_steps <- 2000
  patches <- 100
  species <- 50
  
  landscape_rep<-landscape_generate(patches = patches, time_steps = time_steps, spatial_env = TRUE, temporal_env = TRUE, burn_in = 200)
  env.df<-landscape_rep$env.df
  disp_mat<-landscape_rep$disp_mat
  landscape<-landscape_rep$landscape
  
  write.csv(x = env.df, file = paste("./data/landscape_data_v1/env_",r,".csv",sep = ""))
  write.csv(x = disp_mat, file = paste("./data/landscape_data_v1/disp_mat_",r,".csv",sep = ""))
  write.csv(x = landscape, file = paste("./data/landscape_data_v1/landscape_",r,".csv",sep = ""))
  write.csv(x = data.frame(Tmax = landscape_rep$Tmax, burn_in = landscape_rep$burn_in), file = paste("./data/landscape_data_v1/time_",r,".csv",sep = ""))
}

