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
  env1Scale <- 500
  
  landscape_rep<-landscape_generate(patches = patches, time_steps = time_steps, env1Scale = env1Scale, spatial_env = TRUE, temporal_env = TRUE, burn_in = 200)
  env.df<-landscape_rep$env.df
  disp_mat<-landscape_rep$disp_mat
  landscape<-landscape_rep$landscape
  
  write.csv(x = env.df, file = paste("./data/landscape_data/env_",r,".csv",sep = ""))
  write.csv(x = disp_mat, file = paste("./data/landscape_data/disp_mat_",r,".csv",sep = ""))
  write.csv(x = landscape, file = paste("./data/landscape_data/landscape_",r,".csv",sep = ""))
  write.csv(x = data.frame(Tmax = landscape_rep$Tmax, burn_in = landscape_rep$burn_in), file = paste("./data/landscape_data/time_",r,".csv",sep = ""))
}

setwd("C:\\Users\\andy\\Downloads\\plthompson-Meta_com_framework-36ca456_ver1")

r = 10
env.df <- read.csv(paste0("./data/landscape_data/env_",r,".csv"))
# fluctuation plot
# windows()
ggplot(env.df, aes(x = time, y = env1, group = interaction(x,y), color = interaction(x,y)))+
  geom_line()+
  scale_color_viridis_d(guide = F)

# distribution plot
# windows()
ggplot(env.df[env.df$time == 2200, ], aes(x = x, y = y, color = env2)) +
  geom_point(size = 3) +
  scale_color_gradient(low="blue", high="yellow")
# env1 has temporal fluctuation, env2 does not.
