####################################
#----------------------------------
# Prepare model function
#---------------------------------
# "landscape_generate" function
#---------------------------------
## Inputs ##
# patches
# time_steps
# envlScale
# spatial_env, temporal_env: not being used
# burn_in: stable env times
#---------------------------------
## Outputs ##
# a list contains:
# env.df: dataframe with coordinates and env1 and env2 of each patch in each time scale
# disp_mat: dispersal rate of each pair of patches
# landscape: coordinates of each patch
# Tmax: maximum time 
# burn_in: stable env times
#---------------------------------

library(ggplot2)
library(som.nn)
require(RandomFields)
require(vegan)
require(dplyr)
landscape_generate <- function(patches = 100, time_steps = 2000, 
                               env1Scale = 500, burn_in = 200) {
  
  ## Total number of iterations
  Tmax <- time_steps + burn_in
  
  ## create non-repeated patches
  repeat {
    landscape <- round(data.frame(x = runif(patches, min = 1, max = 100),
                                  y = runif(patches, min = 1, max = 100)))
    if(dim(unique(landscape))[1] == dim(landscape)[1]) {break}
  }
  
  ## distance matrix
  dist_mat <- as.matrix(dist.torus(coors = landscape)) # distance
  
  ## dispersal matrix 
  disp_mat <- exp(-0.1*dist_mat) # dispersal
  diag(disp_mat) <- 0 # no dispersal within patch
  disp_mat <- apply(disp_mat, 1, function(x) x/sum(x)) 
  # colSums(disp_mat) == 1
  # hist(as.numeric(disp_mat))
  
  # # test the argument for strength of dispersal limitation 
  # b = 0.01
  # disp_mat_1 <- disp_mat^b %*% diag(1 / colSums(disp_mat^b))
  # colSums(disp_mat_1) == 1
  # boxplot(as.numeric(disp_mat_1))
  
  
  ####generate spatially autocorrelated environment####
  ## fluctuated environment
  repeat {
    model <- RMexp(var = 0.5, scale = env1Scale) + 
      RMnugget(var = 0) + # nugget
      RMtrend(mean = 0.05) # and mean
    
    RF <- RFsimulate(model = model,x = landscape$x*10, y = landscape$y*10,
                     T = (1+burn_in):Tmax)
    env.df <- data.frame(env1 = decostand(RF$variable1, method = "range"),
                         x = landscape$x, y = landscape$y,
                         time = rep((1+burn_in):Tmax, each = patches))
    burn.env1 <- env.df[env.df$time == burn_in+1,]
    if((max(burn.env1$env1) - min(burn.env1$env1)) > 0.6) {break} 
    # make sure the environment is not homogeneous
  }
  
  burn.env1$time <- NULL
  burn.env <- left_join(data.frame(time = rep(1:burn_in,each = patches),
                                   x = landscape$x, 
                                   y = landscape$y), 
                        burn.env1)
  env.df <- bind_rows(burn.env,env.df)
  
  ## fixed environment
  model <- RMexp(var = 0.5, scale = 100) + 
    RMnugget(var = 0) + # nugget
    RMtrend(mean = 0.05) # and mean
  RF <- RFsimulate(model = model,
                   x = landscape$x,
                   y = landscape$y, 
                   T = 1) # no temporal fluctuation
  env.df$env2<-c(decostand(RF$variable1,method = "range"))
  
  ## smoothing
  ecum <- ecdf(env.df$env1) # empirical distribution function
  env.cum <- ecum(env.df$env1) 
  env.df$env1 <- env.cum # "smooth" the environment data
  
  ecum <- ecdf(env.df$env2)
  env.cum <- ecum(env.df$env2)
  env.df$env2 <- env.cum
  
  return(list(env.df = env.df, disp_mat = disp_mat, landscape = landscape,
              Tmax = Tmax, burn_in = burn_in))
}



