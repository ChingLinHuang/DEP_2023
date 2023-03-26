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
######################################
#### "trait_generate" function
#---------------------------------
## Inputs ##
# species: number of species in the metacommunity
# type: archetypes of metacommunity
# sig_p2: not being used
#---------------------------------
## Outputs ##
# list contains:
# B: competition parameter matrix
# env.traits: niche optima (trait value) of each species
#---------------------------------
######################################

######################################
# 2021/10/04
# 
# Set different strength of autocorrelation (var, scale)
# 
# 
# 
######################################


landscape_generate <- function(patches = 40, time_steps = 2000, spatial_env = TRUE, temporal_env = TRUE, burn_in = 200) {
  library(ggplot2)
  library(som.nn)
  Tmax <- time_steps +burn_in
  
  repeat {
    landscape <- round(data.frame(x = runif(patches, min = 1, max = 100), y = runif(patches, min = 1, max = 100)))
    if(dim(unique(landscape))[1] == dim(landscape)[1]) {break} # no repeated patches
  }
  
  dist_mat <- as.matrix(dist.torus(coors = landscape)) # distance
  
  disp_mat <- exp(-0.1*dist_mat) # dispersal
  diag(disp_mat) <- 0 # no dispersal within patch
  disp_mat <- apply(disp_mat, 1, function(x) x/sum(x)) # Every patch disperse the same number of individuals
  
  env.df <- data.frame(env1 = 0,
                       env2 = 0,
                       env3 = 0,
                       env4 = 0,
                       x = landscape$x, 
                       y = landscape$y, 
                       time = rep((1+burn_in):Tmax,each = patches))
  ### generate spatially autocorrelated environment###
  require(RandomFields)
  require(vegan)
  require(dplyr)
  # different scenarios of spatial and temporal autocorrelation
  autocorr <- data.frame(var = c(0.5,0.5,50,50), 
                         scale = c(500, 50000, 500, 50000))
  for (i in 1:4){
  repeat {
    model <- RMexp(var = autocorr$var[i], scale= autocorr$scale[i]) + # with variance 4 and scale 10
      RMnugget(var=0) + # nugget
      RMtrend(mean=0.05) # and mean
    
    RF <- RFsimulate(model = model,x = landscape$x*10, y = landscape$y*10, T = (1+burn_in):Tmax)
    env.df[ ,i] <- decostand(RF$variable1,method = "range")
    burn.env.i <- env.df[env.df$time == burn_in+1, i]
    if((max(burn.env.i)-min(burn.env.i)) > 0.6 || i < 3) {break} # make sure the environment is not homogeneous
  }
  
  ecum <- ecdf(env.df[ ,i]) # empirical distribution function
  env.cum <- ecum(env.df[ ,i]) 
  env.df[ ,i] <- env.cum # "smooth" the environment data
  }
  
  # burn-in time env
  burn.env <- env.df[env.df$time == burn_in+1, ] 
  burn.env <- burn.env[rep(1:patches, time = burn_in), ]
  burn.env$time <- rep(1:burn_in,each = patches)
  env.df <- bind_rows(burn.env, env.df)
  
  # windows()
  # fluctuation plot
  ggplot(env.df, aes(x = time, y = env1, group = interaction(x,y), color = interaction(x,y)))+
    geom_line()+
    scale_color_viridis_d(guide = F)
  # windows()
  # Env in final state
  ggplot(env.df[env.df$time == 2200, ], aes(x = x, y = y, color = env1)) +
    geom_point(size = 5) +
    scale_color_gradient(low="blue", high="yellow")
  
  return(list(env.df = env.df, disp_mat = disp_mat, landscape = landscape, Tmax = Tmax, burn_in = burn_in))
}

trait_generate <- function(species = 25, type, sig_p2 = 0.5){
  #environmental trait
  env.traits <- data.frame(z1 = seq(from = 0,to = 1,length = species), z2 = runif(n = species,min = 0,max = 1)) 
  env.traits <- env.traits[sample(1:species, size = species, replace = F),]
  
  bdiag1<- 1
  # no interspecific interaction
  if(type == "no_inter"){
    B <- matrix(0, nrow = species, ncol = species)
    
  }
  
  # interspecific interaction = intraspecific interaction
  if(type == "neutral"){
    B <- matrix(1, nrow = species, ncol = species)
  }
  
  # interspecific interaction < intraspecific interaction 
  if(type == "competitive"){
    B <- matrix(runif(n = species*species)*0.5, nrow = species, ncol = species) 
  }
  
  # interspecific interaction > intraspecific interaction
  if(type == "priority"){
    B <- matrix(runif(n = species*species)*1.5, nrow = species, ncol = species)
  }
  
  # some species are the strong competitors  
  if(type == "patch_dynamics"){
    B <- matrix(runif(n = species*species)*0.5, nrow = species, ncol = species)
    B[1:round(species*0.3),] <- 1.25+runif(n=round(species*0.3),min = -0.25,max = 0.25)
    B[lower.tri(B)]<- matrix(runif(n = species*species)*0.5, nrow = species, ncol = species)[lower.tri(matrix(runif(n = species*species)*0.5, nrow = species, ncol = species))]
  }
  diag(B)<-bdiag1
  B <- B*0.05
  
  return(list(B=B, env.traits=env.traits))
}
