library(adespatial)
library(tidyverse)
library(som.nn)
library(vegan)

### variation partitioning (ref: Borcard et al. NumEcol with R)
VP <- function(spe, env, dist.mat){
  eigenfunc <- dbmem(as.dist(dist.mat))
  spe <- decostand(spe, method = "standardize")
  # env 
  env.rda <- rda(X = spe, Y = env)
  anova(env.rda)
  RsquareAdj(env.rda)
  
  # spatial
  spatial.rda <- rda(spe ~ ., eigenfunc)
  RsquareAdj(spatial.rda)
  
  # env + spatial
  part <- varpart(spe, env, eigenfunc)
  
  # part <- varpart(spe, env, eigenfunc.fwd)
  return(list(part = part))
}



