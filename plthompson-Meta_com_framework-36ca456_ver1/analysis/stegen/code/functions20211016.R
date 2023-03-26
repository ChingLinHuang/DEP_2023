# 20210412
# Stegen framework
library(tidyverse)
library(vegan)
library(weimea)
library(iCAMP)


# function "perm.test"
#####
# Inputs:
#  pair: matrix with nrow = 2
#  method: method to calculate distance
#  n: number of permuting
# Output:
#  quartile of the observed value
#####
perm.test <- function (pair, method = "enclidean", n = 999){
  d_obs <- vegdist(pair, method = method)
  d <- vector(mode = "numeric", length = n)
  for (i in 1:n){
    pair[1, ] <- pair[1, sample(ncol(pair))]
    d[i] <- vegdist(pair, method = method)
  }
  return(sum(d_obs > d)/1000)
}

perm.test.trait.bmntd <- function (pair, trait, n = 999){
  dist.mat.trait <- dist(trait, diag = T, upper = T) %>% as.matrix
  colnames(dist.mat.trait) <- colnames(pair)
  d_obs <- bmntd(pair, dist.mat.trait) %>% as.numeric
  d <- vector(mode = "numeric", length = n)
  for (i in 1:n){
    pair[1, ] <- pair[1, sample(ncol(pair))]
    d[i] <- bmntd(pair, dist.mat.trait) %>% as.numeric
  }
  return((d_obs - mean(d))/sd(d))
}

# Stegen' framework
# function "stegen"
#####
# Inputs:
#  trait: trait values for each species, row is species and column is trait
#  spe: species composition in each releve, row is site and column is species 
#  method1: difference of trait
#  method2: difference of species composition
#  n1, n2: number of permutation
# Output:
#  res: a list contains "mat and "ratio".
#  n_pair: number of pairs
#  mat: matrix with variables: row and column indices, quartile in stage 1 and stage 2.
#  ratio: fraction of selection (S), drift with dispersal limitation (DL), homogenizing dispersal (HD) and drift only (D).
#  

stegen <- function(spe, trait, method1 = "bmntd", method2 = "euclidean", n1 = 999, n2 = 999){
  nr <- nrow(spe)
  res <- list()
  res$mat <- data.frame(matrix(nrow = nr*(nr - 1)/2, ncol = 4))
  colnames(res$mat) <- c("row", "col", "stage_1", "stage_2")
  res$ratio <- c(0,0,0,0)
  names(res$ratio) <- c("S", "DL", "HD", "D")
  res$n_pair <- nr*(nr - 1)/2
  
  k <- 1
  for (i in 1:(nr-1)){
    for (j in (i+1):nr){
      print(paste("j = ", j))
      bNTI <- perm.test.trait.bmntd(pair = spe[c(i,j), ], trait = trait, n = n1)
      if ((bNTI < -2) || (bNTI > 2)){
        res$ratio[1] <- res$ratio[1] + 1
        p2 <- NA
      } else {
        p2 <- perm.test(pair = spe[c(i,j), ], method = method2, n = n2)
        if (p2 > 0.9) {
          res$ratio[2] <- res$ratio[2] + 1
        }else if (p2 < 0.1) {
          res$ratio[3] <- res$ratio[3] + 1
        }else {
          res$ratio[4] <- res$ratio[4] + 1
        }
      }
      
      res$mat[k, ] <- c(i, j, bNTI, p2)
      k <- k + 1
    }
  }
  res$ratio <- res$ratio / res$n_pair
  
  return (res)
}

# example
res <- stegen(spe, trait)
