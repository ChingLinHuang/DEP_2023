
library(vegan)
setwd("C:\\Users\\andy\\Downloads\\analysis\\data")
load("spe_3\\spe_rep16env2k1i12j8.rdata")
write.csv(dat_spe, file = "spe_3\\spe_rep16env2k1i12j8.csv")
load("trait_3\\trait_rep16env2k1i12j8.rdata")
write.csv(dat_trait, file = "trait_3\\trait_rep16env2k1i12j8.csv")

spe <- dat_spe
trait <- dat_trait

comdistnt_v1 <- function (comm, dis, abundance.weighted = FALSE, exclude.conspecifics = FALSE) {
  N <- dim(comm)[1]
  comm <- decostand(comm, method = "total", MARGIN = 1)
  comdisnt <- matrix(nrow = N, ncol = N)
  for (i in 1:(N - 1)) {
    for (j in (i + 1):N) {
      sppInSample1 <- colnames(comm[i, comm[i, ] > 0, 
                                    drop = FALSE])
      sppInSample2 <- colnames(comm[j, comm[j, ] > 0, 
                                    drop = FALSE])
      if ((length(sppInSample1) >= 1) && (length(sppInSample2) >= 
                                          1)) {
        sample.dis <- dis[sppInSample1, sppInSample2, 
                          drop = FALSE]
        if (exclude.conspecifics) {
          sample.dis[sample.dis == 0] <- NA
        }
        sample1NT <- apply(sample.dis, 1, min, na.rm = TRUE)
        sample1NT[sample1NT == Inf] <- NA
        sample2NT <- apply(sample.dis, 2, min, na.rm = TRUE)
        sample2NT[sample2NT == Inf] <- NA
        if (abundance.weighted) {
          sample1.weights <- as.numeric(comm[i, sppInSample1])
          sample2.weights <- as.numeric(comm[j, sppInSample2])
          if (any(is.na(sample1NT))) {
            miss <- which(is.na(sample1NT))
            sample1NT <- sample1NT[-miss]
            sample1.weights <- sample1.weights[-miss]
            sample1.weights <- sample1.weights/sum(sample1.weights)
          }
          if (any(is.na(sample2NT))) {
            miss <- which(is.na(sample2NT))
            sample2NT <- sample2NT[-miss]
            sample2.weights <- sample2.weights[-miss]
            sample2.weights <- sample2.weights/sum(sample2.weights)
          }
          sampleNT <- c(sample1NT, sample2NT)
          sample.weights <- c(sample1.weights, sample2.weights)
          comdisnt[i, j] <- weighted.mean(sampleNT, 
                                          sample.weights, na.rm = TRUE)
        }
        else {
          comdisnt[i, j] <- mean(c(sample1NT, sample2NT), 
                                 na.rm = TRUE)
        }
      }
      else {
        comdisnt[i, j] <- NA
      }
    }
  }
  rownames(comdisnt) <- colnames(comdisnt) <- rownames(comm)
  return(as.dist(t(comdisnt)))
}


bNTI <- function(spe, trait, beta.reps = 999){
  # distance matrix of trait
  dist_trait <- as.matrix(dist(trait))
  
  # empirical beta mntd
  beta.mntd.weighted = as.matrix(comdistnt(comm = spe, dis = dist_trait, abundance.weighted = T))

  # null model
  rand.weighted.bMNTD.comp = array(c(-999), 
                                   dim = c(nrow(spe), nrow(spe), 
                                           beta.reps))
  dim(rand.weighted.bMNTD.comp)
  
  for (rep in 1:beta.reps){
    rand.weighted.bMNTD.comp[ , ,rep] = as.matrix(comdistnt(spe, taxaShuffle(dist_trait), abundance.weighted = T))
    
    print(c(date(),rep))
  }
  
  weighted.bNTI = matrix(c(NA),nrow = nrow(spe), ncol = nrow(spe))
  dim(weighted.bNTI)
  
  for (columns in 1:(nrow(spe)-1)) {
    for (rows in (columns+1):nrow(spe)) {
      rand.vals = rand.weighted.bMNTD.comp[rows,columns,]
      weighted.bNTI[rows,columns] = (beta.mntd.weighted[rows,columns] - mean(rand.vals)) / sd(rand.vals)
      rm("rand.vals")
    }
  }
  rownames(weighted.bNTI) = rownames(spe)
  colnames(weighted.bNTI) = rownames(spe)
  return(weighted.bNTI) 
}

# bNTI(spe, trait, beta.reps = 100)

