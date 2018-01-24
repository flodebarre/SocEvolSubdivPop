# Script to estimate the probability that two individuals have the same parent,
# when there is some distribution of number of propagules produced
# (instead of the same number for all)

estimateQ <- function(demesize = 4, avePropNb = 200, nrep = 10^6){
  # Parameters
  # demesize  Number of individuals per deme
  # avePropNb expected number of propagules per individual
  # nrep  number of replicates
    
  oneIteration <- function(samplingfunction, alsoTwo = TRUE){
    # Draw numbers of propagules
    # Choose the law
    PropNb <- samplingfunction()
    # Sample the next generation
    NextGen <- sample(1:demesize, demesize, replace = TRUE, prob = PropNb)
    # Compute proba same parent
    sameParent <- 0
    for(i in 1:(demesize-1)){
      for(j in (i+1):demesize){
        if(NextGen[i] == NextGen[j]) sameParent <- sameParent + 1
        }
    }
    sP2 <- NA
    if(alsoTwo){
    # Draw two adults of the next generation
    NextGen2 <- sample(1:demesize, 2, replace = TRUE, prob = PropNb)
    sP2 <- (1*(NextGen[1]==NextGen[2]))
    }
  #  freqSameParent <- sameParent / (demesize* (demesize-1)/2)
    return(c(sameParent, sP2))
  }
  
  # oneIterationTwoIndiv <- function(samplingfunction){
  #   # Draw numbers of propagules
  #   # Choose the law
  #   PropNb <- samplingfunction()
  #   # Draw two adults of the next generation
  #   NextGen <- sample(1:demesize, 2, replace = TRUE, prob = PropNb)
  #   #  freqSameParent <- sameParent / (demesize* (demesize-1)/2)
  #   return(1*(NextGen[1]==NextGen[2]))
  # }
  
  
  PoissonSample <- function(){
    rpois(demesize, avePropNb)
  }
  
  BinomialSample <- function(){
    rbinom(demesize, avePropNb*2, 1/2)  
  }
  
  BinomialSampleUneq <- function(){
    rbinom(demesize, avePropNb*1/0.1, 0.1)  
  }
  
  NoVarSample <- function(){
    rep(avePropNb, demesize)  
  }
  
  vPois <- replicate(nrep, oneIteration(PoissonSample))
  mP1 <- mean(vPois[1,]/(demesize*(demesize-1)/2))
  mP2 <- mean(vPois[2,])
  
  vCst <- replicate(nrep, oneIteration(NoVarSample))
  mC1 <- mean(vCst[1,]/(demesize*(demesize-1)/2))
  mC2 <- mean(vCst[2,])
  
  #  vPois2 <- replicate(nrep, oneIterationTwoIndiv(PoissonSample))
#  mP2 <- mean(vPois2)
  
  facPois <- 1 - avePropNb*demesize*exp( - avePropNb*demesize) - exp(-avePropNb* demesize)

  return(c(mP1 = mP1, mP2 = mP2, mC1 = mC1, mC2 = mC2, facPois = facPois))
}

dsizes <- seq(2, 10, by = 1)
nr <- 10^7
savetab <- as.data.frame(matrix(0, nrow = length(dsizes), ncol = 5))
names(savetab) <- c("mP1", "mP2", "mC1", "mC2", "facPois")

for(i in seq_along(dsizes)){
  print(paste0(i, "/", length(dsizes)))
  savetab[i, ] <- estimateQ(demesize = dsizes[i], nrep = nr)
}

write.csv(cbind(dsizes, savetab), file = "save_probasameparent.csv", row.names = FALSE)

m <- read.csv("save_probasameparent.csv")
m

#vBinom <- replicate(nrep, oneIteration(BinomialSample))
#mean(vBinom)
#hist(vBinom)

#vBinomU <- replicate(nrep, oneIteration(BinomialSampleUneq))
#mean(vBinomU)
#hist(vBinomU)


