# Parameters
demesize <- 4
avePropNb <- demesize*20 # expected number of propagules per individual

oneIteration <- function(samplingfunction){
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
  freqSameParent <- sameParent / (demesize* (demesize-1)/2)
  return(freqSameParent)
}

PoissonSample <- function(){
  rpois(demesize, avePropNb)
}

BinomialSample <- function(){
  rbinom(demesize, avePropNb*2, 1/2)  
}

BinomialSampleUneq <- function(){
  rbinom(demesize, avePropNb*1/0.1, 0.1)  
}

nrep <- 10^6
vPois <- replicate(nrep, oneIteration(PoissonSample))
hist(vPois*demesize)
mean(vPois)

vBinom <- replicate(nrep, oneIteration(BinomialSample))
mean(vBinom)
hist(vBinom)
