## Initializations ##
# Clear the memory
rm(list = ls())
set.seed(123456789)

drawDemeSizes <- function(N = 1000, probs = rep(1/100, 100)){
  # N       Total population size
  # probs   Probabilities of joining a given group;
  #          vector length indicates maximum number of groups
  chooseGroup <- sample(seq_along(probs), N, probs, replace = TRUE)
  groupAbundances <- rep(0, length(probs))
  for(i in seq_along(probs)){
    groupAbundances[i] <- sum(chooseGroup == i)
  }
  return(groupAbundances)
}

simulation <- function(m=0.01, mutationProba = 0.05, selStrength = 0.05){
# Arguments:
#  m             emigration probability
#  mutationProba mutation probability
#  selStrength   strength of selection

# Parameters
demeSizes <- drawDemeSizes(N = 1000, probs = rep(1/100, 100)) # Number of sites in a deme
nbDemes <- length(demeSizes) # Number of demes in the population (N = demesize*nbdemes)
totalSites <- sum(demeSizes)

# Demographic parameters
deathProba <- 0.1
baselineBirthProba <- 1.0

mutationBias <- 0.5

# Simulation parameters
ntimesteps <- 1000000 # Max number of time steps



# Social parameters
b <- 5
c <- 1

# Initializations
population <- 1+0*matrix(1:(2*nbDemes), ncol = 2)
population[demeSizes < 2, ] <- 0
  # dimension 1: deme ID
  # dimension 2: type of site (1: type A, 2: type B)
popInTime <- matrix(1:(ntimesteps*2), ncol = 2)

global <- colSums(population)
global

IDs <- c(1:(nbDemes), -(1:(nbDemes)))

birthPropensities <- 0*population
 # column 1: typeA,
 # column 2: typeB.
for(i in 1:nbDemes){
  iA <- population[i,1]
  iB <- population[i,2]
  availableSpaceHome <- (demeSizes[i] - iA - iB)/demeSize[i]
  availableSpaceAway <- ( sum(demeSizes[-i]) - (global[1]+global[2] - iA - iB))/ (sum(demeSizes[-i]))
  birthPropensities[i,1] <- (baselineBirthProba + selStrength * ((population[i,1]-1)*b - c)) * ((1-m)*availableSpaceHome + m * availableSpaceAway) * population[i,1]
  birthPropensities[i,2] <- (baselineBirthProba + selStrength * ((population[i,1])*b )) * ((1-m)*availableSpaceHome + m * availableSpaceAway) * population[i,2]
}
birthPropensities


for (itime in 1:ntimesteps){
  totBirth <- sum(birthPropensities)
  totDeath <- sum(global) * deathProba
  isBirth <- rbinom(1, 1, prob = totBirth/(totDeath+totBirth))

  if(isBirth == 1){
  # Pick who reproduces
    reproducing <- sample(IDs, 1, prob = c(birthPropensities))
    if(reproducing<0) typeParent <- 2 else typeParent <- 1
    locParent <- abs(reproducing)
  # Where to
    iA <- population[locParent,1]
    iB <- population[locParent,2]
    availableSpaceHome <- (demeSizes[locParent] - iA - iB)/demeSize[locParent]
    availableSpaceAway <- (sum(demeSizes[-i]) - (global[1]+global[2] - iA - iB))/ (sum(demeSizes[-i]))
    isHome <- rbinom(1, 1, prob = availableSpaceHome / (availableSpaceAway + availableSpaceHome))
    if(isHome == 1){
      locChild <- locParent
    }else{ # Reproduces away
      possibleLocations <- (1:nbDemes)[-locParent]
      emptySpaces <- demeSizes[possibleLocations] - (population[possibleLocations, 1] + population[possibleLocations, 2])
      locChild <- sample(possibleLocations, 1, prob = emptySpaces)
    }
  # Mutation?
    typeChild <- typeParent
    isMut <- rbinom(1, 1, prob = mutationProba)
    if(isMut == 1){
      typeChild <- 2 - rbinom(1, 1, prob = mutationBias)
    }
  # Update state of the population
    population[locChild, typeChild] <- population[locChild, typeChild] + 1
    #
    #
    # DEATH
  }else{
    dying <- sample(IDs, 1, prob = c(population)) # same probas of dying for every one, so proportional to number
    if(dying<0) typeDead <- 2 else typeDead <- 1
    locDead <- abs(dying)

    # Update population state
    population[locDead, typeDead] <- population[locDead, typeDead] - 1
  }
  global <- colSums(population)
  # Recompute birth propensities
    for(i in 1:nbDemes){
      iA <- population[i,1]
      iB <- population[i,2]
      availableSpaceHome <- (demeSizes[i] - iA - iB)/demeSize[i]
      availableSpaceAway <- ( sum(demeSizes[-i]) - (global[1]+global[2] - iA - iB))/ (sum(demeSizes[-i]))
      birthPropensities[i,1] <- (baselineBirthProba + selStrength * ((population[i,1]-1)*b - c)) * ((1-m)*availableSpaceHome + m * availableSpaceAway) * population[i,1]
      birthPropensities[i,2] <- (baselineBirthProba + selStrength * ((population[i,1])*b )) * ((1-m)*availableSpaceHome + m * availableSpaceAway) * population[i,2]
    }
  popInTime[itime,] <- global
}

#plot(popInTime[,1], type="l", ylim = c(0,nbDemes*demeSize))
#lines(popInTime[,2], type="l", col=2)

#plot((popInTime[,1]+popInTime[,2])/(nbDemes*demeSize), type="l", ylim = c(0,1), main="Site occupancy")

#plot((popInTime[,1])/(popInTime[,1]+popInTime[,2]), type="l", ylim=c(0,1), main="Proportion of altruists")
#abline(h=mutationBias, lty=3)
out <- mean((popInTime[,1])/(popInTime[,1]+popInTime[,2]))
return(out)
}

require(parallel)
mList <- c(0.001, 0.01, 0.1, 0.15, 0.2)
mutList <- c(0.001, 0.01, 0.1, 0.5)
selList <- c(0.05)

pars <- expand.grid(m = mList, mutationProba = mutList, selStrength = selList)
res <- mclapply(seq_len(nrow(pars)), #
                function(i) do.call(simulation, pars[i,]))
