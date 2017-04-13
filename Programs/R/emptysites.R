rm(list = ls())

simulation <- function(m=0.01, mutationProba = 0.05, selStrength = 0.05){
# Arguments:
#  m             emigration probability
#  mutationProba mutation probability
  
# Parameters
demeSize <- 4 # Number of sites in a deme
nbDemes <- 30 # Number of demes in the population (N = demesize*nbdemes)

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
population
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
  availableSpaceHome <- (demeSize - iA - iB)/demeSize
  availableSpaceAway <- ((nbDemes -1)*demeSize - (global[1]+global[2] - iA - iB))/ ((nbDemes -1)*demeSize)
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
    locparent <- abs(reproducing)
  # Where to
    iA <- population[locparent,1]
    iB <- population[locparent,2]
    availableSpaceHome <- (demeSize - iA - iB)/demeSize
    availableSpaceAway <- ((nbDemes -1)*demeSize - (global[1]+global[2] - iA - iB))/ ((nbDemes -1)*demeSize)
    isHome <- rbinom(1, 1, prob = availableSpaceHome / (availableSpaceAway + availableSpaceHome))
    if(isHome == 1){
      locChild <- locparent
    }else{ # Reproduces away
      possibleLocations <- (1:nbDemes)[-locparent]
      emptySpaces <- demeSize - (population[possibleLocations, 1] + population[possibleLocations, 2]) 
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
  # Recompute birth propensities
    for(i in 1:nbDemes){
      iA <- population[i,1]
      iB <- population[i,2]
      availableSpaceHome <- (demeSize - iA - iB)/demeSize
      availableSpaceAway <- ((nbDemes -1)*demeSize - (global[1]+global[2] - iA - iB))/ ((nbDemes -1)*demeSize)
      birthPropensities[i,1] <- (baselineBirthProba + selStrength * ((population[i,1]-1)*b - c)) * ((1-m)*availableSpaceHome + m * availableSpaceAway) * population[i,1]
      birthPropensities[i,2] <- (baselineBirthProba + selStrength * ((population[i,1])*b )) * ((1-m)*availableSpaceHome + m * availableSpaceAway) * population[i,2]
    }
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
selList <- c(0.05, 0.2)

pars <- expand.grid(m = mList, mutationProba = mutList, selStrength = selList)
res <- mclapply(seq_len(nrow(pars)), #
                function(i) do.call(simulation, pars[i,]))

