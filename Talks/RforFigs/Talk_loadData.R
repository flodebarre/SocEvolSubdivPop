# List all data files that have been generated
myFiles <- list.files(path = paste0("../../Programs/C/Results/HtgIsld", thed, "/"), pattern="HtgIsl*")

##
## Generate table of all parameter combinations
##
# Obtain parameter values from the file name
GetParms <- function(fileName){
  # Extract parameters, separated by "_"
  prs <- strsplit(fileName, split = "_")[[1]]
  # Life-cycle information (remove file prefix)
  prs[1] <- strsplit(prs[1], split="HtgIsl")[[1]][2]
  # Htg information (remove "htg")
  prs[7] <- strsplit(prs[7], split="htg")[[1]][2]
  # Remove ".txt"
  prs[7] <- strsplit(prs[7], split=".txt")[[1]][1]
  return(prs)
}
# Assemble parameters for all data files
tmp <- matrix(unlist(lapply(myFiles, GetParms)), byrow = TRUE, nrow = length(myFiles))
# Need to separate numbers and text
#  Numeric values
numPars <- data.frame(matrix(as.numeric(tmp[,2:7]), nrow = length(myFiles)))
names(numPars) <- c("mig", "mB", "p", "mu", "sel", "htg")
#  Text
updPars <- data.frame(matrix(tmp[,1], ncol=1), stringsAsFactors = FALSE)
names(updPars) <- c("upd")
# Combine them in single data frame
allParms <- cbind(updPars, numPars)

##
## Load simulation data
##
getSimData <- function(fN){
  # Prepend path to file
  fileName <- paste0("../../Programs/C/Results/HtgIsld", thed, "/", fN)
  # Check that the file exists and has size>0
  if(file.exists(fileName)){
    sz <- file.info(fileName)$size
    if(sz>1) m <- read.table(fileName) else m <- NA # Remove empty files to avoid errors
  } else {
    m <- NA
  }
  return(m)
}
res <- lapply(myFiles, getSimData)
head(res)

# Note: 
#  with heterogeneous deme sizes, the total population size may vary!

ComputeStats <- function(i){
  v <- unname(unlist(res[[i]])) # Extract the vector of results
  popsize <- length(v)-1 # Max population size (-1 because nn+1 elements from 0 to nn)
  nbAs <- seq(0, popsize) # Vector of abundances
  nreplicates <- sum(v) # Number of data points
  m <- sum(v*nbAs)/nreplicates # Compute the mean number of A individuals in the simulations
  propA <- m/popsize # Convert into proportion of type-A individuals
  dci <- 1.960 * sqrt(propA * (1-propA) / nreplicates) # Confidence interval for the proportion
  return(c(pA = propA, dci = dci, popsize = popsize))
}

# Compute some stats on the results
# (mean prop of A, population size)
simStats <- lapply(seq_along(myFiles), ComputeStats)
tmp <- data.frame(matrix(unlist(simStats), byrow = TRUE, ncol = 3))
names(tmp) <- c("pA", "dci", "popsize")
head(tmp)

# Combine with parameter values
alldata <- cbind(allParms, tmp, fileName=myFiles)
head(alldata)

# Get unique parameter lists
updList <- sort(unique(alldata$upd))
migList <- sort(unique(alldata$mig))
mBList <- sort(unique(alldata$mB))
pList <- sort(unique(alldata$p))
mutList <- sort(unique(alldata$mu))
selList <- sort(unique(alldata$sel))
htgList <- sort(unique(alldata$htg))

p <- pList[1]
