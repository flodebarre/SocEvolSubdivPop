for(i in dev.list())dev.off()

# for mutt in 0.001 0.01 0.1 0.25
# do
# for mB in 15.0
# do
# for mp in 0.45
# do
# for mig in 0.01 0.1 0.2 0.3 0.4 0.5 0.75 0.9 
# do
# for omega in 0.05 0.1 0.5
# do
# for ishtg in 0 1

# ProBLem Was Missing UPDATING in file name...

specify_decimal <- function(x, k) format(round(x, k), nsmall=k)

# Parameters tested
mutList <- c(0.001, 0.01, 0.1, 0.25)
migList <- c(0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 0.9)
selList <- c(0.05, 0.1, 0.5)
mBList <- c(15.0)
htgList <- c(0, 1)
updList <- c("WF", "BD", "DB")
p <- 0.45

pars <- expand.grid(mu=mutList, mig=migList, sel=selList, mB = mBList, htg = htgList, upd = updList)

# Load simulation data
getSimData <- function(mu, mig, sel, mB, htg, upd){
  fileName <- paste0("../C/Results/HtgIsl", upd,"_",mig,"_", specify_decimal(mB, 1), "_", p, "_",mu,"_",sel, "_htg", htg, ".txt")
  sz <- file.info(fileName)$size
  if(sz>1) m <- read.table(fileName) else m <- NA
  return(m)
}
res <- lapply(seq_len(nrow(pars)), function(i) do.call(getSimData, pars[i,]))

# Note: 
#  with heterogeneous deme sizes, the total population size may vary!

computeStats <- function(i){
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
simStats <- lapply(seq_len(nrow(pars)), computeStats)
tmp <- data.frame(matrix(unlist(simStats), byrow = TRUE, ncol = 3))
names(tmp) <- c("pA", "dci", "popsize")

# Combine with parameter values
alldata <- cbind(pars, tmp)
alldata

alldata[is.na(alldata$pA),]

plotDist <- function(i){
  v <- unname(unlist(res[[i]])) # Extract the vector of results
  nn <- length(v)-1 # Max population size
  nAs <- seq(0, nn) # Vector of abundances
  plot(nAs, v)
}


################################################################
## PLOTTING

  # PROPORTION OF ALTRUISTS IN THE POPULATION
  # nA / (nA + nB)
PlotProp <- function(upd, sel, htg, ylim=c(0,1), addAnalysis=FALSE){
    par(las = 1)
    plot(0, type = "n", xlim = c(0,max(migList)), ylim = ylim, 
         xlab = "Emigration m", ylab = "Proportion of altruists", axes = FALSE)
    title(main = paste0("sel=", sel, ", htg=", htg))
    colMut <- seq_along(mutList)
    rect(0, 0, par("usr")[2], par("usr")[4], col = 
           "grey")
    for(i in seq(0,1,by=0.01)) abline(h=i, lty=1, col=gray(1))
    abline(h=0.45, col="black", lty=2)
    for(imu in  seq_along(mutList)){
      if(addAnalysis){ # Add analytical prediction
        # Define a function of mig for the specific set of parameters
        tmpP <- function(x) get(paste0("p", upd))(b=mBList[1], c=1, p=p, sel=sel, mut=mutList[imu], m=x, g=0, n=4, d=30, Idself=1, Ieself=0)
        # Plot it
        curve(tmpP, from=0, to=max(migList), col=colMut[imu], add = TRUE)
      }
      # Simulation Data: extract the relevant data for the set of parameters
      subdata <- alldata[alldata$mu == mutList[imu] & alldata$upd == upd,]
      sub <- subdata[subdata$sel== sel & subdata$htg==htg, ]
      # Plot estimated frequency
      points(sub$mig, sub$pA, col = colMut[imu], type="p", pch=4)
      # Plot CI
      arrows(sub$mig, sub$pA - sub$dci, sub$mig, sub$pA + sub$dci, col = colMut[imu], angle = 90, length = 0.1, code = 3)
    }
    axis(1, pos = 0)
    axis(2, pos = 0)
  }

PlotProp("WF", 0.05, 1)
PlotProp("WF", 0.05, 0, addAnalysis = TRUE)

PlotProp("WF", 0.1, 1)
PlotProp("WF", 0.1, 0, addAnalysis = TRUE)

PlotProp("WF", 0.5, 1)
PlotProp("WF", 0.5, 0, addAnalysis = TRUE)


PlotProp("BD", 0.05, 1)
PlotProp("BD", 0.05, 0, addAnalysis = TRUE)

PlotProp("BD", 0.1, 1)
PlotProp("BD", 0.1, 0, addAnalysis = TRUE)

PlotProp("BD", 0.5, 1)
PlotProp("BD", 0.5, 0, addAnalysis = TRUE)

PlotProp("DB", 0.05, 1)
PlotProp("DB", 0.05, 0, addAnalysis = TRUE)

PlotProp("DB", 0.1, 1)
PlotProp("DB", 0.1, 0, addAnalysis = TRUE)

PlotProp("DB", 0.5, 1)
PlotProp("DB", 0.5, 0)

selList
PlotProp(0.05, 5, 0.01, c(0.4,0.45))  

PlotProp(0.05, 15, 0.01, c(0,1))  

PlotProp(0.05, 30, 0.01, c(0,1))  
# -> This one (above is pretty odd!!)
PlotDensity(0.05, 30, 0.01, c(0,1))  
# But likely problem with NAs

PlotProp(0.05, 5, 0.1, c(0,1))  

PlotProp(0.05, 15, 0.1, c(0.2,0.5))  
# Good one too
PlotDensity(0.05, 15, 0.1, c(0.,1))  

PlotProp(0.05, 30, 0.1, c(0,1))  

PlotProp(0.05, 5, 0.2, c(0,1))  

PlotProp(0.05, 15, 0.2, c(0,1))  
# Increases as well
PlotDensity(0.05, 15, 0.2, c(0,1))  

PlotProp(0.05, 30, 0.2, c(0,1))  


PlotProp(0.1, 15, 0.1, c(0.,0.2))  
PlotProp(0.5, 15, 0.1, c(0.,1))  

PlotProp(0.1, 15, 0.2, c(0,0.2))  
PlotProp(0.5, 15, 0.2, c(0,1))  

#selList <- c(0.05, 0.1, 0.5)
#mBList <- c(5.0, 15.0, 30.0)
#dList <- c(0.01, 0.1, 0.2)

?abline


