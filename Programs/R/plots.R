## The results are in files
## ../C/Results/s1${updating}_${mig}_${mB}_${mp}_${mutt}_${omega}.txt
##
## And the parameters are
##
## mutt in 0.01 0.1 0.25
## mB in 15.0
## mp in 0.45
## mig in 0.01 0.05 0.1 0.2 0.5 0.025 0.075 0.15 0.3 0.4
## omega in 0.005 0.05 0.5
## updating in DB BD WF
##

# INITIALIZATIONS ------------------------------------------------------------
# Clear memory
rm(list=ls())

# Clear graphs
if(length(dev.list())>0){
  for (i in dev.list()) dev.off()
}

# Check working directory
if(!file.exists("_README_R.txt")) stop("You need to set the working directory to the folder where the R scripts are. Use setwd().")

# Source parameters and functions
source("common.R")

## PARAMETERS
# Whether interactions are with others only ("1") or
# interactions with self are allowed ("0")
noself <- 1

# Structure of the population
DemeSize <- 3 # Number of individuals in each deme
NbDemes <- 4 # Number of demes
theN.island <- DemeSize * NbDemes # Size of the population 

migrange <- c(10^-6, 0.55) # Range of migration values for the plot

## COMBINE ALL PARAMETERS
allparms <- expand.grid(mig=miglist, mB = Blist, p=plist, mu=mulist, updaterule=updatelist, noselfinteractions=noself, omega=omegalist)

# LOAD DATA ------------------------------------------------------------
# Define function to load data
loaddata <- function(mig, mB, p, mu, updaterule, noselfinteractions, omega){
  N <- theN.island
  mb <- sprintf("%.1f", mB); 
  filename <- paste(pathtodata, "s", noselfinteractions, updaterule, "_", mig, "_", mb, "_", p, "_", mu, "_", omega, ".txt", sep="")
  if(file.exists(filename)) return(unlist(c(read.table(filename))))
  else{ # Print a message if the file has not been found, and return 0s.
    cat("File ", filename, " not loaded.\n")
    return(rep(0,N+1)); 
  }
}

# Define function to load data and return mean trait
datamean <- function(mig, mB, p, mu, updaterule, noselfinteractions, omega){
  N <- theN.island # Population size
  v <- loaddata(mig, mB, p, mu, updaterule, noselfinteractions, omega) # Load data
  p1 <- (0:N)/N # Values of population size in the data
  m <- sum(p1*v)/sum(v)
  return(c(m=m, nb=sum(v))) # and nb is number of simulations
}

# Load data and compute mean trait
allmeans <- t(vapply(seq_len(nrow(allparms)), #
                            function(i) do.call(datamean, allparms[i,]), #
                            FUN.VALUE = c(0,0)))

# Combine with corresponding parameters
res <- cbind(allparms, m=allmeans[,1], nb=allmeans[,2])

print(res)

## PLOTTING FUNCTIONS

EXplot.mig <- function(res, b, c=1, p, updaterule, mu, omega, N=DemeSize, D=NbDemes, xrange=migrange, plotsims=TRUE,...){
    simp <- res[res$mB==b & res$p==p & res$updaterule==updaterule & res$mu==mu & res$omega==omega, ]
    print(simp$m)
    points(simp$mig, simp$m, pch=1, ...) # Plot points 
}

initplot.mig(yline=plist, xlim = migrange, ylim=c(0., 0.6))
for (i in seq_along(mulist)) {
  EXplot.mig(res, b=Blist[1], p=0.45, updaterule = "DB", mu=mulist[i], omega = omegalist[3], type="o", col=i)
}

