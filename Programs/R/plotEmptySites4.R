##
## PLOTTING SIMULATIONS WITH EMPTY SITES
## (SI)
##

# Close all windows and clear memory
for (i in dev.list())dev.off()
rm(list=ls())

source("globalGraphParms.R")

# 5: B = 1, D = 0.2
# Parameters tested
mutList <- c(0.001, 0.01, 0.1, 0.25)
migList <- c(0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
selList <- c(0.005, 0.01, 0.05, 0.1, 0.5)
mBList <- c(5.0, 15.0, 30.0)
dList <- c(0.01, 0.1, 0.2)
thep <- 0.45

# All parameter combinations
pars <- expand.grid(mu=mutList, mig=migList, sel=selList, mB = mBList, d = dList)

# Function to print decimals even when .0
specify_decimal <- function(x, k) format(round(x, k), nsmall=k)

# Load simulation data
getSimData <- function(mu, mig, sel, mB, d){
#  Scripts/EmptySites_${mig}_${mB}_${mp}_${mutt}_${omega}_${death}.sh
  fileName <- paste0("../C/Results/EmptySites_",mig,"_", specify_decimal(mB, 1), "_", thep, "_",mu,"_",sel, "_", d, ".txt")
  m <- read.table(fileName)
  names(m) <- c("freqA", "totpopprop")
  return(m)
}
cat('Loading simulation data (takes about 5 minutes)...\n')
res <- lapply(seq_len(nrow(pars)), function(i) do.call(getSimData, pars[i,]))
# Note: this takes time because the files are big
cat('... data loaded!\n')

# Compute the means
simMeans <- lapply(seq_len(nrow(pars)), function(i) {colMeans(res[[i]])})
# Delta for confidence interval
simDCI <- lapply(seq_len(nrow(pars)), function(i){
  mm <- colMeans(res[[i]])
  nn <- nrow(res[[i]])
  return(1.96 * sqrt(mm*(1-mm)/nn))
})

# Reformat as table
tabMeans <- as.data.frame(matrix(unlist(simMeans), ncol = 2, byrow = TRUE))
names(tabMeans) <- c("freqA", "totpopprop")

# Reformat as table (DCI)
tabDCI <- as.data.frame(matrix(unlist(simDCI), ncol = 2, byrow = TRUE))
names(tabDCI) <- c("DCIfreqA", "DCItotpopprop")

# Check reformatting is OK
tabMeans[c(1,2),]
simMeans[[2]]

# Put in table with parameters
data1 <- cbind(pars, baselineB=1.0, tabMeans, tabDCI)
data <- data1

################################################################
## PLOTTING

# PROPORTION OF ALTRUISTS IN THE POPULATION
# nA / (nA + nB)
PlotProp <- function(sel, mB, d, ylim, byLines = 0.1, cexlab = 1.6, cexpoints = 1.6){
  par(las = 1)
  # Initialize plot window
  plot(0, type = "n", xlim = c(0,max(migList)), ylim = ylim, 
       xlab = "", ylab = "", axes = FALSE)
  #  title(main = "Population density")
  mtext(side = 2, "Proportion of altruists", line = 1.5, las=0, cex=cexlab)
  mtext(side = 1, expression(paste("Emigration probability (",italic(m),")")), line = 1.5, las=0, cex=cexlab)
  #  title(main = paste0("sel=", sel, ", mB=", mB, ", d=", d))
  rect(0, ylim[1], par("usr")[2], min(1, par("usr")[4]), col = 
         rectColor, border = NA)
  # Horizontal lines on the graph
  for(i in seq(0,1,by=byLines)) abline(h=i, lty=rectLlty, col=rectLines)
  # Horizontal line at p
  lines(c(0, par("usr")[2]), rep(thep,2), col="black", lty=2)
  
  # Colors
  colMut <- mygradient
  
  for(imu in  seq_along(mutList)){
    # Extract the data that correspond to the parameters
    subdata <- data[data$mu == mutList[imu],]
    sub <- subdata[subdata$sel== sel & subdata$mB==mB & subdata$d == d, ]
    # Plot average freq of A
    points(sub$mig, sub$freqA, col = colMut[imu], type="p", pch=pchs[imu], cex = cexpoints, bg=MakeTransparent(colMut[imu]))
    # Plot CI
    arrows(sub$mig, sub$freqA - sub$DCIfreqA, sub$mig, sub$freqA + sub$DCIfreqA, col = colMut[imu], angle = 90, length = 0.1, code = 3)
  }
  axis(1, pos = ylim[1], at = seq(0,1,by=0.2))
  axis(2, pos = 0)
}

PlotDensity <- function(sel, mB, d, ylim, byLines = 0.1, cexlab = 1.6, cexpoints = 1.6){
  plot(0, type = "n", xlim = c(0,max(migList)), ylim = ylim, 
       xlab = "", ylab = "", axes = FALSE)
#  title(main = "Population density")
  mtext(side = 2, "Proportion of altruists", line = 1.5, las=0, cex=cexlab)
  mtext(side = 1, expression(paste("Emigration probability (",italic(m),")")), line = 1.5, las=0, cex=cexlab)
        
    rect(0, ylim[1], par("usr")[2], min(1, par("usr")[4]), col = 
         rectColor, border = NA)
  # Horizontal lines on the graph
  for(i in seq(0,1,by=byLines)) abline(h=i, lty=rectLlty, col=rectLines)

  colMut <- mygradient
  
  for(imu in  seq_along(mutList)){
    subdata <- data[data$mu == mutList[imu],]
    sub <- subdata[subdata$sel== sel & subdata$mB==mB & subdata$d == d, ]
    points(sub$mig, sub$totpopprop, col = colMut[imu], pch=pchs[imu], bg = MakeTransparent(colMut[imu]), cex = cexpoints)
    # Plot CI
    arrows(sub$mig, sub$totpopprop - sub$DCItotpopprop, sub$mig, sub$totpopprop + sub$DCItotpopprop, col = colMut[imu], angle = 90, length = 0.1, code = 3)
    
  }
  axis(1, pos = ylim[1], at = seq(0,1,by=0.2))
  axis(2, pos = 0)
}

# Function to plot the distribution of values as function of time
# (to identify potential problems)
PlotDist <- function(sel, mB, d, mu, mig){
  # Get the index of the parameter combination
  subdata <- data[data$mu == mu & data$mig == mig,]
  sub <- subdata[subdata$sel== sel & subdata$mB==mB & subdata$d == d, ]
  iparm <- as.numeric(rownames(sub))
#  halfcell <- 0.05
  par(mfrow=c(2,1))
#    hist(res[[iparm]]$freqA, breaks = seq(-halfcell, 1+halfcell, by = 2*halfcell))
#    hist(res[[iparm]]$totpopprop, breaks = seq(-halfcell, 1+halfcell, by = 2*halfcell))
  plot(res[[iparm]]$freqA, type="l", ylim = c(-0.1,1.1))
  plot(res[[iparm]]$totpopprop, type = "l", ylim = c(-0.1, 1.1))
  par(mfrow=c(1,1))
}

PlotFDpdf <- function(sel, mB, d, mu){
  prefix <- paste0("Pics/Emptysites_", sel, "_", mB, "_", d, "_", mu)
  wpdf <- 4.5
  hpdf <- 5.5
  thecex <- 1.2
  marpdf <- c(2.5, 2.5, 0., 0.)+0.1
  pdf(file=paste0(prefix, "_freq.pdf"), width = wpdf, height = hpdf)
    par(mar = marpdf, las = 1, cex = thecex)
    par(mgp=c(3, .6, 0)) # Position of the tick labels
    PlotProp(sel, mB, d, mu)
  dev.off()
  pdf(file=paste0(prefix, "_dens.pdf"), width = wpdf, height = hpdf)
  par(mar = marpdf, las = 1, cex = thecex)
  par(mgp=c(3, .6, 0)) # Position of the tick labels
  PlotDensity(sel, mB, d, mu)
  dev.off()
  system(paste0("xdg-open ", prefix, "_freq.pdf"))
  system(paste0("xdg-open ", prefix, "_dens.pdf"))
}

PlotFDpdf(0.05, 15, 0.01, c(0.,1))
PlotFDpdf(0.05, 15, 0.1, c(0.,1))

ssel <- 0.01
PlotProp(ssel, 5, 0.01, c(0.45,0.5))  
PlotProp(ssel, 15, 0.01, c(0.3,0.5))  
PlotProp(ssel, 30, 0.01, c(0.3,0.5))  

PlotProp(ssel, 5, 0.1, c(0.3,0.5))  
PlotProp(ssel, 15, 0.1, c(0.3,0.5))  
PlotProp(ssel, 30, 0.1, c(0.3,0.5))  

PlotProp(ssel, 5, 0.2, c(0.3,0.5))  
PlotProp(ssel, 15, 0.2, c(0.3,0.5))  
PlotProp(ssel, 30, 0.2, c(0.3,0.5))  


ssel <- 0.005
PlotProp(ssel, 5, 0.01, c(0.45,0.5))  
PlotProp(ssel, 15, 0.01, c(0.3,0.5))  
PlotProp(ssel, 30, 0.01, c(0.3,0.5))  

PlotProp(ssel, 5, 0.1, c(0.3,0.5))  
PlotProp(ssel, 15, 0.1, c(0.3,0.5))  
PlotProp(ssel, 30, 0.1, c(0.3,0.5))  

PlotProp(ssel, 5, 0.2, c(0.3,0.5))  
PlotProp(ssel, 15, 0.2, c(0.3,0.5))  
PlotProp(ssel, 30, 0.2, c(0.3,0.5))  

ssel <- 0.05
PlotProp(ssel, 5, 0.01, c(0.,1))  
# Interesting one
PlotProp(ssel, 15, 0.01, c(0.,1))  
PlotDensity(ssel, 15, 0.01, c(0.,1))  

PlotProp(ssel, 30, 0.01, c(0.,1))  # Simulation problems

PlotProp(ssel, 5, 0.1, c(0.,1))  
# Interesting one
PlotProp(ssel, 15, 0.1, c(0.,1))  
PlotDensity(ssel, 15, 0.1, c(0.,1))  

PlotProp(ssel, 30, 0.1, c(0.,1))  

PlotProp(ssel, 5, 0.2, c(0.,1))  
PlotProp(ssel, 15, 0.2, c(0.,1))  
PlotProp(ssel, 30, 0.2, c(0.,1))  


PlotDensity(0.005, 30, 0.2, c(0,1))  

PlotDist(0.005, 15, 0.1, mutList[4], 0.1)  

PlotProp(0.05, 5, 0.01, c(0.4,0.45))  

PlotProp(0.05, 15, 0.01, c(0,1))  
PlotDensity(0.05, 15, 0.01, c(0,1))  

PlotProp(0.05, 30, 0.01, c(0,1))  
# -> This one (above is pretty odd!!)
PlotDensity(0.05, 30, 0.01, c(0,1))  
# But likely problem with NAs
# Not problem with population sizes
PlotDist(0.05, 30, 0.01, mutList[4], 0.1)

PlotProp(0.05, 5, 0.1, c(0,1))  

PlotProp(0.05, 15, 0.1, c(0.,1))  
# Good one too
PlotDensity(0.05, 15, 0.1, c(0.,1))  
PlotDist(0.05, 15, 0.1, mutList[4], 0.1)


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


