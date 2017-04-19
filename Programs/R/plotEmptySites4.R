dev.off()

# New script because too many different parameters to deal with

# for mutt in 0.001 0.01 0.1 0.25
# do
# for mB in 5.0 15.0 30.0
# do
# for mp in 0.45
# do
# for mig in 0.01 0.1 0.2 0.3 0.4 0.5 0.75 0.9 #0.01 0.025 0.05 0.075 0.1 0.125 0.15 0.2 0.25 0.3 0.35 0.4 0.45 0.5
# do
# for omega in 0.05 0.1 0.5
# do
# for death in 0.01 0.1 0.2

specify_decimal <- function(x, k) format(round(x, k), nsmall=k)

# 5: B = 1, D = 0.2
# Parameters tested
mutList <- c(0.001, 0.01, 0.1, 0.25)
migList <- c(0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 0.9)
selList <- c(0.05, 0.1, 0.5)
mBList <- c(5.0, 15.0, 30.0)
dList <- c(0.01, 0.1, 0.2)

pars <- expand.grid(mu=mutList, mig=migList, sel=selList, mB = mBList, d = dList)

# Load simulation data
getSimData <- function(mu, mig, sel, mB, d){
#  Scripts/EmptySites_${mig}_${mB}_${mp}_${mutt}_${omega}_${death}.sh
  fileName <- paste0("../C/Results/EmptySites_",mig,"_", specify_decimal(mB, 1), "_0.45_",mu,"_",sel, "_", d, ".txt")
  m <- read.table(fileName)
  names(m) <- c("freqA", "totpopprop")
  return(m)
}
res <- lapply(seq_len(nrow(pars)), function(i) do.call(getSimData, pars[i,]))
# Notes: this takes time because the files are big


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
PlotProp <- function(sel, mB, d, ylim){
  par(las = 1)
  plot(0, type = "n", xlim = c(0,max(migList)), ylim = ylim, 
       xlab = "Emigration m", ylab = "Proportion of altruists", axes = FALSE)
  title(main = paste0("sel=", sel, ", mB=", mB, ", d=", d))
  colMut <- seq_along(mutList)
  rect(0, 0, par("usr")[2], par("usr")[4], col = 
         "grey")
  for(i in seq(0,1,by=0.01)) abline(h=i, lty=1, col=gray(1))
  abline(h=0.45, col="black", lty=2)
  for(imu in  seq_along(mutList)){
    subdata <- data[data$mu == mutList[imu],]
    sub <- subdata[subdata$sel== sel & subdata$mB==mB & subdata$d == d, ]
    points(sub$mig, sub$freqA, col = colMut[imu], type="p", pch=4)
    arrows(sub$mig, sub$freqA - sub$DCIfreqA, sub$mig, sub$freqA + sub$DCIfreqA, col = colMut[imu], angle = 90, length = 0.1, code = 3)
  }
  axis(1, pos = 0)
  axis(2, pos = 0)
}

PlotDensity <- function(sel, mB, d, ylim){
  plot(0, type = "n", xlim = c(0,max(migList)), ylim = ylim, 
       xlab = "Emigration m", ylab = "Proportion of sites", axes = FALSE)
  title(main = "Population density")
  rect(0, 0, par("usr")[2], par("usr")[4], col = 
         "grey")
  for(i in seq(0,1,by=0.01)) abline(h=i, lty=1, col=gray(1))
  for(imu in  seq_along(mutList)){
    subdata <- data[data$mu == mutList[imu],]
    sub <- subdata[subdata$sel== sel & subdata$mB==mB & subdata$d == d, ]
    points(sub$mig, sub$totpopprop, col = colMut[imu], pch=imu)
    axis(1, pos = 0)
    axis(2, pos = 0)
  }
}

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


