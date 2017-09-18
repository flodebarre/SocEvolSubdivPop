dev.off()

# New script because too many different parameters to deal with

# Parameters tested
mutList <- c(0.001, 0.01, 0.1, 0.25)
migList <- c(0.025, 0.075, 0.15, 0.3, 0.4, 0.125, 0.175, 0.25, 0.35, 0.45)
selList <- c(0.05)
pars <- expand.grid(mu=mutList, mig=migList, sel=selList)

# Load simulation data
getSimData <- function(mu, mig, sel){
  fileName <- paste0("../C/Results/EmptySites4_",mig,"_15.0_0.45_",mu,"_",sel,".txt")
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
data1 <- cbind(pars, mB = 15, baselineD = 0.01, baselineB=1.0, tabMeans, tabDCI)
data <- data1

################################################################
## PLOTTING

# PROPORTION OF ALTRUISTS IN THE POPULATION
# nA / (nA + nB)
par(las = 1)
plot(0, type = "n", xlim = c(0,max(migList)), ylim = c(0.,1), 
     xlab = "Emigration m", ylab = "Proportion", axes = FALSE)
title(main = "Proportion of altruists")
colMut <- seq_along(mutList)
rect(0, 0, par("usr")[2], par("usr")[4], col = 
       "grey")
for(i in seq(0,1,by=0.01)) abline(h=i, lty=1, col=gray(1))
abline(h=0.45, col="black", lty=2)
for(imu in  seq_along(mutList)){
  subdata <- data[data$mu == mutList[imu],]
  sub <- subdata
  points(sub$mig, sub$freqA, col = colMut[imu], type="o", pch=4)
  arrows(sub$mig, sub$freqA - sub$DCIfreqA, sub$mig, sub$freqA + sub$DCIfreqA, col = colMut[imu], angle = 90, length = 0.1, code = 3)
}
axis(1, pos = 0)
axis(2, pos = 0)


?abline


plot(0, type = "n", xlim = c(0,max(migList)), ylim = c(0,1), 
     xlab = "Emigration m", ylab = "Proportion of sites")
title(main = "Population density")
rect(0, 0, par("usr")[2], par("usr")[4], col = 
       "grey")
for(i in seq(0,1,by=0.01)) abline(h=i, lty=1, col=gray(1))
for(imu in  seq_along(mutList)){
  subdata <- data[data$mu == mutList[imu],]
  points(subdata$mig, subdata$totpopprop, col = colMut[imu])
}


migList
m <- read.table("../C/TEST.txt")
plot(m$freq, ylim=c(0,1))
points(m$totpop, col=2)
mean(m$freq)

# TEST
x <- runif(10000)
lambda <- 1/2
z <- -log(x)/lambda
hist(z)
mean(z)
