dev.off()

# # To run the scripts on my own ComputeRates
# for mutt in 0.001 0.01 0.1 0.25
# do
# for mB in 15.0
# do
# for mp in 0.45
# do
# for mig in 0.025 0.075 0.15 0.3 0.4 0.125 0.175 0.25 0.35 0.45
# do
# for omega in 0.05
# do
# 
# # Change parameters
# #  Change the mp parameter
# sed -e "s/XXXX/${mp}/" -e "s/BBBB/${mB}/" -e "s/MMMM/${mutt}/" -e "s/GGGG/${mig}/" -e "s/OOOO/${omega}/" island_emptysites.c > Scripts/EmptySites_${mig}_${mB}_${mp}_${mutt}_${omega}.c
# 
# # Compile the script
# cc Scripts/EmptySites_${mig}_${mB}_${mp}_${mutt}_${omega}.c -o Scripts/EmptySites_${mig}_${mB}_${mp}_${mutt}_${omega} -lm
# 
# printf "Running simulation EmptySites_${mig}_${mB}_${mp}_${mutt}_${omega}\n"
# # Run the script
# ./Scripts/EmptySites_${mig}_${mB}_${mp}_${mutt}_${omega} > Results/EmptySites_${mig}_${mB}_${mp}_${mutt}_${omega}.txt

# Parameters tested
mutList <- c(0.001, 0.01, 0.1, 0.25)
migList <- c(0.025, 0.075, 0.15, 0.3, 0.4, 0.125, 0.175, 0.25, 0.35, 0.45)
selList <- c(0.05)
pars <- expand.grid(mu=mutList, mig=migList, sel=selList)

# Load simulation data
getSimData <- function(mu, mig, sel){
  fileName <- paste0("../C/Results/EmptySites_",mig,"_15.0_0.45_",mu,"_",sel,".txt")
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
data1 <- cbind(pars, mB = 15, baselineB=1.0, tabMeans, tabDCI)
data1

rm(res, tabMeans, tabDCI)

# Load the second batch of data
# Load simulation data
getSimData2 <- function(mu, mig, sel){
  fileName <- paste0("../C/Results/EmptySites2_",mig,"_15.0_0.45_",mu,"_",sel,".txt")
  m <- read.table(fileName)
  names(m) <- c("freqA", "totpopprop")
  return(m)
}
res2 <- lapply(seq_len(nrow(pars)), function(i) do.call(getSimData2, pars[i,]))
# Notes: this takes time because the files are big


# Compute the means
simMeans2 <- lapply(seq_len(nrow(pars)), function(i) {colMeans(res2[[i]])})
# Delta for confidence interval
simDCI2 <- lapply(seq_len(nrow(pars)), function(i){
  mm <- colMeans(res2[[i]])
  nn <- nrow(res2[[i]])
  return(1.96 * sqrt(mm*(1-mm)/nn))
})

# Reformat as table
tabMeans2 <- as.data.frame(matrix(unlist(simMeans2), ncol = 2, byrow = TRUE))
names(tabMeans2) <- c("freqA", "totpopprop")

# Reformat as table (DCI)
tabDCI2 <- as.data.frame(matrix(unlist(simDCI2), ncol = 2, byrow = TRUE))
names(tabDCI2) <- c("DCIfreqA", "DCItotpopprop")

data2 <- cbind(pars, mB = 15, baselineB=2.0, tabMeans2, tabDCI2)
data2

#data <- rbind(data1, data2)

rm(res2, tabMeans2, tabDCI2)

# Load the third batch of data
# Load simulation data
getSimData3 <- function(mu, mig, sel){
  fileName <- paste0("../C/Results/EmptySites3_",mig,"_5.0_0.45_",mu,"_",sel,".txt")
  m <- read.table(fileName)
  names(m) <- c("freqA", "totpopprop")
  return(m)
}
res3 <- lapply(seq_len(nrow(pars)), function(i) do.call(getSimData3, pars[i,]))
# Notes: this takes time because the files are big


# Compute the means
simMeans3 <- lapply(seq_len(nrow(pars)), function(i) {colMeans(res3[[i]])})
# Delta for confidence interval
simDCI3 <- lapply(seq_len(nrow(pars)), function(i){
  mm <- colMeans(res3[[i]])
  nn <- nrow(res3[[i]])
  return(1.96 * sqrt(mm*(1-mm)/nn))
})

# Reformat as table
tabMeans3 <- as.data.frame(matrix(unlist(simMeans3), ncol = 2, byrow = TRUE))
names(tabMeans3) <- c("freqA", "totpopprop")

# Reformat as table (DCI)
tabDCI3 <- as.data.frame(matrix(unlist(simDCI3), ncol = 2, byrow = TRUE))
names(tabDCI3) <- c("DCIfreqA", "DCItotpopprop")

data3 <- cbind(pars, mB = 5, baselineB=1.0, tabMeans3, tabDCI3)

data <- rbind(data1, data2, data3)
data
################################################################
## PLOTTING

# PROPORTION OF ALTRUISTS IN THE POPULATION
# nA / (nA + nB)
par(las = 1)
plot(0, type = "n", xlim = c(0,max(migList)), ylim = c(0,1), 
     xlab = "Emigration m", ylab = "Proportion", axes = FALSE)
title(main = "Proportion of altruists")
colMut <- seq_along(mutList)
rect(0, 0, par("usr")[2], par("usr")[4], col = 
       "grey")
for(i in seq(0,1,by=0.01)) abline(h=i, lty=1, col=gray(1))
abline(h=0.45, col="black", lty=2)
for(imu in  seq_along(mutList)){
  subdata <- data[data$mu == mutList[imu],]
  sub <- subdata[subdata$baselineB==1 & subdata$mB==15,]
  points(sub$mig, sub$freqA, col = colMut[imu], type="o")
  arrows(sub$mig, sub$freqA - sub$DCIfreqA, sub$mig, sub$freqA + sub$DCIfreqA, col = colMut[imu], angle = 90, length = 0.1, code = 3)
  # 
  # sub <- subdata[subdata$baselineB==2 & subdata$mB==15,]
  # points(sub$mig, sub$freqA, col = colMut[imu], type="o", pch=2)
  # arrows(sub$mig, sub$freqA - sub$DCIfreqA, sub$mig, sub$freqA + sub$DCIfreqA, col = colMut[imu], angle = 90, length = 0.1, code = 3)
  
  # sub <- subdata[subdata$baselineB==1 & subdata$mB==5,]
  # points(sub$mig, sub$freqA, col = colMut[imu], type="o", pch=4)
  # arrows(sub$mig, sub$freqA - sub$DCIfreqA, sub$mig, sub$freqA + sub$DCIfreqA, col = colMut[imu], angle = 90, length = 0.1, code = 3)
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
