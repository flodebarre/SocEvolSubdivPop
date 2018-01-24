v <- rpois(10^7, 200)

mean(v)
var(v)

v <- rpois(10^8, 200)

200-mean(v)
200-var(v)



v2 <- runif(10^8)
1/2-mean(v2)

testPoisson <- function(nrep=10^8, mv=200){
  v <- rpois(nrep, mv)
  return(c(m=mean(v), v = var(v)))
}

testvals <- seq(2, 1000, by = 10)
testP <- matrix(0, ncol = 3, nrow = length(testvals))
for(i in seq_along(testvals)){
  cat(i, " ")
  testP[i,] <- c(testvals[i], testPoisson(mv=testvals[i], nrep = 10^7))  
}

dx <- max(abs(c(testP[,2], testP[,3])-testP[,1]))
plot(testP[,1], testP[,2]-testP[,1], col = 2, type = "p", ylim = c(-dx, dx))
points(testP[,1], testP[,3]-testP[,1], col = 3, type = "p", pch = 2)

plot(testP[,1], abs(testP[,2]-testP[,1]), col = 2, type = "p")

# Relative difference
plot(testP[,1], abs(testP[,2]-testP[,1])/testP[,1], col = 2, type = "p")

plot(testP[,1], abs(testP[,2]-testP[,1]), col = 2, type = "p", ylim = c(0, dx))
points(testP[,1], abs(testP[,3]-testP[,1]), col = 3, type = "p", pch = 2)

# Difference between mean and variance
plot(testP[,1], testP[,2]-testP[,3])

