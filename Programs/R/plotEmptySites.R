dev.off()

m <- read.table("../C/TEST.txt")
names(m) <- c("freq", "totpop")
plot(m$freq, ylim=c(0,1))
points(m$totpop, col=2)
mean(m$freq)

# TEST
x <- runif(10000)
lambda <- 1/2
z <- -log(x)/lambda
hist(z)
mean(z)
