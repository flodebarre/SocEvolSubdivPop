m <- read.csv("save_probasameparent.csv")

par(las = 1)
plot(range(m$dsizes), rep(0,2), ylim = c(0, 0.5), type = "n", 
     xlab = "Deme size", ylab = "Proba IBD")
curve(1/x, from = min(m$dsizes), to = max(m$dsizes), add = TRUE)
points(m$dsizes, m$mP1, col = 2, pch = 1)
points(m$dsizes, m$mP2, col = 3, pch = 1)
points(m$dsizes, m$mC1, col = 2, pch = 2)
points(m$dsizes, m$mC2, col = 3, pch = 2)


dx <- max(c(abs((m$mP1 - 1/m$dsizes))/(1/m$dsizes), abs((m$mP2 - 1/m$dsizes))/(1/m$dsizes)))
plot(range(m$dsizes), rep(0,2), type = "l", lty = 2, ylim = c(-dx, dx), xlab = "Deme size", ylab = "(n Estimate - 1)")
points(m$dsizes, (m$mP1 - 1/m$dsizes)/(1/m$dsizes), col = 2)
points(m$dsizes, (m$mP2 - 1/m$dsizes)/(1/m$dsizes), col = 3)
points(m$dsizes, (m$mC1 - 1/m$dsizes)/(1/m$dsizes), col = 2, pch = 2)
points(m$dsizes, (m$mC2 - 1/m$dsizes)/(1/m$dsizes), col = 3, pch = 2)

