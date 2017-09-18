rm(list = ls())
for(i in dev.list())dev.off()
source("analyticsQ.R")

migpoints <- seq(0, 1, length.out = 501)
npts <- length(migpoints)
muts <- c(10^(-5), 0.005, 0.1)

colmut <- c("#152D7B", "#234BCD", "#976C52")

dopdf <- TRUE

initplotQ <- function(savepdf = FALSE){
  if(savepdf){
    filename <- "../Pics"
    pdf()
  }
  par(las = 1)
  plot(0, type = "n", ylim = c(0, 1), xlim = c(0, 1), axes = FALSE, ylab = "", xlab = "")
  axis(1, pos = 0)
  axis(2, pos = 0)
  mtext("Emigration probability m", 1, line = 2)
  mtext("Probabilities of identity by descent", 2, line = 2, las = 3)
}

plotlinesQ <- function(LC){
  yI <- yO <- as.list(muts)
  for (i in seq_along(muts)){
    yI[[i]][1:npts] <- get(paste0("Qin", LC))(p = 0.45, sel = 0, mut = muts[i], g = 0, n = 4, d = 30, Idself = 1, Ieself = 0, m = migpoints)
    yO[[i]][1:npts] <- get(paste0("Qout", LC))(p = 0.45, sel = 0, mut = muts[i], g = 0, n = 4, d = 30, Idself = 1, Ieself = 0, m = migpoints)
    lines(migpoints, yI[[i]], type = "l", col = colmut[i], lty = ltys[1], lwd = lwdfig)
    lines(migpoints, yO[[i]], type = "l", col = colmut[i], lty = ltys[2], lwd = lwdfig)
    text(1, yI[[i]][npts], labels = substitute(mu * "=" * MM, list(MM = muts[i])), adj = c(1, 1.5), col = colmut[i])
  }
  midpt <- floor(npts/4)
  deltaadj <- 0.5
  text(migpoints[midpt], yI[[2]][midpt], labels = "Qin", adj = c(0, 0 - deltaadj))
  text(migpoints[midpt], yO[[2]][midpt], labels = "Qout", adj = c(0, 1 + deltaadj))
}


ltys <- c(1, 2)
lwdfig <- 2
#par(mfrow = c(1,2 ))
# Moran
# Initiate plot
initplotQ()
plotlinesQ("M")
# Wright-Fisher
initplotQ()
plotlinesQ("WF")
