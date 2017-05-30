rm(list = ls())
for(i in dev.list())dev.off()
source("../Mathematica/analyticsQ.R")
source("globalGraphParms.R")
migpoints <- seq(0, 1, length.out = 501)
npts <- length(migpoints)
muts <- c(10^(-4), 0.005, 0.1)

colmut <- c("#152D7B", "#234BCD", "#976C52")
ltys <- c(1, 2)
lwdfig <- 2.5

cexplot <- 1.3
cexlab <- 1.4
cexaxis <- 1
dopdf <- TRUE

wpdfQ <- 5
hpdfQ <- 4.25
marpdfQ <- c(3, 3, 0.5, 0)+0.2
initplotQ <- function(savepdf = FALSE, LC = ""){
  if(savepdf){
    filename <- paste0("Pics/Qplot", LC, ".pdf")
    pdf(filename, width = wpdfQ, height = hpdfQ)
  }
  par(las = 1, fg = fgcol, bg = bgcol, mar = marpdfQ, cex = cexplot, cex.axis <- cexaxis)
  par(mgp=mgppdf) # Position of the tick labels
  
  plot(0, type = "n", ylim = c(0, 1), xlim = c(0, 1), axes = FALSE, ylab = "", xlab = "")
  axis(1, pos = 0)
  axis(2, pos = 0)
  mtext("Emigration probability m", 1, line = 1.5, cex = cexlab)
  mtext("Probabilities of identity by descent", 2, line = 1.5, las = 3, cex = cexlab)
}
closeplotQ <- function(savepdf = FALSE, LC = ""){
  if(savepdf){
    dev.off()
    system(paste0("xdg-open ", "Pics/Qplot", LC, ".pdf"))
    }
}
plotlinesQ <- function(LC){
  yI <- yO <- as.list(muts)
  for (i in seq_along(muts)){
    yI[[i]][1:npts] <- get(paste0("Qin", LC))(p = 0.45, sel = 0, mut = muts[i], g = 0, n = 4, d = 30, Idself = 1, Ieself = 0, m = migpoints)
    yO[[i]][1:npts] <- get(paste0("Qout", LC))(p = 0.45, sel = 0, mut = muts[i], g = 0, n = 4, d = 30, Idself = 1, Ieself = 0, m = migpoints)
    lines(migpoints, yI[[i]], type = "l", col = colmut[i], lty = ltys[1], lwd = lwdfig)
    lines(migpoints, yO[[i]], type = "l", col = colmut[i], lty = ltys[2], lwd = lwdfig)
    if(yI[[i]][npts] > 0.95) adjy <- 1.25 else adjy <- -0.3
    text(1, yI[[i]][npts], labels = substitute(mu * "=" * MM, list(MM = muts[i])), adj = c(1, adjy), col = colmut[i])
  }
  midpt <- floor(npts/2.5)
  deltaadj <- 0.5
  text(migpoints[midpt], yI[[2]][midpt], labels = "Qin", adj = c(0, 0 - deltaadj))
  text(migpoints[midpt], yO[[2]][midpt], labels = "Qout", adj = c(0, 1 + deltaadj))
}


#par(mfrow = c(1,2 ))
# Moran
# Initiate plot
for(LC in c("M", "WF")){
  initplotQ(dopdf, LC = LC)
  plotlinesQ(LC)
  closeplotQ(dopdf, LC = LC)
}
