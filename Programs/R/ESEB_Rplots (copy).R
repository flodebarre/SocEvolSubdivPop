rm(list = ls())
for(i in dev.list())dev.off()
source("../Mathematica/analyticsQ.R")
source("globalGraphParms.R")
prefix <- "Pics/Rplot"

muts <- c(0.0000000001, 0.001, 0.01, 0.1, 0.25)
thed <- 15 # Number of demes in the population 
migpoints <- seq(0, 1-1/thed, length.out = 501)
npts <- length(migpoints)

colmut <- c(colnomut, rev(mygradient4[(5-length(muts)):4]))
ltys <- c(2, rep(1, length(muts)-1))

lwdfig <- c(3.5, 2)


initplotQ <- function(savepdf = FALSE, LC = ""){
  if(savepdf){
    filename <- paste0(prefix, LC, ".pdf")
    pdf(filename, width = wpdfQ, height = hpdfQ)
  }
  par(las = 1, fg = fgcol, bg = bgcol, mar = marpdfQ, cex = cexplot, cex.axis <- cexaxis)
  par(mgp=mgppdf) # Position of the tick labels
  
  plot(0, type = "n", ylim = c(0, 1), xlim = c(0, max(migpoints)), axes = FALSE, ylab = "", xlab = "")
  axis(1, pos = 0)
  axis(2, pos = 0)
  mtext("Emigration probability m", 1, line = 1.5, cex = cexlab)
  mtext("Relatedness R", 2, line = 1.5, las = 3, cex = cexlab)
}
closeplotQ <- function(savepdf = FALSE, LC = ""){
  if(savepdf){
    dev.off()
    system(paste0("xdg-open ", prefix, LC, ".pdf"))
    }
}

adjy <- c(0, 1, 3)
legtxt <- rep(0, 3)

plotlinesQ <- function(LC){
  yI <- yO <- as.list(muts)
  for (i in rev(seq_along(muts))){
    yI[[i]][1:npts] <- get(paste0("Qin", LC))(p = 0.45, sel = 0, mut = muts[i], g = 0, n = 4, d = thed, Idself = 1, Ieself = 0, m = migpoints)
    yO[[i]][1:npts] <- get(paste0("Qout", LC))(p = 0.45, sel = 0, mut = muts[i], g = 0, n = 4, d = thed, Idself = 1, Ieself = 0, m = migpoints)
    lines(migpoints, (yI[[i]] - yO[[i]])/(1 - yO[[i]]), type = "l", col = colmut[i], lty = ltys[i], lwd = lwdfig[1])
  }
  midpt <- floor(npts/2.5)
  deltaadj <- 0.5
  xleg <- 0.5
  yleg <- 0.9
  legend(xleg, yleg, col = colmut, legend = round(muts, 5), lty = ltys, lwd = rep(lwdfig[1], 3), bty = "n", cex = 1)
  text(xleg, yleg, adj = c(-2, 0.5), labels = expression(mu * " ="))
}

for(LC in c("M", "WF")){
  initplotQ(dopdf, LC = LC)
  plotlinesQ(LC)
  closeplotQ(dopdf, LC = LC)
}


